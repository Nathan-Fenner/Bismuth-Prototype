module Compile where
import Lexer
import AST

import Control.Applicative(Applicative(..))

data CProgram = CProgram
	[CStruct]
	[CUnion]
	-- TODO: instances
	[CFunction] -- all functions (including local anonymous ones)
	[(String, CFunctionName)] -- global func bindings (bismuth name, compiled name)

data CStruct = CStruct
	String -- NAME
	[String] -- FIELDS (all of type BismuthObject)


data CUnion = CUnion
	String -- NAME
	[(String, Int)] -- VARIANTS (NAME & ARITY)

-- Note: unions are all (Long, BismuthObject) pair.

-- TODO: trait definition (they're structs of BismuthObject)
-- TODO: trait providers (they produce trait instances, possibly asking for other instances)

newtype CFunctionName = CFunctionName String deriving (Show, Eq)

data CFunction = CFunction
	CFunctionName -- NAME
	-- TODO: instance parameters are separate (they're more static)
	[String] -- PARAMETERS
	-- The return value is always BismuthObject
	-- TODO: effects
	CBody

data CBody = CBody [CStatement]

data CStatement
	= CDeclare String (Maybe CExpression) -- declares the variable (with the given optional value)
	| CAssign String CExpression -- field access has to be corrected manually
	| CDo CExpression -- runs the expression
	| CIf CExpression CBody CBody
	| CWhile CExpression CBody
	| CReturn CExpression

data CExpression
	= CVar String -- refers to a variable (note: not a function)
	-- TODO: literal instances / instance constructors
	| CDot CExpression String String -- struct and field
	| CFunc String Int -- reference a CFunction with the given number of parameters
	| CConstructStruct {-name of struct-}String {-fields-}[CExpression]
	| CConstructUnion {-variant-}Int {-value-}CExpression
	| CInt Int -- a literal
	| CString String -- note: c_str isn't quite enough, but that's okay

data NameInfo
  = VariableInfo Type
  | TypeInfo Kind {- TODO: instances -}
  | StructInfo StructDefinition
  | UnionInfo UnionDefinition

data Header = Header {hFuncs :: [CFunction], hBindings :: [(String, CFunctionName)], hStructs :: [CStruct], hUnions :: [CUnion]}
data Scope = Scope [(Token, NameInfo)]

addInfo :: Scope -> (Token, NameInfo) -> Checked Scope
addInfo (Scope bindings) (name, info) = case [name' | (name', _) <- bindings, contents name == contents name'] of
	[] -> Right $ Scope ((name, info) : bindings)
	(name' : _) -> Left $ "`" ++ contents name ++ "` at " ++ location name ++ "shadows `" ++ contents name' ++ "` already declared at " ++ location name'

getInfo :: Scope -> String -> Maybe (Location, NameInfo)
getInfo (Scope defs) name = case [(location name', info) | (name', info) <- defs, name == contents name'] of
	(result : _) -> Just result
	[] -> Nothing

type Checked t = Either String t

withContext :: Checked t -> String -> Checked t
withContext (Right x) _ = Right x
withContext (Left message) c = Left (message ++ "\n\t" ++ c)

isContextOf :: String -> Checked t -> Checked t
isContextOf = flip withContext

typeOfFuncDefinition :: FuncDefinition -> Type
typeOfFuncDefinition (FuncDefinition generics arguments returns _) = Type generics term where
	term = build arguments
	build [] = returns
	build ((n, t) : args) = ((TName $ Token "->" (location n)) `TApply` t)  `TApply` build args

compileModule :: (Header, Scope) -> Module -> Checked Header
compileModule (header, scope) (Module declarations) = do
	-- For now, we'll not do parallel errors, since this will take more thought.
	childScope <- buildChildScope declarations scope
	_cFuncs <- checkEach childScope header declarations
	pure undefined -- The module has some tricky details, but they're not interesting enough to bother with yet
	-- etc.
	where
	checkEach _scope' header' [] = Right header'
	checkEach scope' header' (declaration : rest) = do
		header'' <- compileModuleDeclaration (header', scope') declaration
		checkEach scope' header'' rest
	buildChildScope = build where
		build :: [ModuleDeclaration] -> Scope -> Checked Scope
		build [] scope' = Right scope'
		build (x : xs) scope' = addInfo scope' (infoOf x) >>= build xs
	infoOf :: ModuleDeclaration -> (Token, NameInfo)
	infoOf (FuncDeclaration name def) = (name, VariableInfo $ typeOfFuncDefinition def)
	infoOf (StructDeclaration name def) = (name, StructInfo def)
	infoOf (UnionDeclaration name def) = (name, UnionInfo def)
-- data Module = Module [ModuleDeclaration] deriving Show

conflictingNames :: [Token] -> Checked ()
conflictingNames [] = Right ()
conflictingNames (x : xs) = case [x' | x' <- xs, contents x == contents x'] of -- TODO: not quadratic
	[] -> conflictingNames xs
	conflicts -> Left $ "name `" ++ contents x ++ "` at " ++ location x ++ " also appears at " ++ join (map location conflicts)
	where
	join :: [String] -> String
	join [a, b, c] = a ++ ", " ++ b ++ ", and " ++ c
	join [a, b] = a ++ " and " ++ b
	join [a] = a
	join [] = error "cannot join 0 things"
	join (m : ms) = m ++ ", " ++ join ms

compileModuleDeclaration :: (Header, Scope) -> ModuleDeclaration -> Checked Header
compileModuleDeclaration (header, scope) (FuncDeclaration funcName definition) = do
	(header', cReference) <- compileFuncDefinition (header, scope) definition `withContext` ("global function definition for `" ++ contents funcName ++ "`")
	-- We bind the name to the global.
	pure $ header'{ hBindings = (contents funcName, cReference) : hBindings header' }
compileModuleDeclaration (header, scope) (StructDeclaration structName (StructDefinition generics fields)) = ("struct definition for `" ++ contents structName ++ "`") `isContextOf` do
	scope' <- checkGenerics scope generics
	conflictingNames (map fst fields) -- test that none of the names overlap
	mapM_ (checkField scope') fields -- TODO: parallel
	pure $ header{ hStructs = CStruct (contents structName) (map (contents . fst) fields) : hStructs header }
	where
	checkField :: Scope -> (Token, TypeTerm) -> Checked ()
	checkField scope' (name, term) = checkTypeTermConcrete scope' term `withContext` ("in struct field `" ++ contents name ++ "`")
compileModuleDeclaration (header, scope) (UnionDeclaration unionName (UnionDefinition generics variants)) = ("union definition for `" ++ contents unionName ++ "`") `isContextOf` do
	scope' <- checkGenerics scope generics
	conflictingNames (map fst variants) -- test that none of the names overlap
	mapM_ (checkVariant scope') variants -- TODO: parallel
	pure $ header{ hUnions = CUnion (contents unionName) (map (\(n,a) -> (contents n, length a)) variants) : hUnions header }
	where
	checkVariant :: Scope -> (Token, [TypeTerm]) -> Checked ()
	checkVariant scope' (name, args) = mapM_ (\(i, term) -> checkTypeTermConcrete scope' term `withContext` ("argument " ++ show i)) (numbered args) `withContext` ("variant `" ++ contents name ++ "`")
	numbered :: [a] -> [(Int, a)]
	numbered = zip [1..]

checkGeneric :: Scope -> Generic -> Checked Scope
checkGeneric scope (Generic token kind) = addInfo scope (token, TypeInfo kind)

checkGenerics :: Scope -> [Generic] -> Checked Scope -- introduces terms as needed, or errors
checkGenerics scope [] = Right scope
checkGenerics scope (x : xs) = checkGeneric scope x >>= flip checkGenerics xs

checkTypeTermConcrete :: Scope -> TypeTerm -> Checked ()
checkTypeTermConcrete scope term = do
	kind <- checkTypeTerm scope term
	case kind of
		KConcrete -> Right ()
		other -> Left $ "term " ++ "<???>" ++ " is not a concrete kind"

checkTypeTerm :: Scope -> TypeTerm -> Checked Kind
checkTypeTerm scope (TName name) = case getInfo scope (contents name) of
	Nothing -> Left $ "type `" ++ contents name ++ "` is not in scope"
	Just (_, TypeInfo kind) -> pure kind
	Just (locationOf, _) -> Left $ "`" ++ contents name ++ "` defined at " ++ locationOf ++ " is not a type"
checkTypeTerm scope (TApply left right) = do
	funcKind <- checkTypeTerm scope left
	argKind <- checkTypeTerm scope right
	case funcKind of
		KArrow argKind' resultKind -> case argKind == argKind' of
			False -> Left $ "left type term <???> expects an argument of kind <???> but was given an argument of kind <???>, namely right <???>"
			True -> Right resultKind
		KConcrete -> Left $ "left type <???> has kind `type` so it cannot be applied to type <???> of kind <???>"

compileFuncDefinition :: (Header, Scope) -> FuncDefinition -> Checked (Header, CFunctionName)
compileFuncDefinition (header, scope) (FuncDefinition generics arguments returns body) = do
	scope' <- checkGenerics scope generics
	scope'' <- introduceArguments arguments scope'
	pure undefined
	where
	introduceArguments [] scope' = pure scope'
	introduceArguments ((name, term) : rest) scope' = do
		checkTypeTermConcrete scope' term `withContext` ("argument `" ++ contents name ++ "`")
		scope'' <- addInfo scope' (name, VariableInfo (Type [] term))
		introduceArguments rest scope''

data BodyScope = BodyScope {
	bScope :: Scope, -- underlying scope for definitions
	bReturns :: TypeTerm, -- the type that should be returned
	bUninitialized :: [String], -- unusable variables (since they might not be initialized)
	bMutable :: [String] -- list of mutable variables (since closed-over variables, arguments, and globals are immutable)
}

-- Pretty much just have statements and expressions left.
-- Yeah, they're the most complicated part, but the machinery required to
-- compile them is already here.
-- Primarily, we don't even have to do anything fancy because the types will
-- "work out" nicely. Expressions will be given the most general possible type
-- assignable to them, and if a generic value is "lost" or an operation is
-- required that's "too specific" without providing context (like field access)
-- then we reject. There's not really "inference" in this approach, only a small
-- amount of unification to find generic parameters when we've got constraints.

-- Effects is what will make this whole system complicated- but going back and
-- adding them shouldn't be too hard.
-- (This may be a terrible mistake).
-- The main features that are left are effects, their handlers (which are still
-- unclear to me) and constraints. I think constraints will mostly involve
-- mechanical transformations of generics to functions, which shouldn't be too
-- bad. It's a bit awkward because some places might not need them, like
-- structs/unions (it's unclear what benefit there is in these). Alternatively,
-- we can have them and not worry about anything, just need to a do a little
-- extra checking elsewhere, provided that it doesn't break any structure
-- invariants.

-- Of course, there's also sugar syntax we want to support, and I'd like to make
-- errors be able to reference source directly, which will require lots of extra
-- annotation in the AST (that will be a bit annoying to write). It might be
-- more convenient to wrap things up into some sort of fix-point structure to
-- add this easier. Alternatively, just write a pretty-printer and ignore source
-- formatting, especially if it wants to be super-opinionated.

-- The sugars we want to have are:
-- @-syntax for updates (f @x)
-- @-syntax for assignments (y = f @x)

-- "Map" assignment (but also good for arrays) and accesses; foo[x] on the RHS
-- can just become "lookup foo x", but on the LHS it's slightly more complicated
-- with {foo[x] = y} becoming {foo = update foo x y} which isn't so bad (but
-- becomes more complicated-looking with field accesses too).

-- Some sort of range, bounds-checking, or similar feature needs to be added so
-- that safe-indices can exist.

-- A way I thought of to do this was to create an "identity-dependent" type
-- system, wherein types are "dependent" but dependent values can't be
-- introspected, so that they effectively just check flow-based code.

-- For example, you could consider a {Index $vec} type which depends on $vec to
-- be valid. The main issue is that to be convenient, we want subtyping, so that
-- {Index $vec < Index $(push vec x)} for any vec, x. This might be brittle, or
-- it just might not matter. I can see there being greater use for hash-maps and
-- unique-assignment maps (e.g., a hashmap that gives you keys when you insert
-- items, rather than letting you choose them). Such a system is just a vector,
-- really, but semantically easier to work with because you can't range over it
-- or perform random access arbitrarily.

-- "Initiailization patterns" intended to make it easy to do lots of graph- or
-- array-based work might also be a cool feature, but it's unclear whether it
-- will scale.
-- The essential idea is to create a construct which can be applied to a "for"
-- loop to ensure that initialization occurs correctly, something like

-- var output = for new Image over EachPixel (x : Index, y : Index) of image {
--     output[x, y] = image[x, y];
-- }
-- which uses an "EachPixel" pattern for initialization; you can imagine a more
-- complicated one liek
-- var output = for new Image over DownPixels (x : Index, y : index) of image {
--     if y == 0 {
--         output[x, y] = image[x, y];
--         continue;
--     }
--     output[x, y] = average image[x, y] output[x, y-1];
-- }
-- which is guaranteed to never touch "uninitialized" memory (it might actually
-- be initialized for safety/simplicity, but for correctness we can pretend it
-- isn't) and the "iteration pattern" called DownPixels only lets you use pixels
-- above the one you're currently writing.
-- You can imagine similar scheme for traversing graphs, possibly requiring that
-- they possess some (possibly nontrivial) invariants like acyclicness or
-- strongly connectedness.

-- The exact way that such patterns would be implemented is not clear. A simple
-- implementation of the actual order would be a stream of (arbitrary) index
-- values, but the invariants could be difficult to express. Making sure that it
-- is very easy to use these patterns, possibly even with some sort of inference
-- for the pattern of choice, so that the client need not even specify how to
-- loop over their object, at least by default. This might be odd, because it
-- could be difficult to choose from various patterns when they all are
-- satisfied. This can be made a lot easier by putting them into a partial order
-- but there is still the possibility of an ambiguity- forcing the programmer
-- to resolve these cases would be ugly. It would be nice if there was a natural
-- and intrinsic ordering to them, so that specifying it was unneccessary.

-- Such patterns would help make it easier to write correct graph algorithms,
-- because they can often be very difficult to get correct.

-- More type system features we want to have are linear/unique types (linear
-- are actually kinda easy (because we don't need drop-check or lifetimes due
-- to immutability) but annoying to work with; but having @-syntax makes it a
-- lot more tolerable, since "borrows" are then less valuable as a syntax tool.
-- However, it may be that they add some power that I am unaware of to the
-- language.

-- We want to support some kind of safe reflection (or possibly hygenic macro?)
-- and I think the easiest way to do it is to create a Reflective typeclass
-- which is automatically derived for all types, so that any (concrete) type can
-- always be passed to a reflective function. However, I think to be effective,
-- generic functions will need to have lots of higher-order concepts and
-- especially constraints, which may prove annoying. It may be more valuable
-- to instead allow the introspective _creation_ of types to obviate reflection;
-- but this will generally require some form of marshaling back into "regular"
-- values that is tedious and easily mechanizable.

{-
data Block = Block [Statement] deriving Show
data Statement
  = Var Token Type (Maybe Expression)
  | Assign Reference Expression
  | Do Expression
  | If Expression Block Block
  | While Expression Block
  | Return Expression
  deriving Show
  -- TODO: for etc.
data Reference
  = RName Token
  | RDot Reference Token
  deriving Show
  -- TODO: RAccess Reference Expression
data Expression
  = EName Token
  | EDot Expression Token
  -- TODO: | EAccess Expression Expression
  | EApply Expression Expression
  | EConstruct Token [(Token, Expression)]
  | EBang Expression
  | EString String
  | EInt Int
  | EFunc FuncDefinition
  deriving Show
-}
