gh
module Parse where

import Control.Applicative(pure)

import AST
import Lexer
import Parse.Core

parseModule :: Parser Module
parseModule = do
  body <- many parseModuleDeclaration
  pure (Module body)

parseModuleDeclaration :: Checker ModuleDeclaration
parseModuleDeclaration = mux
  [ ("func", makeFunc)
  , ("instance", parseError "`instance` is not implemented")
  ]
  where
  makeFunc :: Parser ModuleDeclaration
  makeFunc = do
    name <- expectClass LexName "expected function name"
    definition <- parseFuncDefinition
    pure $ FuncDeclaration name definition

parseGenerics :: Parser [Generic]
parseGenerics = mux [("[", parseList)] ||| pure [] where
  parseList = do
    first <- parseItem
    rest <- mux
      [(",", parseList)]
      ||| (expectSpecial "]" "expected `]` to close generic list" >> pure [])
    pure $ first : rest
  parseItem = do
    name <- expectClass LexName "expected generic name"
    -- TODO: constraints
    kind <- mux [(":", parseKind)] ||| pure KConcrete
    pure $ Generic name kind

parseFuncDefinition :: Parser FuncDefinition
parseFuncDefinition = do
  generics <- parseGenerics
  arguments <- many parseArgument
  -- TODO: effects
  returns <- checkToken LexOperator "->"
    &&& (\_ -> parseTypeTerm `withContext` "return type")
    ||| (do unit <- hereToken "Unit"; pure (TName unit))
  body <- parseBlock
  pure $ FuncDefinition generics arguments returns body
  where
  parseArgument :: Checker (Token, TypeTerm)
  parseArgument = checkSpecial "(" &&& \open -> do
    argumentName <- expectClass LexName "expected argument name"
    expectSpecial ":" "expected `:` to follow argument name"
    argumentType <- parseTypeTerm
    expectSpecial ")" ("expected `)` to close `(` opened at " ++ location open ++ " for argument `" ++ contents argumentName ++ "`")
    pure $ (argumentName, argumentType)

parseTypeTermAtom :: Checker TypeTerm
parseTypeTermAtom = parseTypeTermName |:| parseTypeTermParens
  where
  parseTypeTermName = do
    name <- checkClass LexName
    pure (TName name)
  parseTypeTermParens = checkSpecial "(" &&& \open -> do
    term <- parseTypeTerm
    expectSpecial ")" ("expected `)` to close `(` at " ++ location open)
    pure term

parseTypeTermApply :: Checker TypeTerm
parseTypeTermApply = parseTypeTermAtom &&& \first -> do
  rest <- many parseTypeTermAtom
  pure (flatten first rest)
  where
  flatten term [] = term
  flatten left (right : rest) = flatten (left `TApply` right) rest

parseTypeTermFunction :: Checker TypeTerm
parseTypeTermFunction = parseTypeTermApply &&& \left ->
  (checkToken LexOperator "->" &&& withArrow left) ||| pure left
  where
  withArrow left arrow = do
    right <- parseTypeTermFunction ||| parseError ("expected term after `->` at " ++ location arrow)
    pure $ (TName arrow `TApply` left) `TApply` right

parseTypeTerm :: Parser TypeTerm
parseTypeTerm = parseTypeTermFunction ||| parseError "expected type term"

parseType :: Parser Type
parseType = do
  generics <- parseGenerics
  term <- parseTypeTerm
  pure (Type generics term)

parseKindAtom :: Checker Kind
parseKindAtom = parseKindConcrete |:| parseKindParens
  where
  parseKindConcrete = do
    _ <- checkToken LexSpecial "type"
    pure KConcrete
  parseKindParens = checkSpecial "(" &&& \open -> do
    kind <- parseKind
    expectSpecial ")" ("expected `)` to close `(` at " ++ location open)
    pure kind

parseKindArrow :: Checker Kind
parseKindArrow = parseKindAtom &&& \left ->
  (checkToken LexOperator "->" &&& withArrow left) ||| pure left
  where
  withArrow left arrow = do
    right <- parseKindArrow ||| parseError ("expected kind after `->` at " ++ location arrow)
    pure $ KArrow left right

parseKind :: Parser Kind
parseKind = parseKindArrow ||| parseError "expected kind"

parseBlock :: Parser Block
parseBlock = do
  open <- checkSpecial "{" ||| parseError "expected `{` to open block"
  body <- many parseStatement
  expectSpecial "}" ("expected `}` to close block opened at " ++ location open)
  pure (Block body)

parseStatement :: Checker Statement
parseStatement = parseIf |:| parseWhile |:| parseVar |:| parseOther where
  parseIf :: Checker Statement
  parseIf = checkSpecial "if" &&& parseIfBody where
    parseIfBody :: Token -> Parser Statement
    parseIfBody ifToken = do
      condition <- parseExpression `withContext` ("for the condition of the `if` statement started at " ++ location ifToken)
      thenBlock <- parseBlock `withContext` ("in an `if` statement started at " ++ location ifToken)
      elseBlock <- checkSpecial "else" &&& parseElse ||| (pure $ Block [])
      pure (If condition thenBlock elseBlock)
      where
      parseElse elseToken = (checkIfElse ||| parseBlock) `withContext` ("in an `else` block at " ++ location elseToken)
      checkIfElse = checkSpecial "if" &&& (\ifToken' -> parseIfBody ifToken' >>= \subIf -> pure $ Block [subIf])
  parseWhile = checkSpecial "while" &&& parseWhileBody where
    parseWhileBody whileToken = do
      condition <- parseExpression `withContext` ("for the condition of the `while` statement started at " ++ location whileToken)
      body <- parseBlock `withContext` ("for the body of the `while` statement started at " ++ location whileToken)
      pure (While condition body)
  parseVar = checkSpecial "var" &&& parseVarBody where
    parseVarBody varToken = do
      varName <- expectClass LexName ("expected variable name after token `var` to declare variable at " ++ location varToken)
      _ <- checkSpecial ":" ||| parseError ("expected `:` to follow name `" ++ contents varName ++ "` in `var` declaration to annotate type")
      varType <- parseType `withContext` ("type of variable `" ++ contents varName ++ "` declared at " ++ location varName)
      assignTo <- checkToken LexOperator "=" &&& (withEquals varName) ||| pure Nothing
      _ <- checkSpecial ";" ||| parseError ("expected `;` to follow declaration of `" ++ contents varName ++ "` at " ++ location varName)
      pure (Var varName varType assignTo)
      where
      withEquals varName equalsToken = fmap Just parseExpression `withContext` ("after `=` at " ++ location equalsToken ++ " in declaration of variable `" ++ contents varName ++ "` at " ++ location varName)
  parseOther = negative (checkSpecial "}") &&& const parseOtherBody where
    parseOtherBody = do
      start <- currentLocation
      expression <- parseExpression
      checkToken LexOperator "=" &&& parseAssignment start expression ||| parseDo start expression
    parseAssignment start left equalsToken = do
      right <- parseExpression
      expectSpecial ";" ("expected `;` to follow assignment started at " ++ start ++ " with equals at " ++ location equalsToken)
      referenceLeft <- convert left
      pure (Assign referenceLeft right)
      where
      convert (EName token) = pure (RName token)
      convert (EDot object field) = do
        object' <- convert object
        pure $ RDot object' field
      convert EApply{} = parseError "cannot use a function application as the left-hand-side of an assignment"
      convert EConstruct{} = parseError "cannot use a constructed literal as the left-hand-side of an assignment"
      convert EBang{} = parseError "cannot use an effect invocation as the left-hand-side of an assignment"
      convert EString{} = parseError "cannot use a string literal as the left-hand-side of an assignment"
      convert EInt{} = parseError "cannot use an integer literal as the left-hand-side of an assignment"
      convert EFunc{} = parseError "cannot use an anonymous function as the left-hand-side of an assignment"
    parseDo start expression = do
       expectSpecial ";" ("expected `;` to follow expression started at " ++ start)
       pure (Do expression)

data OpAssociativity = OpLeft | OpRight deriving Eq

parseExpression :: Parser Expression
parseExpression = parseOperators where
  parseOperators = parseOperator
    [ (OpLeft, ["|>"])
    , (OpRight, ["&&", "||"])
    , (OpLeft, ["==", "~=", ">", "<", ">=", "<="])
    , (OpRight, [":>"])
    , (OpRight, ["++"])
    , (OpLeft, ["+", "-"])
    , (OpLeft, ["*", "/", "%"])
    ]
  parseOperator [] = parseApplication
  parseOperator ((associativity, ops) : higherOps) = do
    first <- parseItem
    rest <- many parseSuffix
    pure $ (case associativity of OpLeft -> flattenLeft; OpRight -> flattenRight) first rest
    where
    parseItem = parseOperator higherOps
    parseSuffix = parseOps ops &&& \op -> do
      argument <- parseItem
      pure (op, argument)
    parseOps [] = checkNever
    parseOps (op : more) = checkToken LexOperator op |:| parseOps more
    flattenLeft first [] = first
    flattenLeft first ((op, argument) : rest) = flattenLeft ((EName op `EApply` first) `EApply` argument) rest
    flattenRight first [] = first
    flattenRight first ((op, second) : rest) = (EName op `EApply` first) `EApply` (flattenRight second rest)
  parseApplication = do
    first <- parseObject ||| parseError "expression expected"
    rest <- many (fmap Left (checkSpecial "!") |:| fmap Right parseObject)
    pure (flatten first rest)
    where
    flatten object [] = object
    flatten left (Right right : rest) = flatten (left `EApply` right) rest
    flatten left (Left _ : rest) = flatten (EBang left) rest
  parseObject = parseAtom &&& \first -> do
    rest <- many parseSuffix
    pure (flatten first rest)
    where
    parseSuffix = parseDot where -- TODO: access
      {-
      parseAccess = checkSpecial "[" &&& \open -> do
        body <- parseExpression `withContext` ("in index at " ++ location open)
        expectSpecial "]" ("expected `]` to close `[` for index at " ++ location open)
        pure body
      -}
      parseDot = checkSpecial "." &&& \_ -> do
        name <- expectClass LexName "expected name to follow `.` indicated field access"
        pure name
    flatten first [] = first
    -- TODO: EAccess flatten first (Left index : rest) = flatten (EAccess first index) rest
    flatten first (field : rest) = flatten (EDot first field) rest
  parseAtom = parseName |:| parseParens |:| parseFunc |:| parseInt |:| parseString where
    parseName = checkClass LexName &&& \name -> pure (EName name)
    parseParens = checkSpecial "(" &&& \open -> do
      interior <- parseExpression
      expectSpecial ")" ("expected `)` to close `(` opened at " ++ location open)
      pure interior
    parseFunc = checkSpecial "func" &&& \_ -> do
      def <- parseFuncDefinition
      pure (EFunc def)
    parseInt = checkClass LexNumber &&& \number -> pure (EInt $ read $ contents number) -- TODO: safer
    parseString = checkClass LexString &&& \string -> pure (EString $contents string)
