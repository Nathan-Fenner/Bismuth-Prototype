
module AST where

import Lexer

data Module = Module [ModuleDeclaration] deriving Show
data ModuleDeclaration
  = FuncDeclaration Token FuncDefinition
  deriving Show
  -- TODO: classes, instances, imports.
data FuncDefinition = FuncDefinition [Generic] [(Token, TypeTerm)] TypeTerm Block deriving Show
data Generic = Generic Token Kind deriving Show -- TODO: constraints
data Kind = KConcrete | KArrow Kind Kind deriving Show
data TypeTerm = TName Token | TApply TypeTerm TypeTerm deriving Show
data Type = Type [Generic] TypeTerm deriving Show
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
