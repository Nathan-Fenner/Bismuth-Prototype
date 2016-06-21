
module Lexer(Location, Token(..), LexerClass(..), lexer) where

type Location = String

data Token = Token { contents :: String, location :: Location } deriving Show

data LexerClass = LexName | LexNumber | LexString | LexSpecial | LexOperator deriving (Eq, Show)

lexer :: FilePath -> String -> [(LexerClass, Token)]
lexer file source = lexer' file 1 1 source

specialNames :: [String]
specialNames = ["module", "where", "var", "let", "if", "else", "while", "class", "instance", "func", "type", "struct", "union", "of"]

lexer' :: FilePath -> Int -> Int -> String -> [(LexerClass, Token)]
lexer' _file _line _column "" = []
lexer' file line column (' ' : source) = lexer' file line (column+1) source
lexer' file line _column ('\n' : source) = lexer' file (line+1) 1 source
lexer' file line _column ('\r' : source) = lexer' file line 1 source
lexer' file line column ('\t' : source) = lexer' file line (column+4) source -- TODO; fix this
lexer' file line column (c : source)
  |c `elem` "()[]{}:;,.~!" = (LexSpecial, Token [c] here ) : lexer' file line (column+1) source
  where
  here = file ++ "/" ++ show line ++ ":" ++ show column
lexer' file line column ('"' : source) = (LexString, Token contents' here) : lexer' file line (column + length contents' + 1) rest where
  contents' = takeWhile (/= '"') source
  here = file ++ "/" ++ show line ++ ":" ++ show column
  rest = drop 1 $ dropWhile (/= '"') source
lexer' file line column source@(c : _)
  |c `elem` ['0'..'9'] = (LexNumber, Token numberContents here) : lexer' file line (column + length numberContents) (drop (length numberContents) source)
  |c `elem` alphas = (if nameContents `elem` specialNames then LexSpecial else LexName, Token nameContents here) : lexer' file line (column + length nameContents) (drop (length nameContents) source)
  |c `elem` operators = (LexOperator, Token operatorContents here) : lexer' file line (column + length operatorContents) (drop (length operatorContents) source)
  |otherwise = error $ "Don't know how to handle ```" ++ take 20 source ++ "...```"
  where
  here = file ++ "/" ++ show line ++ ":" ++ show column
  numberContents = takeWhile (`elem` ['0'..'9']++".eE-+") source
  nameContents = takeWhile (`elem` alphas') source
  alphas = ['a'..'z'] ++ ['A'..'Z']
  alphas' = alphas ++ ['0' .. '9'] ++ "'"
  operators = "+-*/<>=~$|&^%"
  operatorContents = takeWhile (`elem` operators) source
