
{-# LANGUAGE DeriveFunctor #-}

module Parse.Core where
import Lexer
import Control.Applicative(Applicative(..))


data Parser t = Parser { runParser :: [(LexerClass, Token)] -> Either String (t, [(LexerClass, Token)]) } deriving Functor
data Checker t = Checker { runChecker :: [(LexerClass, Token)] -> Either String (Maybe (t, [(LexerClass, Token)])) } deriving Functor

instance Applicative Parser where
  pure x = Parser $ \tokens -> Right (x, tokens)
  left <*> right = Parser $ \tokens -> do
    (fun, tokens') <- runParser left tokens
    (arg, tokens'') <- runParser right tokens'
    return (fun arg, tokens'')
instance Monad Parser where
  return = pure
  parser >>= into = Parser $ \tokens -> do
    (arg, tokens') <- runParser parser tokens
    runParser (into arg) tokens'
instance Applicative Checker where
  pure x = Checker $ \tokens -> Right (Just (x, tokens))
  left <*> right = Checker $ \tokens -> do
    result <- runChecker left tokens
    case result of
      Nothing -> pure Nothing
      Just (fun, tokens') -> do
        result' <- runChecker right tokens'
        case result' of
          Nothing -> pure Nothing
          Just (arg, tokens'') -> pure (Just (fun arg, tokens''))
instance Monad Checker where
  return = pure
  parser >>= into = Checker $ \tokens -> do
    result <- runChecker parser tokens
    case result of
      Nothing -> pure Nothing
      Just (arg, tokens') -> runChecker (into arg) tokens'

checkNever :: Checker t
checkNever = Checker $ \_ -> Right Nothing

currentLocation :: Parser String
currentLocation = Parser runner where
  runner [] = Right ("end of file", [])
  runner whole@((_, token) : _) = Right (location token, whole)

hereToken :: String -> Parser Token
hereToken newContents = do
  here <- currentLocation
  pure (Token{contents = newContents, location = here})

parseError :: String -> Parser t
parseError message = Parser runner where
  runner [] = Left $ message ++ " at end of file"
  runner ((_, token) : _) = Left $ message ++ " at " ++ location token

withContext :: Parser t -> String -> Parser t
withContext parser context = Parser $ \tokens -> case runParser parser tokens of
  Right x -> Right x
  Left message -> Left $ context ++ ": " ++ message

nextToken :: Parser (Maybe (LexerClass, Token))
nextToken = Parser runner where
  runner [] = Right (Nothing, [])
  runner (first : rest) = Right $ (Just first, rest)

expectClass :: LexerClass -> String -> Parser Token
expectClass desiredClass message = do
  result <- nextToken
  case result of
    Nothing -> parseError message
    Just (givenClass, token) -> case givenClass == desiredClass of
      True -> return token
      False -> parseError message

checkClass :: LexerClass -> Checker Token
checkClass expected = Checker runner where
  runner [] = Right Nothing
  runner ((c, token) : rest)
    |c == expected = Right $ Just (token, rest)
    |otherwise = Right Nothing

checkToken :: LexerClass -> String -> Checker Token
checkToken lexClass expected = Checker runner where
  runner [] = Right Nothing
  runner ((c, token) : rest)
    |c == lexClass && contents token == expected = Right $ Just (token, rest)
    |otherwise = Right Nothing

checkSpecial :: String -> Checker Token
checkSpecial = checkToken LexSpecial

expectSpecial :: String -> String -> Parser ()
expectSpecial expected message = do
  result <- nextToken
  case result of
    Nothing -> parseError message
    Just (givenClass, token) -> case givenClass == LexSpecial && contents token == expected of
      True -> return ()
      False -> parseError message

(|||) :: Checker x -> Parser x -> Parser x
left ||| right = Parser $ \tokens -> case runChecker left tokens of
  Left message -> Left message
  Right Nothing -> runParser right tokens
  Right (Just (x, rest)) -> Right (x, rest)
infixl 2 |||

(|:|) :: Checker x -> Checker x -> Checker x
left |:| right = Checker $ \tokens -> case runChecker left tokens of
  Left message -> Left message
  Right Nothing -> runChecker right tokens
  Right (Just (x, rest)) -> Right (Just (x, rest))
infixl 2 |:|

(&&&) :: Checker x -> (x -> Parser r) -> Checker r
condition &&& builder = Checker $ \tokens -> case runChecker condition tokens of
  Left message -> Left message
  Right Nothing -> Right Nothing
  Right (Just (x, rest)) -> case runParser (builder x) rest of
    Left message -> Left message
    Right result -> Right (Just result)
infixl 3 &&&

mux :: [(String, Parser t)] -> Checker t
mux [] = checkNever
mux ((switch, parser) : otherParsers) = Checker runner |:| mux otherParsers where
  runner ((LexSpecial, token) : rest)
    |contents token == switch = fmap Just $ runParser parser rest
    |otherwise = Right Nothing
  runner _ = Right Nothing

many :: Checker t -> Parser [t]
many checker = (checker &&& \first -> do rest <- many checker; pure (first:rest)) ||| pure []

negative :: Checker t -> Checker ()
negative checker = Checker $ \tokens -> case runChecker checker tokens of
  Left message -> Left message
  Right Nothing -> Right (Just ((), tokens))
  Right _ -> Right Nothing
