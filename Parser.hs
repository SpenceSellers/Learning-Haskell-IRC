{-# LANGUAGE OverloadedStrings #-}
module Parser (Parser.parse) where
    
import qualified Data.Text as T
import Data.Attoparsec.Text
import qualified Data.Char as Char
import Control.Applicative
import Types
    
parse :: String -> Maybe Message
parse msg = case parseOnly parseMessage (T.pack msg) of
              Left x -> Nothing
              Right message -> Just message

parseMessage :: Parser Message
parseMessage = do
  option "" parseSource 
  
  message <- choice [parsePing, parsePrivmsg]
  return message

parseSource :: Parser String
parseSource = do
  char ':'
  source <- takeTill Char.isSpace
  space
  return (T.unpack source)

parsePing :: Parser Message
parsePing = do
  string "PING :"
  body <- takeText
  return $ Ping (T.unpack body)

parsePrivmsg :: Parser Message
parsePrivmsg = do
  string "PRIVMSG" <* skipSpace
  nick <- takeTill Char.isSpace <* skipSpace <* ":"
  body <- takeText
  return $ PrivMsg (T.unpack nick) (T.unpack body)
