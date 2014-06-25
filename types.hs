module Types where

type UserNick = String
    
type Channel = String
    
data Message = Ping String
             | PrivMsg UserNick String
             | Nick UserNick
             deriving (Show)


class IRCSendable a where
    encode :: a -> String

instance IRCSendable Message where
    encode (Ping s) = "PING :" ++ s
    encode (PrivMsg nick msg) = "PRIVMSG " ++ nick ++ " :" ++ msg
    encode (Nick nick) = "NICK " ++ nick
    
