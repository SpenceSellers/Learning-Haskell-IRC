{-# LANGUAGE OverloadedStrings #-}
module Main where

import Client
import qualified Parser
import Types
import Data.Attoparsec.Text
main :: IO ()
main = do
  
  print $ Parser.parse "PING :When"
        
