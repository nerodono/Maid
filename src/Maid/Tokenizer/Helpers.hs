module Maid.Tokenizer.Helpers
( isValidOperator
, isValidIdentStart
, isValidIdentRest )
where

-- Helpers that are exclusive for Tokenizer

isValidOperator :: Char -> Bool
isValidOperator = (`elem` "^*.>|<+-=&$@")

validIdentStarting :: [Char]
validIdentStarting = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_"

validIdentRest :: [Char]
validIdentRest = validIdentStarting ++ "'$?!"


isValidIdentStart :: Char -> Bool
isValidIdentStart = (`elem` validIdentStarting)

isValidIdentRest :: Char -> Bool
isValidIdentRest = (`elem` validIdentRest)
