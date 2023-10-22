module Maid.Tokenizer.Token
( Token(..)
, Literal(..)
, Keyword(..)
, Bracket(..)
, BracketType(..)
)
where

data Literal = LInt Integer
             | LStr String
             deriving Show
data Keyword = Let
             | If
             | Then
             | Else
             | End
             | Fn
             deriving Show

data Bracket = Round
             | Curly
             | Square
             deriving Show
data BracketType = Open | Close
                 deriving Show

data Token = TIdent String
           | TOperator String
           | TLiteral Literal
           | TKeyword Keyword
           | TBracket Bracket BracketType
           | TNewline
           deriving Show
