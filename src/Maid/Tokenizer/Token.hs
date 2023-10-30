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
             | Infixl
             | Infixr
             | Mut
             | Const
             | Prefixop
             deriving(Show, Eq)

data Bracket = Round
             | Curly
             | Square
             deriving(Show, Eq)
data BracketType = Open | Close
                 deriving(Show, Eq)

data Token = TIdent String
           | TOperator String
           | TLiteral Literal
           | TKeyword Keyword
           | TBracket Bracket BracketType
           | TNewline
           deriving Show
