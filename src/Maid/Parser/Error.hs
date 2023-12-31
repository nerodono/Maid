module Maid.Parser.Error
( Error(..)
, ExpectedToken(..)
)
where

import Maid.Tokenizer.Span ( Spanned )
import Maid.Tokenizer.Token ( Keyword
                            , Token
                            , Bracket
                            , BracketType
                            )

data ExpectedToken = ExpectedLiteral
                   | ExpectedKeyword Keyword
                   | ExpectedBracket Bracket BracketType
                   | ExpectedOperator (Maybe String)
                   deriving Show

data Error = Eof
           | UnexpectedToken (Spanned Token) ExpectedToken
           | UnexpectedKeyword (Spanned Keyword)
           | UnmatchedClosingBracket
           deriving Show
