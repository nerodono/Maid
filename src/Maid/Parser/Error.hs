module Maid.Parser.Error
( Error(..)
, ExpectedToken(..)
)
where

import Maid.Tokenizer.Span ( Spanned, Span )
import Maid.Tokenizer.Token ( Keyword
                            , Token
                            , BracketShape
                            , BracketType
                            )

import qualified Maid.Parser.PrecedenceStore as P

data ExpectedToken = ExpectedLiteral
                   | ExpectedKeyword Keyword
                   | ExpectedBracket (Maybe (BracketShape, BracketType))
                   | ExpectedOperator (Maybe String)
                   deriving Show

data Error = Eof
           | UnexpectedToken (Spanned Token) ExpectedToken
           | UnexpectedKeyword (Spanned Keyword)
           | UnmatchedClosingBracket (Spanned BracketShape)
           | UnknownOperator P.Operator Span
           deriving Show
