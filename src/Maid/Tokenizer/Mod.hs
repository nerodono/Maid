module Maid.Tokenizer.Mod
( tokenize )
where

import Data.Char ( isDigit )

import qualified Maid.Tokenizer.Span as Span

import Maid.Tokenizer.Token ( Token(..)
                            , Literal(..)
                            , Keyword(..)
                            , Bracket(..)
                            , BracketType(..)
                            )
import Maid.Tokenizer.Helpers ( isValidOperator
                              , isValidIdentStart
                              , isValidIdentRest
                              )

import Maid.Helpers ( takeWhile' )

tokenizeAcc :: Integer -> String -> [Span.Spanned Token]
tokenizeAcc acc text = Span.applyOffset acc <$> tokenize text

tokenizeNext :: String -> [Span.Spanned Token]
tokenizeNext = tokenizeAcc 1

headSpan :: Token -> Integer -> Span.Spanned Token
headSpan tok = Span.makeSpanned tok 0

isSkippableWhitespace :: Char -> Bool
isSkippableWhitespace = (`elem` " \t")

mapIdent :: String -> Token
mapIdent "if"   = TKeyword If
mapIdent "else" = TKeyword Else
mapIdent "then" = TKeyword Then
mapIdent "fn"   = TKeyword Fn
mapIdent "end"  = TKeyword End
mapIdent "let"  = TKeyword Let
mapIdent o = TIdent o

collectPred :: (Char -> Bool) -> (String -> (Token, Integer)) -> String -> [Span.Spanned Token]
collectPred predicate mapStr text =
    let (rest, tail', len) = takeWhile' predicate text
        (token, len_acc) = mapStr rest
    in
        headSpan token len : tokenizeAcc (len + len_acc) tail'

zeroAcc :: a -> (a, Integer)
zeroAcc x = (x, 0)

tokenize :: String -> [Span.Spanned Token]

-- Integer literal
tokenize (h:t) | isDigit h = collectPred isDigit f (h:t)
    where f s = zeroAcc $ TLiteral (LInt $ read s)

-- Whitespaces
tokenize ('\n':t) = headSpan TNewline 1 : tokenizeNext t
tokenize (c:t) | isSkippableWhitespace c = tokenizeNext t

-- Operators
tokenize (c:t) | isValidOperator c =
    collectPred isValidOperator f (c:t)
    where f = zeroAcc . TOperator

-- Ident
tokenize (c:t) | isValidIdentStart c =
    collectPred isValidIdentRest f t
    where f = (, 1) . mapIdent

tokenize (c:t) | c == '(' = headSpan (TBracket Round Open)   1 : tokenizeNext t
               | c == ')' = headSpan (TBracket Round Close)  1 : tokenizeNext t
               | c == '{' = headSpan (TBracket Curly Open)   1 : tokenizeNext t
               | c == '}' = headSpan (TBracket Curly Close)  1 : tokenizeNext t
               | c == '[' = headSpan (TBracket Square Open)  1 : tokenizeNext t
               | c == ']' = headSpan (TBracket Square Close) 1 : tokenizeNext t

-- String literal
tokenize ('`':t) = undefined

tokenize [] = []
