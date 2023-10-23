module Maid.Parser.Mod
( expression
, factor
, defaultPrecedence
, minPrecedence
, maxPrecedence
)
where

import Data.Either ( fromRight )

import Maid.Tokenizer.Token ( Token(..)
                            , Keyword(..)
                            , Literal(..)
                            , Bracket(..)
                            , BracketType(..)
                            )
import Maid.Tokenizer.Span ( Spanned(..) )

import Maid.Parser.Ast ( Expr(..)
                       , EIf'(..)
                       )
import Maid.Parser.Error ( Error(..)
                         , ExpectedToken (..)
                         )
import Maid.Parser.PrecedenceStore ( PrecMap
                                   , getOr
                                   , get
                                   , Precedence (..)
                                   , Associativity(..)
                                   , maxPrec
                                   )

type PResult = Either Error (Expr, [Spanned Token])
type SResult = Either Error Expr
type Expected = Either Error [Spanned Token]

defaultPrecedence :: Precedence
defaultPrecedence = Precedence 4 LeftAssoc

maxPrecedence :: Precedence
maxPrecedence = Precedence 20 LeftAssoc

minPrecedence :: Precedence
minPrecedence = Precedence 0 LeftAssoc

factor :: PrecMap -> [Spanned Token] -> PResult
factor pmap (h:t) =
    case token of
        TKeyword kw -> handleKeyword kw
        TIdent ident -> (, t) <$> handleIdent ident
        TLiteral lit -> (, t) <$> handleLit lit
        TBracket Round Open -> handleRoundBracket

        _ -> undefined
    where Spanned span' token = h
          mkSpan = Spanned span'
          expression' = expression pmap

          handleRoundBracket = do
            (inner, t') <- expression' t
            t'' <- expectBracket Round Close t'
            Right ( inner, t'' )

          handleLit :: Literal -> SResult
          handleLit = Right . ELiteral . mkSpan

          handleIdent :: String -> SResult
          handleIdent = Right . EIdent . mkSpan

          handleKeyword :: Keyword -> PResult
          handleKeyword If = do
            (cond, t') <- expression pmap t

            t'' <- expectKeyword Then t'
            (on_true, t''') <- expression' t''
            t'''' <- expectKeyword Else t'''

            (on_false, else_tail) <- expression' t''''

            let ret_tail = fromRight else_tail $ expectKeyword End else_tail
            Right ( EIf $ EIf' cond on_true on_false
                  , ret_tail
                  )

binary :: PrecMap -> Integer -> [Spanned Token] -> PResult
binary pmap prec' tokens
    | prec' == maxPrec' = do
        (lhs_fact, t) <- factor pmap tokens
        (Spanned op_span' operator, t') <- expectOperator t
        let Precedence op_prec _ = getOr operator defaultPrecedence pmap
        if op_prec /= prec' then
            Right (lhs_fact, t)
        else
            proceedMaxPrec t' lhs_fact (Spanned op_span' operator)
    | otherwise = do
        (lhs_expr, t) <- nextBinary tokens
        (Spanned op_span' operator, t') <- expectOperator t
        (rhs_expr, t'') <- nextBinary t'

        undefined
    where maxPrec' = maxPrec pmap
          nextPrec = prec' + 1
          nextBinary = binary pmap nextPrec

          proceedMaxPrec :: [Spanned Token] -> Expr -> Spanned String -> PResult
          proceedMaxPrec tks lhs operator = do
            (rhs_fact, t) <- factor pmap tks

            undefined

expectBracket :: Bracket -> BracketType -> [Spanned Token] -> Expected
expectBracket bracket bracket_type (Spanned _ (TBracket b' bt'):t)
    | b' == bracket && bt' == bracket_type =
        Right t
expectBracket bracket bracket_type (h:_) =
    Left $ UnexpectedToken h (ExpectedBracket bracket bracket_type)
expectBracket _ _ [] = Left Eof

expectOperator :: [Spanned Token] -> Either Error (Spanned String, [Spanned Token])
expectOperator (Spanned span' (TOperator operator):t) =
    Right (Spanned span' operator, t)
expectOperator (token:_) =
    Left $ UnexpectedToken token (ExpectedOperator Nothing)
expectOperator [] = Left Eof

expectKeyword :: Keyword -> [Spanned Token] -> Expected
expectKeyword kw (Spanned _ (TKeyword kw'):t) | kw == kw' = Right t 
expectKeyword kw (token:_) =
    Left $ UnexpectedToken token (ExpectedKeyword kw)
expectKeyword _ [] = Left Eof

expression :: PrecMap -> [Spanned Token] -> PResult
expression _ (h:t) = undefined
expression _ [] = Left Eof
