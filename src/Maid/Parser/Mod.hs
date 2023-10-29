module Maid.Parser.Mod
( expression
, factor
, defaultPrecedence
, minPrecedence
, maxPrecedence
, binary
)
where

import Data.Either ( fromRight )

import Maid.Helpers ( rightToMaybe )
import Maid.Tokenizer.Token ( Token(..)
                            , Keyword(..)
                            , Literal(..)
                            , Bracket(..)
                            , BracketType(..)
                            )
import Maid.Tokenizer.Span ( Spanned(..) )

import Maid.Parser.Ast ( Expr(..)
                       , EIf'(..)
                       , EBinary'(..)
                       )
import Maid.Parser.Error ( Error(..)
                         , ExpectedToken (..)
                         )
import Maid.Parser.PrecedenceStore ( PrecMap
                                   , getOr
                                   , Precedence (..)
                                   , Associativity(..)
                                   , maxPrec
                                   )

type PResult = Either Error (Expr, [Spanned Token])
type SResult = Either Error Expr
type Expected = Either Error [Spanned Token]

type WhileFn = [Spanned Token] -> Maybe (Expr, [Spanned Token])
type ExprReduce = Expr -> Expr -> Expr
type ParseFn = (PrecMap -> [Spanned Token] -> PResult)

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
    | prec' == maxPrec' = parseSingle factor
    | otherwise = parseSingle (\_ tks -> binary pmap nextPrec tks)

    where maxPrec' = maxPrec pmap
          nextPrec = prec' + 1

          parseSingle :: ParseFn -> PResult
          parseSingle fn = do
            (lhs, t) <- fn pmap tokens
            case expectOperator t of
                Right (Spanned op_span' operator, t') -> do
                    let Precedence op_prec _ = getOr operator defaultPrecedence pmap
                    if op_prec /= prec' then
                        Right (lhs, t)
                    else
                        proceedRhs fn lhs (Spanned op_span' operator) t'
                Left _ -> Right (lhs, t)

          proceedRhs :: ParseFn -> Expr -> Spanned String -> [Spanned Token] -> PResult
          proceedRhs fn lhs op' tail' =
            case assoc of
                LeftAssoc  -> do
                    (rhs, t) <- fn pmap tail'
                    let reduced = reduceLAssoc op' lhs rhs
                    while reduced (reduceLAssoc op') laCollect t

                RightAssoc -> undefined
            where Spanned _ op_str = op'
                  Precedence _ assoc = getOr op_str defaultPrecedence pmap

                  laCollect :: [Spanned Token] -> Maybe (Expr, [Spanned Token])
                  laCollect tokens' = do
                    (_, t') <- maybeSameOp $ expectOperator tokens'
                    (rhs, t'') <- rightToMaybe $ fn pmap t'

                    Just (rhs, t'')
                
                  maybeSameOp :: Either Error (Spanned String, [Spanned Token]) -> Maybe (Spanned String, [Spanned Token])
                  maybeSameOp (Right (Spanned s o, tl'))| o == op_str =
                    Just (Spanned s o, tl')
                  maybeSameOp _ = Nothing

          while :: Expr -> ExprReduce -> WhileFn -> [Spanned Token] -> PResult
          while expr _ _ [] = Right (expr, [])
          while expr reduce' fun tokens' =
            case fun tokens' of
                Just (expr', tail') ->
                    while (reduce' expr expr') reduce' fun tail'
                Nothing ->
                    Right (expr, tokens')

          reduceNonAssoc :: Spanned String -> Expr -> Expr -> Expr
          reduceNonAssoc operator' lhs rhs =
            EBinary $ EBinary' lhs rhs operator'

          reduceLAssoc     = reduceNonAssoc
          reduceRAssoc op' = flip $ reduceNonAssoc op'

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
expression pmap = parse
    where parse = binary pmap 0
