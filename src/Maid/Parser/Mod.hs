module Maid.Parser.Mod
( expression
, factor
, defaultPrecedence
, minPrecedence
, maxPrecedence
, binary
, rightAssocDefaultPrecedence
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
                                   , mapAssoc
                                   )

type PResult = Either Error (Expr, [Spanned Token])
type SResult = Either Error Expr
type Expected = Either Error [Spanned Token]

type WhileFn = [Spanned Token] -> Maybe (Expr, [Spanned Token])
type ExprReduce = Expr -> Expr -> Expr
type ParseFn = (PrecMap -> [Spanned Token] -> PResult)

defaultPrecedence :: Precedence
defaultPrecedence = Precedence 4 LeftAssoc

rightAssocDefaultPrecedence :: Precedence
rightAssocDefaultPrecedence = mapAssoc (const RightAssoc) defaultPrecedence

maxPrecedence :: Precedence
maxPrecedence = Precedence 20 LeftAssoc

minPrecedence :: Precedence
minPrecedence = Precedence 0 LeftAssoc

-- Item with max precedence
-- e.g. literals, expressions in brackets, if expressions and so on
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

-- parses binary expression
-- this part is complex, so here's the explanation
--
-- the idea:
--  Generalize recursive descent parser
--
-- Naive recursive descent parser of small grammar will look like this
{-
digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9";
number = digit { digit };

factor     = number | "(" expression ")";
term       = factor { "*" factor };
expression = term { "+" term };
-}
-- This grammar will parse something like 2 + 2 * 2
-- as (+ 2 (2 * 2))
--  and (2 + 2) * 2
-- as (* (+ 2 2) 2)
--
-- the idea here is that items with the highest precedence are lowest on the stack:
-- expression
-- |   +    |
-- t        t
-- --- * ----
--  f      f
--
-- so, the term is multiple of factors and factor is number or expression in brackets
--
-- This function - generalization of that principle on the arbitrary number of operators
binary :: PrecMap -> Integer -> [Spanned Token] -> PResult
binary pmap prec' tokens
    | prec' == maxPrec' =
        -- if we're on the max level of precedence
        --  then it's time to parse factor
        parseSingle factor
    | otherwise =
        -- otherwise we should parse binary expression with the
        -- operands compound of higher precedence items.
        -- eventually it gets on the max precedence level and parses `factor` (the first guard)
        parseSingle (\_ tks -> binary pmap nextPrec tks)

    where maxPrec' = maxPrec pmap
          nextPrec = prec' + 1

          -- Uses passed function to parse binary expression
          -- ParseFn takes tokens and returns operand
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
                    foldlExpr reduced reduce' laCollect t

                RightAssoc -> do
                    (rhs, t)     <- fn pmap tail'
                    (folded, t') <- foldrExpr rhs reduce' raCollect t

                    Right (reduce' lhs folded , t')
            where Spanned _ op_str = op'
                  Precedence _ assoc = getOr op_str defaultPrecedence pmap

                  reduce' = reduceLAssoc op'

                  raCollect :: [Spanned Token] -> Maybe (Expr, [Spanned Token])
                  raCollect tokens' = do
                    (_, t') <- maybeSameOp $ expectOperator tokens'
                    (rhs, t'') <- rightToMaybe $ fn pmap t'

                    Just (rhs, t'')

                  laCollect :: [Spanned Token] -> Maybe (Expr, [Spanned Token])
                  laCollect tokens' = do
                    (_, t') <- maybeSameOp $ expectOperator tokens'
                    (rhs, t'') <- rightToMaybe $ fn pmap t'

                    Just (rhs, t'')
                
                  maybeSameOp :: Either Error (Spanned String, [Spanned Token]) -> Maybe (Spanned String, [Spanned Token])
                  maybeSameOp (Right (Spanned s o, tl'))| o == op_str =
                    Just (Spanned s o, tl')
                  maybeSameOp _ = Nothing
          
          foldrExpr :: Expr -> ExprReduce -> WhileFn -> [Spanned Token] -> PResult
          foldrExpr expr _ _ [] = Right (expr, [])
          foldrExpr expr reduce' fun tokens' =
            case fun tokens' of
                Just (expr', tail') -> do
                    (l, t) <- foldrExpr expr' reduce' fun tail'

                    Right (reduce' expr l, t)

          foldlExpr :: Expr -> ExprReduce -> WhileFn -> [Spanned Token] -> PResult
          foldlExpr expr _ _ [] = Right (expr, [])
          foldlExpr expr reduce' fun tokens' =
            case fun tokens' of
                Just (expr', tail') ->
                    foldlExpr (reduce' expr expr') reduce' fun tail'
                Nothing ->
                    Right (expr, tokens')

          reduceLAssoc :: Spanned String -> Expr -> Expr -> Expr
          reduceLAssoc operator' lhs rhs =
            EBinary $ EBinary' lhs rhs operator'

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
