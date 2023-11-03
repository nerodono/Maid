module Maid.Parser.Expression
( expression )
where

import qualified Data.Bifunctor              as Bi
import qualified Maid.Parser.PrecedenceStore as P

import Maid.Tokenizer.Token ( Token(..)
                            , BracketShape(..)
                            , BracketType(..)
                            )
import Maid.Tokenizer.Span ( Spanned(..)
                           , Span(..)
                           )

import Maid.Parser.Ast ( Expr(..)
                       , EBinary'(..)
                       , EUnary'(..)
                       )
import Maid.Parser.Error ( Error(..)
                         , ExpectedToken(..)
                         )

type ParseF = [Spanned Token] -> FallibleTailed Expr

type BracketFilter = BracketShape -> BracketType -> Bool
type Fallible a = Either Error a

type FallibleTailed  a = Either Error (a, [Spanned Token])
type FallibleTailedS a = Either Error (Spanned a, [Spanned Token])

factor :: P.Store -> [Spanned Token] -> FallibleTailed Expr
factor tree (h:t) =
    case bare_token of
        TLiteral lit ->
            Right ( ELiteral $ withSpan lit
                  , t )
        TBracket Round Open -> do
            -- fromRoot call here needed to restore original
            -- precedence map
            (expr, t') <- expression (P.fromRoot tree) t
            (b, t'') <- expectBracket t'

            _ <- filterExact Round Close b
            Right (expr, t'')

        _ ->
            Left $ UnexpectedToken h ExpectedLiteral

    where Spanned span' bare_token  = h
          filterExact bs bt = filterBracket $ bracketExact bs bt
          withSpan = Spanned span'

factor _ [] = Left Eof

type FoldlFn = Expr -> [Spanned Token] -> FallibleTailed Expr

expression :: P.StoreTree -> [Spanned Token] -> FallibleTailed Expr
expression tree =
    case P.splitTree tree of
        Just (current_precedence, whats_left) ->
            parse current_precedence $ expression whats_left
        Nothing ->
            factor $ P.root tree
    where
        parse :: Integer -> ParseF -> [Spanned Token] -> FallibleTailed Expr
        parse cur_prec parse' tokens =
            maybeUnary cur_prec parse' tokens >>= \(expr, tail') ->
                case expectOperator tail' of
                    Right (Spanned span' operator, _) ->
                        knownOperator span' (P.binary operator) >>= \(P.Precedence assoc precedence) ->
                            if precedence /= cur_prec then
                                Right (expr, tail')
                            else case assoc of
                                P.LeftAssoc  ->
                                    foldlExpr (leftFold $ expectSameOp operator) expr tail'
                                P.RightAssoc ->
                                    rightFold (expectSameOp operator) expr tail'
                    Left _ ->
                        Right (expr, tail')
            where
                leftFold :: OperatorF -> Expr -> [Spanned Token] -> FallibleTailed Expr
                leftFold expectOperator' lhs input = do
                    (operator, t) <- expectOperator' input
                    (rhs, t')     <- parse' t
                    Right ( EBinary $ EBinary' operator lhs rhs
                          , t'
                          )
                
                rightFold :: OperatorF -> Expr -> [Spanned Token] -> FallibleTailed Expr
                rightFold expectOperator' lhs input = do
                    (operator, tail') <- expectOperator' input
                    (rhs, tail'') <- parse' tail'

                    case rightFold expectOperator' rhs tail'' of
                        Right (rest, t) ->
                            Right ( EBinary $ EBinary' operator lhs rest
                                  , t )
                        Left _ -> Right ( EBinary $ EBinary' operator lhs rhs
                                        , tail'' )

        
        foldlExpr :: FoldlFn -> Expr -> [Spanned Token] -> FallibleTailed Expr
        foldlExpr fold_fn expr input =
            case fold_fn expr input of
                Right (new_expr, tail') ->
                    foldlExpr fold_fn new_expr tail'
                Left _ ->
                    Right (expr, input)

        maybeUnary :: Integer -> ParseF -> [Spanned Token] -> FallibleTailed Expr
        maybeUnary cur_prec parse' tokens =
            case expectOperator tokens of
                Right (Spanned span' operator, t) ->
                    knownOperator span' (P.unary operator) >>= \(P.Precedence _ precedence) ->
                        if precedence /= cur_prec then
                            parse' tokens
                        else
                            Bi.first (EUnary . EUnary' (Spanned span' operator)) <$> parse' t
                Left _ ->
                    parse' tokens

        knownOperator :: Span -> P.Operator -> Fallible P.Precedence
        knownOperator span' op =
            maybe (Left $ UnknownOperator op span') Right (P.executeRoot (P.lookup op) tree)

-- Expects

type OperatorF = [Spanned Token] -> FallibleTailedS String
expectSameOp :: String -> OperatorF
expectSameOp s tokens =
    expectOperator tokens >>= \(Spanned span' operator, tail') ->
        if s /= operator then
            Left $ UnexpectedToken (Spanned span' $ TOperator operator) (ExpectedOperator $ Just s)
        else
            Right ( Spanned span' operator
                  , tail'
                  )

expectOperator :: [Spanned Token] -> FallibleTailedS String
expectOperator (h:t) =
    case h of
        Spanned span' (TOperator operator) ->
            Right ( Spanned span' operator
                  , t )
        tok -> Left $ UnexpectedToken tok (ExpectedOperator Nothing)

expectOperator [] = Left Eof

expectBracket :: [Spanned Token] -> FallibleTailedS (BracketShape, BracketType)
expectBracket (Spanned span' bare:t) =
    case bare of
        TBracket shape' type' -> Right (Spanned span' (shape', type'), t)
        tk -> Left $ UnexpectedToken (Spanned span' tk) (ExpectedBracket Nothing)

bracketExact :: BracketShape -> BracketType -> BracketFilter
bracketExact s t s' t' =
    s == s' && t == t'

filterBracket :: BracketFilter -> Spanned (BracketShape, BracketType) -> Fallible (Spanned (BracketShape, BracketType))
filterBracket f (Spanned span' (bs, bt)) =
    if f bs bt then
        Right $ Spanned span' (bs, bt)
    else
        Left $ UnexpectedToken (Spanned span' $ TBracket bs bt) $ ExpectedBracket (Just (bs, bt))

