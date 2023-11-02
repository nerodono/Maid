module Maid.Parser.Expression
( expression )
where

import qualified Maid.Parser.PrecedenceStore as P

import Maid.Tokenizer.Token ( Token(..)
                            , BracketShape(..)
                            , BracketType(..)
                            )
import Maid.Tokenizer.Span ( Spanned(..)
                           , unwrapSpanned
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

factor :: P.StoreTree -> [Spanned Token] -> FallibleTailed Expr
factor tree (h:t) =
    case bare_token of
        TLiteral lit ->
            Right ( ELiteral $ withSpan lit
                  , t )
        TBracket Round Open -> do
            (expr, t') <- expression tree t
            (b, t'') <- expectBracket t'

            _ <- filterExact Round Close b
            Right (expr, t'')
        
        _ ->
            Left $ UnexpectedToken h ExpectedLiteral

    where Spanned span' bare_token  = h
          filterExact bs bt = filterBracket $ bracketExact bs bt
          withSpan = Spanned span'

factor _ [] = Left Eof

expression :: P.StoreTree -> [Spanned Token] -> FallibleTailed Expr
expression tree =
    case P.splitTree tree of
        Just (current_precedence, whats_left) ->
            parse $ expression whats_left
        Nothing ->
            factor tree
    where
        parse :: ParseF -> [Spanned Token] -> FallibleTailed Expr
        parse parse' tokens =
            undefined

-- Expects

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

