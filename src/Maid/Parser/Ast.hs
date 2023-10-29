{-# LANGUAGE DuplicateRecordFields #-}

module Maid.Parser.Ast
( Expr(..)
, EBinary'(..)
, EUnary'(..)
, EIf'(..)
, toSExpr
)
where

import Maid.Tokenizer.Span ( Spanned(..) )

import qualified Maid.Tokenizer.Token as T

data EBinary' = EBinary' { lhs :: Expr
                         , rhs :: Expr
                         , operator :: Spanned String
                         }
              deriving Show
data EUnary'  = EUnary' { operator :: Spanned String
                        , rhs :: Expr
                        }
              deriving Show

data EIf' = EIf' { cond :: Expr
                 , on_true :: Expr
                 , on_false :: Expr
                 }
          deriving Show

data Expr = EBinary EBinary'
          | EUnary EUnary'
          | EIdent (Spanned String)
          | ELiteral (Spanned T.Literal)
          | EIf EIf'
          deriving Show

toSExpr :: Expr -> String
toSExpr (EBinary (EBinary' lhs rhs (Spanned _ op'))) =
    "(" ++ op' ++ " " ++ toSExpr lhs ++ " " ++ toSExpr rhs ++ ")"
toSExpr (ELiteral (Spanned _ lit)) =
    case lit of
        T.LInt i -> show i
        _ -> error "Others are not covered currently"
toSExpr _ = error "Unimplemented"
