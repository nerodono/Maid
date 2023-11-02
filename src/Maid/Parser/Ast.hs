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

data EBinary' = EBinary' { operator :: Spanned String
                         , lhs :: Expr
                         , rhs :: Expr
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
          | EApply Expr Expr
          deriving Show

toSExpr :: Expr -> String
toSExpr (EBinary (EBinary' (Spanned _ op') lhs rhs)) =
    "(" ++ op' ++ " " ++ toSExpr lhs ++ " " ++ toSExpr rhs ++ ")"
toSExpr (EUnary (EUnary' (Spanned _ op') rhs)) =
    "(" ++ op' ++ " " ++ toSExpr rhs ++ ")"
toSExpr (EIdent (Spanned _ ident)) = ident
toSExpr (ELiteral (Spanned _ lit)) =
    case lit of
        T.LInt i -> show i
        _ -> error "Others are not covered currently"
toSExpr (EIf (EIf' cond on_true on_false)) =
    "(if " ++ toSExpr cond ++ " " ++ toSExpr on_true ++ " " ++ toSExpr on_false ++ ")"
toSExpr (EApply lhs argument) =
    "(@apply " ++ toSExpr lhs ++ " " ++ toSExpr argument ++ ")"
