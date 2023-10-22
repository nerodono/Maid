{-# LANGUAGE DuplicateRecordFields #-}

module Maid.Parser.Ast
( Expr(..)
, EBinary'(..)
, EUnary'(..)
, EIf'(..)
)
where

import Maid.Tokenizer.Span ( Spanned )

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

data Expr = EBinary (Spanned EBinary')
          | EUnary (Spanned EUnary')
          | EIdent (Spanned String)
          | ELiteral (Spanned T.Literal)
          | EIf EIf'
          deriving Show
