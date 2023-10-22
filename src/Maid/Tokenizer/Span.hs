{-# LANGUAGE FlexibleInstances #-}
module Maid.Tokenizer.Span
( Span(..)
, Spanned(..)
, applyOffset
, makeSpanned )
where

data Span = Span { offset :: Integer, size :: Integer }
          deriving Show
data Spanned a = Spanned Span a
               deriving Show

applyOffset :: Integer -> Spanned a -> Spanned a
applyOffset acc spanned =
    let (Spanned span' value) = spanned
        (Span offset size) = span'
        newSpan = Span (offset + acc) size
    in
        Spanned newSpan value

makeSpanned :: a -> Integer -> Integer -> Spanned a
makeSpanned value offset size = Spanned (Span offset size) value
