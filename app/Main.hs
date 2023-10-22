module Main
( main )
where

import Maid.Tokenizer.Mod ( tokenize )
import Text.Pretty.Simple ( pPrint )

main :: IO ()
main =
    let tokens = tokenize "`hello` <> `world`"
    in
        pPrint tokens

