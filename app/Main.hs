module Main
( main )
where

import Maid.Tokenizer.Mod ( tokenize )

import qualified Maid.Parser.PrecedenceStore as PS
import Maid.Parser.Mod ( factor, defaultPrecedence )

import Text.Pretty.Simple ( pPrint )
import System.Environment ( getArgs, getExecutablePath )
import System.IO ( readFile' )

executeFromFile :: FilePath -> IO ()
executeFromFile path = do
    content <- readFile' path
    let tokens = tokenize content

    let pmap = PS.fromList [("+", defaultPrecedence)
                           , ("*", PS.mapPrecedence (+1) defaultPrecedence)
                           ]
    let data' = factor pmap tokens
    pPrint data'

    pure ()

main :: IO ()
main = do
    args <- getArgs
    exe <- getExecutablePath
    case args of
        [filename] -> executeFromFile filename
        _ ->
            putStrLn (exe ++ " usage: " ++ exe ++ " <filename>.maid")

