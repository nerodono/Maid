module Main
( main )
where

import Maid.Tokenizer.Mod ( tokenize )

import qualified Maid.Parser.PrecedenceStore as PS

import Maid.Parser.Ast ( toSExpr )
import Maid.Parser.Mod ( binary, defaultPrecedence )

import Text.Pretty.Simple ( pPrint )
import System.Environment ( getArgs, getExecutablePath )
import System.IO ( readFile' )

executeFromFile :: FilePath -> IO ()
executeFromFile path = do
    content <- readFile' path
    putStrLn "Expr: "
    putStrLn content

    let tokens = tokenize content

    let pmap = PS.fromList [ ("+", defaultPrecedence)
                           , ("*", PS.mapPrecedence (+1) defaultPrecedence)
                           , ("^", PS.mapPrecedence (+2) defaultPrecedence)
                           ]
    let data' = binary pmap 0 tokens

    case data' of
        Left e -> do
            putStrLn "Error:"
            pPrint e
        Right (r, t) -> do
            putStrLn "Result: "
            putStrLn $ toSExpr r

            putStrLn "Tail: "
            pPrint t

    pure ()

main :: IO ()
main = do
    args <- getArgs
    exe <- getExecutablePath
    case args of
        [filename] -> executeFromFile filename
        _ ->
            putStrLn (exe ++ " usage: " ++ exe ++ " <filename>.maid")

