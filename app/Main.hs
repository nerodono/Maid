module Main
( main )
where

import Maid.Tokenizer.Mod ( tokenize )

import qualified Maid.Parser.PrecedenceStore as PS

import Maid.Parser.Ast ( toSExpr )
import Maid.Parser.Mod ( expression
                       , defaultPrecedence
                       , maxPrecedence
                       )

import Text.Pretty.Simple ( pPrint )
import System.Environment ( getArgs, getExecutablePath )
import System.IO ( readFile' )

executeFromFile :: FilePath -> IO ()
executeFromFile path = do
    content <- readFile' path
    putStrLn "Expr: "
    putStrLn content

    let tokens = tokenize content

    let pmap = PS.fromList [ (PS.binaryOp "+", defaultPrecedence)
                           , (PS.unaryOp "-", PS.mapPrecedence (flip (-) 2) maxPrecedence)
                           , (PS.binaryOp "+", PS.mapPrecedence (+1) defaultPrecedence)
                           ]
    let data' = expression pmap tokens

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

