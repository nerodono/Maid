module Main
( main )
where

import Maid.Tokenizer.Mod ( tokenize )

import qualified Maid.Parser.PrecedenceStore as PS

import Maid.Parser.Ast ( toSExpr )
import Maid.Parser.Expression ( expression )

import Text.Pretty.Simple ( pPrint )
import System.Environment ( getArgs, getExecutablePath )
import System.IO ( readFile' )

executeFromFile :: FilePath -> IO ()
executeFromFile path = do
    content <- readFile' path
    putStrLn "Expr: "
    putStrLn content

    let precedences = PS.fromList [ (PS.binary "+", PS.leftAssoc  4)
                                  , (PS.binary "-", PS.leftAssoc  4)
                                  , (PS.binary "*", PS.leftAssoc  5)
                                  , (PS.binary "^", PS.leftAssoc 7)
                                  , (PS.unary  "-", PS.leftAssoc  6)
                                  ]
    let tokens = tokenize content
    let parsed = expression (PS.fromRoot precedences) tokens

    case parsed of
        Right (result, tail') -> do
            putStrLn "Result:"
            putStrLn $ toSExpr result
            putStrLn "Tail:"
            pPrint tail'
        Left err -> do
            putStrLn "Error:"
            pPrint err

    pure ()

main :: IO ()
main = do
    args <- getArgs
    exe <- getExecutablePath
    case args of
        [filename] -> executeFromFile filename
        _ ->
            putStrLn (exe ++ " usage: " ++ exe ++ " <filename>.maid")

