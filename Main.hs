module Main where

import Debug.Trace( trace )
import System.Process
import Data.Char
import System.Environment
import Parser
import RustGen
import PythonGen
import FSharpGen
import JSGen
import CodeGen
import Text.Megaparsec as P

main :: IO ()
main = do
    [mode, fn, outFn, supportCode] <- getArgs
    file <- readFile fn
    writeFile outFn $ dispatch mode supportCode parseLexeme fn file 
    where
        dispatch "python" = compile Python
        dispatch "rust" = compile Rust
        dispatch "fs" = compile FSharp
        dispatch "js" = compile JS
        dispatch lang = error $ lang ++ " is not support for now"