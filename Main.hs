module Main where

import qualified System.Environment as E

import Diagram
import Input


main = do
    args <- E.getArgs
    if length args /= 1
        then error "Usage: dynkin <diagram.txt>"
        else run (args !! 0)


run file = do
    inp <- readFile file
    let d = seq inp $ readDiagram inp
    putStrLn $ show d

