module Main where

import qualified Data.Array as A
import qualified System.Environment as E

import Data.Array ((!))
import System.Directory
import System.Exit
import System.IO
import System.Process

import Diagram
import Input


-- Set debug to true and mathematica scripts
--  won't be removed after execution

debug = False


main = do
    args <- E.getArgs
    if length args /= 1
        then error "Usage: dynkin <diagram.txt>"
        else run (args !! 0)


run file = do
    let script = file ++ ".m"

    inp <- readFile file
    let d = seq inp $ readDiagram inp

    let ns = nodes d
    let ls = links d

    scriptExists <- doesFileExist script
    if scriptExists
        then removeFile script
        else return ()

    h <- openFile script WriteMode
    let write = hPutStrLn h

    write $ "(* Dynkin v1.0: " ++ title d ++ " *)"
    write ""

    utils <- readFile "Utils.m"
    write utils

    let is = A.range (A.bounds ns)
    let n = length is
    write $ "(* Cartan subalgebra dimensionality: " ++ show n ++ " *)"
    write ""

    write "(* Finding simple roots: *)"
    let a1 = foldl (++) "{1" (replicate (n-1) ", 0") ++ "}"
    write $ "p1 = " ++ a1 ++ ";"

    let iter k = do
        let nt = ns ! k

        let coords = ["p" ++ show k ++ "x" ++ show i | i <- [1..k]]
        let coordsStrPrefix = "{" ++ foldl1 (\a b -> a ++ ", " ++ b) coords
        let coordsStr = coordsStrPrefix ++ "}"
        let coordsStrFull = coordsStrPrefix ++ foldl (++) "" (replicate (n-k) ", 0") ++ "}"

        let conds = ["Evaluate[" ++ coordsStrFull ++ " . p" ++ show i ++ "] == "
                        ++ nodeLength nt ++ " * " ++ nodeLength (ns ! i) ++ " * "
                        ++ angleCosStr (ls ! (k, i)) | i <- [1..(k-1)]]

        let allConds = foldl (++) "" $ map (++ " && ") conds

        let sumSquares = foldl1 (\a b -> a ++ " + " ++ b)
                ["p" ++ show k ++ "x" ++ show i ++ "^2" | i <- [1..k]]

        write $ "p" ++ show k ++ " = Simplify[ " ++ coordsStrFull ++ " /. MySolve["
                    ++ allConds ++ "p" ++ show k ++ "x" ++ show k ++ " > 0 && "
                    ++ sumSquares ++ " == " ++ nodeLength nt ++ "^2"
                    ++ ", " ++ coordsStr ++ "] ];"

    sequence $ map iter [2..n]

    write $ "primeRoots = {"
        ++ foldl (\a b -> a ++ ", " ++ b) "p1" ["p" ++ show i | i <- [2..n]]
        ++ "};"
    write "roots = ExpandRoots[primeRoots];"
    write $ "Print[\"rootsLen = \", Length[roots], \";\"];"
    write "PrintRoots[roots, \"r\"];"
    
    closed <- hClose h

    done@(exitCode, stdout, stderr) <- seq closed $
            readProcessWithExitCode "math" ["-script", script] ""

    if debug
        then return ()
        else seq done $ removeFile script

    if exitCode == ExitSuccess
        then putStr stdout
        else error $ errByCode exitCode


errByCode ExitSuccess = undefined
errByCode (ExitFailure 11) = "There is no such Lie algebra"
errByCode (ExitFailure code)  = "Unknown error code (" ++ show code ++ ")"


