module Diagram where

import qualified Data.Array as A


type NodeInd = Int


data NodeType
    = NodeOrdinary
    | NodeShortened
    | NodeExtraShort
    deriving (Show, Read, Eq)


data LinkType
    = LinkZero
    | LinkSingle
    | LinkDouble
    | LinkTriple
    deriving (Show, Read, Eq)


data Diagram = Diagram {
    title :: String,
    nodes :: A.Array Int NodeType,
    links :: A.Array (Int, Int) LinkType
} deriving (Show, Read, Eq)


nodeLength :: NodeType -> String
nodeLength NodeOrdinary = "1"
nodeLength NodeShortened = "(1/Sqrt[2])"
nodeLength NodeExtraShort = "(1/Sqrt[3])"


angleCosStr :: LinkType -> String
angleCosStr LinkZero = "0"
angleCosStr LinkSingle = "(-1/2)"
angleCosStr LinkDouble = "(-1/Sqrt[2])"
angleCosStr LinkTriple = "(-Sqrt[3]/2)"

