module Diagram where

import qualified Data.Array as A


type NodeInd = Int


data NodeType
    = NodeOrdinary
    | NodeShortened
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

