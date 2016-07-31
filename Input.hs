module Input where

import qualified Data.Array as A
import qualified Data.List as L

import Diagram


data Token
    = Title String
    | Node NodeInd NodeType
    | Link (NodeInd, NodeInd) LinkType
    deriving (Show, Read, Eq)


isTitle (Title _) = True
isTitle _         = False

isNode (Node _ _) = True
isNode _        = False

isLink (Link _ _) = True
isLink _        = False


readDiagram :: String -> Diagram
readDiagram str
  | length ts < 1   = error "No title declaration found"
  | length ts > 1   = error "Duplicate title declaration"
  | length ns == 0  = error "No diagram nodes present"
  | otherwise       = Diagram { title = t, nodes = n, links = l }
  where
    tokens = map read (filter (not . null) $ lines str) :: [Token]
    ts     = filter isTitle tokens
    t      = let Title x = ts !! 0 in x

    ns     = L.sortBy (\(Node i1 _) (Node i2 _) -> compare i1 i2) $ filter isNode tokens
    ns_b   = let Node i _ = head ns in i
    ns_e   = let Node i _ = last ns in i
    n      = A.array (ns_b, ns_e) [(i, ty) | Node i ty <- ns]

    ls     = filter isLink tokens
    l_r    = ((ns_b, ns_b), (ns_e, ns_e))
    l      = A.array l_r [((i, j), getLink i j) | (i, j) <- A.range l_r]

    getLink i j
      | len > 1    = error $ "Duplicate link present: " ++ show i ++ " - " ++ show j
      | len == 1   = let Link (_, _) ty = f !! 0 in ty
      | otherwise  = LinkZero
      where
        f = filter (\(Link (x, y) _) -> i == x && j == y || i == y && j == x) ls
        len = length f

