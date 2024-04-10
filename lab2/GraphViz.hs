module Lab2.GraphViz
    ( Tree (..)
    ) where

data Tree = Node String [Tree]

instance Show Tree where
    show tree = "digraph {\n" ++ snd (showNode tree 0) ++ "}"

showNode :: Tree -> Int -> (Int, String)
showNode (Node name children) prv =
    let (cur, subtree) = showChildren children prv $ prv + 1
    in (cur, "\tnode" ++ show prv ++ "[label = \""
             ++ name ++ "\"]\n" ++ subtree)

showChildren :: [Tree] -> Int -> Int -> (Int, String)
showChildren [] _ cur = (cur, "")
showChildren (x:xs) prv cur =
    let (nxt, child) = showNode x cur
        (cnt, other) = showChildren xs prv nxt
    in (cnt, child ++ "\tnode" ++ show prv ++ " -> node"
             ++ show cur ++ "\n" ++ other)
