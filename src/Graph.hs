
module Graph where

import           Control.Applicative
import qualified Data.Array                   as DA
import           Data.Graph
import           Data.GraphViz
import           Data.List                    (nub)
import           Data.Map                     (Map, fromList, (!))

import           System.Environment


import           Math.Geometry.Grid           (boundary, contains, distance,
                                               indices, isAdjacent,
                                               minimalPaths, neighbours, size,
                                               tileCount)
import           Math.Geometry.Grid.Hexagonal (hexHexGrid)
import           Math.Geometry.Grid.Square    (rectSquareGrid)
-- use for (!)
import qualified Math.Geometry.GridMap        as GM
import           Math.Geometry.GridMap.Lazy   (lazyGridMap)


myGraph :: Graph
myGraph = buildG bounds edges
    where bounds = (1,4)
          edges = [(1,3), (1,4), (2,3), (2,4), (3,4)]

myGraphShow = do
    print $ "The edges are " ++ (show.edges) myGraph
    print $ "The vertices are " ++ (show.vertices) myGraph


-- graph from an adjacency list
adjGraph :: Graph
adjGraph = fst $ graphFromEdges' [ ("Node 1", 1, [3,4])
                                 , ("Node 2", 2, [3,4])
                                 , ("Node 3", 3, [4])
                                 , ("Node 4", 4, []) ]

showAdjGraph = do
    print $ "The edges are " ++ (show.edges) adjGraph
    print $ "The vertices are " ++ (show.vertices) adjGraph



-- Topological Sort or Ordering on Graph
topo = do
    ls <-  lines <$> readFile "input.txt"
    let g = graph ls
    putStrLn $ showTopSort ls g

-- | The nub function removes duplicate elements from a list.
-- | In particular, it keeps only the first occurrence of each element.

-- | (!) - Find the value at a key. Calls error when the element can not be found.
-- | e.g fromList [(5,'a'), (3,'b')] ! 5 == 'a'

graph :: Ord k => [k] -> Graph
graph ls = buildG bounds edges
    where bounds = (1, (length.nub) ls)
          edges = tuples $ map (mappingStrToNum !) ls
          mappingStrToNum = fromList $ zip (nub ls) [1..] -- [("under..", 1), (do H.., 2)..]
          tuples (a:b:cs) = (a,b) : tuples cs
          tuples _        = []

showTopSort ls g =
    unlines $ map (mappingNumToStr !) (topSort g)
    where mappingNumToStr = fromList $ zip [1..] (nub ls)


bolla ls = do
    let on = (1, (length.nub) ls)
    print on
    let mapst = fromList $ zip (nub ls) [1..]
    print mapst
    let eddy = map (mapst !) ls
    print eddy
    let tuply = tup eddy
    print tuply
    let grapg = buildG on tuply
    print grapg
    let ts = topSort grapg
    print ts
    let poo = unlines $ map (mapNs !) ts
    print poo
    where
        tup (a:b:cs) = (a,b) : tup cs
        tup _        = []

        mapNs = fromList $ zip [1..] (nub ls)




-- Traversing a graph DEPTH-FIRST

graphEd :: (Graph, Vertex -> (Int, Int, [Int]))
graphEd = graphFromEdges'  [ (1, 1, [3, 4] )
                            , (2, 2, [3, 4])
                            , (3, 3, [4])
                            , (4, 4, []) ]

depth g i = depth' g [] i
depth' g@(gShape, gMapping) seen i = key : concatMap goDeeper adjacent
      where goDeeper v = if v `elem` seen then [] else depth' g (i:seen) v
            adjacent = gShape DA.! i
            (_, key, _) = gMapping i



-- Traversing a graph BREADTH-FIRST
-- depth-first is not feasable for infinite grapg
-- breadth-first suitable for finding shortest path
bGraph :: Graph
bGraph = buildG bounds edges
    where bounds = (1,7)
          edges = [(1,2), (1,5), (2,3), (2,4), (5,6), (5,7), (3,1)]

breadth g i = bf [] [i]
    where bf :: [Int] -> [Int] -> [Int]
          bf seen forest
                    | null forest = []
                    | otherwise = forest ++ bf (forest ++ seen) (concatMap goDeeper forest)
                    where goDeeper v = if v `elem` seen then [] else g DA.! v

-- First level Vertices --- breadth bGraph 1



-- Plotting
graphDot :: DotGraph Int
graphDot = graphElemsToDot graphParams nodes edgs

graphParams :: GraphvizParams Int String Bool () String
graphParams = defaultParams

nodes :: [(Int, String)]
nodes = map (\x -> (x, "")) [1..4]

edgs :: [(Int, Int, Bool)]
edgs = [ (1, 2, True)
        , (2, 3, True)
        , (3, 4, True)
        , (1, 4, True)
        , (2, 4, True)
        , (1, 3, True)]

main = addExtension (runGraphviz graphDot) Png "graphDot"


-- Using Directed Acyclic Word Graph




-- Hexagonal graph
hexmain = do
         let putStrLn' str = putStrLn ('\n':str)
         putStrLn' "Indices of hex grid:"
         print $ indices hex
         putStrLn' "Neighbors around (1,1) of hex grid:"
         print $ neighbours hex (1,1)
         putStrLn' "Indices of rect grid:"
         print $ indices rect
         putStrLn' "Neighbors around (1,1) of rect grid:"
         print $ neighbours rect (1,1)
         putStrLn' "value of hex at index (1,1)"
         print $ hexM GM.! (1,1)



hex = hexHexGrid 4
rect = rectSquareGrid 3 5
hexM = lazyGridMap hex [1..]
