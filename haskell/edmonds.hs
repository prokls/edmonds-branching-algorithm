import Data.List (minimumBy, partition, isPrefixOf)
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as Set
import System.Exit
import System.Environment

-- [Graph DATA STRUCTURE]

data Edge = Edge
  { src :: Integer
  , dst :: Integer
  , weight :: Float
  } deriving (Eq, Show)

data Graph = Graph
  { vertices :: [Integer]
  , edges    :: [Edge]
  , root     :: Integer
  } deriving (Eq, Show)

type Vertex = Integer
type Path = [Edge]

-- [PARSING & REPRESENTATION]

-- Given a string representation of an Edge, parse this line
readEdgeLine :: String -> Edge
readEdgeLine str = case words str of
  [s,d,w] -> (Edge (read s) (read d) (read w))
  _       -> error "Invalid edge line"

-- Given a string representation of (#vertices, #edges, root), parse this line
readHeaderLine :: String -> (Integer, Integer, Integer)
readHeaderLine l = case words l of
  [a,b,c] -> ((read a), (read b), (read c))
  _ -> error "Invalid header line"

-- helper function: dispatch line parsing
fromString' :: [String] -> Graph
fromString' [] = (Graph [] [] 0)
fromString' (l:ls) =
  case readHeaderLine l of
    (nrV, _, r) -> (Graph [1,2..nrV] es r)
  where
    es = map (readEdgeLine) ls

-- Given string representation of a graph, parse lines to retrieve graph
fromString :: String -> Graph
fromString str = fromString' (filter (\ l -> noHash l && noBLine l) (lines str))
  where
    noHash = (not . (isPrefixOf "#"))
    noBLine = (not . (isPrefixOf "b "))

-- Given a set of Edges, represent edges as string
showEdges :: [Edge] -> String -> String
showEdges [] _          = ""
showEdges (e:es) prefix = prefix ++ (show $ src e) ++ " " ++ (show $ dst e) ++
                          " " ++ (show $ weight e) ++ "\n" ++ (showEdges es prefix)

-- Given a graph, represent as string
toString :: Graph -> String
toString (Graph vs es r) =
    (show $ maxVertex vs) ++ " " ++
    (show $ length es) ++ " " ++
    (show r) ++ " " ++
    (show $ totalGraphWeight) ++ "\n" ++
    (showEdges es "")
  where
    maxVertex (v':vs') = if v' > maxVertex vs' then v' else maxVertex vs'
    maxVertex []       = 0
    totalGraphWeight   = sum $ map weight es

-- [AUXILIARY FUNCTIONS]

-- Given a set of edges, determine the smallest weight occuring in this set
minWeightEdge :: [Edge] -> Edge
minWeightEdge [] = error "Missing an edge"
minWeightEdge es = minimumBy (comparing weight) es

-- Given a path, traverse this path backwards until given vertex is found
-- Returns a subset of the provided path
backTraversePath :: (Path, Vertex) -> Path
backTraversePath ([], _) = []
backTraversePath ((e:p), v) =
  if src e == v
  then (e:p)
  else (e:(backTraversePath (p, v)))

-- Given a path, traverse it until we hit some vertex already visited
-- Returns the path traversed and the vertex hit again
traversePath :: Path -> Path -> Set Vertex -> (Path, Vertex)
traversePath [] _ _ = ([], -1)
traversePath (e:p) path visited =
  if Set.member (src e) visited
  then (e:path, src e)
  else traversePath p (e:path) (Set.insert (src e) visited)

-- Given a path, retrieve some cycle traversed inside
getCycle :: Path -> Path
getCycle [] = []
getCycle p@(e:_) =
  backTraversePath $ traversePath p [] (Set.fromList [dst e])

-- Given a set of edges, a path traversed so far and an initial vertex
-- Return set of paths leading to dead ends or cycles (subsets of edges)
dfs :: [Edge] -> Path -> Vertex -> [Path]
dfs es base r =
    if null newEdges
    then [base]
    else
      [e:base | e <- deadEdges] ++
        (concat [(dfs es (e:base) (dst e)) | e <- nextEdges])
  where
    srcs = Set.fromList $ map src base
    dsts = Set.fromList $ map dst base
    reachedVertices = Set.union srcs dsts
    start = if null base then r else (dst $ head base)
    newEdges = filter (\ e -> (src e) == start && notElem e base) es
    revisit e = Set.member (dst e) reachedVertices
    (deadEdges, nextEdges) = partition revisit newEdges

-- [EDMONDS HELPERS]

-- Given arbitrary edges (s, d, w), return edges with unique (s, d) and w minimized
removeMultiEdges :: [Edge] -> [Edge]
removeMultiEdges []           = []
removeMultiEdges gedges@(e:_) =
  minEdge:(removeMultiEdges different)
  where
    (alike, different) = partition (\ e' -> (src e, dst e) == (src e', dst e')) gedges
    minEdge            = minWeightEdge alike

-- Given a set of edges (s, d, w), retrieve a subset
-- such that d is unique and w minimized
cheapestEdges :: [Edge] -> [Edge]
cheapestEdges []     = []
cheapestEdges gedges@(e:_) =
  cheapestEdge:(cheapestEdges different)
  where
    (alike, different) = partition ((== (dst e)) . dst) gedges
    cheapestEdge       = minWeightEdge alike


-- [EDMONDS BRANCHING ALGORITHM]

-- map edge to new problem according to EBA
edmondsEdge :: Edge -> [Edge] -> Set Vertex -> Vertex -> Edge
edmondsEdge edge@(Edge u v w) es cycleVertices contractVertex
    | not uInC && vInC     = (Edge u contractVertex (w - cheapW))
    | uInC && not vInC     = (Edge contractVertex v w)
    | otherwise            = edge
  where
    cheapW = minimum $ map weight $ filter ((== v) . dst) es
    uInC = Set.member u cycleVertices
    vInC = Set.member v cycleVertices

-- Given a graph and a cycle, retrieve a problem with a contracted vertex.
-- Call recursion and decontract result. Return this result solving the original problem.
edmondsCycle :: Graph -> Path -> Graph
edmondsCycle g' cy =
    (Graph (vertices g') (concat [(filter (/= inCycleUv) cycleEdges), map unmapEdge (edges recurse)]) (root g'))
  where
    newVertex = (maximum $ vertices g') + 1
    cycleVertices = Set.insert (src $ head cy) $ Set.fromList $ map dst cy
    inCycle e = (Set.member (src e) cycleVertices && Set.member (dst e) cycleVertices)
    (cycleEdges, otherEdges) = partition inCycle (edges g')
    mapEdge e = edmondsEdge e (edges g') cycleVertices newVertex
    associatedEdges = [(e, mapEdge e) | e <- otherEdges]
    recurse = edmonds (Graph (newVertex:(vertices g')) (map snd associatedEdges) (root g'))
    unmapEdge e = fst $ head $ filter ((== e) . snd) associatedEdges
    uvc = head $ filter ((== newVertex) . dst) (edges recurse)
    uv = unmapEdge uvc
    inCycleUv = head $ filter ((== (dst uv)) . dst) cy

-- High-level call for EBA. Given a graph, return a min-weight arborescence
-- (GHCI example following)
-- :load *edmonds
-- let vs = [1,2,3,4,5,6]
-- let es = [(Edge 1 4 10.0), (Edge 1 2 10.0), (Edge 4 5 2.0), (Edge 1 3 2.0), (Edge 5 2 2.0), (Edge 3 4 4.0), (Edge 4 6 4.0), (Edge 2 3 1.0), (Edge 2 6 8.0)]
-- let arb = edmonds (Graph vs es 1)
-- arb
edmonds :: Graph -> Graph
edmonds g@(Graph vs es r) =
    if null cycles
    then (Graph vs cheapEdges r)
    else (edmondsCycle g (head cycles))
  where
    notToRoot = filter ((/= r) . dst) es
    reducedEdges = (removeMultiEdges notToRoot)
    cheapEdges = (cheapestEdges reducedEdges)
    paths = filter (not . null) $ concat $ map (\ v -> (dfs cheapEdges [] v)) vs
    cycles = filter (not . null) (map getCycle paths)

-- [MAIN ROUTINE]

main :: IO ()
main = do
  args <- getArgs
  let argc = length args
  let filepath = head args
  if argc == 1
    then do
      putStrLn
        =<< return . toString . edmonds . fromString
        =<< readFile filepath
    else do
      putStrLn "usage: ./edmonds <di.graph>"
      exitFailure
