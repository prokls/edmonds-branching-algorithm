import Control.Monad
import Data.List (minimumBy, partition, isPrefixOf)
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map
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

readEdgeLine :: String -> Edge
readEdgeLine s = case words s of
  [s,d,w]   -> (Edge (read s) (read d) (read w))
  otherwise -> error "Invalid edge line"

readHeaderLine :: String -> (Integer, Integer, Integer)
readHeaderLine l = case words l of
  [a,b,c] -> ((read a), (read b), (read c))
  _ -> error "Invalid header line"

fromString' :: [String] -> Graph
fromString' (l:ls) =
  case readHeaderLine l of
    (nrV, nrE, root) -> (Graph [1,2..nrV] edges root)
  where
    edges = map (readEdgeLine) ls

fromString :: String -> Graph
fromString src = fromString' (filter (\ l -> noHash l && noBLine l) (lines src))
  where
    noHash = (not . (isPrefixOf "#"))
    noBLine = (not . (isPrefixOf "b "))

showEdges :: [Edge] -> String
showEdges []     = ""
showEdges (e:es) = (show $ src e) ++ " " ++ (show $ dst e) ++ " " ++
                   (show $ weight e) ++ "\n" ++ (showEdges es)

toString :: Graph -> String
toString (Graph vs es r) =
    (show $ maxVertex vs) ++ " " ++
    (show $ length es) ++ " " ++
    (show r) ++ " " ++
    (show $ totalGraphWeight) ++ "\n" ++
    showEdges es
  where
    maxVertex (v':vs') = if v' > maxVertex vs' then v' else maxVertex vs'
    maxVertex []       = 0
    totalGraphWeight   = sum $ map weight es

-- [AUXILIARY FUNCTIONS]

minWeightEdge :: [Edge] -> Edge
minWeightEdge [] = error "Missing an edge"
minWeightEdge es = minimumBy (comparing weight) es

getCycle :: Path -> Path
getCycle p =
    dropWhile (\ e -> Set.member (src e) reached) p
  where
    srcs = Set.fromList $ map src p
    dsts = Set.fromList $ map dst p
    reached = Set.union srcs dsts

dfs :: [Edge] -> Path -> Vertex -> [Path]
dfs es base r =
    [e:base | e <- deadEdges] ++
      (concat [(dfs es (e:base) r) | e <- nextEdges])
  where
    srcs = Set.fromList $ map src base
    dsts = Set.fromList $ map dst base
    reachedVertices = Set.union srcs dsts
    start = if null base then r else (src $ last es)
    newEdges = filter (\ e -> (notElem e base) && (src e) == start) es
    contEdges = filter (\ e -> Set.member (src e) reachedVertices) newEdges
    revisit e = Set.member (dst e) reachedVertices
    (deadEdges, nextEdges) = partition revisit contEdges

-- [EDMONDS HELPERS]

removeMultiEdges :: [Edge] -> [Edge]
removeMultiEdges []           = []
removeMultiEdges gedges@(e:_) =
  minEdge:(removeMultiEdges different)
  where
    (alike, different) = partition (\ e' -> (src e, dst e) == (src e', dst e')) gedges
    minEdge            = minWeightEdge alike

cheapestEdges :: [Edge] -> [Edge]
cheapestEdges []     = []
cheapestEdges gedges@(e:_) =
  cheapestEdge:(cheapestEdges different)
  where
    (alike, different) = partition ((== (dst e)) . dst) gedges
    cheapestEdge       = minWeightEdge alike


-- [EDMONDS BRANCHING ALGORITHM]

edmondsEdge :: Edge -> [Edge] -> Set Vertex -> Vertex -> Edge
edmondsEdge edge@(Edge u v w) es cycleVertices contractVertex
    | not uInC && vInC     = (Edge u contractVertex (w - cheapW))
    | uInC && not vInC     = (Edge contractVertex v w)
    | not uInC && not vInC = edge
  where
    cheapW = minimum $ map weight $ filter ((== v) . dst) es
    uInC = (Set.member u cycleVertices)
    vInC = (Set.member v cycleVertices)

edmondsCycle :: Graph -> [Edge] -> Graph
edmondsCycle g' cycle =
    (Graph (vertices g') (filter (/= inCycleUv) (edges g')) (root g'))
  where
    newVertex = (maximum $ vertices g') + 1
    cycleVertices = Set.insert (src $ head cycle) $ Set.fromList $ map dst cycle
    edEdge e = (edmondsEdge e (edges g') cycleVertices newVertex)
    associatedEdges = [(e, edEdge e) | e <- (edges g')]
    recurse = (edmonds (Graph (newVertex:(vertices g')) (map snd associatedEdges) (root g')))
    uv = (head $ filter ((== newVertex) . dst) (edges recurse)) :: Edge
    oldUv = snd $ head $ filter (\ (o, n) -> n == uv) associatedEdges
    inCycleUv = head $ filter ((== (dst oldUv)) . dst) cycle

edmonds :: Graph -> Graph
edmonds g@(Graph vs es r) =
    if null cycles
    then (Graph vs cheapEdges r)
    else (edmondsCycle (Graph vs cheapEdges r) (head cycles))
  where
    notToRoot = filter ((/= r) . dst) es
    reducedEdges = (removeMultiEdges es) -- notToRoot
    cheapEdges = (cheapestEdges es)
    paths = (dfs cheapEdges [] r)
    cycles = filter (not . null) (map getCycle paths)

-- [MAIN ROUTINE, using monads]

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
