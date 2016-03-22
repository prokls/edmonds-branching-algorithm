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
  [s,d,w] -> (Edge (read s) (read d) (read w))
  _       -> error "Invalid edge line"

readHeaderLine :: String -> (Integer, Integer, Integer)
readHeaderLine l = case words l of
  [a,b,c] -> ((read a), (read b), (read c))
  _ -> error "Invalid header line"

fromString' :: [String] -> Graph
fromString' [] = (Graph [] [] 0)
fromString' (l:ls) =
  case readHeaderLine l of
    (nrV, _, root) -> (Graph [1,2..nrV] edges root)
  where
    edges = map (readEdgeLine) ls

fromString :: String -> Graph
fromString src = fromString' (filter (\ l -> noHash l && noBLine l) (lines src))
  where
    noHash = (not . (isPrefixOf "#"))
    noBLine = (not . (isPrefixOf "b "))

showEdges :: [Edge] -> String -> String
showEdges _ []          = ""
showEdges (e:es) prefix = prefix ++ (show $ src e) ++ " " ++ (show $ dst e) ++
                          " " ++ (show $ weight e) ++ "\n" ++ (showEdges es prefix)

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

--takeMiddle :: (a -> Bool) -> (a -> Bool) -> Bool -> [a] -> [a]
--takeMiddle c1 c2 st (e:es) =
--  if st
--  then
--    if c2 e
--    then (e:(takeMiddle c1 c2 True es))
--    else [e]
--  else
--    if c1 e
--    then (takeMiddle c1 c2 False es)
--    else (e:(takeMiddle c1 c2 True es))
--takeMiddle _ _ _ [] = []

minWeightEdge :: [Edge] -> Edge
minWeightEdge [] = error "Missing an edge"
minWeightEdge es = minimumBy (comparing weight) es

--traverseCycle :: Path -> Set Vertex -> Vertex
--traverseCycle [] _ = -1
--traverseCycle (e:p) visited =
--  if Set.member (dst e) visited
--  then dst e
--  else traverseCycle p (Set.insert (dst e) visited)

--getCycle :: Path -> Path
--getCycle p =
--    if cycleVertex == -1
--    then []
--    else (reverse (dropWhile cond (reverse (dropWhile cond p))))
--    --takeMiddle ((/= cycleVertex) . dst) ((/= cycleVertex) . src) False p
--  where
--    cycleVertex = traverseCycle p (Set.fromList [src $ head p])
--    cond e = (dst e /= cycleVertex) && (src e /= cycleVertex)

backTraversePath :: (Path, Vertex) -> Path
backTraversePath ([], _) = []
backTraversePath ((e:p), v) =
  if src e == v
  then (e:p)
  else (e:(backTraversePath (p, v)))

traversePath :: Path -> Path -> Set Vertex -> (Path, Vertex)
traversePath [] _ _ = ([], -1)
traversePath (e:p) path visited =
  if Set.member (src e) visited
  then (e:path, src e)
  else traversePath p (e:path) (Set.insert (src e) visited)

getCycle :: Path -> Path
getCycle [] = []
getCycle p@(e:_) =
  backTraversePath $ traversePath p [] (Set.fromList [dst e])


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
    | otherwise            = edge
  where
    cheapW = minimum $ map weight $ filter ((== v) . dst) es
    uInC = Set.member u cycleVertices
    vInC = Set.member v cycleVertices

edmondsCycle :: Graph -> Path -> Graph
edmondsCycle g' cycle =
    (Graph (vertices g') (concat [(filter (/= inCycleUv) cycleEdges), map unmapEdge (edges recurse)]) (root g'))
  where
    newVertex = (maximum $ vertices g') + 1
    cycleVertices = Set.insert (src $ head cycle) $ Set.fromList $ map dst cycle
    inCycle e = (Set.member (src e) cycleVertices && Set.member (dst e) cycleVertices)
    (cycleEdges, otherEdges) = partition inCycle (edges g')
    mapEdge e = edmondsEdge e (edges g') cycleVertices newVertex
    associatedEdges = [(e, mapEdge e) | e <- otherEdges]
    recurse = edmonds (Graph (newVertex:(vertices g')) (map snd associatedEdges) (root g'))
    unmapEdge e = fst $ head $ filter ((== e) . snd) associatedEdges
    uvc = head $ filter ((== newVertex) . dst) (edges recurse)
    uv = unmapEdge uvc
    inCycleUv = head $ filter ((== (dst uv)) . dst) cycle

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
