import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Text.Printf
import System.Exit
import System.Environment

-- [Graph DATA STRUCTURE]

data Graph = Graph
  { vertices :: [Integer]
  , edges    :: [(Integer, Integer, Float)]
  , root     :: Integer
  } deriving (Show)

showIntegerList :: [Integer] -> String
showIntegerList is = concat $ intersperse " " . map show $ is

showVertices :: [Integer] -> String
showVertices vs = showIntegerList vs

showEdges :: [(Integer, Integer, Float)] -> String
showEdges []           = []
showEdges ((s,d,w):es) = (show s) ++ " " ++ (show d) ++ " " ++ (show w) ++ "\n" ++ (showEdges es)

showRoot :: Integer -> String
showRoot r = show r

maxVertex :: [Integer] -> Maybe Integer
maxVertex []     = Nothing
maxVertex (v:vs) = case maxVertex vs of
  Just n -> if n > v then Just n else Just v
  Nothing -> Just v

maxWeight :: [(Integer, Integer, Float)] -> Maybe Float
maxWeight []           = Nothing
maxWeight ((s,d,w):es) = case maxWeight es of
  Just n -> if n > w then Just n else Just w
  Nothing -> Just w

minWeight :: [(Integer, Integer, Float)] -> Maybe Float
minWeight []           = Nothing
minWeight ((s,d,w):es) = case minWeight es of
  Just n -> if n < w then Just n else Just w
  Nothing -> Just w

totalGraphWeight :: [(Integer, Integer, Float)] -> Float
totalGraphWeight []           = 0.0
totalGraphWeight ((s,d,w):es) = w + (totalGraphWeight es)

-- [PARSING]

readEdgeLine :: String -> (Integer, Integer, Float)
readEdgeLine s = case words s of
  [s,d,w] -> (read s, read d, read w)
  true -> (0, 0, 0.0)

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
fromString src = fromString' (filter (\ v -> not (isPrefixOf "#" v)) (lines src))

-- [HELPERS]

minWeightEdge :: Graph -> Integer -> Integer -> Maybe Float
minWeightEdge (Graph v [] r) s d                        = Nothing
minWeightEdge (Graph v ((src,dest,weight):edges) r) s d =
  if src == s && dest == d
  then case minWeightEdge (Graph v edges r) s d of
    Just min -> if min > weight then Just weight else Just min
    Nothing -> Just weight
  else minWeightEdge (Graph v edges r) s d

maxWeightEdge :: Graph -> Integer -> Integer -> Maybe Float
maxWeightEdge (Graph v [] r) s d                        = Nothing
maxWeightEdge (Graph v ((src,dest,weight):edges) r) s d =
  if src == s && dest == d
  then case maxWeightEdge (Graph v edges r) s d of
    Just max -> if max < weight then Just weight else Just max
    Nothing -> Just weight
  else maxWeightEdge (Graph v edges r) s d

minWeightDest :: Graph -> Integer -> Maybe Float
minWeightDest (Graph v [] r) d                        = Nothing
minWeightDest (Graph v ((src,dest,weight):edges) r) d =
  if dest == d
  then case minWeightDest (Graph v edges r) d of
    Just min -> if min > weight then Just weight else Just min
    Nothing -> Just weight
  else minWeightDest (Graph v edges r) d

directlyReachable :: Graph -> Integer -> Integer -> Bool
directlyReachable (Graph v [] r) src dest = False
directlyReachable (Graph v ((s,d,w):es) r) src dest =
  if (s == src) && (d == dest)
    then True
    else directlyReachable (Graph v es r) src dest

removeMultiEdges :: Graph -> Graph
removeMultiEdges (Graph v [] r) = (Graph v [] r)
removeMultiEdges g = next
  where
    (Graph v (e:es) r) = g
    (s,d,w)            = e
    minimalWeight      = fromMaybe w (minWeightEdge g s d)
    exclMultiE         = (filter (\ (src,dst,weight) -> src /= s || dst /= d) es)
    rec                = removeMultiEdges (Graph v exclMultiE r)
    next               = (Graph v ((s,d,minimalWeight):(edges rec)) r)

nextEdges :: Graph -> Integer -> [(Integer, Integer, Float)]
nextEdges (Graph v es r) start = filter (\ (s, d, w) -> s == start) es

--isCompleteCycle :: Graph -> Bool
--isCompleteCycle (Graph v es r) = s == d
--  where
--    (s, _) = head es
--    (_, d) = last es

-- Does Graph's edges visit the given vertex anytime?
reaches :: Graph -> Integer -> Bool
reaches (Graph v [] r) vertex               = False
reaches (Graph v [(from,to,w)] r) vertex    = from == vertex || to == vertex
reaches (Graph v ((from,to,w):es) r) vertex =
  if from == vertex
    then True
    else reaches (Graph v es r) vertex

-- Is any path described by Graph's edges visited more than once?
isRevisiting :: Graph -> Bool
isRevisiting (Graph v [] r)               = False
isRevisiting (Graph v ((from,to,w):es) r) =
  if reaches (Graph v es r) from
    then True
    else isRevisiting (Graph v es r)

-- Find cyclic subgraph using depth first search starting from any of vertices
dfs'' :: Graph -> [(Integer, Integer, Float)] -> Graph -> Maybe Graph
dfs'' base [] accu = Nothing
dfs'' base (option:options) accu = case ret of
    Just cycle -> Just cycle
    Nothing -> dfs'' base options accu
  where
    (Graph v e r) = accu
    (s,d,w) = option
    uniqV1 = if elem s v then v else s:v
    uniqV2 = if elem d uniqV1 then uniqV1 else d:uniqV1
    ret = dfs' base 0 (Graph uniqV2 (e ++ [option]) r) -- unidiomatic

dfs' :: Graph -> Integer -> Graph -> Maybe Graph
dfs' base 0 (Graph _ [] _) = Nothing
dfs' base 0 (Graph v path r) =
  if isRevisiting (Graph v path r)
    then Just (Graph v path r)
    else dfs'' base options (Graph v path r)
  where
    (Graph _ edges _) = base
    (s, d, w)         = last path
    options           = filter (\ e -> notElem e path) (nextEdges base d)
dfs' base start (Graph v path r) =
    dfs'' base options (Graph v path r)
  where
    options           = filter (\ e -> notElem e path) (nextEdges base start)


dfs :: Graph -> [Integer] -> Maybe Graph
dfs base []           = Nothing
dfs base (v:vertices) =
  case rec of
    Just cycle -> if start == end then Just cycle else dfs base vertices
    Nothing -> dfs base vertices
  where
    (Graph vs _ root) = base
    rec              = dfs' base v (Graph [] [] root)
    (start, _, _)    = head (edges $ fromMaybe (Graph [] [] 0) rec)
    (_, end, _)      = last (edges $ fromMaybe (Graph [] [] 0) rec)

someCycleGraph :: Graph -> Maybe Graph
someCycleGraph (Graph v e r) = dfs (Graph v e r) v

cheapestEdges :: Graph -> Graph
cheapestEdges graph =
  (Graph (vertices graph) ((s,d,mw):uniqueEdges) (root graph))
  where
    ((s,d,w):es) = edges graph
    mw           = fromMaybe 0.0 $ minWeightDest graph d
    uniqueEdges  = (filter (\ (src,dst,weight) -> src /= s || dst /= d) es)

removeEdgesByDest :: Graph -> Integer -> Graph
removeEdgesByDest (Graph v e r) d = (Graph v (filter (\ (src,dst,weight) -> dst /= d) e) r)

-- [EDMONDS BRANCHING ALGORITHM]

vertexInCycle :: Graph -> Integer -> Bool
vertexInCycle g vertex = elem vertex (vertices g)

edgeInCycle :: Graph -> (Integer, Integer, Float) -> Bool
edgeInCycle g e = elem e (edges g)

newVertex :: Graph -> Integer
newVertex g = (maximum $ vertices g) + 1

incoming :: Graph -> Integer -> Maybe (Integer, Integer, Float)
incoming g dst =
  if length incomings > 0
  then Just $ head incomings
  else Nothing
  where
    incomings = filter (\ (s,d,w) -> d == dst) (edges g)

weight :: (Integer, Integer, Float) -> Float
weight (s,d,w) = w

mapEdge :: Graph -> Integer -> (Integer, Integer, Float) -> Graph -> ((Integer, Integer, Float), (Integer, Integer, Float))
mapEdge g v_c (s,d,w) cycle =
  ((s,d,w),
  if not (vertexInCycle cycle s) && vertexInCycle cycle d
  then
    (s, v_c, w - (weight (fromMaybe (0,0,0.0) (incoming g d))))
  else if vertexInCycle cycle s && not (vertexInCycle cycle d) then
    (v_c, d, w)
  else
    (s,d,w))

lookupOld :: (Integer, Integer, Float) -> [(Integer, Integer, Float)] -> [(Integer, Integer, Float)] -> Maybe (Integer, Integer, Float)
lookupOld newEdge _ [] = Nothing
lookupOld newEdge [] _ = Nothing
lookupOld newEdge (o:old) (e:new) =
  if e == newEdge then Just o else lookupOld newEdge old new

recurseEdmonds :: Graph -> Integer -> Graph -> Graph
recurseEdmonds cheapG v_c cycle = 
    (Graph (vertices cheapG) (nEC ++ nEs) (root cheapG))
    -- create new graph
    -- remember correspondence
    -- invoke computeEdmonds
    -- map back
    -- remove one edge
    -- return graph
  where
    mapped = map (\ e -> mapEdge cheapG v_c e cycle) (edges cheapG)
    (oldEdges, newEdges) = unzip mapped
--
    (Graph vs es r) = computeEdmonds (Graph (v_c:vertices cheapG) newEdges (root cheapG))
    (u,v,w) = fromMaybe (0,0,0.0) $ case (incoming (Graph vs es r) v_c) of
       Just i -> lookupOld i oldEdges newEdges
       Nothing -> error "Edge got lost - internal error"
    nEs = map (\ e -> fromMaybe (0,0,0.0) $ lookupOld e oldEdges newEdges) es
    discardEdge = case incoming cycle v of
      Just edge -> edge
      Nothing -> (0,0,0.0)
    nEC = filter (/= discardEdge) (edges cycle)


computeEdmonds :: Graph -> Graph
computeEdmonds g =
  if isJust cycle
  then recurseEdmonds cheapG v_c (fromMaybe (Graph [] [] 0) cycle)
  else cheapG
  where
    redG   = removeMultiEdges $ removeEdgesByDest g (root g)
    cheapG = cheapestEdges redG
    cycle  = someCycleGraph cheapG
    v_c    = newVertex cheapG

toString :: Graph -> String
toString (Graph v e r) =
  (show (fromMaybe 0 (maxVertex v))) ++ " " ++
  (show (length e)) ++ " " ++
  (show r) ++ " " ++
  (show (totalGraphWeight e)) ++ "\n" ++
  showEdges e

main :: IO ()
main =
  do
    args <- getArgs
    let argc = length args
    let filepath = head args
    if argc == 1
      then do
        putStrLn
          =<< return . toString . computeEdmonds . fromString
          =<< readFile filepath
      else do
        putStrLn "usage: ./edmonds <di.graph>"
        exitFailure -- (exitWith 1)