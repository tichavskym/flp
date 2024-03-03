import System.IO
import System.Environment
import Text.Read
-- TODO are these imports ok? according to the assignment
import Data.List (sortBy, foldl')
import Data.Function (on)
import Data.Map.Strict as Map

-- This argument parsing logic was adapted from the book Learn You Haskell For Great Good
dispatch :: [(String, [String] -> IO ())]
dispatch = [("-1", classify), ("-2", train)]

main :: IO ()
main = do
    (command : args) <- getArgs
    case Prelude.lookup command dispatch of
        Just action -> action args
        Nothing -> error "Command not found."

data Tree = EmptyTree | Leaf String | Node (Int, Float) Tree Tree deriving (Show)

classify :: [FilePath] -> IO ()
classify (tree_filename:args) = do
    handle <- openFile tree_filename ReadMode
    tree <- buildTree handle 0
    hClose handle
    classify' tree args

classify' :: Tree -> [FilePath] -> IO ()
classify' tree [filepath] = 
    withFile filepath ReadMode (\handle -> do
        classifyLines handle tree
    )
classify' _ (_:_) = 
    error "Too much arguments provided"
classify' _ _ = 
    error "Argument is missing"

splitLine :: [Char] -> [String]
splitLine line = words [if c == ',' then ' ' else c|c <- line]

classifyLines :: Handle -> Tree -> IO ()
classifyLines handle tree = do
    line <- hGetLine handle
    classifyOneLine (splitLine line) tree
    eof <- hIsEOF handle
    if eof then
        return ()
    else
        classifyLines handle tree

classifyOneLine:: [String] -> Tree -> IO ()
classifyOneLine values tree = do
    case tree of
        (Node (i, t) l r) -> do
            let value = values !! i
            if toNumber value <= t then
                classifyOneLine values l
            else
                classifyOneLine values r
        (Leaf c) -> do
            putStrLn c
        _ -> error "Unknown element in the tree!"


toNumber :: Read a => String -> a
toNumber x =
    case readMaybe x of
        Just number -> number
        Nothing -> error "Poorly formatted file, failed to parse a number"

buildTree :: Handle -> Int -> IO Tree
buildTree handle level = do
    -- TODO: I could check for whitespace characters in here
    line <- hGetLine handle
    let node = words (Prelude.drop (2*level) line)
    case node of
        ["Node:", a, b] -> do
            left <- buildTree handle (level+1)
            right <- buildTree handle (level+1)
            return $ Node (toNumber (init a), toNumber b) left right
        ["Leaf:", c] -> 
            return $ Leaf c
        _ -> error "Poorly formatted input file"

-- TODO what if those files have newlines at the end?

train :: [FilePath] -> IO ()
train (training_dataset_filename:_) = do
    contents <- readFile training_dataset_filename
    let dataset = Prelude.map splitLine (lines contents)
    trainTree dataset 0  
    return ()

sortAttribute :: [[String]] -> Int -> [[String]]
sortAttribute list i = sortBy (compare `on` \l-> l !! i) list

features :: Foldable t => [t a] -> Int
features [] = 0
features (x:_) = length x - 1

thresholdSplit :: [[String]] -> [[String]] -> Int -> Float -> ([[String]], [[String]])
thresholdSplit [] y f t = ([], y)
thresholdSplit (x:xs) y f t = do
    if toNumber (x !! f) < t then
        thresholdSplit xs (x:y) f t  
    else
        (x:xs, y)

trainTree :: [[String]] -> Int -> IO ()
trainTree dataset indent = do
    if length (countClasses dataset) == 1 then do
        putStrLn (concat (replicate indent " ") ++ "Leaf: " ++ last (head dataset))
    else do
        let (g, f, t) = trainTree' dataset (features dataset - 1)
        putStrLn (concat (replicate indent " ") ++ "Node: " ++ show f ++ ", " ++ show t)
        let (x, y) = thresholdSplit (sortAttribute dataset f) [[]] f t    
        trainTree x (indent + 2)
        trainTree y (indent + 2)
    

trainTree' :: [[String]] -> Int -> (Float, Int, Float)
trainTree' _ (-1) = (1000, 0 ,0)
trainTree' dataset feature =
    if g < g2 then
        (g, f , t)
    else (g2, f2, t2)
    where
        (g, f, t) = trainTree'' [[]] (sortAttribute dataset feature) (1000, 0, 0.0) feature
        (g2, f2, t2) = trainTree' dataset (feature-1)

    
getMean :: [String] -> [String] -> Int -> Float
getMean x y f =
    (read (y !! f) + read (x !! f)) / 2

myConcat :: [[a]] -> [[a]] -> [[a]]
myConcat [[]] y = y
myConcat x y = x ++ y

trainTree'' :: [[String]] -> [[String]] -> (Float, Int, Float) -> Int -> (Float, Int, Float)
trainTree'' _ [] (ginisc, feature, threshold) _ = (ginisc, feature, threshold)
trainTree'' _ [_] (ginisc, feature, threshold) _ = (ginisc, feature, threshold)
trainTree'' x (y:ys) (gini, feature, threshold) current_feature =
    if new_gini < gini then
        trainTree'' (myConcat x [y]) ys (new_gini, current_feature, new_threshold) current_feature
        else trainTree'' (myConcat x [y]) ys (gini, feature, threshold) current_feature
    where
        new_threshold = getMean y (head ys) current_feature
        new_gini = giniScore (myConcat x [y]) ys

giniScore :: [[String]] -> [[String]] -> Float
giniScore f s = 
    (giniScore' f + giniScore' s)/(fromIntegral (length f) + fromIntegral (length s))

giniScore' :: [[String]] -> Float
giniScore' x = (1 - sum (Prelude.map (\(_, b) -> (b/total)*(b/total))  (Map.toList (countClasses x)))) * total
    where
        total = fromIntegral (length x)

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just (last xs)

-- TODO
countClasses :: [[String]] -> Map String Float
countClasses = Data.List.foldl' (\mp x -> case safeLast x of 
    Just l -> Map.insertWith (+) l 1 mp
    Nothing -> mp) Map.empty
