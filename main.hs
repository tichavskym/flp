import System.IO
import System.Environment
import Text.Read

-- This argument parsing logic was adapted from the book Learn You Haskell For Great Good
dispatch :: [(String, [String] -> IO ())]
dispatch = [("-1", classify), ("-2", train)]

main :: IO ()
main = do
    (command : args) <- getArgs
    let (Just action) = lookup command dispatch
    action args

data Tree = EmptyTree | Leaf String | Node (Int, Float) Tree Tree deriving (Show)

classify :: [FilePath] -> IO ()
classify (tree_filename:args) = do
    handle <- openFile tree_filename ReadMode
    tree <- buildTree handle 0
    hClose handle
    c <- classify' tree args
    print c

classify' :: Tree -> [FilePath] -> IO ()
classify' tree [filepath] = 
    withFile filepath ReadMode (\handle -> do
        classifyLines handle tree
    )
classify' _ (_:_) = 
    error "Too much arguments provided"
classify' _ _ = 
    error "Argument is missing"

classifyLines :: Handle -> Tree -> IO ()
classifyLines handle tree = do
    line <- hGetLine handle
    classifyOneLine (words [if c == ',' then ' ' else c|c <- line]) tree
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
            print c
        _ -> error "Unknown element in the tree!"


train :: [FilePath] -> IO ()
train (trainingData:_) = return ()

toNumber :: Read a => String -> a
toNumber x =
    case readMaybe x of
        Just number -> number
        Nothing -> error "Poorly formatted file, failed to parse a number"

buildTree :: Handle -> Int -> IO Tree
buildTree handle level = do
    -- TODO: I could check for whitespace characters in here
    line <- hGetLine handle
    let node = words (drop (2*level) line)
    print node
    case node of
        ["Node:", a, b] -> do
            left <- buildTree handle (level+1)
            right <- buildTree handle (level+1)
            return $ Node (toNumber (init a), toNumber b) left right
        ["Leaf:", c] -> 
            return $ Leaf c
        _ -> error "Poorly formatted input file"

-- TODO what if those files have newlines at the end?
