import System.IO
import System.Environment
import Text.Read

-- This argument parsing logic was adapted from the book Learn You Haskell For Great Good
dispatch :: [(String, [String] -> IO ())]
dispatch = [("-1", classify), ("-2", train)]

main = do
    (command : args) <- getArgs
    let (Just action) = lookup command dispatch
    action args

data Tree = EmptyTree | Leaf String | Node (Integer, Float) Tree Tree deriving (Show)

classify :: [FilePath] -> IO ()
classify (tree_filename:args) = do
    handle <- openFile tree_filename ReadMode
    tree <- buildTree handle 0
    print tree
    hClose handle
    
    -- writeFile dest contents

train :: [FilePath] -> IO ()
train (trainingData:_) = return ()

toNumber x =
    case readMaybe x of
        Just number -> number
        Nothing -> error "Poorly formatted file, failed to parse a number"

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
