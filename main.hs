import System.IO
import System.Environment

-- This argument parsing logic was adapted from the book Learn You Haskell For Great Good
dispatch :: [(String, [String] -> IO ())]
dispatch = [("-1", classify), ("-2", train)]

main = do
    (command : args) <- getArgs
    let (Just action) = lookup command dispatch
    action args

classify :: [FilePath] -> IO ()
classify (tree_filename:args) = do 
    contents <- readFile tree_filename
    putStr contents
    
    -- writeFile dest contents

train :: [FilePath] -> IO ()
train (trainingData:_) = do
    contents <- readFile trainingData
    putStr contents
