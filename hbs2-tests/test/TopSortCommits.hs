module Main where

import Control.Monad
import Control.Monad (replicateM)
import Data.Graph
import Data.List.Split
import System.Random

-- main = do
--     input <- getContents
--     let commitLines = lines input
--     let commitEdges = [(commit, parent) | line <- commitLines, let [commit, parent] = splitOn "|" line]
--     let (graph, nodeFromVertex, vertexFromKey) = graphFromEdges [(commit, commit, [parent]) | (commit, parent) <- commitEdges]
--     let sortedVertices = topSort graph
--     let sortedCommits = reverse [commit | vertex <- sortedVertices, let (commit, _, _) = nodeFromVertex vertex]
--     let ordered = zip sortedCommits [1..]
--     forM_ ordered \(s,n) ->  putStrLn (s <> " " <> show n)




generateCommitGraph :: Int -> IO [(String, String)]
generateCommitGraph edgesCount = do
    gen <- getStdGen
    let commitIds = randomRs ('a','z') gen :: [Char]
    let commitNames = take edgesCount $ map (\id -> "commit" ++ [id]) commitIds
    let parentNames = "root" : init commitNames
    return $ zip commitNames parentNames

main :: IO ()
main = do
    let edgesCount = 1000000 -- Set the desired number of edges
    commitEdges <- generateCommitGraph edgesCount
    mapM_ print commitEdges
    let (graph, nodeFromVertex, vertexFromKey) = graphFromEdges [(commit, commit, [parent]) | (commit, parent) <- commitEdges]
    let sortedVertices = topSort graph
    let sortedCommits = reverse [commit | vertex <- sortedVertices, let (commit, _, _) = nodeFromVertex vertex]
    let ordered = zip sortedCommits [1..]
    forM_ ordered \(s,n) ->  putStrLn (s <> " " <> show n)
