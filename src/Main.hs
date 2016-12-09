module Main where

import ReferenceGame (Agent, object, speak, listen, learn, startingAgent)

main :: IO ()
main =
  do final <- foldr (flip (>>=)) (return startingAgent) (replicate 10000 step)
     _ <- putStrLn (show final)
     return ()

step :: Agent -> IO Agent
step ag =
  do obj <- object
     msg <- speak obj ag
     heard <- listen msg ag
     let ag' = if obj == heard then learn (obj, msg) ag else ag
     return ag'
