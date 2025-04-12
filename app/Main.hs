module Main (main) where

import Numeric.Gurobi

main :: IO ()
main = do
  env <- emptyEnv 
  setStrParam env "LogFile" "gurobi.log"
  startEnv env

  model <- newModel env "mip1"
  

  return ()
