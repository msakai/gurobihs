
{- This example formulates and solves the following simple MIP model:

     maximize    x +   y + 2 z
     subject to  x + 2 y + 3 z <= 4
                 x +   y       >= 1
                 x, y, z binary
-}
module Main (main) where

import Control.Monad

import Numeric.Gurobi

main :: IO ()
main = do
  env <- emptyEnv
  setStrParam env "LogFile" "gurobi.log"
  startEnv env

  -- Create a new model
  model <- newModel env "mip1"

  -- Create variables
  x <- addVar model "x" BINARY
  y <- addVar model "y" BINARY
  z <- addVar model "z" BINARY

  setObjective model (exprFromTerms [(1,x), (1,y), (2,z)]) MAXIMIZE

  -- Add constraint: x + 2 y + 3 z <= 4
  c0 <- addConstr model (exprFromTerms [(1,x), (2,y), (3,z)]) LESS_EQUAL 4 "c0"

  -- Add constraint: x + y >= 1
  c1 <- addConstr model (exprFromTerms [(1,x), (1,y)]) GREATER_EQUAL 1 "c1"

  -- Optimize model
  optimize model

  write model "mip1.lp"

  status <- getStatus model
  print status

  objval <- getObjVal model
  print objval

  forM_ [x, y, z] $ \v -> do
    name <- getVarName v
    val <- getX v
    putStrLn $ name ++ " = " ++ show val

  return ()
