module Main (main) where

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

  setObjective model ([(1,x), (1,y), (2,z)], 0) MAXIMIZE

  -- Add constraint: x + 2 y + 3 z <= 4
  c0 <- addConstr model ([(1,x), (2,y), (3,z)], 0) LESS_EQUAL 4 "c0"

  -- Add constraint: x + y >= 1
  c1 <- addConstr model ([(1,x), (1,y)], 0) GREATER_EQUAL 1 "c1"

  -- Optimize model
  optimize model

  write model "mip1.lp"

  status <- getStatus model
  print status

  objval <- getObjVal model
  print objval

  vx <- getX x
  vy <- getX y
  vz <- getX z
  print (vx, vy, vz)

  return ()
