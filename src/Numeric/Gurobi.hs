{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module Numeric.Gurobi where

import Control.Exception
import Control.Monad
import Data.Function
import Data.Hashable
import Data.IORef
import Data.Ord
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Foreign
import Foreign.C
import GHC.Generics (Generic)

import qualified Numeric.Gurobi.C as C

data Error = Error C.ErrorCode String
   deriving (Eq, Ord, Show, Read, Generic)

instance Exception Error

instance Hashable Error where
  hashWithSalt salt (Error err msg) = hashWithSalt salt (fromIntegral err :: Int, msg)

checkError :: C.Env -> IO C.ErrorCode -> IO ()
checkError env action = do
  err <- action
  if err /= 0
    then do
      msg <- peekCString =<< C.geterrormsg env
      throwIO $ Error err msg
    else return ()

data DataType
  = DTInt
  | DTDouble
  | DTString
  deriving (Eq, Ord, Enum, Bounded, Generic, Show, Read)

instance Hashable DataType

data AttrType
  = ModelAtrr
  | VarAttr
  | LinearConstrAttr
  | SOS1ConstrAttr
  | SOS2ConstrAttr
  | QConstrAttr
  | GenConstrAttr
  deriving (Eq, Ord, Enum, Bounded, Generic, Show, Read)

instance Hashable AttrType

getAttrInfo :: Model -> String -> IO (DataType, AttrType, Bool)
getAttrInfo model name =
  withModelPtr model $ \modelP ->
  alloca $ \typeP ->
  alloca $ \attrTypeP ->
  alloca $ \settableP ->
  withCString name $ \nameP -> do
    modelCheckError modelP $ C.getattrinfo modelP nameP typeP attrTypeP settableP
    dataType <- peek typeP
    attrType <- peek attrTypeP
    settable <- peek settableP
    return (toEnum (fromIntegral dataType), toEnum (fromIntegral attrType), settable /= 0)

getIntAttr :: Model -> String -> IO Int
getIntAttr model attrname = do
  withCString attrname $ \attrnameP -> getIntAttrPtr model attrnameP

getIntAttrPtr :: Model -> CString -> IO Int
getIntAttrPtr model attrnameP =
  withModelPtr model $ \modelP -> do
  alloca $ \valueP -> do
    modelCheckError modelP $ C.getintattr modelP attrnameP valueP
    fromIntegral <$> peek valueP

setIntAttr :: Model -> String -> Int -> IO ()
setIntAttr model attrname newvalue = do
  withCString attrname $ \attrnameP -> setIntAttrPtr model attrnameP newvalue

setIntAttrPtr :: Model -> CString -> Int -> IO ()
setIntAttrPtr model attrnameP newvalue =
  withModelPtr model $ \modelP -> do
     modelCheckError modelP $ C.setintattr modelP attrnameP (fromIntegral newvalue)

getDblAttr :: Model -> String -> IO Double
getDblAttr model attrname = do
  withCString attrname $ \attrnameP -> getDblAttrPtr model attrnameP

getDblAttrPtr :: Model -> CString -> IO Double
getDblAttrPtr model attrnameP =
  withModelPtr model $ \modelP ->
  alloca $ \valueP -> do
    modelCheckError modelP $ C.getdblattr modelP attrnameP valueP
    realToFrac <$> peek valueP

setDblAttr :: Model -> String -> Double -> IO ()
setDblAttr model attrname newvalue =
  withCString attrname $ \attrnameP -> do
    setDblAttrPtr model attrnameP newvalue

setDblAttrPtr :: Model -> CString -> Double -> IO ()
setDblAttrPtr model attrnameP newvalue  = do
  withModelPtr model $ \modelP -> do
    modelCheckError modelP $ C.setdblattr modelP attrnameP (realToFrac newvalue)

getDblAttrElement :: Model -> String -> Int -> IO Double
getDblAttrElement model attrname element = do
  withCString attrname $ \attrnameP -> getDblAttrElementPtr model attrnameP element

getDblAttrElementPtr :: Model -> CString -> Int -> IO Double
getDblAttrElementPtr model attrnameP element =
  withModelPtr model $ \modelP ->
  alloca $ \valueP -> do
    modelCheckError modelP $ C.getdblattrelement modelP attrnameP (fromIntegral element) valueP
    realToFrac <$> peek valueP

setDblAttrArray :: Model -> String -> Int -> Int -> Ptr CDouble -> IO ()
setDblAttrArray model attrname start len values = do
  withCString attrname $ \attrnameP -> setDblAttrArrayPtr model attrnameP start len values

setDblAttrArrayPtr :: Model -> CString -> Int -> Int -> Ptr CDouble -> IO ()
setDblAttrArrayPtr model attrnameP start len values =
  withModelPtr model $ \modelP -> do
    modelCheckError modelP $ C.setdblattrarray modelP attrnameP (fromIntegral start) (fromIntegral len) values

getStrAttrElement :: Model -> String -> Int -> IO String
getStrAttrElement model attrname element = do
  withCString attrname $ \attrnameP -> getStrAttrElementPtr model attrnameP element

getStrAttrElementPtr :: Model -> CString -> Int -> IO String
getStrAttrElementPtr model attrnameP element =
  withModelPtr model $ \modelP ->
  alloca $ \valueP -> do
    modelCheckError modelP $ C.getstrattrelement modelP attrnameP (fromIntegral element) valueP
    p <- peek valueP
    peekCString p

data ModelStatusCode
  = LOADED
  | OPTIMAL
  | INFEASIBLE
  | INF_OR_UNBD
  | UNBOUNDED
  | CUTOFF
  | ITERATION_LIMIT
  | NODE_LIMIT
  | TIME_LIMIT
  | SOLUTION_LIMIT
  | INTERRUPTED
  | NUMERIC
  | SUBOPTIMAL
  | INPROGRESS
  | USER_OBJ_LIMIT
  | WORK_LIMIT
  | MEM_LIMIT
  deriving (Show, Read, Eq, Ord, Bounded, Generic)

instance Enum ModelStatusCode where
  fromEnum LOADED          = 1
  fromEnum OPTIMAL         = 2
  fromEnum INFEASIBLE      = 3
  fromEnum INF_OR_UNBD     = 4
  fromEnum UNBOUNDED       = 5
  fromEnum CUTOFF          = 6
  fromEnum ITERATION_LIMIT = 7
  fromEnum NODE_LIMIT      = 8
  fromEnum TIME_LIMIT      = 9
  fromEnum SOLUTION_LIMIT  = 10
  fromEnum INTERRUPTED     = 11
  fromEnum NUMERIC         = 12
  fromEnum SUBOPTIMAL      = 13
  fromEnum INPROGRESS      = 14
  fromEnum USER_OBJ_LIMIT  = 15
  fromEnum WORK_LIMIT      = 16
  fromEnum MEM_LIMIT       = 17

  toEnum 1  = LOADED
  toEnum 2  = OPTIMAL
  toEnum 3  = INFEASIBLE
  toEnum 4  = INF_OR_UNBD
  toEnum 5  = UNBOUNDED
  toEnum 6  = CUTOFF
  toEnum 7  = ITERATION_LIMIT
  toEnum 8  = NODE_LIMIT
  toEnum 9  = TIME_LIMIT
  toEnum 10 = SOLUTION_LIMIT
  toEnum 11 = INTERRUPTED
  toEnum 12 = NUMERIC
  toEnum 13 = SUBOPTIMAL
  toEnum 14 = INPROGRESS
  toEnum 15 = USER_OBJ_LIMIT
  toEnum 16 = WORK_LIMIT
  toEnum 17 = MEM_LIMIT
  toEnum _  = error "Prelude.Enum.ModelStatusCode.toEnum: bad argument"

instance Hashable ModelStatusCode

getStatus :: Model -> IO ModelStatusCode
getStatus model = toEnum <$> getIntAttrPtr model C.iNT_ATTR_STATUS_PTR

getObjVal :: Model -> IO Double
getObjVal model = getDblAttrPtr model C.dBL_ATTR_OBJVAL_PTR

getVarName :: Var -> IO String
getVarName v = getStrAttrElementPtr (varModel v) C.sTR_ATTR_VARNAME_PTR (varIndex v)

getX :: Var -> IO Double
getX v = getDblAttrElementPtr (varModel v) C.dBL_ATTR_X_PTR (varIndex v)

emptyEnv :: IO C.Env
emptyEnv =
  alloca $ \envP -> do
    -- err <- C.emptyenv envP
    err <- C.emptyenvadv envP nullFunPtr nullFunPtr nullFunPtr nullFunPtr nullFunPtr nullFunPtr nullPtr
    env <- peek envP
    when (err /= 0) $ do
      msg <- peekCString =<< C.geterrormsg env
      C.freeenv env
      throwIO $ Error err msg
    return env

loadEnv :: FilePath -> IO C.Env
loadEnv logfilename =
  alloca $ \envP -> do
    withCString logfilename $ \logfilenameP -> do
      err <- C.loadenv envP logfilenameP
      env <- peek envP
      when (err /= 0) $ do
        msg <- peekCString =<< C.geterrormsg env
        C.freeenv env
        throwIO $ Error err msg
      return env

startEnv :: C.Env -> IO ()
startEnv env = checkError env $ C.startenv env

setStrParam :: C.Env -> String -> String -> IO ()
setStrParam env param value = do
  withCString param $ \paramP -> do
    withCString value $ \valueP -> do
      checkError env $ C.setstrparam env paramP valueP

data Model
  = Model
  { modelPtr :: C.Model
  , modelVarCounter :: IORef Int
  , modelConstrCounter :: IORef Int
  }

instance Eq Model where
  (==) = (==) `on` modelPtr

instance Ord Model where
  compare = comparing modelPtr

instance Hashable Model where
  hashWithSalt salt model = hashWithSalt salt (modelPtr model)

withModelPtr :: Model -> (C.Model -> IO a) -> IO a
withModelPtr model block = block (modelPtr model)

modelCheckError :: C.Model -> IO C.ErrorCode -> IO ()
modelCheckError modelP action = do
  env <- C.getenv modelP
  checkError env action

newModel :: C.Env -> String -> IO Model
newModel env name = do
  alloca $ \modelP -> do
    withCString name $ \nameP -> do
      err <- C.newmodel env modelP nameP 0 nullPtr nullPtr nullPtr nullPtr nullPtr
      model <- peek modelP
      when (err /= 0) $ do
        msg <- peekCString =<< C.geterrormsg env
        _ <- C.freemodel model
        throwIO $ Error err msg
      varCounter <- newIORef 0
      constrCounter <- newIORef 0
      return (Model model varCounter constrCounter)

data VariableType = CONTINUOUS | BINARY | INTEGER | SEMICONT | SEMIINT
  deriving (Show, Read, Eq, Ord, Bounded, Generic)

instance Hashable VariableType

variableTypeToCChar :: VariableType -> CChar
variableTypeToCChar CONTINUOUS = fromIntegral $ fromEnum 'C'
variableTypeToCChar BINARY     = fromIntegral $ fromEnum 'B'
variableTypeToCChar INTEGER    = fromIntegral $ fromEnum 'I'
variableTypeToCChar SEMICONT   = fromIntegral $ fromEnum 'S'
variableTypeToCChar SEMIINT    = fromIntegral $ fromEnum 'N'

data Var
  = Var
  { varModel :: !Model
  , varIndex :: !Int
  }
  deriving (Eq, Ord)

instance Hashable Var where
  hashWithSalt salt (Var model index) = hashWithSalt salt (model, index)

data Constr
  = Constr
  { constrModel :: !Model
  , constrIndex :: Int
  }
  deriving (Eq, Ord)

instance Hashable Constr where
  hashWithSalt salt (Constr model index) = hashWithSalt salt (model, index)

addVar :: Model -> String -> VariableType -> IO Var
addVar model@Model{ modelVarCounter = varCounter } varname vtype =
  withModelPtr model $ \modelP ->
  withCString varname $ \varnameP -> do
    let vindP = nullPtr
        vvalP = nullPtr
        obj = 0
        lb = - C.iNFINITY
        ub = C.iNFINITY
    modelCheckError modelP $ C.addvar modelP 0 vindP vvalP obj lb ub (variableTypeToCChar vtype) varnameP
    n <- readIORef varCounter
    writeIORef varCounter $! n + 1
    pure $ Var{ varModel = model, varIndex = n }

addBinaryVar :: Model -> String -> IO Var
addBinaryVar model varname = addVar model varname BINARY

data LinExpr = LinExpr !(Maybe Model) (IntMap Double) Double

constExpr :: Double -> LinExpr
constExpr x = LinExpr Nothing IntMap.empty x

exprFromTerms :: [(Double, Var)] -> LinExpr
exprFromTerms [] = constExpr 0
exprFromTerms tts@((_,v0) : ts)
  | or [varModel v0 /= varModel v | (_,v) <- ts] = error "model mismatch"
  | otherwise = LinExpr (if IntMap.null m1 then Nothing else Just (varModel v0)) m1 0
  where
    m1 = foldl' ins IntMap.empty tts
    ins m (c, v) = IntMap.alter (\case
                     Nothing -> Just c
                     Just c'
                       | c + c' == 0 -> Nothing
                       | otherwise -> Just (c + c')
                     ) (varIndex v) m

data ConstraintSense = LESS_EQUAL | GREATER_EQUAL | EQUAL
  deriving (Eq, Ord, Enum, Bounded, Generic, Show, Read)

instance Hashable ConstraintSense

constraintSenseToCChar :: ConstraintSense -> CChar
constraintSenseToCChar LESS_EQUAL    = fromIntegral $ fromEnum $ '<'
constraintSenseToCChar GREATER_EQUAL = fromIntegral $ fromEnum $ '>'
constraintSenseToCChar EQUAL         = fromIntegral $ fromEnum $ '='

addConstr :: Model -> LinExpr -> ConstraintSense -> Double -> String -> IO Constr
addConstr model (LinExpr (Just model') _terms _constant) _sense _rhs _constrname
  | model /= model' = error "model mismatch"
addConstr model@Model{ modelConstrCounter = constrCounter } (LinExpr _ terms constant) sense rhs constrname =
  withModelPtr model $ \modelP ->
  withCString constrname $ \constrnameP -> do
    let numnz = IntMap.size terms
    allocaArray numnz $ \cind -> allocaArray numnz $ \cval -> do
      forM_ (zip [0..] (IntMap.toList terms)) $ \(i, (v, c)) -> do
        pokeElemOff cind i (fromIntegral v)
        pokeElemOff cval i (realToFrac c :: CDouble)
      modelCheckError modelP $ C.addconstr modelP (fromIntegral numnz) cind cval (constraintSenseToCChar sense) (realToFrac (rhs - constant)) constrnameP
    n <- readIORef constrCounter
    writeIORef constrCounter $! n + 1
    pure $ Constr{ constrModel = model, constrIndex = n }

data ObjectiveSense = MINIMIZE | MAXIMIZE
  deriving (Eq, Ord, Enum, Bounded, Generic, Show, Read)

instance Hashable ObjectiveSense

objectiveSenseToInt :: ObjectiveSense -> Int
objectiveSenseToInt MINIMIZE = fromIntegral C.mINIMIZE
objectiveSenseToInt MAXIMIZE = fromIntegral C.mAXIMIZE

-- TODO: use GRBsetobjective
setObjective :: Model -> LinExpr -> ObjectiveSense -> IO ()
setObjective model (LinExpr (Just model') _terms _constant) _sense
  | model /= model' = error "model mismatch"
setObjective model@Model{ modelVarCounter = varCounter } (LinExpr _ terms constant) sense = do
  -- numVars <- getIntAttrPtr model iNT_ATTR_NUMVARS
  numVars <- readIORef varCounter
  setIntAttrPtr model C.iNT_ATTR_MODELSENSE_PTR (objectiveSenseToInt sense)
  setDblAttrPtr model C.dBL_ATTR_OBJCON_PTR constant
  allocaArray numVars $ \cval-> do
    forM_ [0..numVars-1] $ \i ->
      pokeElemOff cval i (0 :: CDouble)
    forM_ (IntMap.toList terms) $ \(v, c) -> do
      pokeElemOff cval v (realToFrac c :: CDouble)
    setDblAttrArrayPtr model C.dBL_ATTR_OBJ_PTR 0 numVars cval

optimize :: Model -> IO ()
optimize model =
  withModelPtr model $ \modelP -> do
    modelCheckError modelP $ C.optimize modelP

write :: Model -> FilePath -> IO ()
write model filename =
  withModelPtr model $ \modelP ->
  withCString filename $ \filenameP -> do
    modelCheckError modelP $ C.write modelP filenameP
