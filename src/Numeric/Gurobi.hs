module Numeric.Gurobi where

import Control.Exception
import Control.Monad
import Data.IORef
import Foreign
import Foreign.C
import qualified Numeric.Gurobi.C as C

data Error = Error C.ErrorCode String
   deriving (Show)

instance Exception Error

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
  deriving (Show, Eq, Ord, Enum, Bounded)

data AttrType
  = ModelAtrr
  | VarAttr
  | LinearConstrAttr
  | SOS1ConstrAttr
  | SOS2ConstrAttr
  | QConstrAttr
  | GenConstrAttr
  deriving (Show, Eq, Ord, Enum, Bounded)

getAttrInfo :: Model -> String -> IO (DataType, AttrType, Bool)
getAttrInfo Model{ modelPtr = model } name = do
  alloca $ \typeP -> do
    alloca $ \attrTypeP -> do
      alloca $ \settableP -> do
        withCString name $ \nameP -> do
          env <- C.getenv model
          checkError env $ C.getattrinfo model nameP typeP attrTypeP settableP
          dataType <- peek typeP
          attrType <- peek attrTypeP
          settable <- peek settableP
          return (toEnum (fromIntegral dataType), toEnum (fromIntegral attrType), settable /= 0)

getIntAttr :: Model -> String -> IO Int
getIntAttr model attrname = do
  withCString attrname $ \attrnameP -> getIntAttrPtr model attrnameP

getIntAttrPtr :: Model -> CString -> IO Int
getIntAttrPtr Model{ modelPtr = model } attrnameP = do
  env <- C.getenv model
  alloca $ \valueP -> do
    checkError env $ C.getintattr model attrnameP valueP
    fromIntegral <$> peek valueP

setIntAttr :: Model -> String -> Int -> IO ()
setIntAttr model attrname newvalue = do
  withCString attrname $ \attrnameP -> setIntAttrPtr model attrnameP newvalue

setIntAttrPtr :: Model -> CString -> Int -> IO ()
setIntAttrPtr Model{ modelPtr = model } attrnameP newvalue  = do
  env <- C.getenv model
  checkError env $ C.setintattr model attrnameP (fromIntegral newvalue)


getDblAttr :: Model -> String -> IO Double
getDblAttr model attrname = do
  withCString attrname $ \attrnameP -> getDblAttrPtr model attrnameP

getDblAttrPtr :: Model -> CString -> IO Double
getDblAttrPtr Model{ modelPtr = model } attrnameP = do
  env <- C.getenv model
  alloca $ \valueP -> do
    checkError env $ C.getdblattr model attrnameP valueP
    realToFrac <$> peek valueP

setDblAttr :: Model -> String -> Double -> IO ()
setDblAttr model attrname newvalue = do
  withCString attrname $ \attrnameP -> setDblAttrPtr model attrnameP newvalue

setDblAttrPtr :: Model -> CString -> Double -> IO ()
setDblAttrPtr Model{ modelPtr = model } attrnameP newvalue  = do
  env <- C.getenv model
  checkError env $ C.setdblattr model attrnameP (realToFrac newvalue)

getDblAttrElement :: Model -> String -> Int -> IO Double
getDblAttrElement model attrname element = do
  withCString attrname $ \attrnameP -> getDblAttrElementPtr model attrnameP element

getDblAttrElementPtr :: Model -> CString -> Int -> IO Double
getDblAttrElementPtr Model{ modelPtr = model } attrnameP element = do
  env <- C.getenv model
  alloca $ \valueP -> do
    checkError env $ C.getdblattrelement model attrnameP (fromIntegral element) valueP
    realToFrac <$> peek valueP

setDblAttrArray :: Model -> String -> Int -> Int -> Ptr CDouble -> IO ()
setDblAttrArray model attrname start len values = do
  withCString attrname $ \attrnameP -> setDblAttrArrayPtr model attrnameP start len values

setDblAttrArrayPtr :: Model -> CString -> Int -> Int -> Ptr CDouble -> IO ()
setDblAttrArrayPtr Model{ modelPtr = model } attrnameP start len values = do
  env <- C.getenv model
  checkError env $ C.setdblattrarray model attrnameP (fromIntegral start) (fromIntegral len) values

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
  deriving (Show, Read, Eq, Ord, Bounded)

variableTypeToCChar :: VariableType -> CChar
variableTypeToCChar CONTINUOUS = fromIntegral $ fromEnum 'C'
variableTypeToCChar BINARY     = fromIntegral $ fromEnum 'B'
variableTypeToCChar INTEGER    = fromIntegral $ fromEnum 'I'
variableTypeToCChar SEMICONT   = fromIntegral $ fromEnum 'S'
variableTypeToCChar SEMIINT    = fromIntegral $ fromEnum 'N'

newtype Var = Var CInt
  deriving Eq

newtype Constr = Constr CInt
  deriving Eq

addVar :: Model -> String -> VariableType -> IO Var
addVar Model{ modelPtr = model, modelVarCounter = varCounter } varname vtype = do
  env <- C.getenv model
  withCString varname $ \varnameP -> do
    let vindP = nullPtr
        vvalP = nullPtr
        obj = 0
        lb = - C.iNFINITY
        ub = C.iNFINITY
    checkError env $ C.addvar model 0 vindP vvalP obj lb ub (variableTypeToCChar vtype) varnameP
  n <- readIORef varCounter
  writeIORef varCounter $! n + 1
  pure $ Var (fromIntegral n)

addBinaryVar :: Model -> String -> IO Var
addBinaryVar model varname = addVar model varname BINARY

type LinExpr = ([(Double, Var)], Double)

data ConstraintSense = LESS_EQUAL | GREATER_EQUAL | EQUAL
  deriving (Show, Read, Eq, Ord, Bounded)

constraintSenseToCChar :: ConstraintSense -> CChar
constraintSenseToCChar LESS_EQUAL    = fromIntegral $ fromEnum $ '<'
constraintSenseToCChar GREATER_EQUAL = fromIntegral $ fromEnum $ '>'
constraintSenseToCChar EQUAL         = fromIntegral $ fromEnum $ '='

addConstr :: Model -> LinExpr -> ConstraintSense -> Double -> String -> IO Constr
addConstr Model{ modelPtr = model, modelConstrCounter = constrCounter } (terms, constant) sense rhs constrname = do
  let numnz = length terms
  env <- C.getenv model
  withCString constrname $ \constrnameP -> do
    allocaArray numnz $ \cind -> do
      allocaArray numnz $ \cval -> do
        forM_ (zip [0..] terms) $ \(i, (c, Var v)) -> do
          pokeElemOff cind i v
          pokeElemOff cval i (realToFrac c :: CDouble)
        checkError env $ C.addconstr model (fromIntegral numnz) cind cval (constraintSenseToCChar sense) (realToFrac (rhs - constant)) constrnameP
  n <- readIORef constrCounter
  writeIORef constrCounter $! n + 1
  pure $ Constr (fromIntegral n)

data ObjectiveSense = MINIMIZE | MAXIMIZE

objectiveSenseToInt :: ObjectiveSense -> Int
objectiveSenseToInt MINIMIZE = fromIntegral C.mINIMIZE
objectiveSenseToInt MAXIMIZE = fromIntegral C.mAXIMIZE

-- TODO: use GRBsetobjective
setObjective :: Model -> LinExpr -> ObjectiveSense -> IO ()
setObjective model@Model{ modelVarCounter = varCounter } (terms, constant) sense = do
  -- numVars <- getIntAttrPtr model iNT_ATTR_NUMVARS
  numVars <- readIORef varCounter
  setIntAttrPtr model C.iNT_ATTR_MODELSENSE_PTR (objectiveSenseToInt sense)
  setDblAttrPtr model C.dBL_ATTR_OBJCON_PTR constant
  allocaArray numVars $ \cval-> do
    forM_ [0..numVars-1] $ \i ->
      pokeElemOff cval i (0 :: CDouble)
    forM_ (terms) $ \(c, Var v) -> do
      pokeElemOff cval (fromIntegral v) (realToFrac c :: CDouble)
    setDblAttrArrayPtr model C.dBL_ATTR_OBJ_PTR 0 numVars cval

optimize :: Model -> IO ()
optimize Model{ modelPtr = cmodel } = do
  env <- C.getenv cmodel
  checkError env $ C.optimize cmodel

write :: Model -> FilePath -> IO ()
write Model{ modelPtr = cmodel } filename = do
  env <- C.getenv cmodel
  withCString filename $ \filenameP -> do
    checkError env $ C.write cmodel filenameP
