module Numeric.Gurobi where

import Control.Exception
import Control.Monad
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

getAttrInfo :: C.Model -> String -> IO (DataType, AttrType, Bool)
getAttrInfo model name = do
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

newModel :: C.Env -> String -> IO C.Model
newModel env name = do
  alloca $ \modelP -> do
    withCString name $ \nameP -> do
      err <- C.newmodel env modelP nameP 0 nullPtr nullPtr nullPtr nullPtr nullPtr
      model <- peek modelP
      when (err /= 0) $ do
        msg <- peekCString =<< C.geterrormsg env
        _ <- C.freemodel model
        throwIO $ Error err msg
      return model
