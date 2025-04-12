{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
module Numeric.Gurobi.C where

#include <gurobi_c.h>

import Foreign.C
import Foreign
import GHC.Ptr
import GHC.Exts (Addr##)

-- /* Copyright 2024, Gurobi Optimization, LLC */

data Batch_
data Model_
data Env_

type Batch = Ptr Batch_
type Model = Ptr Model_
type Env = Ptr Env_

##if !defined(mingw32_HOST_OS)
##define stdcall ccall
##endif
 
-- /* Version numbers */

version_major, version_minor, version_technical :: Int
version_major     = #const GRB_VERSION_MAJOR
version_minor     = #const GRB_VERSION_MINOR
version_technical = #const GRB_VERSION_TECHNICAL

-- /* Default and max priority for Compute Server jobs */

default_cs_priority, max_cs_priority :: CInt
default_cs_priority = #const DEFAULT_CS_PRIORITY
max_cs_priority = #const MAX_CS_PRIORITY

-- /* Default port number for Compute Server */

default_cs_port :: CInt
default_cs_port = #const DEFAULT_CS_PORT

-- /* Default Compute Server hangup duration */

default_cs_hangup :: CInt
default_cs_hangup = #const DEFAULT_CS_HANGUP

-- /* Error codes: adjust MIN/MAX if adding new codes */

type ErrorCode = CInt

c_MIN_ERROR                    = #const GRB_C_MIN_ERROR                   
eRROR_OUT_OF_MEMORY            = #const GRB_ERROR_OUT_OF_MEMORY           
eRROR_NULL_ARGUMENT            = #const GRB_ERROR_NULL_ARGUMENT           
eRROR_INVALID_ARGUMENT         = #const GRB_ERROR_INVALID_ARGUMENT        
eRROR_UNKNOWN_ATTRIBUTE        = #const GRB_ERROR_UNKNOWN_ATTRIBUTE       
eRROR_DATA_NOT_AVAILABLE       = #const GRB_ERROR_DATA_NOT_AVAILABLE      
eRROR_INDEX_OUT_OF_RANGE       = #const GRB_ERROR_INDEX_OUT_OF_RANGE      
eRROR_UNKNOWN_PARAMETER        = #const GRB_ERROR_UNKNOWN_PARAMETER       
eRROR_VALUE_OUT_OF_RANGE       = #const GRB_ERROR_VALUE_OUT_OF_RANGE      
eRROR_NO_LICENSE               = #const GRB_ERROR_NO_LICENSE              
eRROR_SIZE_LIMIT_EXCEEDED      = #const GRB_ERROR_SIZE_LIMIT_EXCEEDED     
eRROR_CALLBACK                 = #const GRB_ERROR_CALLBACK                
eRROR_FILE_READ                = #const GRB_ERROR_FILE_READ               
eRROR_FILE_WRITE               = #const GRB_ERROR_FILE_WRITE              
eRROR_NUMERIC                  = #const GRB_ERROR_NUMERIC                 
eRROR_IIS_NOT_INFEASIBLE       = #const GRB_ERROR_IIS_NOT_INFEASIBLE      
eRROR_NOT_FOR_MIP              = #const GRB_ERROR_NOT_FOR_MIP             
eRROR_OPTIMIZATION_IN_PROGRESS = #const GRB_ERROR_OPTIMIZATION_IN_PROGRESS
eRROR_DUPLICATES               = #const GRB_ERROR_DUPLICATES              
eRROR_NODEFILE                 = #const GRB_ERROR_NODEFILE                
eRROR_Q_NOT_PSD                = #const GRB_ERROR_Q_NOT_PSD               
eRROR_QCP_EQUALITY_CONSTRAINT  = #const GRB_ERROR_QCP_EQUALITY_CONSTRAINT 
eRROR_NETWORK                  = #const GRB_ERROR_NETWORK                 
eRROR_JOB_REJECTED             = #const GRB_ERROR_JOB_REJECTED            
eRROR_NOT_SUPPORTED            = #const GRB_ERROR_NOT_SUPPORTED           
eRROR_EXCEED_2B_NONZEROS       = #const GRB_ERROR_EXCEED_2B_NONZEROS      
eRROR_INVALID_PIECEWISE_OBJ    = #const GRB_ERROR_INVALID_PIECEWISE_OBJ   
eRROR_UPDATEMODE_CHANGE        = #const GRB_ERROR_UPDATEMODE_CHANGE       
eRROR_CLOUD                    = #const GRB_ERROR_CLOUD                   
eRROR_MODEL_MODIFICATION       = #const GRB_ERROR_MODEL_MODIFICATION      
eRROR_CSWORKER                 = #const GRB_ERROR_CSWORKER                
eRROR_TUNE_MODEL_TYPES         = #const GRB_ERROR_TUNE_MODEL_TYPES        
eRROR_SECURITY                 = #const GRB_ERROR_SECURITY                
c_MAX_ERROR                    = #const GRB_C_MAX_ERROR                   

-- /* Constraint senses */

-- #define GRB_LESS_EQUAL    '<'
lESS_EQUAL :: CChar
lESS_EQUAL = #const GRB_LESS_EQUAL

-- #define GRB_GREATER_EQUAL '>'
gREATER_EUQAL :: CChar
gREATER_EUQAL = #const GRB_GREATER_EQUAL

-- #define GRB_EQUAL         '='
eQUAL :: CChar
eQUAL = #const GRB_EQUAL

-- /* Variable types */
-- 
-- #define GRB_CONTINUOUS 'C'
-- #define GRB_BINARY     'B'
-- #define GRB_INTEGER    'I'
-- #define GRB_SEMICONT   'S'
-- #define GRB_SEMIINT    'N'

-- /* Objective sense */
-- 
-- #define GRB_MINIMIZE 1
-- #define GRB_MAXIMIZE -1

-- /* SOS types */
-- 
-- #define GRB_SOS_TYPE1 1
-- #define GRB_SOS_TYPE2 2

-- /* Numeric constants */
-- 
-- #define GRB_INFINITY  1e100
-- #define GRB_UNDEFINED 1e101
-- #define GRB_MAXINT    2000000000

-- /* Limits */
-- 
-- #define GRB_MAX_NAMELEN    255
-- #define GRB_MAX_STRLEN     512
-- #define GRB_MAX_TAGLEN    10240
-- #define GRB_MAX_CONCURRENT 64

-- /* Callback */

-- #define CB_ARGS GRBmodel *model, void *cbdata, int where, void *usrdata
data CBData_
type CBData = Ptr CBData_
type CB a = Model -> CBData -> CInt -> Ptr a -> IO CInt

-- #define LOGCB_ARGS char *msg, void *logdata
type LogCB a = CString -> Ptr a -> IO CInt

-- /* Query interface */

-- int __stdcall
--   GRBgetattrinfo(GRBmodel *model, const char *attrname, int *datatypeP,
--                  int *attrtypeP, int *settableP);
foreign import stdcall unsafe "GRBgetattrinfo" getattrinfo
  :: Model -- ^ model
  -> CString -- ^ attrname
  -> Ptr CInt -- ^ datatypeP
  -> Ptr CInt -- ^ attrtypeP
  -> Ptr CInt -- ^ settableP
  -> IO ErrorCode

-- int __stdcall
--   GRBisattravailable(GRBmodel *model, const char *attrname);
foreign import stdcall unsafe "GRBisattravailable" isattravailable
  :: Model -- ^ model
  -> CString -- ^ attrname
  -> IO ErrorCode

-- int __stdcall
--   GRBgetintattr(GRBmodel *model, const char *attrname, int *valueP);
foreign import stdcall unsafe "GRBgetintattr" getintattr
  :: Model -- ^ model
  -> CString -- ^ attrname
  -> Ptr CInt -- ^ valueP
  -> IO ErrorCode

-- int __stdcall
--   GRBsetintattr(GRBmodel *model, const char *attrname, int newvalue);
foreign import stdcall unsafe "GRBsetintattr" setintattr
  :: Model -- ^ model
  -> CString -- ^ attrname
  -> CInt -- ^ newvalue
  -> IO ErrorCode

-- int __stdcall
--   GRBgetintattrelement(GRBmodel *model, const char *attrname,
--                        int element, int *valueP);
foreign import stdcall unsafe "GRBgetintattrelement" getintattrelement
  :: Model -- ^ model
  -> CString -- ^ attrname
  -> CInt -- ^ element
  -> Ptr CInt -- ^ valueP
  -> IO ErrorCode

-- int __stdcall
--   GRBsetintattrelement(GRBmodel *model, const char *attrname,
--                        int element, int newvalue);
foreign import stdcall unsafe "GRBsetintattrelement" setintattrelement
  :: Model -- ^ model
  -> CString -- ^ attrname
  -> CInt -- ^ element
  -> CInt -- ^ newvalue
  -> IO ErrorCode

-- int __stdcall
--   GRBgetintattrarray(GRBmodel *model, const char *attrname,
--                      int first, int len, int *values);
foreign import stdcall unsafe "GRBgetintattrarray" getintattrarray
  :: Model -- ^ model
  -> CString -- ^ attrname
  -> CInt -- ^ first
  -> CInt -- ^ len
  -> Ptr CInt -- ^ values
  -> IO ErrorCode -- ^ return value

-- int __stdcall
--   GRBsetintattrarray(GRBmodel *model, const char *attrname,
--                      int first, int len, int *newvalues);
foreign import stdcall unsafe "GRBsetintattrarray" setintattrarray
  :: Model -- ^ model
  -> CString -- ^ attrname
  -> CInt -- ^ first
  -> CInt -- ^ len
  -> Ptr CInt -- ^ newvalues
  -> IO ErrorCode

-- int __stdcall
--   GRBgetintattrlist(GRBmodel *model, const char *attrname,
--                     int len, int *ind, int *values);
foreign import stdcall unsafe "GRBgetintattrlist" getintattrlist
  :: Model -- ^ model
  -> CString -- ^ attrname
  -> CInt -- ^ len
  -> Ptr CInt -- ^ ind
  -> Ptr CInt -- ^ values
  -> IO ErrorCode 

-- int __stdcall
--   GRBsetintattrlist(GRBmodel *model, const char *attrname,
--                     int len, int *ind, int *newvalues);
foreign import stdcall unsafe "GRBsetintattrlist" setintattrlist
  :: Model -- ^ model
  -> CString -- ^ attrname
  -> CInt -- ^ len
  -> Ptr CInt -- ^ ind
  -> Ptr CInt -- ^ newvalues
  -> IO ErrorCode

-- int __stdcall
--   GRBgetcharattrelement(GRBmodel *model, const char *attrname,
--                         int element, char *valueP);
foreign import stdcall unsafe "GRBgetcharattrelement" getcharattrelement
  :: Model -- ^ model
  -> CString -- ^ attrname
  -> CInt -- ^ element
  -> Ptr CChar -- ^ valueP
  -> IO ErrorCode

-- int __stdcall
--   GRBsetcharattrelement(GRBmodel *model, const char *attrname,
--                         int element, char newvalue);
foreign import stdcall unsafe "GRBsetcharattrelement" setcharattrelement
  :: Model
  -> CString
  -> CInt
  -> CChar
  -> IO ErrorCode

-- int __stdcall
--   GRBgetcharattrarray(GRBmodel *model, const char *attrname,
--                       int first, int len, char *values);
foreign import stdcall unsafe "GRBgetcharattrarray" getcharattrarray
  :: Model
  -> CString
  -> CInt
  -> CInt
  -> Ptr CChar
  -> IO ErrorCode

-- int __stdcall
--   GRBsetcharattrarray(GRBmodel *model, const char *attrname,
--                       int first, int len, char *newvalues);
foreign import stdcall unsafe "GRBsetcharattrarray" setcharattrarray
  :: Model
  -> CString
  -> CInt
  -> CInt
  -> Ptr CChar
  -> IO ErrorCode

-- int __stdcall
--   GRBgetcharattrlist(GRBmodel *model, const char *attrname,
--                      int len, int *ind, char *values);
foreign import stdcall unsafe "GRBgetcharattrlist" getcharattrlist
  :: Model
  -> CString
  -> CInt
  -> Ptr CInt
  -> Ptr CChar
  -> IO ErrorCode

-- int __stdcall
--   GRBsetcharattrlist(GRBmodel *model, const char *attrname,
--                      int len, int *ind, char *newvalues);
foreign import stdcall unsafe "GRBsetcharattrlist" setcharattrlist
  :: Model
  -> CString
  -> CInt
  -> Ptr CInt
  -> Ptr CChar
  -> IO ErrorCode

-- int __stdcall
--   GRBgetdblattr(GRBmodel *model, const char *attrname, double *valueP);
foreign import stdcall unsafe "GRBgetdblattr" getdblattr
  :: Model -- ^ model
  -> CString -- ^ attrname
  -> Ptr CDouble -- ^ valueP
  -> IO ErrorCode

-- int __stdcall
--   GRBsetdblattr(GRBmodel *model, const char *attrname, double newvalue);
foreign import stdcall unsafe "GRBsetdblattr" setdblattr
  :: Model -- ^ model
  -> CString -- ^ attrname
  -> CDouble -- ^ newvalue
  -> IO ErrorCode

-- int __stdcall
--   GRBgetdblattrelement(GRBmodel *model, const char *attrname,
--                        int element, double *valueP);
foreign import stdcall unsafe "GRBgetdblattrelement" getdblattrelement
  :: Model -- ^ model
  -> CString -- ^ attrname
  -> CInt -- ^ element
  -> Ptr CDouble -- ^ valueP
  -> IO ErrorCode

-- int __stdcall
--   GRBsetdblattrelement(GRBmodel *model, const char *attrname,
--                        int element, double newvalue);
foreign import stdcall unsafe "GRBsetdblattrelement" setdblattrelement
  :: Model -- ^ model
  -> CString -- ^ attrname
  -> CInt -- ^ element
  -> CDouble -- ^ newvalue
  -> IO ErrorCode

-- int __stdcall
--   GRBgetdblattrarray(GRBmodel *model, const char *attrname,
--                      int first, int len, double *values);
foreign import stdcall unsafe "GRBgetdblattrarray" getdblattrarray
  :: Model -- ^ model
  -> CString -- ^ attrname
  -> CInt -- ^ first
  -> CInt -- ^ len
  -> Ptr CDouble -- ^ values
  -> IO ErrorCode

-- int __stdcall
--   GRBsetdblattrarray(GRBmodel *model, const char *attrname,
--                      int first, int len, double *newvalues);
foreign import stdcall unsafe "GRBsetdblattrarray" setdblattrarray
  :: Model -- ^ model
  -> CString -- ^ attrname
  -> CInt -- ^ first
  -> CInt -- ^ len
  -> Ptr CDouble -- ^ newvalues
  -> IO ErrorCode

-- int __stdcall
--   GRBgetdblattrlist(GRBmodel *model, const char *attrname,
--                     int len, int *ind, double *values);
foreign import stdcall unsafe "GRBgetdblattrlist" getdblattrlist
  :: Model -- ^ model
  -> CString -- ^ attrname
  -> CInt -- ^ len
  -> Ptr CInt -- ^ ind
  -> Ptr CDouble -- ^ values
  -> IO ErrorCode

-- int __stdcall
--   GRBsetdblattrlist(GRBmodel *model, const char *attrname,
--                     int len, int *ind, double *newvalues);
foreign import stdcall unsafe "GRBsetdblattrlist" setdblattrlist
  :: Model -- ^ model
  -> CString -- ^ attrname
  -> CInt -- ^ len
  -> Ptr CInt -- ^ ind
  -> Ptr CDouble -- ^ newvalues
  -> IO ErrorCode

-- int __stdcall
--   GRBgetstrattr(GRBmodel *model, const char *attrname, char **valueP);
foreign import stdcall unsafe "GRBgetstrattr" getstrattr
  :: Model -- ^ model
  -> CString -- ^ attrname
  -> Ptr CString -- ^ valueP
  -> IO ErrorCode

-- int __stdcall
--   GRBsetstrattr(GRBmodel *model, const char *attrname, const char *newvalue);
foreign import stdcall unsafe "GRBsetstrattr" setstrattr
  :: Model -- ^ model
  -> CString -- ^ attrname
  -> CString -- ^ newvalue
  -> IO ErrorCode

-- int __stdcall
--   GRBgetstrattrelement(GRBmodel *model, const char *attrname,
--                        int element, char **valueP);
foreign import stdcall unsafe "GRBgetstrattrelement" getstrattrelement
  :: Model -- ^ model
  -> CString -- ^ attrname
  -> CInt -- ^ element
  -> Ptr CString -- ^ valueP
  -> IO ErrorCode

-- int __stdcall
--   GRBsetstrattrelement(GRBmodel *model, const char *attrname,
--                        int element, const char *newvalue);
foreign import stdcall unsafe "GRBsetstrattrelement" setstrattrelement
  :: Model -- ^ model
  -> CString -- ^ attrname
  -> CInt -- ^ element
  -> CString -- ^ newvalue
  -> IO ErrorCode

-- int __stdcall
--   GRBgetstrattrarray(GRBmodel *model, const char *attrname,
--                      int first, int len, char **values);
foreign import stdcall unsafe "GRBgetstrattrarray" getstrattrarray
  :: Model -- ^ model
  -> CString -- ^ attrname
  -> CInt -- ^ first
  -> CInt -- ^ len
  -> Ptr CString -- ^ values
  -> IO ErrorCode

-- int __stdcall
--   GRBsetstrattrarray(GRBmodel *model, const char *attrname,
--                      int first, int len, char **newvalues);
foreign import stdcall unsafe "GRBsetstrattrarray" setstrattrarray
  :: Model -- ^ model
  -> CString -- ^ attrname
  -> CInt -- ^ first
  -> CInt -- ^ len
  -> Ptr CString -- ^ newvalues
  -> IO ErrorCode

-- int __stdcall
--   GRBgetstrattrlist(GRBmodel *model, const char *attrname,
--                     int len, int *ind, char **values);
foreign import stdcall unsafe "GRBgetstrattrlist" getstrattrlist
  :: Model -- ^ model
  -> CString -- ^ attrname
  -> CInt -- ^ len
  -> Ptr CInt -- ^ ind
  -> Ptr CString -- ^ values
  -> IO ErrorCode

-- int __stdcall
--   GRBsetstrattrlist(GRBmodel *model, const char *attrname,
--                     int len, int *ind, char **newvalues);
foreign import stdcall unsafe "GRBsetstrattrlist" setstrattrlist
  :: Model -- ^ model
  -> CString -- ^ attrname
  -> CInt -- ^ len
  -> Ptr CInt -- ^ ind
  -> Ptr CString -- ^ newvalues
  -> IO ErrorCode

-- int __stdcall
--   GRBsetcallbackfunc(GRBmodel *model,
--                      int (__stdcall *cb)(CB_ARGS),
--                      void  *usrdata);
foreign import stdcall unsafe "GRBsetcallbackfunc" setcallbackfunc
  :: Model -- ^ model
  -> FunPtr (CB a) -- ^ cb
  -> Ptr a -- ^ usrdata
  -> IO ErrorCode

-- int __stdcall
--   GRBgetcallbackfuncenv(GRBenv *env,
--                         int (__stdcall **cbP)(CB_ARGS));
foreign import stdcall unsafe "GRBgetcallbackfuncenv" getcallbackfuncenv
  :: Env -- ^ env
  -> Ptr (FunPtr (CB a)) -- ^ cbP
  -> IO ErrorCode

-- int __stdcall
--   GRBsetcallbackfuncenv(GRBenv *env,
--                         int (__stdcall *cb)(CB_ARGS),
--                         void  *usrdata);
foreign import stdcall unsafe "GRBsetcallbackfuncenv" setcallbackfuncenv
  :: Env -- ^ env
  -> FunPtr (CB a) -- ^ cb
  -> Ptr a -- ^ usrdata
  -> IO ErrorCode

-- int __stdcall
--   GRBgetcallbackfunc(GRBmodel *model,
--                      int (__stdcall **cbP)(CB_ARGS));
foreign import stdcall unsafe "GRBgetcallbackfunc" getcallbackfunc
  :: Model -- ^ model
  -> Ptr (FunPtr (CB a)) -- ^ cbP
  -> IO ErrorCode

-- int __stdcall
--   GRBsetlogcallbackfunc(GRBmodel       *model,
--                         int (__stdcall *cb)(LOGCB_ARGS),
--                         void           *logdata);
foreign import stdcall unsafe "GRBsetlogcallbackfunc" setlogcallbackfunc
  :: Model -- ^ model
  -> FunPtr (LogCB a) -- ^ cb
  -> Ptr a -- ^ logdata
  -> IO ErrorCode

-- int __stdcall
--   GRBsetlogcallbackfuncenv(GRBenv         *env,
--                            int (__stdcall *cb)(LOGCB_ARGS),
--                            void           *logdata);
foreign import stdcall unsafe "GRBsetlogcallbackfuncenv" setlogcallbackfuncenv
  :: Env -- ^ env
  -> FunPtr (LogCB a) -- ^ cb
  -> Ptr a -- ^ logdata
  -> IO ErrorCode

-- int __stdcall
--   GRBgetlogcallbackfuncenv(GRBenv         *env,
--                            int (__stdcall **cbP)(LOGCB_ARGS),
--                            void           **logdataP);
foreign import stdcall unsafe "GRBgetlogcallbackfuncenv" getlogcallbackfuncenv
  :: Env -- ^ env
  -> Ptr (FunPtr (LogCB a)) -- ^ cbP
  -> Ptr (Ptr a) -- ^ logdataP
  -> IO ErrorCode

-- int __stdcall
--   GRBcbproceed(void *cbdata);
foreign import stdcall unsafe "GRBcbproceed" cbproceed
  :: CBData -- ^ cbdata
  -> IO ErrorCode

-- int __stdcall
--   GRBcbget(void *cbdata, int where, int what, void *resultP);
foreign import stdcall unsafe "GRBcbget" cbget
  :: CBData -- ^ cbdata
  -> CInt -- ^ where
  -> CInt -- ^ what
  -> Ptr a -- ^ resultP
  -> IO ErrorCode

-- int __stdcall
--   GRBcbsetintparam(void *cbdata, const char *paramname, int newvalue);
foreign import stdcall unsafe "GRBcbsetintparam" cbsetintparam
  :: CBData -- ^ cbdata
  -> CString -- ^ paramname
  -> CInt -- ^ newvalue
  -> IO ErrorCode

-- int __stdcall
--   GRBcbsetdblparam(void *cbdata, const char *paramname, double newvalue);
foreign import stdcall unsafe "GRBcbsetdblparam" cbsetdblparam
  :: CBData -- ^ cbdata
  -> CString -- ^ paramname
  -> CDouble -- ^ newvalue
  -> IO ErrorCode

-- int __stdcall
--   GRBcbsetstrparam(void *cbdata, const char *paramname, const char *newvalue);
foreign import stdcall unsafe "GRBcbsetstrparam" cbsetstrparam
  :: CBData -- ^ cbdata
  -> CString -- ^ paramname
  -> CString -- ^ newvalue
  -> IO ErrorCode

-- int __stdcall
--   GRBcbsetparam(void *cbdata, const char *paramname, const char *newvalue);
foreign import stdcall unsafe "GRBcbsetparam" cbsetparam
  :: CBData -- ^ cbdata
  -> CString -- ^ paramname
  -> CString -- ^ newvalue
  -> IO ErrorCode

-- int __stdcall
--   GRBcbsolution(void *cbdata, const double *solution, double *objvalP);
foreign import stdcall unsafe "GRBcbsolution" cbsolution
  :: CBData -- ^ cbdata
  -> Ptr CDouble -- ^ solution
  -> Ptr CDouble -- ^ objvalP
  -> IO ErrorCode

-- int __stdcall
--   GRBcbcut(void *cbdata, int cutlen, const int *cutind, const double *cutval,
--            char cutsense, double cutrhs);
foreign import stdcall unsafe "GRBcbcut" cbcut
  :: CBData -- ^ cbdata
  -> CInt -- ^ cutlen
  -> Ptr CInt -- ^ cutind
  -> Ptr CDouble -- ^ cutval
  -> CChar -- ^ cutsense
  -> CDouble -- ^ cutrhs
  -> IO ErrorCode

-- int __stdcall
--   GRBcblazy(void *cbdata, int lazylen, const int *lazyind,
--             const double *lazyval, char lazysense, double lazyrhs);
foreign import stdcall unsafe "GRBcblazy" cblazy
  :: CBData -- ^ cbdata
  -> CInt -- ^ lazylen
  -> Ptr CInt -- ^ lazyind
  -> Ptr CDouble -- ^ lazyval
  -> CChar -- ^ lazysense
  -> CDouble -- ^ lazyrhs
  -> IO ErrorCode

-- /*
--    ATTRIBUTES
-- */

-- /* Model attributes */
-- 
-- #define GRB_INT_ATTR_NUMCONSTRS    "NumConstrs"    /* # of constraints */
-- #define GRB_INT_ATTR_NUMVARS       "NumVars"       /* # of vars */
-- #define GRB_INT_ATTR_NUMSOS        "NumSOS"        /* # of sos constraints */
-- #define GRB_INT_ATTR_NUMQCONSTRS   "NumQConstrs"   /* # of quadratic constraints */
-- #define GRB_INT_ATTR_NUMGENCONSTRS "NumGenConstrs" /* # of general constraints */
-- #define GRB_INT_ATTR_NUMNZS        "NumNZs"        /* # of nz in A */
-- #define GRB_DBL_ATTR_DNUMNZS       "DNumNZs"       /* # of nz in A */
-- #define GRB_INT_ATTR_NUMQNZS       "NumQNZs"       /* # of nz in Q */
-- #define GRB_INT_ATTR_NUMQCNZS      "NumQCNZs"      /* # of nz in q constraints */
-- #define GRB_INT_ATTR_NUMINTVARS    "NumIntVars"    /* # of integer vars */
-- #define GRB_INT_ATTR_NUMBINVARS    "NumBinVars"    /* # of binary vars */
-- #define GRB_INT_ATTR_NUMPWLOBJVARS "NumPWLObjVars" /* # of variables with PWL obj. */
-- #define GRB_STR_ATTR_MODELNAME     "ModelName"     /* model name */
-- #define GRB_INT_ATTR_MODELSENSE    "ModelSense"    /* 1=min, -1=max */
-- #define GRB_DBL_ATTR_OBJCON        "ObjCon"        /* Objective constant */
-- #define GRB_INT_ATTR_IS_MIP        "IsMIP"         /* Is model a MIP? */
-- #define GRB_INT_ATTR_IS_QP         "IsQP"          /* Is model a QP/MIQP (without Q/NL constraints)? */
-- #define GRB_INT_ATTR_IS_QCP        "IsQCP"         /* Model has quadratic constr? */
-- #define GRB_INT_ATTR_IS_MULTIOBJ   "IsMultiObj"    /* Model has multiple objectives? */
-- #define GRB_INT_ATTR_LICENSE_EXPIRATION "LicenseExpiration" /* License expiration date */
-- #define GRB_INT_ATTR_NUMTAGGED     "NumTagged"     /* number of tagged elements in model */
-- #define GRB_INT_ATTR_FINGERPRINT   "Fingerprint"   /* fingerprint computed from the model data and attributes influencing the optimization process */
-- 
-- /* Batch attributes */
-- #define GRB_INT_ATTR_BATCHERRORCODE    "BatchErrorCode"
-- #define GRB_STR_ATTR_BATCHERRORMESSAGE "BatchErrorMessage"
-- #define GRB_STR_ATTR_BATCHID           "BatchID"
-- #define GRB_INT_ATTR_BATCHSTATUS       "BatchStatus"
-- 
-- /* Variable attributes */
-- 
-- #define GRB_DBL_ATTR_LB             "LB"              /* Lower bound */
-- #define GRB_DBL_ATTR_UB             "UB"              /* Upper bound */
-- #define GRB_DBL_ATTR_OBJ            "Obj"             /* Objective coeff */
-- #define GRB_CHAR_ATTR_VTYPE         "VType"           /* Integrality type */
-- #define GRB_DBL_ATTR_START          "Start"           /* MIP start value, depends on startnumber */
-- #define GRB_DBL_ATTR_PSTART         "PStart"          /* LP primal solution warm start */
-- #define GRB_INT_ATTR_BRANCHPRIORITY "BranchPriority"  /* MIP branch priority */
-- #define GRB_STR_ATTR_VARNAME        "VarName"         /* Variable name */
-- #define GRB_INT_ATTR_PWLOBJCVX      "PWLObjCvx"       /* Convexity of variable PWL obj */
-- #define GRB_DBL_ATTR_VARHINTVAL     "VarHintVal"      /* variable hint value */
-- #define GRB_INT_ATTR_VARHINTPRI     "VarHintPri"      /* variable hint priority */
-- #define GRB_INT_ATTR_PARTITION      "Partition"       /* user specified variable partition */
-- #define GRB_INT_ATTR_POOLIGNORE     "PoolIgnore"      /* Ignore variable for solution identity check in solution pool */
-- #define GRB_STR_ATTR_VTAG           "VTag"            /* variable tags */
-- 
-- /* Constraint attributes */
-- 
-- #define GRB_STR_ATTR_CTAG       "CTag"       /* linear constraint tags */
-- #define GRB_DBL_ATTR_RHS        "RHS"        /* RHS */
-- #define GRB_DBL_ATTR_DSTART     "DStart"     /* LP dual solution warm start */
-- #define GRB_CHAR_ATTR_SENSE     "Sense"      /* Sense ('<', '>', or '=') */
-- #define GRB_STR_ATTR_CONSTRNAME "ConstrName" /* Constraint name */
-- #define GRB_INT_ATTR_LAZY       "Lazy"       /* Lazy constraint? */
-- 
-- /* Quadratic constraint attributes */
-- 
-- #define GRB_STR_ATTR_QCTAG    "QCTag"   /* quadratic constraint tags */
-- #define GRB_DBL_ATTR_QCRHS    "QCRHS"   /* QC RHS */
-- #define GRB_CHAR_ATTR_QCSENSE "QCSense" /* QC sense ('<', '>', or '=') */
-- #define GRB_STR_ATTR_QCNAME   "QCName"  /* QC name */
-- 
-- /* General constraint attributes */
-- 
-- #define GRB_INT_ATTR_GENCONSTRTYPE  "GenConstrType"  /* Type of general constraint */
-- #define GRB_STR_ATTR_GENCONSTRNAME  "GenConstrName"  /* Name of general constraint */
-- 
-- /* General function constraint attributes */
-- 
-- #define GRB_INT_ATTR_FUNCPIECES      "FuncPieces"       /* An option for PWL translation */
-- #define GRB_DBL_ATTR_FUNCPIECEERROR  "FuncPieceError"   /* An option for PWL translation */
-- #define GRB_DBL_ATTR_FUNCPIECELENGTH "FuncPieceLength"  /* An option for PWL translation */
-- #define GRB_DBL_ATTR_FUNCPIECERATIO  "FuncPieceRatio"   /* An option for PWL translation */
-- #define GRB_INT_ATTR_FUNCNONLINEAR   "FuncNonlinear"    /* An option for PWL translation */
-- 
-- /* Model statistics */
-- 
-- #define GRB_DBL_ATTR_MAX_COEFF      "MaxCoeff"     /* Max (abs) nz coeff in A */
-- #define GRB_DBL_ATTR_MIN_COEFF      "MinCoeff"     /* Min (abs) nz coeff in A */
-- #define GRB_DBL_ATTR_MAX_BOUND      "MaxBound"     /* Max (abs) finite var bd */
-- #define GRB_DBL_ATTR_MIN_BOUND      "MinBound"     /* Min (abs) var bd */
-- #define GRB_DBL_ATTR_MAX_OBJ_COEFF  "MaxObjCoeff"  /* Max (abs) obj coeff */
-- #define GRB_DBL_ATTR_MIN_OBJ_COEFF  "MinObjCoeff"  /* Min (abs) obj coeff */
-- #define GRB_DBL_ATTR_MAX_RHS        "MaxRHS"       /* Max (abs) rhs coeff */
-- #define GRB_DBL_ATTR_MIN_RHS        "MinRHS"       /* Min (abs) rhs coeff */
-- #define GRB_DBL_ATTR_MAX_QCCOEFF    "MaxQCCoeff"   /* Max (abs) nz coeff in Q */
-- #define GRB_DBL_ATTR_MIN_QCCOEFF    "MinQCCoeff"   /* Min (abs) nz coeff in Q */
-- #define GRB_DBL_ATTR_MAX_QOBJ_COEFF "MaxQObjCoeff" /* Max (abs) obj coeff of quadratic part */
-- #define GRB_DBL_ATTR_MIN_QOBJ_COEFF "MinQObjCoeff" /* Min (abs) obj coeff of quadratic part */
-- #define GRB_DBL_ATTR_MAX_QCLCOEFF   "MaxQCLCoeff"  /* Max (abs) nz coeff in linear part of Q */
-- #define GRB_DBL_ATTR_MIN_QCLCOEFF   "MinQCLCoeff"  /* Min (abs) nz coeff in linear part of Q */
-- #define GRB_DBL_ATTR_MAX_QCRHS      "MaxQCRHS"     /* Max (abs) rhs of Q */
-- #define GRB_DBL_ATTR_MIN_QCRHS      "MinQCRHS"     /* Min (abs) rhs of Q */
-- 
-- /* Model solution attributes */
-- 
-- #define GRB_DBL_ATTR_RUNTIME       "Runtime"     /* Run time for optimization */
-- #define GRB_DBL_ATTR_WORK          "Work"        /* Work for optimization */
-- #define GRB_INT_ATTR_STATUS        "Status"      /* Optimization status */
-- #define GRB_DBL_ATTR_OBJVAL        "ObjVal"      /* Solution objective */
-- #define GRB_DBL_ATTR_OBJBOUND      "ObjBound"    /* Best bound on solution */
-- #define GRB_DBL_ATTR_OBJBOUNDC     "ObjBoundC"   /* Continuous bound */
-- #define GRB_DBL_ATTR_POOLOBJBOUND  "PoolObjBound" /* Best bound on pool solution */
-- #define GRB_DBL_ATTR_POOLOBJVAL    "PoolObjVal"  /* Solution objective, depends on solutionnumber */
-- #define GRB_DBL_ATTR_MIPGAP        "MIPGap"      /* MIP optimality gap */
-- #define GRB_INT_ATTR_SOLCOUNT      "SolCount"    /* # of solutions found */
-- #define GRB_DBL_ATTR_ITERCOUNT     "IterCount"   /* Iters performed (simplex) */
-- #define GRB_INT_ATTR_BARITERCOUNT  "BarIterCount" /* Iters performed (barrier) */
-- #define GRB_DBL_ATTR_NODECOUNT     "NodeCount"    /* Nodes explored (B&C) */
-- #define GRB_DBL_ATTR_OPENNODECOUNT "OpenNodeCount" /* Unexplored nodes (B&C) */
-- #define GRB_INT_ATTR_HASDUALNORM   "HasDualNorm"  /* 0, no basis,
--                                                      1, has basis, so can be computed
--                                                      2, available */
-- #define GRB_INT_ATTR_CONCURRENTWINMETHOD  "ConcurrentWinMethod"      /* method that solved LP using concurrent */
-- 
-- /* Variable attributes related to the current solution */
-- 
-- #define GRB_DBL_ATTR_X         "X"         /* Solution value */
-- #define GRB_DBL_ATTR_XN        "Xn"        /* Alternate MIP solution, depends on solutionnumber */
-- #define GRB_DBL_ATTR_BARX      "BarX"      /* Best barrier primal iterate */
-- #define GRB_DBL_ATTR_BARPI     "BarPi"     /* Best barrier dual iterate */
-- #define GRB_DBL_ATTR_RC        "RC"        /* Reduced costs */
-- #define GRB_DBL_ATTR_VDUALNORM "VDualNorm" /* Dual norm square */
-- #define GRB_INT_ATTR_VBASIS    "VBasis"    /* Variable basis status */
-- 
-- /* Constraint attributes related to the current solution */
-- 
-- #define GRB_DBL_ATTR_PI        "Pi"        /* Dual value */
-- #define GRB_DBL_ATTR_QCPI      "QCPi"      /* Dual value for QC */
-- #define GRB_DBL_ATTR_SLACK     "Slack"     /* Constraint slack */
-- #define GRB_DBL_ATTR_QCSLACK   "QCSlack"   /* QC Constraint slack */
-- #define GRB_DBL_ATTR_CDUALNORM "CDualNorm" /* Dual norm square */
-- #define GRB_INT_ATTR_CBASIS    "CBasis"    /* Constraint basis status */
-- 
-- /* Solution quality attributes */
-- 
-- #define GRB_DBL_ATTR_MAX_VIO                "MaxVio"
-- #define GRB_DBL_ATTR_BOUND_VIO              "BoundVio"
-- #define GRB_DBL_ATTR_BOUND_SVIO             "BoundSVio"
-- #define GRB_INT_ATTR_BOUND_VIO_INDEX        "BoundVioIndex"
-- #define GRB_INT_ATTR_BOUND_SVIO_INDEX       "BoundSVioIndex"
-- #define GRB_DBL_ATTR_BOUND_VIO_SUM          "BoundVioSum"
-- #define GRB_DBL_ATTR_BOUND_SVIO_SUM         "BoundSVioSum"
-- #define GRB_DBL_ATTR_CONSTR_VIO             "ConstrVio"
-- #define GRB_DBL_ATTR_CONSTR_SVIO            "ConstrSVio"
-- #define GRB_INT_ATTR_CONSTR_VIO_INDEX       "ConstrVioIndex"
-- #define GRB_INT_ATTR_CONSTR_SVIO_INDEX      "ConstrSVioIndex"
-- #define GRB_DBL_ATTR_CONSTR_VIO_SUM         "ConstrVioSum"
-- #define GRB_DBL_ATTR_CONSTR_SVIO_SUM        "ConstrSVioSum"
-- #define GRB_DBL_ATTR_CONSTR_RESIDUAL        "ConstrResidual"
-- #define GRB_DBL_ATTR_CONSTR_SRESIDUAL       "ConstrSResidual"
-- #define GRB_INT_ATTR_CONSTR_RESIDUAL_INDEX  "ConstrResidualIndex"
-- #define GRB_INT_ATTR_CONSTR_SRESIDUAL_INDEX "ConstrSResidualIndex"
-- #define GRB_DBL_ATTR_CONSTR_RESIDUAL_SUM    "ConstrResidualSum"
-- #define GRB_DBL_ATTR_CONSTR_SRESIDUAL_SUM   "ConstrSResidualSum"
-- #define GRB_DBL_ATTR_DUAL_VIO               "DualVio"
-- #define GRB_DBL_ATTR_DUAL_SVIO              "DualSVio"
-- #define GRB_INT_ATTR_DUAL_VIO_INDEX         "DualVioIndex"
-- #define GRB_INT_ATTR_DUAL_SVIO_INDEX        "DualSVioIndex"
-- #define GRB_DBL_ATTR_DUAL_VIO_SUM           "DualVioSum"
-- #define GRB_DBL_ATTR_DUAL_SVIO_SUM          "DualSVioSum"
-- #define GRB_DBL_ATTR_DUAL_RESIDUAL          "DualResidual"
-- #define GRB_DBL_ATTR_DUAL_SRESIDUAL         "DualSResidual"
-- #define GRB_INT_ATTR_DUAL_RESIDUAL_INDEX    "DualResidualIndex"
-- #define GRB_INT_ATTR_DUAL_SRESIDUAL_INDEX   "DualSResidualIndex"
-- #define GRB_DBL_ATTR_DUAL_RESIDUAL_SUM      "DualResidualSum"
-- #define GRB_DBL_ATTR_DUAL_SRESIDUAL_SUM     "DualSResidualSum"
-- #define GRB_DBL_ATTR_INT_VIO                "IntVio"
-- #define GRB_INT_ATTR_INT_VIO_INDEX          "IntVioIndex"
-- #define GRB_DBL_ATTR_INT_VIO_SUM            "IntVioSum"
-- #define GRB_DBL_ATTR_COMPL_VIO              "ComplVio"
-- #define GRB_INT_ATTR_COMPL_VIO_INDEX        "ComplVioIndex"
-- #define GRB_DBL_ATTR_COMPL_VIO_SUM          "ComplVioSum"
-- #define GRB_DBL_ATTR_KAPPA                  "Kappa"
-- #define GRB_DBL_ATTR_KAPPA_EXACT            "KappaExact"
-- #define GRB_DBL_ATTR_N2KAPPA                "N2Kappa"
-- 
-- /* LP sensitivity analysis */
-- 
-- #define GRB_DBL_ATTR_SA_OBJLOW "SAObjLow"
-- #define GRB_DBL_ATTR_SA_OBJUP  "SAObjUp"
-- #define GRB_DBL_ATTR_SA_LBLOW  "SALBLow"
-- #define GRB_DBL_ATTR_SA_LBUP   "SALBUp"
-- #define GRB_DBL_ATTR_SA_UBLOW  "SAUBLow"
-- #define GRB_DBL_ATTR_SA_UBUP   "SAUBUp"
-- #define GRB_DBL_ATTR_SA_RHSLOW "SARHSLow"
-- #define GRB_DBL_ATTR_SA_RHSUP  "SARHSUp"
-- 
-- /* IIS */
-- 
-- #define GRB_INT_ATTR_IIS_MINIMAL   "IISMinimal"   /* Boolean: Is IIS Minimal? */
-- #define GRB_INT_ATTR_IIS_LB        "IISLB"        /* Boolean: Is var LB in IIS? */
-- #define GRB_INT_ATTR_IIS_UB        "IISUB"        /* Boolean: Is var UB in IIS? */
-- #define GRB_INT_ATTR_IIS_CONSTR    "IISConstr"    /* Boolean: Is constr in IIS? */
-- #define GRB_INT_ATTR_IIS_SOS       "IISSOS"       /* Boolean: Is SOS in IIS? */
-- #define GRB_INT_ATTR_IIS_QCONSTR   "IISQConstr"   /* Boolean: Is QConstr in IIS? */
-- #define GRB_INT_ATTR_IIS_GENCONSTR "IISGenConstr" /* Boolean: Is general constr in IIS? */
-- 
-- #define GRB_INT_ATTR_IIS_LBFORCE        "IISLBForce"        /* Force var LB to be (1) or to not be (0) in final IIS */
-- #define GRB_INT_ATTR_IIS_UBFORCE        "IISUBForce"        /* Force var UB to be (1) or to not be (0) in final IIS */
-- #define GRB_INT_ATTR_IIS_CONSTRFORCE    "IISConstrForce"    /* Force constr to be (1) or to not be (0) in final IIS */
-- #define GRB_INT_ATTR_IIS_SOSFORCE       "IISSOSForce"       /* Force SOS to be (1) or to not be (0) in final IIS */
-- #define GRB_INT_ATTR_IIS_QCONSTRFORCE   "IISQConstrForce"   /* Force QConstr to be (1) or to not be (0) in final IIS */
-- #define GRB_INT_ATTR_IIS_GENCONSTRFORCE "IISGenConstrForce" /* Force general constr to be (1) or to not be (0) in final IIS */
-- 
-- /* Tuning */
-- 
-- #define GRB_INT_ATTR_TUNE_RESULTCOUNT "TuneResultCount"
-- 
-- /* Advanced simplex features */
-- 
-- #define GRB_DBL_ATTR_FARKASDUAL  "FarkasDual"
-- #define GRB_DBL_ATTR_FARKASPROOF "FarkasProof"
-- #define GRB_DBL_ATTR_UNBDRAY     "UnbdRay"
-- #define GRB_INT_ATTR_INFEASVAR   "InfeasVar"
-- #define GRB_INT_ATTR_UNBDVAR     "UnbdVar"
-- 
-- /* Presolve attribute */
-- 
-- #define GRB_INT_ATTR_VARPRESTAT "VarPreStat"
-- #define GRB_DBL_ATTR_PREFIXVAL  "PreFixVal"
-- 
-- /* Multi objective attribute, controlled by parameter ObjNumber (= i)
--  *
--  * Note if you add an new attribute, adjust the const array
--  * OBJNUMATTRNAMES and define OBJNUMATTRS in attrcache.c.
--  */
-- #define GRB_DBL_ATTR_OBJN         "ObjN"         /* ith objective */
-- #define GRB_DBL_ATTR_OBJNVAL      "ObjNVal"      /* Solution objective for Multi-objectives, also depends on solutionnumber */
-- #define GRB_DBL_ATTR_OBJNCON      "ObjNCon"      /* constant term */
-- #define GRB_DBL_ATTR_OBJNWEIGHT   "ObjNWeight"   /* weight */
-- #define GRB_INT_ATTR_OBJNPRIORITY "ObjNPriority" /* priority */
-- #define GRB_DBL_ATTR_OBJNRELTOL   "ObjNRelTol"   /* relative tolerance */
-- #define GRB_DBL_ATTR_OBJNABSTOL   "ObjNAbsTol"   /* absolute tolerance */
-- #define GRB_STR_ATTR_OBJNNAME     "ObjNName"     /* name */
-- 
-- /* Scenario attributes, controlled by parameter ScenarioNumber (= i)
--  *
--  * Note if you add an new attribute, adjust the const array
--  * SCENARIONUMATTRNAMES and define SCENARIONUMATTRS in attrcache.c.
--  */
-- 
-- #define GRB_DBL_ATTR_SCENNLB       "ScenNLB"       /* lower bound in scenario i */
-- #define GRB_DBL_ATTR_SCENNUB       "ScenNUB"       /* upper bound in scenario i */
-- #define GRB_DBL_ATTR_SCENNOBJ      "ScenNObj"      /* objective in scenario i */
-- #define GRB_DBL_ATTR_SCENNRHS      "ScenNRHS"      /* right hand side in scenario i */
-- #define GRB_STR_ATTR_SCENNNAME     "ScenNName"     /* name of scenario i */
-- #define GRB_DBL_ATTR_SCENNX        "ScenNX"        /* solution value in scenario i */
-- #define GRB_DBL_ATTR_SCENNOBJBOUND "ScenNObjBound" /* objective bound for scenario i */
-- #define GRB_DBL_ATTR_SCENNOBJVAL   "ScenNObjVal"   /* objective value for scenario i */
-- 
-- #define GRB_INT_ATTR_NUMOBJ       "NumObj"       /* number of objectives */
-- #define GRB_INT_ATTR_NUMSCENARIOS "NumScenarios" /* number of scenarios */
-- #define GRB_INT_ATTR_NUMSTART     "NumStart"     /* number of MIP starts */
-- 
-- 
-- /* Memory consumption statistics */
-- 
-- #define GRB_DBL_ATTR_MEMUSED      "MemUsed"        /* current amount of allocated memory (in GB) in master environment */
-- #define GRB_DBL_ATTR_MAXMEMUSED   "MaxMemUsed"     /* maximum amount of allocated memory (in GB) in master environment */
-- 
-- 
-- /* Alternate define */
-- 
-- #define GRB_DBL_ATTR_Xn "Xn"
-- 
-- /* General constraints */
-- 
-- #define GRB_GENCONSTR_MAX         0
-- #define GRB_GENCONSTR_MIN         1
-- #define GRB_GENCONSTR_ABS         2
-- #define GRB_GENCONSTR_AND         3
-- #define GRB_GENCONSTR_OR          4
-- #define GRB_GENCONSTR_NORM        5
-- #define GRB_GENCONSTR_NL          6
-- #define GRB_GENCONSTR_INDICATOR   7
-- #define GRB_GENCONSTR_PWL         8
-- #define GRB_GENCONSTR_POLY        9
-- #define GRB_GENCONSTR_EXP        10
-- #define GRB_GENCONSTR_EXPA       11
-- #define GRB_GENCONSTR_LOG        12
-- #define GRB_GENCONSTR_LOGA       13
-- #define GRB_GENCONSTR_POW        14
-- #define GRB_GENCONSTR_SIN        15
-- #define GRB_GENCONSTR_COS        16
-- #define GRB_GENCONSTR_TAN        17
-- #define GRB_GENCONSTR_LOGISTIC   18
-- 
-- #define NUMGENCONSTYPES          19
-- 
-- /* Operation codes for genconstrNL */
-- 
-- #define GRB_OPCODE_CONSTANT      0
-- #define GRB_OPCODE_VARIABLE      1
-- #define GRB_OPCODE_PLUS          2
-- #define GRB_OPCODE_MINUS         3
-- #define GRB_OPCODE_MULTIPLY      4
-- #define GRB_OPCODE_DIVIDE        5
-- #define GRB_OPCODE_UMINUS        6
-- #define GRB_OPCODE_SQUARE        7
-- #define GRB_OPCODE_SQRT          8
-- #define GRB_OPCODE_SIN           9
-- #define GRB_OPCODE_COS           10
-- #define GRB_OPCODE_TAN           11
-- #define GRB_OPCODE_POW           12
-- #define GRB_OPCODE_EXP           13
-- #define GRB_OPCODE_LOG           14
-- #define GRB_OPCODE_LOG2          15
-- #define GRB_OPCODE_LOG10         16
-- #define GRB_OPCODE_LOGISTIC      17
-- 
-- /*
--    CALLBACKS
-- */
-- 
-- /* For callback */
-- 
-- #define GRB_CB_POLLING   0
-- #define GRB_CB_PRESOLVE  1
-- #define GRB_CB_SIMPLEX   2
-- #define GRB_CB_MIP       3
-- #define GRB_CB_MIPSOL    4
-- #define GRB_CB_MIPNODE   5
-- #define GRB_CB_MESSAGE   6
-- #define GRB_CB_BARRIER   7
-- #define GRB_CB_MULTIOBJ  8
-- #define GRB_CB_IIS       9
-- 
-- /* Supported names for callback */
-- 
-- #define GRB_CB_PRE_COLDEL  1000
-- #define GRB_CB_PRE_ROWDEL  1001
-- #define GRB_CB_PRE_SENCHG  1002
-- #define GRB_CB_PRE_BNDCHG  1003
-- #define GRB_CB_PRE_COECHG  1004
-- 
-- #define GRB_CB_SPX_ITRCNT  2000
-- #define GRB_CB_SPX_OBJVAL  2001
-- #define GRB_CB_SPX_PRIMINF 2002
-- #define GRB_CB_SPX_DUALINF 2003
-- #define GRB_CB_SPX_ISPERT  2004
-- 
-- #define GRB_CB_MIP_OBJBST         3000
-- #define GRB_CB_MIP_OBJBND         3001
-- #define GRB_CB_MIP_NODCNT         3002
-- #define GRB_CB_MIP_SOLCNT         3003
-- #define GRB_CB_MIP_CUTCNT         3004
-- #define GRB_CB_MIP_NODLFT         3005
-- #define GRB_CB_MIP_ITRCNT         3006
-- #define GRB_CB_MIP_OPENSCENARIOS  3007
-- #define GRB_CB_MIP_PHASE          3008
-- 
-- #define GRB_CB_MIPSOL_SOL            4001
-- #define GRB_CB_MIPSOL_OBJ            4002
-- #define GRB_CB_MIPSOL_OBJBST         4003
-- #define GRB_CB_MIPSOL_OBJBND         4004
-- #define GRB_CB_MIPSOL_NODCNT         4005
-- #define GRB_CB_MIPSOL_SOLCNT         4006
-- #define GRB_CB_MIPSOL_OPENSCENARIOS  4007
-- #define GRB_CB_MIPSOL_PHASE          4008
-- 
-- #define GRB_CB_MIPNODE_STATUS         5001
-- #define GRB_CB_MIPNODE_REL            5002
-- #define GRB_CB_MIPNODE_OBJBST         5003
-- #define GRB_CB_MIPNODE_OBJBND         5004
-- #define GRB_CB_MIPNODE_NODCNT         5005
-- #define GRB_CB_MIPNODE_SOLCNT         5006
-- #define GRB_CB_MIPNODE_BRVAR          5007
-- #define GRB_CB_MIPNODE_OPENSCENARIOS  5008
-- #define GRB_CB_MIPNODE_PHASE          5009
-- 
-- #define GRB_CB_MSG_STRING  6001
-- #define GRB_CB_RUNTIME     6002
-- #define GRB_CB_WORK        6003
-- #define GRB_CB_MEMUSED     6004
-- #define GRB_CB_MAXMEMUSED  6005
-- 
-- #define GRB_CB_BARRIER_ITRCNT  7001
-- #define GRB_CB_BARRIER_PRIMOBJ 7002
-- #define GRB_CB_BARRIER_DUALOBJ 7003
-- #define GRB_CB_BARRIER_PRIMINF 7004
-- #define GRB_CB_BARRIER_DUALINF 7005
-- #define GRB_CB_BARRIER_COMPL   7006
-- 
-- #define GRB_CB_MULTIOBJ_OBJCNT  8001
-- #define GRB_CB_MULTIOBJ_SOLCNT  8002
-- #define GRB_CB_MULTIOBJ_SOL     8003
-- #define GRB_CB_MULTIOBJ_ITRCNT  8004
-- #define GRB_CB_MULTIOBJ_OBJBST  8005 /* if single objective is an LP we
--                                       * still do not have a "_OBJVAL", the
--                                       * user can query the _OBJBST/_OBJBND
--                                       * values instead */
-- #define GRB_CB_MULTIOBJ_OBJBND  8006
-- #define GRB_CB_MULTIOBJ_STATUS  8007
-- #define GRB_CB_MULTIOBJ_MIPGAP  8008
-- #define GRB_CB_MULTIOBJ_NODCNT  8009
-- #define GRB_CB_MULTIOBJ_NODLFT  8010
-- #define GRB_CB_MULTIOBJ_RUNTIME 8011
-- #define GRB_CB_MULTIOBJ_WORK    8012
-- /* TODO maybe we should also support, think about in MIP/LP cases if not applicable
-- #define GRB_CB_MULTIOBJ_PRIMINF 8012
-- #define GRB_CB_MULTIOBJ_DUALINF 8013
-- #define GRB_CB_MULTIOBJ_ISPERT  8014
-- */
-- 
-- #define GRB_CB_IIS_CONSTRMIN    9001
-- #define GRB_CB_IIS_CONSTRMAX    9002
-- #define GRB_CB_IIS_CONSTRGUESS  9003
-- #define GRB_CB_IIS_BOUNDMIN     9004
-- #define GRB_CB_IIS_BOUNDMAX     9005
-- #define GRB_CB_IIS_BOUNDGUESS   9006
-- 
-- /* FeasRelax method parameter values */
-- 
-- #define GRB_FEASRELAX_LINEAR      0
-- #define GRB_FEASRELAX_QUADRATIC   1
-- #define GRB_FEASRELAX_CARDINALITY 2

-- int __stdcall
--   GRBgetcoeff(GRBmodel *model, int constr, int var, double *valP);
foreign import stdcall unsafe "GRBgetcoeff" getcoeff
  :: Model -- ^ model
  -> CInt -- ^ constr
  -> CInt -- ^ var
  -> Ptr CDouble -- valP
  -> IO ErrorCode

-- int __stdcall
--   GRBgetconstrs(GRBmodel *model, int *numnzP, int *cbeg,
--                 int *cind, double *cval, int start, int len);
foreign import stdcall unsafe "GRBgetconstrs" getconstrs
  :: Model -- ^ model
  -> Ptr CInt -- ^ numnzP
  -> Ptr CInt -- ^ cbeg
  -> Ptr CInt -- ^ cind
  -> Ptr CDouble -- ^ cval
  -> CInt -- ^ start
  -> CInt -- ^ len
  -> IO ErrorCode

-- int __stdcall
--   GRBXgetconstrs(GRBmodel *model, size_t *numnzP, size_t *cbeg,
--                  int *cind, double *cval, int start, int len);
foreign import stdcall unsafe "GRBXgetconstrs" xgetconstrs
  :: Model -- ^ model
  -> Ptr CSize -- ^ numnzP
  -> Ptr CSize -- ^ cbeg
  -> Ptr CInt -- ^ cind
  -> Ptr CDouble -- ^ cval
  -> CInt -- ^ start
  -> CInt -- ^ len
  -> IO ErrorCode

-- int __stdcall
--   GRBgetvars(GRBmodel *model, int *numnzP, int *vbeg, int *vind,
--              double *vval, int start, int len);
foreign import stdcall unsafe "GRBgetvars" getvars
  :: Model -- ^ model
  -> Ptr CInt -- ^ numnzP
  -> Ptr CInt -- ^ vbeg
  -> Ptr CInt -- ^ vind
  -> Ptr CDouble -- ^ vval
  -> CInt -- ^ start
  -> CInt -- ^ len
  -> IO ErrorCode

-- int __stdcall
--   GRBXgetvars(GRBmodel *model, size_t *numnzP, size_t *vbeg, int *vind,
--               double *vval, int start, int len);
foreign import stdcall unsafe "GRBXgetvars" xgetvars
  :: Model -- ^ model
  -> Ptr CSize -- ^ numnzP
  -> Ptr CSize -- ^ vbeg
  -> Ptr CInt -- ^ vind
  -> Ptr CDouble -- ^ vval
  -> CInt -- ^ start
  -> CInt -- ^ len
  -> IO ErrorCode

-- int __stdcall
--   GRBgetsos(GRBmodel *model, int *nummembersP, int *sostype, int *beg,
--             int *ind, double *weight, int start, int len);
foreign import stdcall unsafe "GRBgetsos" getsos
  :: Model -- ^ model
  -> Ptr CInt -- ^ nummembersP
  -> Ptr CInt -- ^ sostype
  -> Ptr CInt -- ^ beg
  -> Ptr CInt -- ^ ind
  -> Ptr CDouble -- ^ weight
  -> CInt -- ^ start
  -> CInt -- ^ len
  -> IO ErrorCode

-- int __stdcall
--   GRBgetgenconstrMax(GRBmodel *model, int genconstr, int *resvarP,
--                      int *nvarsP, int *vars, double *constantP);
foreign import stdcall unsafe "GRBgetgenconstrMax" getgenconstrMax
  :: Model -- ^ model
  -> CInt -- ^ genconstr
  -> Ptr CInt -- ^ resvarP
  -> Ptr CInt -- ^ nvarsP
  -> Ptr CInt -- ^ vars
  -> Ptr CDouble -- ^ constantP
  -> IO ErrorCode

-- int __stdcall
--   GRBgetgenconstrMin(GRBmodel *model, int genconstr, int *resvarP,
--                      int *nvarsP, int *vars, double *constantP);
foreign import stdcall unsafe "GRBgetgenconstrMin" getgenconstrMin
  :: Model -- ^ model
  -> CInt -- ^ genconstr
  -> Ptr CInt -- ^ resvarP
  -> Ptr CInt -- ^ nvarsP
  -> Ptr CInt -- ^ vars
  -> Ptr CDouble -- ^ constantP
  -> IO ErrorCode

-- int __stdcall
--   GRBgetgenconstrAbs(GRBmodel *model, int genconstr, int *resvarP, int *argvarP);
foreign import stdcall unsafe "GRBgetgenconstrAbs" getgenconstrAbs
  :: Model -- ^ model
  -> CInt -- ^ genconstr
  -> Ptr CInt -- ^ resvarP
  -> Ptr CInt -- ^ argvarP
  -> IO ErrorCode

-- int __stdcall
--   GRBgetgenconstrAnd(GRBmodel *model, int genconstr, int *resvarP,
--                      int *nvarsP, int *vars);
foreign import stdcall unsafe "GRBgetgenconstrAnd" getgenconstrAnd
  :: Model -- ^ model
  -> CInt -- ^ genconstr
  -> Ptr CInt -- ^ resvarP
  -> Ptr CInt -- ^ nvarsP
  -> Ptr CInt -- ^ vars
  -> IO ErrorCode

-- int __stdcall
--   GRBgetgenconstrOr(GRBmodel *model, int genconstr, int *resvarP,
--                     int *nvarsP, int *vars);
foreign import stdcall unsafe "GRBgetgenconstrOr" getgenconstrOr
  :: Model -- ^ model
  -> CInt -- ^ genconstr
  -> Ptr CInt -- ^ resvarP
  -> Ptr CInt -- ^ nvarsP
  -> Ptr CInt -- ^ vars
  -> IO ErrorCode

-- int __stdcall
--   GRBgetgenconstrNorm(GRBmodel *model, int genconstr, int *resvarP,
--                       int *nvarsP, int *vars, double *whichP);
foreign import stdcall unsafe "GRBgetgenconstrNorm" getgenconstrNorm
  :: Model -- ^ model
  -> CInt -- ^ genconstr
  -> Ptr CInt -- ^ resvarP
  -> Ptr CInt -- ^ nvarsP
  -> Ptr CInt -- ^ vars
  -> Ptr CDouble -- ^ whichP
  -> IO ErrorCode

-- int __stdcall
-- GRBgetgenconstrNL(GRBmodel *model, int genconstr, int *resvarP, int *nnodesP,
--                   int *opcode, double *data, int *parent);
foreign import stdcall unsafe "GRBgetgenconstrNL" getgenconstrNL
  :: Model -- ^ model
  -> CInt -- ^ genconstr
  -> Ptr CInt -- ^ resvarP
  -> Ptr CInt -- ^ nnodesP
  -> Ptr CInt -- ^ opcode
  -> Ptr CDouble -- ^ data
  -> Ptr CInt -- ^ parent
  -> IO ErrorCode

-- int __stdcall
--   GRBgetgenconstrIndicator(GRBmodel *model, int genconstr, int *binvarP, int *binvalP,
--                            int *nvarsP, int *vars, double *vals,
--                            char *senseP, double *rhsP);
foreign import stdcall unsafe "GRBgetgenconstrIndicator" getgenconstrIndicator
  :: Model -- ^ model
  -> CInt -- ^ genconstr
  -> Ptr CInt -- ^ binvarP
  -> Ptr CInt -- ^ binvalP
  -> Ptr CInt -- ^ nvarsP
  -> Ptr CInt -- ^ vars
  -> Ptr CDouble -- ^ vals
  -> Ptr CChar -- ^ senseP
  -> Ptr CDouble -- ^ rhsP
  -> IO ErrorCode

-- int __stdcall
--   GRBgetgenconstrPWL(GRBmodel *model, int genconstr, int *xvarP, int *yvarP,
--                      int *nptsP, double *xpts, double *ypts);
foreign import stdcall unsafe "GRBgetgenconstrPWL" getgenconstrPWL
  :: Model -- ^ model
  -> CInt -- ^ genconstr
  -> Ptr CInt -- ^ xvarP
  -> Ptr CInt -- ^ yvarP
  -> Ptr CInt -- ^ nptsP
  -> Ptr CDouble -- ^ xpts
  -> Ptr CDouble -- ^ ypts
  -> IO ErrorCode

-- int __stdcall
--   GRBgetgenconstrPoly(GRBmodel *model, int genconstr, int *xvarP,
--                       int *yvarP, int *plenP, double *p);
foreign import stdcall unsafe "GRBgetgenconstrPoly" getgenconstrPoly
  :: Model -- ^ model
  -> CInt -- ^ genconstr
  -> Ptr CInt -- ^ xvarP
  -> Ptr CInt -- ^ yvarP
  -> Ptr CInt -- ^ plenP
  -> Ptr CDouble -- ^ p
  -> IO ErrorCode

-- int __stdcall
--   GRBgetgenconstrExpA(GRBmodel *model, int genconstr, int *xvarP,
--                       int *yvarP, double *aP);
foreign import stdcall unsafe "GRBgetgenconstrExpA" getgenconstrExpA
  :: Model -- ^ model
  -> CInt -- ^ genconstr
  -> Ptr CInt -- ^ xvarP
  -> Ptr CInt -- ^ yvarP
  -> Ptr CDouble -- ^ aP
  -> IO ErrorCode

-- int __stdcall
--   GRBgetgenconstrLogA(GRBmodel *model, int genconstr, int *xvarP,
--                       int *yvarP, double *aP);
foreign import stdcall unsafe "GRBgetgenconstrLogA" getgenconstrLogA
  :: Model -- ^ model
  -> CInt -- ^ genconstr
  -> Ptr CInt -- ^ xvarP
  -> Ptr CInt -- ^ yvarP
  -> Ptr CDouble -- ^ aP
  -> IO ErrorCode

-- int __stdcall
--   GRBgetgenconstrPow(GRBmodel *model, int genconstr, int *xvarP,
--                      int *yvarP, double *aP);
foreign import stdcall unsafe "GRBgetgenconstrPow" getgenconstrPow
  :: Model -- ^ model
  -> CInt -- ^ genconstr
  -> Ptr CInt -- ^ xvarP
  -> Ptr CInt -- ^ yvarP
  -> Ptr CDouble -- ^ aP
  -> IO ErrorCode

-- int __stdcall
--   GRBgetgenconstrExp(GRBmodel *model, int genconstr, int *xvarP, int *yvarP);
foreign import stdcall unsafe "GRBgetgenconstrExp" getgenconstrExp
  :: Model -- ^ model
  -> CInt -- ^ genconstr
  -> Ptr CInt -- ^ xvarP
  -> Ptr CInt -- ^ yvarP
  -> IO ErrorCode

-- int __stdcall
--   GRBgetgenconstrLog(GRBmodel *model, int genconstr, int *xvarP, int *yvarP);
foreign import stdcall unsafe "GRBgetgenconstrLog" getgenconstrLog
  :: Model -- ^ model
  -> CInt -- ^ genconstr
  -> Ptr CInt -- ^ xvarP
  -> Ptr CInt -- ^ yvarP
  -> IO ErrorCode

-- int __stdcall
--   GRBgetgenconstrLogistic(GRBmodel *model, int genconstr, int *xvarP,
--                           int *yvarP);
foreign import stdcall unsafe "GRBgetgenconstrLogistic" getgenconstrLogistic
  :: Model -- ^ model
  -> CInt -- ^ genconstr
  -> Ptr CInt -- ^ xvarP
  -> Ptr CInt -- ^ yvarP
  -> IO ErrorCode

-- int __stdcall
--   GRBgetgenconstrSin(GRBmodel *model, int genconstr, int *xvarP, int *yvarP);
foreign import stdcall unsafe "GRBgetgenconstrSin" getgenconstrSin
  :: Model -- ^ model
  -> CInt -- ^ genconstr
  -> Ptr CInt -- ^ xvarP
  -> Ptr CInt -- ^ yvarP
  -> IO ErrorCode

-- int __stdcall
--   GRBgetgenconstrCos(GRBmodel *model, int genconstr, int *xvarP, int *yvarP);
foreign import stdcall unsafe "GRBgetgenconstrCos" getgenconstrCos
  :: Model -- ^ model
  -> CInt -- ^ genconstr
  -> Ptr CInt -- ^ xvarP
  -> Ptr CInt -- ^ yvarP
  -> IO ErrorCode

-- int __stdcall
--   GRBgetgenconstrTan(GRBmodel *model, int genconstr, int *xvarP, int *yvarP);
foreign import stdcall unsafe "GRBgetgenconstrTan" getgenconstrTan
  :: Model -- ^ model
  -> CInt -- ^ genconstr
  -> Ptr CInt -- ^ xvarP
  -> Ptr CInt -- ^ yvarP
  -> IO ErrorCode

-- int __stdcall
--   GRBgetq(GRBmodel *model, int *numqnzP, int *qrow, int *qcol, double *qval);
foreign import stdcall unsafe "GRBgetq" getq
  :: Model -- ^ model
  -> Ptr CInt -- ^ numqnzP
  -> Ptr CInt -- ^ qrow
  -> Ptr CInt -- ^ qcol
  -> Ptr CDouble -- ^ qval
  -> IO ErrorCode

-- int __stdcall
--   GRBgetqconstr(GRBmodel *model, int qconstr,
--                 int *numlnzP, int *lind, double *lval,
--                 int *numqnzP, int *qrow, int *qcol, double *qval);
foreign import stdcall unsafe "GRBgetqconstr" getqconstr
  :: Model -- ^ model
  -> CInt -- ^ qconstr
  -> Ptr CInt -- ^ numlnzP
  -> Ptr CInt -- ^ lind
  -> Ptr CDouble -- ^ lval
  -> Ptr CInt -- ^ numqnzP
  -> Ptr CInt -- ^ qrow
  -> Ptr CInt -- ^ qcol
  -> Ptr CDouble -- ^ qval
  -> IO ErrorCode

-- int __stdcall
--   GRBgetvarbyname(GRBmodel *model, const char *name, int *indexP);
foreign import stdcall unsafe "GRBgetvarbyname" getvarbyname
  :: Model -- ^ model
  -> CString -- ^ name
  -> Ptr CInt -- ^ indexP
  -> IO ErrorCode

-- int __stdcall
--   GRBgetconstrbyname(GRBmodel *model, const char *name, int *indexP);
foreign import stdcall unsafe "GRBgetconstrbyname" getconstrbyname
  :: Model -- ^ model
  -> CString -- ^ name
  -> Ptr CInt -- ^ indexP
  -> IO ErrorCode

-- int __stdcall
--   GRBgetqconstrbyname(GRBmodel *model, const char *name, int *indexP);
foreign import stdcall unsafe "GRBgetqconstrbyname" getqconstrbyname
  :: Model -- ^ model
  -> CString -- ^ name
  -> Ptr CInt -- ^ indexP
  -> IO ErrorCode

-- int __stdcall
--   GRBgetpwlobj(GRBmodel *model, int var, int *pointsP,
--                double *x, double *y);
foreign import stdcall unsafe "GRBgetpwlobj" getpwlobj
  :: Model -- ^ model
  -> CInt -- ^ var
  -> Ptr CInt -- ^ pointsP
  -> Ptr CDouble -- ^ x
  -> Ptr CDouble -- ^ y
  -> IO ErrorCode

-- int __stdcall
--   GRBoptimize(GRBmodel *model);
foreign import stdcall safe "GRBoptimize" optimize
  :: Model -- ^ model
  -> IO ErrorCode

-- int __stdcall
--   GRBoptimizeasync(GRBmodel *model);
foreign import stdcall unsafe "GRBoptimizeasync" optimizeasync
  :: Model -- ^ model
  -> IO ErrorCode

-- int __stdcall
--   GRBoptimizebatch(GRBmodel *model, char *batchid);
foreign import stdcall safe "GRBoptimizebatch" optimizebatch
  :: Model -- ^ model
  -> CString -- ^ batchid
  -> IO ErrorCode

-- GRBmodel * __stdcall
--   GRBcopymodel(GRBmodel *model);
foreign import stdcall safe "GRBcopymodel" copymodel
  :: Model -- ^ model
  -> IO Model

-- int __stdcall
--   GRBcopymodeltoenv(GRBmodel *model, GRBenv *env, GRBmodel **resultP);
foreign import stdcall safe "GRBcopymodeltoenv" copymodeltoenv
  :: Model -- ^ model
  -> Env -- ^ env
  -> Ptr Model -- ^ resultP
  -> IO ErrorCode

-- int __stdcall
--   GRBfixmodel(GRBmodel *model, GRBmodel **fixedP);
foreign import stdcall safe "GRBfixmodel" fixmodel
  :: Model -- ^ model
  -> Ptr Model -- ^ fixedP
  -> IO ErrorCode

-- int __stdcall
--   GRBfeasrelax(GRBmodel *model, int relaxobjtype, int minrelax,
--                double *lbpen, double *ubpen, double *rhspen,
--                double *feasobjP);
foreign import stdcall safe "GRBfeasrelax" feasrelax
  :: Model -- ^ model
  -> CInt -- ^ relaxobjtype
  -> CInt -- ^ minrelax
  -> Ptr CDouble -- ^ lbpen
  -> Ptr CDouble -- ^ ubpen
  -> Ptr CDouble -- ^ rhspen
  -> Ptr CDouble -- ^ feasobjP
  -> IO ErrorCode

-- int __stdcall
--   GRBsinglescenariomodel(GRBmodel *model, GRBmodel **singlescenarioP);
foreign import stdcall safe "GRBsinglescenariomodel" singlescenariomodel
  :: Model -- ^ model
  -> Ptr Model -- ^ singlescenarioP
  -> IO ErrorCode

-- int __stdcall
--   GRBconverttofixed(GRBmodel *model);
foreign import stdcall safe "GRBconverttofixed" converttofixed
  :: Model -- ^ model
  -> IO ErrorCode

-- /* Undocumented routines */
-- 
-- int __stdcall
--   GRBgetcbwhatinfo(void *cbdata, int what, int *typeP, int *sizeP);
foreign import stdcall unsafe "GRBgetcbwhatinfo" getcbwhatinfo
  :: CBData -- ^ cbdata
  -> CInt -- ^ what
  -> Ptr CInt -- ^ typeP
  -> Ptr CInt -- ^ sizeP
  -> IO ErrorCode

-- int __stdcall
--   GRBrelaxmodel(GRBmodel *model, GRBmodel **relaxedP);
foreign import stdcall safe "GRBrelaxmodel" relaxmodel
  :: Model -- ^ model
  -> Ptr Model -- ^ relaxedP
  -> IO ErrorCode

-- int __stdcall
--   GRBpresolvemodel(GRBmodel *model, GRBmodel **presolvedP);
foreign import stdcall safe "GRBpresolvemodel" presolvemodel
  :: Model -- ^ model
  -> Ptr Model -- ^ presolvedP
  -> IO ErrorCode

-- int __stdcall
--   GRBiismodel(GRBmodel *model, GRBmodel **iisP);
foreign import stdcall safe "GRBiismodel" iismodel
  :: Model -- ^ model
  -> Ptr Model -- ^ iisP
  -> IO ErrorCode

-- int __stdcall
--   GRBfeasibility(GRBmodel *model, GRBmodel **feasP);
foreign import stdcall safe "GRBfeasibility" feasibility
  :: Model -- ^ model
  -> Ptr Model -- ^ feasP
  -> IO ErrorCode

-- int __stdcall
--   GRBlinearizemodel(GRBmodel *model, GRBmodel **linearizedP);
foreign import stdcall safe "GRBlinearizemodel" linearizemodel
  :: Model -- ^ model
  -> Ptr Model -- ^ linearizedP
  -> IO ErrorCode

-- int __stdcall
--   GRBresultmodel(GRBmodel *model, char *type, GRBmodel **resultP);
foreign import stdcall safe "GRBresultmodel" resultmodel
  :: Model -- ^ model
  -> CString -- ^ type
  -> Ptr Model -- ^ resultP
  -> IO ErrorCode

-- GRBmodel * __stdcall
--   GRBfixedmodel(GRBmodel *model);
foreign import stdcall safe "GRBfixedmodel" fixedmodel
  :: Model -- ^ model
  -> IO Model

-- int __stdcall
--   GRBdualmodel(GRBmodel *model, GRBmodel **dualP);
foreign import stdcall safe "GRBdualmodel" dualmodel
  :: Model -- ^ model
  -> Ptr Model -- ^ dualP
  -> IO ErrorCode

-- #define MALLOCCB_ARGS size_t size, void *syscbusrdata
type MallocCB usrdata = CSize -> Ptr usrdata -> IO (Ptr ())

-- #define CALLOCCB_ARGS size_t nmemb, size_t size, void *syscbusrdata
type CallocCB usrdata = CSize -> CSize -> Ptr usrdata -> IO (Ptr ())

-- #define REALLOCCB_ARGS void *ptr, size_t size, void *syscbusrdata
type ReallocCB usrdata = Ptr () -> CSize -> Ptr usrdata -> IO (Ptr ())

-- #define FREECB_ARGS void *ptr, void *syscbusrdata
type FreeCB usrdata = Ptr () -> Ptr usrdata -> IO ()

data Thread_
type Thread = Ptr Thread_

-- #define THREADCREATECB_ARGS void **threadP, void (*start_routine)(void *), void *arg, void *syscbusrdata
type ThreadCreateCB usrdata = Ptr Thread -> FunPtr (Ptr () -> IO ()) -> Ptr () -> Ptr usrdata -> IO CInt

-- #define THREADJOINCB_ARGS void *thread, void *syscbusrdata
type ThreadJoinCB usrdata = Ptr Thread -> Ptr usrdata -> IO ()

apitype :: CInt
apitype = -1
-- apitype = 0

-- #define GRBemptyenvadv(envP, malloccb, callocbc, realloccb, freecb, threadcreatecb, threadjoincb, syscbusrdata) GRBemptyenvadvinternal(envP, -1, GRB_VERSION_MAJOR, GRB_VERSION_MINOR, GRB_VERSION_TECHNICAL, malloccb, callocbc, realloccb, freecb, threadcreatecb, threadjoincb, syscbusrdata)
emptyenvadv envP malloccb callocbc realloccb freecb threadcreatecb threadjoincb syscbusrdata = emptyenvadvinternal envP apitype
  (#const GRB_VERSION_MAJOR)
  (#const GRB_VERSION_MINOR)
  (#const GRB_VERSION_TECHNICAL)
  malloccb
  callocbc
  realloccb
  freecb
  threadcreatecb
  threadjoincb
  syscbusrdata

-- int __stdcall
--   GRBemptyenvadvnocheck(GRBenv **envP,
--                         void * (__stdcall *malloccb)(MALLOCCB_ARGS),
--                         void * (__stdcall *calloccb)(CALLOCCB_ARGS),
--                         void * (__stdcall *realloccb)(REALLOCCB_ARGS),
--                         void   (__stdcall *freecb)(FREECB_ARGS),
--                         int    (__stdcall *threadcreatecb)(THREADCREATECB_ARGS),
--                         void   (__stdcall *threadjoincb)(THREADJOINCB_ARGS),
--                         void              *syscbusrdata);

-- int __stdcall
--   GRBemptyenvadvinternal(GRBenv **envP, int apitype, int major, int minor, int tech,
--                          void * (__stdcall *malloccb)(MALLOCCB_ARGS),
--                          void * (__stdcall *calloccb)(CALLOCCB_ARGS),
--                          void * (__stdcall *realloccb)(REALLOCCB_ARGS),
--                          void   (__stdcall *freecb)(FREECB_ARGS),
--                          int    (__stdcall *threadcreatecb)(THREADCREATECB_ARGS),
--                          void   (__stdcall *threadjoincb)(THREADJOINCB_ARGS),
--                          void              *syscbusrdata);
foreign import stdcall safe "GRBemptyenvadvinternal" emptyenvadvinternal
  :: Ptr Env -- ^ envP
  -> CInt -- ^ apitype
  -> CInt -- ^ major
  -> CInt -- ^ minor
  -> CInt -- ^ tech
  -> FunPtr (MallocCB usrdata) -- ^ malloccb
  -> FunPtr (CallocCB usrdata) -- ^ calloccb
  -> FunPtr (ReallocCB usrdata) -- ^ realloccb
  -> FunPtr (FreeCB usrdata) -- ^ freecb
  -> FunPtr (ThreadCreateCB usrdata) -- ^ threadcreatecb
  -> FunPtr (ThreadJoinCB usrdata) -- ^ threadjoincb
  -> Ptr usrdata -- ^ syscbusrdata
  -> IO ErrorCode

-- int __stdcall
--   GRBreadmodel(GRBenv *env, const char *filename, GRBmodel **modelP);
foreign import stdcall safe "GRBreadmodel" readmodel
  :: Env -- ^ env
  -> CString -- ^ filename
  -> Ptr Model -- ^ modelP
  -> IO ErrorCode

-- int __stdcall
--   GRBread(GRBmodel *model, const char *filename);
foreign import stdcall safe "GRBread" read
  :: Model -- ^ model
  -> CString -- ^ filename
  -> IO ErrorCode

-- int __stdcall
--   GRBwrite(GRBmodel *model, const char *filename);
foreign import stdcall safe "GRBwrite" write
  :: Model -- ^ model
  -> CString -- ^ filename
  -> IO ErrorCode

-- int __stdcall
--   GRBismodelfile(const char *filename);
foreign import stdcall safe "GRBismodelfile" ismodelfile
  :: CString -- ^ filename
  -> IO ErrorCode

-- int __stdcall
--   GRBisattrfile(const char *filename);
foreign import stdcall safe "GRBisattrfile" isattrfile
  :: CString -- ^ filename
  -> IO ErrorCode

-- int __stdcall
--   GRBfiletype(const char *filename);
foreign import stdcall safe "GRBfiletype" filetype
  :: CString -- ^ filename
  -> IO ErrorCode

-- int __stdcall
--   GRBisrecordfile(const char *filename);
foreign import stdcall safe "GRBisrecordfile" isrecordfile
  :: CString -- ^ filename
  -> IO ErrorCode

-- int __stdcall
--   GRBgetjsonsolution(GRBmodel *model, char **buffP);
foreign import stdcall safe "GRBgetjsonsolution" getjsonsolution
  :: Model -- ^ model
  -> Ptr CString -- ^ buffP
  -> IO ErrorCode

-- int __stdcall
--   GRBloadjson(GRBenv *env, const char *fname, char **buffP);
foreign import stdcall safe "GRBloadjson" loadjson
  :: Env -- ^ env
  -> CString -- ^ fname
  -> Ptr CString -- ^ buffP
  -> IO ErrorCode

-- int __stdcall
--   GRBnewmodel(GRBenv *env, GRBmodel **modelP, const char *Pname, int numvars,
--               double *obj, double *lb, double *ub, char *vtype,
--               char **varnames);
foreign import stdcall safe "GRBnewmodel" newmodel
  :: Env -- ^ env
  -> Ptr Model -- ^ modelP
  -> CString -- ^ Pname
  -> CInt -- ^ numvars
  -> Ptr CDouble -- ^ obj
  -> Ptr CDouble -- ^ lb
  -> Ptr CDouble -- ^ ub
  -> Ptr CChar -- ^ vtype
  -> Ptr (Ptr CString) -- ^ varnames
  -> IO ErrorCode

-- int __stdcall
--   GRBloadmodel(GRBenv *env, GRBmodel **modelP, const char *Pname,
--                int numvars, int numconstrs,
--                int objsense, double objcon, double *obj,
--                char *sense, double *rhs,
--                int *vbeg, int *vlen, int *vind, double *vval,
--                double *lb, double *ub, char *vtype,
--                char **varnames, char **constrnames);
foreign import stdcall safe "GRBloadmodel" loadmodel
  :: Env -- ^ env
  -> Ptr Model -- ^ modelP
  -> CString -- ^ Pname
  -> CInt -- ^ numvars
  -> CInt -- ^ numconstrs
  -> CInt -- ^ objsense
  -> CDouble -- ^ objcon
  -> Ptr CDouble -- ^ obj
  -> Ptr CChar -- ^ sense
  -> Ptr CDouble -- ^ rhs
  -> Ptr CInt -- ^ vbeg
  -> Ptr CInt -- ^ vlen
  -> Ptr CInt -- ^ vind
  -> Ptr CDouble -- ^ vval
  -> Ptr CDouble -- ^ lb
  -> Ptr CDouble -- ^ ub
  -> Ptr CChar -- ^ vtype
  -> Ptr (Ptr CString) -- ^ varnames
  -> Ptr (Ptr CString) -- ^ constrnames
  -> IO ErrorCode

-- int __stdcall
--   GRBXloadmodel(GRBenv *env, GRBmodel **modelP, const char *Pname,
--                 int numvars, int numconstrs,
--                 int objsense, double objcon, double *obj,
--                 char *sense, double *rhs,
--                 size_t *vbeg, int *vlen, int *vind, double *vval,
--                 double *lb, double *ub, char *vtype,
--                 char **varnames, char **constrnames);
foreign import stdcall safe "GRBXloadmodel" xloadmodel
  :: Env -- ^ env
  -> Ptr Model -- ^ modelP
  -> CString -- ^ Pname
  -> CInt -- ^ numvars
  -> CInt -- ^ numconstrs
  -> CInt -- ^ objsense
  -> CDouble -- ^ objcon
  -> Ptr CDouble -- ^ obj
  -> Ptr CChar -- ^ sense
  -> Ptr CDouble -- ^ rhs
  -> Ptr CSize -- ^ vbeg
  -> Ptr CInt -- ^ vlen
  -> Ptr CInt -- ^ vind
  -> Ptr CDouble -- ^ vval
  -> Ptr CDouble -- ^ lb
  -> Ptr CDouble -- ^ ub
  -> Ptr CChar -- ^ vtype
  -> Ptr (Ptr CString) -- ^ varnames
  -> Ptr (Ptr CString) -- ^ constrnames
  -> IO ErrorCode

-- int __stdcall
--   GRBaddvar(GRBmodel *model, int numnz, int *vind, double *vval,
--             double obj, double lb, double ub, char vtype,
--             const char *varname);
foreign import stdcall unsafe "GRBaddvar" addvar
  :: Model -- ^ model
  -> CInt -- ^ numnz
  -> Ptr CInt -- ^ vind
  -> Ptr CDouble -- ^ vval
  -> CDouble -- ^ obj
  -> CDouble -- ^ lb
  -> CDouble -- ^ ub
  -> CChar -- ^ vtype
  -> CString -- ^ varname
  -> IO ErrorCode

-- int __stdcall
--   GRBaddvars(GRBmodel *model, int numvars, int numnz,
--              int *vbeg, int *vind, double *vval,
--              double *obj, double *lb, double *ub, char *vtype,
--              char **varnames);
foreign import stdcall unsafe "GRBaddvars" addvars
  :: Model -- ^ model
  -> CInt -- ^ numvars
  -> CInt -- ^ numnz
  -> Ptr CInt -- ^ vbeg
  -> Ptr CInt -- ^ vind
  -> Ptr CDouble -- ^ vval
  -> Ptr CDouble -- ^ obj
  -> Ptr CDouble -- ^ lb
  -> Ptr CDouble -- ^ ub
  -> Ptr CChar -- ^ vtype
  -> Ptr (Ptr CString) -- ^ varnames
  -> IO ErrorCode

-- int __stdcall
--   GRBXaddvars(GRBmodel *model, int numvars, size_t numnz,
--               size_t *vbeg, int *vind, double *vval,
--               double *obj, double *lb, double *ub, char *vtype,
--               char **varnames);
foreign import stdcall unsafe "GRBXaddvars" xaddvars
  :: Model -- ^ model
  -> CInt -- ^ numvars
  -> CSize -- ^ numnz
  -> Ptr CSize -- ^ vbeg
  -> Ptr CInt -- ^ vind
  -> Ptr CDouble -- ^ vval
  -> Ptr CDouble -- ^ obj
  -> Ptr CDouble -- ^ lb
  -> Ptr CDouble -- ^ ub
  -> Ptr CChar -- ^ vtype
  -> Ptr (Ptr CString) -- ^ varnames
  -> IO ErrorCode

-- int __stdcall
--   GRBaddconstr(GRBmodel *model, int numnz, int *cind, double *cval,
--                char sense, double rhs, const char *constrname);
foreign import stdcall unsafe "GRBaddconstr" addconstr
  :: Model -- ^ model
  -> CInt -- ^ numnz
  -> Ptr CInt -- ^ cind
  -> Ptr CDouble -- ^ cval
  -> CChar -- ^ sense
  -> CDouble -- ^ rhs
  -> CString -- ^ constrname
  -> IO ErrorCode

-- int __stdcall
--   GRBaddconstrs(GRBmodel *model, int numconstrs, int numnz,
--                 int *cbeg, int *cind, double *cval,
--                 char *sense, double *rhs, char **constrnames);
foreign import stdcall unsafe "GRBaddconstrs" addconstrs
  :: Model -- ^ model
  -> CInt -- ^ numconstrs
  -> CInt -- ^ numnz
  -> Ptr CInt -- ^ cbeg
  -> Ptr CInt -- ^ cind
  -> Ptr CDouble -- ^ cval
  -> Ptr CChar -- ^ sense
  -> Ptr CDouble -- ^ rhs
  -> Ptr (Ptr CString) -- ^ constrnames
  -> IO ErrorCode

-- int __stdcall
--   GRBXaddconstrs(GRBmodel *model, int numconstrs, size_t numnz,
--                  size_t *cbeg, int *cind, double *cval,
--                  char *sense, double *rhs, char **constrnames);
foreign import stdcall unsafe "GRBXaddconstrs" xaddconstrs
  :: Model -- ^ model
  -> CInt -- ^ numconstrs
  -> CSize -- ^ numnz
  -> Ptr CSize -- ^ cbeg
  -> Ptr CInt -- ^ cind
  -> Ptr CDouble -- ^ cval
  -> Ptr CChar -- ^ sense
  -> Ptr CDouble -- ^ rhs
  -> Ptr (Ptr CString) -- ^ constrnames
  -> IO ErrorCode

-- int __stdcall
--   GRBaddrangeconstr(GRBmodel *model, int numnz, int *cind, double *cval,
--                     double lower, double upper, const char *constrname);
foreign import stdcall unsafe "GRBaddrangeconstr" addrangeconstr
  :: Model -- ^ model
  -> CInt -- ^ numnz
  -> Ptr CInt -- ^ cind
  -> Ptr CDouble -- ^ cval
  -> CDouble -- ^ lower
  -> CDouble -- ^ upper
  -> CString -- ^ constrname
  -> IO ErrorCode

-- int __stdcall
--   GRBaddrangeconstrs(GRBmodel *model, int numconstrs, int numnz,
--                      int *cbeg, int *cind, double *cval,
--                      double *lower, double *upper, char **constrnames);
foreign import stdcall unsafe "GRBaddrangeconstrs" addrangeconstrs
  :: Model -- ^ model
  -> CInt -- ^ numconstrs
  -> CInt -- ^ numnz
  -> Ptr CInt -- ^ cbeg
  -> Ptr CInt -- ^ cind
  -> Ptr CDouble -- ^ cval
  -> Ptr CDouble -- ^ lower
  -> Ptr CDouble -- ^ upper
  -> Ptr (Ptr CString) -- ^ constrnames
  -> IO ErrorCode

-- int __stdcall
--   GRBXaddrangeconstrs(GRBmodel *model, int numconstrs, size_t numnz,
--                       size_t *cbeg, int *cind, double *cval,
--                       double *lower, double *upper, char **constrnames);
foreign import stdcall unsafe "GRBXaddrangeconstrs" xaddrangeconstrs
  :: Model -- ^ model
  -> CInt -- ^ numconstrs
  -> CSize -- ^ numnz
  -> Ptr CSize -- ^ cbeg
  -> Ptr CInt -- ^ cind
  -> Ptr CDouble -- ^ cval
  -> Ptr CDouble -- ^ lower
  -> Ptr CDouble -- ^ upper
  -> Ptr (Ptr CString) -- ^ constrnames
  -> IO ErrorCode

-- int __stdcall
--   GRBaddsos(GRBmodel *model, int numsos, int nummembers, int *types,
--             int *beg, int *ind, double *weight);
foreign import stdcall unsafe "GRBaddsos" addsos
  :: Model -- ^ model
  -> CInt -- ^ numsos
  -> CInt -- ^ nummembers
  -> Ptr CInt -- ^ types
  -> Ptr CInt -- ^ beg
  -> Ptr CInt -- ^ ind
  -> Ptr CDouble -- ^ weight
  -> IO ErrorCode

-- int __stdcall
--   GRBaddgenconstrMax(GRBmodel *model, const char *name,
--                      int resvar, int nvars, const int *vars,
--                      double constant);
foreign import stdcall unsafe "GRBaddgenconstrMax" addgenconstrMax
  :: Model -- ^ model
  -> CString -- ^ name
  -> CInt -- ^ resvar
  -> CInt -- ^ nvars
  -> Ptr CInt -- ^ vars
  -> CDouble -- ^ constant
  -> IO ErrorCode

-- int __stdcall
--   GRBaddgenconstrMin(GRBmodel *model, const char *name,
--                      int resvar, int nvars, const int *vars,
--                      double constant);
foreign import stdcall unsafe "GRBaddgenconstrMin" addgenconstrMin
  :: Model -- ^ model
  -> CString -- ^ name
  -> CInt -- ^ resvar
  -> CInt -- ^ nvars
  -> Ptr CInt -- ^ vars
  -> CDouble -- ^ constant
  -> IO ErrorCode

-- int __stdcall
--   GRBaddgenconstrAbs(GRBmodel *model, const char *name,
--                      int resvar, int argvar);
foreign import stdcall unsafe "GRBaddgenconstrAbs" addgenconstrAbs
  :: Model -- ^ model
  -> CString -- ^ name
  -> CInt -- ^ resvar
  -> CInt -- ^ argvar
  -> IO ErrorCode

-- int __stdcall
--   GRBaddgenconstrAnd(GRBmodel *model, const char *name,
--                      int resvar, int nvars, const int *vars);
foreign import stdcall unsafe "GRBaddgenconstrAnd" addgenconstrAnd
  :: Model -- ^ model
  -> CString -- ^ name
  -> CInt -- ^ resvar
  -> CInt -- ^ nvars
  -> Ptr CInt -- ^ vars
  -> IO ErrorCode

-- int __stdcall
--   GRBaddgenconstrOr(GRBmodel *model, const char *name,
--                     int resvar, int nvars, const int *vars);
foreign import stdcall unsafe "GRBaddgenconstrOr" addgenconstrOr
  :: Model -- ^ model
  -> CString -- ^ name
  -> CInt -- ^ resvar
  -> CInt -- ^ nvars
  -> Ptr CInt -- ^ vars
  -> IO ErrorCode

-- int __stdcall
--   GRBaddgenconstrNorm(GRBmodel *model, const char *name,
--                       int resvar, int nvars, const int *vars, double which);
foreign import stdcall unsafe "GRBaddgenconstrNorm" addgenconstrNorm
  :: Model -- ^ model
  -> CString -- ^ name
  -> CInt -- ^ resvar
  -> CInt -- ^ nvars
  -> Ptr CInt -- ^ vars
  -> CDouble -- ^ which
  -> IO ErrorCode

-- int __stdcall
-- GRBaddgenconstrNL(GRBmodel *model,
--                   const char *name, int resvar, int nnodes, const int *opcode,
--                   const double *data, const int *parent);
foreign import stdcall unsafe "GRBaddgenconstrNL" addgenconstrNL
  :: Model -- ^ model
  -> CString -- ^ name
  -> CInt -- ^ resvar
  -> CInt -- ^ nnodes
  -> Ptr CInt -- ^ opcode
  -> Ptr CDouble -- ^ data
  -> Ptr CInt -- ^ parent
  -> IO ErrorCode

-- int __stdcall
--   GRBaddgenconstrIndicator(GRBmodel *model, const char *name,
--                            int binvar, int binval, int nvars, const int *vars,
--                            const double *vals, char sense, double rhs);
foreign import stdcall unsafe "GRBaddgenconstrIndicator" addgenconstrIndicator
  :: Model -- ^ model
  -> CString -- ^ name
  -> CInt -- ^ binvar
  -> CInt -- ^ binval
  -> CInt -- ^ nvars
  -> Ptr CInt -- ^ vars
  -> Ptr CDouble -- ^ vals
  -> CChar -- ^ sense
  -> CDouble -- ^ rhs
  -> IO ErrorCode

-- int __stdcall
--   GRBaddgenconstrPWL(GRBmodel *model, const char *name,
--                      int xvar, int yvar, int npts,
--                      const double *xpts, const double *ypts);
foreign import stdcall unsafe "GRBaddgenconstrPWL" addgenconstrPWL
  :: Model -- ^ model
  -> CString -- ^ name
  -> CInt -- ^ xvar
  -> CInt -- ^ yvar
  -> CInt -- ^ npts
  -> Ptr CDouble -- ^ xpts
  -> Ptr CDouble -- ^ ypts
  -> IO ErrorCode

-- int __stdcall
--   GRBaddgenconstrPoly(GRBmodel *model, const char *name, int xvar, int yvar,
--                       int plen, const double *p, const char *options);
foreign import stdcall unsafe "GRBaddgenconstrPoly" addgenconstrPoly
  :: Model -- ^ model
  -> CString -- ^ name
  -> CInt -- ^ xvar
  -> CInt -- ^ yvar
  -> CInt -- ^ plen
  -> Ptr CDouble -- ^ p
  -> CString -- ^ options
  -> IO ErrorCode

-- int __stdcall
--   GRBaddgenconstrExpA(GRBmodel *model, const char *name, int xvar,
--                       int yvar, double a, const char *options);
foreign import stdcall unsafe "GRBaddgenconstrExpA" addgenconstrExpA
  :: Model -- ^ model
  -> CString -- ^ name
  -> CInt -- ^ xvar
  -> CInt -- ^ yvar
  -> CDouble -- ^ a
  -> CString -- ^ options
  -> IO ErrorCode

-- int __stdcall
--   GRBaddgenconstrLogA(GRBmodel *model, const char *name, int xvar,
--                       int yvar, double a, const char *options);
foreign import stdcall unsafe "GRBaddgenconstrLogA" addgenconstrLogA
  :: Model -- ^ model
  -> CString -- ^ name
  -> CInt -- ^ xvar
  -> CInt -- ^ yvar
  -> CDouble -- ^ a
  -> CString -- ^ options
  -> IO ErrorCode

-- int __stdcall
--   GRBaddgenconstrPow(GRBmodel *model, const char *name, int xvar,
--                      int yvar, double a, const char *options);
foreign import stdcall unsafe "GRBaddgenconstrPow" addgenconstrPow
  :: Model -- ^ model
  -> CString -- ^ name
  -> CInt -- ^ xvar
  -> CInt -- ^ yvar
  -> CDouble -- ^ a
  -> CString -- ^ options
  -> IO ErrorCode

-- int __stdcall
--   GRBaddgenconstrExp(GRBmodel *model, const char *name, int xvar,
--                      int yvar, const char *options);
foreign import stdcall unsafe "GRBaddgenconstrExp" addgenconstrExp
  :: Model -- ^ model
  -> CString -- ^ name
  -> CInt -- ^ xvar
  -> CInt -- ^ yvar
  -> CString -- ^ options
  -> IO ErrorCode

-- int __stdcall
--   GRBaddgenconstrLog(GRBmodel *model, const char *name, int xvar,
--                      int yvar, const char *options);
foreign import stdcall unsafe "GRBaddgenconstrLog" addgenconstrLog
  :: Model -- ^ model
  -> CString -- ^ name
  -> CInt -- ^ xvar
  -> CInt -- ^ yvar
  -> CString -- ^ options
  -> IO ErrorCode

-- int __stdcall
--   GRBaddgenconstrSin(GRBmodel *model, const char *name, int xvar,
--                      int yvar, const char *options);
foreign import stdcall unsafe "GRBaddgenconstrSin" addgenconstrSin
  :: Model -- ^ model
  -> CString -- ^ name
  -> CInt -- ^ xvar
  -> CInt -- ^ yvar
  -> CString -- ^ options
  -> IO ErrorCode

-- int __stdcall
--   GRBaddgenconstrCos(GRBmodel *model, const char *name, int xvar,
--                      int yvar, const char *options);
foreign import stdcall unsafe "GRBaddgenconstrCos" addgenconstrCos
  :: Model -- ^ model
  -> CString -- ^ name
  -> CInt -- ^ xvar
  -> CInt -- ^ yvar
  -> CString -- ^ options
  -> IO ErrorCode

-- int __stdcall
--   GRBaddgenconstrTan(GRBmodel *model, const char *name, int xvar,
--                      int yvar, const char *options);
foreign import stdcall unsafe "GRBaddgenconstrTan" addgenconstrTan
  :: Model -- ^ model
  -> CString -- ^ name
  -> CInt -- ^ xvar
  -> CInt -- ^ yvar
  -> CString -- ^ options
  -> IO ErrorCode

-- int __stdcall
--   GRBaddgenconstrLogistic(GRBmodel *model, const char *name, int xvar,
--                           int yvar, const char *options);
foreign import stdcall unsafe "GRBaddgenconstrLogistic" addgenconstrLogistic
  :: Model -- ^ model
  -> CString -- ^ name
  -> CInt -- ^ xvar
  -> CInt -- ^ yvar
  -> CString -- ^ options
  -> IO ErrorCode

-- int __stdcall
--   GRBaddqconstr(GRBmodel *model, int numlnz, int *lind, double *lval,
--                 int numqnz, int *qrow, int *qcol, double *qval,
--                 char sense, double rhs, const char *QCname);
foreign import stdcall unsafe "GRBaddqconstr" addqconstr
  :: Model -- ^ model
  -> CInt -- ^ numlnz
  -> Ptr CInt -- ^ lind
  -> Ptr CDouble -- ^ lval
  -> CInt -- ^ numqnz
  -> Ptr CInt -- ^ qrow
  -> Ptr CInt -- ^ qcol
  -> Ptr CDouble -- ^ qval
  -> CChar -- ^ sense
  -> CDouble -- ^ rhs
  -> CString -- ^ QCname
  -> IO ErrorCode

-- int __stdcall
--   GRBaddcone(GRBmodel *model, int nummembers, int *members);
foreign import stdcall unsafe "GRBaddcone" addcone
  :: Model -- ^ model
  -> CInt -- ^ nummembers
  -> Ptr CInt -- ^ members
  -> IO ErrorCode

-- int __stdcall
--   GRBaddqpterms(GRBmodel *model, int numqnz, int *qrow, int *qcol,
--                 double *qval);
foreign import stdcall unsafe "GRBaddqpterms" addqpterms
  :: Model -- ^ model
  -> CInt -- ^ numqnz
  -> Ptr CInt -- ^ qrow
  -> Ptr CInt -- ^ qcol
  -> Ptr CDouble -- ^ qval
  -> IO ErrorCode

-- int __stdcall
--   GRBdelvars(GRBmodel *model, int len, int *ind);
foreign import stdcall unsafe "GRBdelvars" delvars
  :: Model -- ^ model
  -> CInt -- ^ len
  -> Ptr CInt -- ^ ind
  -> IO ErrorCode

-- int __stdcall
--   GRBdelconstrs(GRBmodel *model, int len, int *ind);
foreign import stdcall unsafe "GRBdelconstrs" delconstrs
  :: Model -- ^ model
  -> CInt -- ^ len
  -> Ptr CInt -- ^ ind
  -> IO ErrorCode

-- int __stdcall
--   GRBdelsos(GRBmodel *model, int len, int *ind);
foreign import stdcall unsafe "GRBdelsos" delsos
  :: Model -- ^ model
  -> CInt -- ^ len
  -> Ptr CInt -- ^ ind
  -> IO ErrorCode

-- int __stdcall
--   GRBdelgenconstrs(GRBmodel *model, int len, int *ind);
foreign import stdcall unsafe "GRBdelgenconstrs" delgenconstrs
  :: Model -- ^ model
  -> CInt -- ^ len
  -> Ptr CInt -- ^ ind
  -> IO ErrorCode

-- int __stdcall
--   GRBdelqconstrs(GRBmodel *model, int len, int *ind);
foreign import stdcall unsafe "GRBdelqconstrs" delqconstrs
  :: Model -- ^ model
  -> CInt -- ^ len
  -> Ptr CInt -- ^ ind
  -> IO ErrorCode

-- int __stdcall
--   GRBdelq(GRBmodel *model);
foreign import stdcall unsafe "GRBdelq" delq
  :: Model -- ^ model
  -> IO ErrorCode

-- int __stdcall
--   GRBchgcoeffs(GRBmodel *model, int cnt, int *cind, int *vind, double *val);
foreign import stdcall unsafe "GRBchgcoeffs" chgcoeffs
  :: Model -- ^ model
  -> CInt -- ^ cnt
  -> Ptr CInt -- ^ cind
  -> Ptr CInt -- ^ vind
  -> Ptr CDouble -- ^ val
  -> IO ErrorCode

-- int __stdcall
--   GRBXchgcoeffs(GRBmodel *model, size_t cnt, int *cind, int *vind, double *val);
foreign import stdcall unsafe "GRBXchgcoeffs" xchgcoeffs
  :: Model -- ^ model
  -> CSize -- ^ cnt
  -> Ptr CInt -- ^ cind
  -> Ptr CInt -- ^ vind
  -> Ptr CDouble -- ^ val
  -> IO ErrorCode

-- int __stdcall
--   GRBsetpwlobj(GRBmodel *model, int var, int points, double *x,
--                double *y);
foreign import stdcall unsafe "GRBsetpwlobj" setpwlobj
  :: Model -- ^ model
  -> CInt -- ^ var
  -> CInt -- ^ points
  -> Ptr CDouble -- ^ x
  -> Ptr CDouble -- ^ y
  -> IO ErrorCode

-- int __stdcall
--   GRBupdatemodel(GRBmodel *model);
foreign import stdcall safe "GRBupdatemodel" updatemodel
  :: Model -- ^ model
  -> IO ErrorCode

-- int __stdcall
--   GRBreset(GRBmodel *model, int clearall);
foreign import stdcall safe "GRBreset" reset
  :: Model -- ^ model
  -> CInt -- ^ clearall
  -> IO ErrorCode

-- int __stdcall
--   GRBfreemodel(GRBmodel *model);
foreign import stdcall safe "GRBfreemodel" freemodel
  :: Model -- ^ model
  -> IO ErrorCode

-- int __stdcall
--   GRBcomputeIIS(GRBmodel *model);
foreign import stdcall safe "GRBcomputeIIS" computeIIS
  :: Model -- ^ model
  -> IO ErrorCode

-- /* simplex advanced routines */
-- 
-- typedef struct _GRBsvec
-- {
--   int     len; /* sparse vector length. -1: It is a dense vector. */
--   int    *ind; /* indices array of the sparse vector */
--   double *val; /* value array of the sparse vector */
-- } GRBsvec;
-- 
-- int __stdcall
--   GRBFSolve(GRBmodel *model, GRBsvec *b, GRBsvec *x);
-- 
-- int __stdcall
--   GRBBinvColj(GRBmodel *model, int j, GRBsvec *x);
-- 
-- int __stdcall
--   GRBBinvj(GRBmodel *model, int j, GRBsvec *x);
-- 
-- int __stdcall
--   GRBBSolve(GRBmodel *model, GRBsvec *b, GRBsvec *x);
-- 
-- int __stdcall
--   GRBBinvi(GRBmodel *model, int i, GRBsvec *x);
-- 
-- int __stdcall
--   GRBBinvRowi(GRBmodel *model, int i, GRBsvec *x);
-- 
-- int __stdcall
--   GRBgetBasisHead(GRBmodel *model, int *bhead);
-- 
-- int __stdcall
--   GRBcbstoponemultiobj(GRBmodel *model, void *cbdata, int objnum);
-- 
-- int __stdcall
--   GRBsingularvectors(GRBmodel *model, double *left, double *right);

-- /* Model status codes */

-- #define GRB_LOADED          1
-- #define GRB_OPTIMAL         2
-- #define GRB_INFEASIBLE      3
-- #define GRB_INF_OR_UNBD     4
-- #define GRB_UNBOUNDED       5
-- #define GRB_CUTOFF          6
-- #define GRB_ITERATION_LIMIT 7
-- #define GRB_NODE_LIMIT      8
-- #define GRB_TIME_LIMIT      9
-- #define GRB_SOLUTION_LIMIT 10
-- #define GRB_INTERRUPTED    11
-- #define GRB_NUMERIC        12
-- #define GRB_SUBOPTIMAL     13
-- #define GRB_INPROGRESS     14
-- #define GRB_USER_OBJ_LIMIT 15
-- #define GRB_WORK_LIMIT     16
-- #define GRB_MEM_LIMIT      17
-- 
-- /* Basis status info */
-- 
-- #define GRB_BASIC           0
-- #define GRB_NONBASIC_LOWER -1
-- #define GRB_NONBASIC_UPPER -2
-- #define GRB_SUPERBASIC     -3
-- 
-- /* Undocumented routines */
-- 
-- int __stdcall
--   GRBstrongbranch(GRBmodel *model, int num, int *cand,
--                   double *downobjbd, double *upobjbd, int *statusP);
-- /**************/
-- /* Parameters */
-- /**************/
-- 
-- /* Termination */
-- 
-- #define GRB_INT_PAR_BARITERLIMIT   "BarIterLimit"
-- #define GRB_DBL_PAR_CUTOFF         "Cutoff"
-- #define GRB_DBL_PAR_ITERATIONLIMIT "IterationLimit"
-- #define GRB_DBL_PAR_NODELIMIT      "NodeLimit"
-- #define GRB_INT_PAR_SOLUTIONLIMIT  "SolutionLimit"
-- #define GRB_DBL_PAR_TIMELIMIT      "TimeLimit"
-- #define GRB_DBL_PAR_WORKLIMIT      "WorkLimit"
-- #define GRB_DBL_PAR_MEMLIMIT       "MemLimit"
-- #define GRB_DBL_PAR_SOFTMEMLIMIT   "SoftMemLimit"
-- #define GRB_DBL_PAR_BESTOBJSTOP    "BestObjStop"
-- #define GRB_DBL_PAR_BESTBDSTOP     "BestBdStop"
-- 
-- /* Tolerances */
-- 
-- #define GRB_DBL_PAR_FEASIBILITYTOL "FeasibilityTol"
-- #define GRB_DBL_PAR_INTFEASTOL     "IntFeasTol"
-- #define GRB_DBL_PAR_MARKOWITZTOL   "MarkowitzTol"
-- #define GRB_DBL_PAR_MIPGAP         "MIPGap"
-- #define GRB_DBL_PAR_MIPGAPABS      "MIPGapAbs"
-- #define GRB_DBL_PAR_OPTIMALITYTOL  "OptimalityTol"
-- #define GRB_DBL_PAR_PSDTOL         "PSDTol"
-- 
-- /* Simplex */
-- 
-- #define GRB_INT_PAR_METHOD            "Method"
-- #define GRB_INT_PAR_CONCURRENTMETHOD  "ConcurrentMethod"
-- #define GRB_DBL_PAR_PERTURBVALUE      "PerturbValue"
-- #define GRB_DBL_PAR_OBJSCALE          "ObjScale"
-- #define GRB_INT_PAR_SCALEFLAG         "ScaleFlag"
-- #define GRB_INT_PAR_SIMPLEXPRICING    "SimplexPricing"
-- #define GRB_INT_PAR_QUAD              "Quad"
-- #define GRB_INT_PAR_NORMADJUST        "NormAdjust"
-- #define GRB_INT_PAR_SIFTING           "Sifting"
-- #define GRB_INT_PAR_SIFTMETHOD        "SiftMethod"
-- #define GRB_INT_PAR_LPWARMSTART       "LPWarmStart"
-- #define GRB_INT_PAR_NETWORKALG        "NetworkAlg"
-- 
-- /* Barrier */
-- 
-- #define GRB_DBL_PAR_BARCONVTOL     "BarConvTol"
-- #define GRB_INT_PAR_BARCORRECTORS  "BarCorrectors"
-- #define GRB_INT_PAR_BARHOMOGENEOUS "BarHomogeneous"
-- #define GRB_INT_PAR_BARORDER       "BarOrder"
-- #define GRB_DBL_PAR_BARQCPCONVTOL  "BarQCPConvTol"
-- #define GRB_INT_PAR_CROSSOVER      "Crossover"
-- #define GRB_INT_PAR_CROSSOVERBASIS "CrossoverBasis"
-- 
-- /* MIP */
-- 
-- #define GRB_INT_PAR_BRANCHDIR          "BranchDir"
-- #define GRB_INT_PAR_DEGENMOVES         "DegenMoves"
-- #define GRB_INT_PAR_DISCONNECTED       "Disconnected"
-- #define GRB_DBL_PAR_HEURISTICS         "Heuristics"
-- #define GRB_DBL_PAR_IMPROVESTARTGAP    "ImproveStartGap"
-- #define GRB_DBL_PAR_IMPROVESTARTTIME   "ImproveStartTime"
-- #define GRB_DBL_PAR_IMPROVESTARTNODES  "ImproveStartNodes"
-- #define GRB_INT_PAR_INTEGRALITYFOCUS   "IntegralityFocus"
-- #define GRB_INT_PAR_MINRELNODES        "MinRelNodes"
-- #define GRB_INT_PAR_MIPFOCUS           "MIPFocus"
-- #define GRB_INT_PAR_NLPHEUR            "NLPHeur"
-- #define GRB_STR_PAR_NODEFILEDIR        "NodefileDir"
-- #define GRB_DBL_PAR_NODEFILESTART      "NodefileStart"
-- #define GRB_INT_PAR_NODEMETHOD         "NodeMethod"
-- #define GRB_DBL_PAR_NORELHEURTIME      "NoRelHeurTime"
-- #define GRB_DBL_PAR_NORELHEURWORK      "NoRelHeurWork"
-- #define GRB_INT_PAR_OBBT               "OBBT"
-- #define GRB_INT_PAR_PUMPPASSES         "PumpPasses"
-- #define GRB_INT_PAR_RINS               "RINS"
-- #define GRB_STR_PAR_SOLFILES           "SolFiles"
-- #define GRB_INT_PAR_STARTNODELIMIT     "StartNodeLimit"
-- #define GRB_INT_PAR_SUBMIPNODES        "SubMIPNodes"
-- #define GRB_INT_PAR_SYMMETRY           "Symmetry"
-- #define GRB_INT_PAR_VARBRANCH          "VarBranch"
-- #define GRB_INT_PAR_SOLUTIONNUMBER     "SolutionNumber"
-- #define GRB_INT_PAR_ZEROOBJNODES       "ZeroObjNodes"
-- 
-- /* MIP cuts */
-- 
-- #define GRB_INT_PAR_CUTS            "Cuts"
-- 
-- #define GRB_INT_PAR_CLIQUECUTS      "CliqueCuts"
-- #define GRB_INT_PAR_COVERCUTS       "CoverCuts"
-- #define GRB_INT_PAR_FLOWCOVERCUTS   "FlowCoverCuts"
-- #define GRB_INT_PAR_FLOWPATHCUTS    "FlowPathCuts"
-- #define GRB_INT_PAR_GUBCOVERCUTS    "GUBCoverCuts"
-- #define GRB_INT_PAR_IMPLIEDCUTS     "ImpliedCuts"
-- #define GRB_INT_PAR_PROJIMPLIEDCUTS "ProjImpliedCuts"
-- #define GRB_INT_PAR_MIPSEPCUTS      "MIPSepCuts"
-- #define GRB_INT_PAR_MIRCUTS         "MIRCuts"
-- #define GRB_INT_PAR_STRONGCGCUTS    "StrongCGCuts"
-- #define GRB_INT_PAR_MODKCUTS        "ModKCuts"
-- #define GRB_INT_PAR_ZEROHALFCUTS    "ZeroHalfCuts"
-- #define GRB_INT_PAR_NETWORKCUTS     "NetworkCuts"
-- #define GRB_INT_PAR_SUBMIPCUTS      "SubMIPCuts"
-- #define GRB_INT_PAR_INFPROOFCUTS    "InfProofCuts"
-- #define GRB_INT_PAR_RLTCUTS         "RLTCuts"
-- #define GRB_INT_PAR_RELAXLIFTCUTS   "RelaxLiftCuts"
-- #define GRB_INT_PAR_BQPCUTS         "BQPCuts"
-- #define GRB_INT_PAR_PSDCUTS         "PSDCuts"
-- #define GRB_INT_PAR_LIFTPROJECTCUTS "LiftProjectCuts"
-- #define GRB_INT_PAR_MIXINGCUTS      "MixingCuts"
-- #define GRB_INT_PAR_DUALIMPLIEDCUTS "DualImpliedCuts"
-- 
-- #define GRB_INT_PAR_CUTAGGPASSES    "CutAggPasses"
-- #define GRB_INT_PAR_CUTPASSES       "CutPasses"
-- #define GRB_INT_PAR_GOMORYPASSES    "GomoryPasses"
-- 
-- /* Distributed algorithms */
-- 
-- #define GRB_STR_PAR_WORKERPOOL      "WorkerPool"
-- #define GRB_STR_PAR_WORKERPASSWORD  "WorkerPassword"
-- 
-- /* Licensing and Compute Server */
-- #define GRB_STR_PAR_COMPUTESERVER     "ComputeServer"
-- #define GRB_STR_PAR_TOKENSERVER       "TokenServer"
-- #define GRB_STR_PAR_SERVERPASSWORD    "ServerPassword"
-- #define GRB_INT_PAR_SERVERTIMEOUT     "ServerTimeout"
-- #define GRB_STR_PAR_CSROUTER          "CSRouter"
-- #define GRB_STR_PAR_CSGROUP           "CSGroup"
-- #define GRB_DBL_PAR_CSQUEUETIMEOUT    "CSQueueTimeout"
-- #define GRB_INT_PAR_CSPRIORITY        "CSPriority"
-- #define GRB_INT_PAR_CSIDLETIMEOUT     "CSIdleTimeout"
-- #define GRB_INT_PAR_CSTLSINSECURE     "CSTLSInsecure"
-- #define GRB_INT_PAR_TSPORT            "TSPort"
-- #define GRB_STR_PAR_CLOUDACCESSID     "CloudAccessID"
-- #define GRB_STR_PAR_CLOUDSECRETKEY    "CloudSecretKey"
-- #define GRB_STR_PAR_CLOUDPOOL         "CloudPool"
-- #define GRB_STR_PAR_CLOUDHOST         "CloudHost"
-- #define GRB_STR_PAR_CSMANAGER         "CSManager"
-- #define GRB_STR_PAR_CSAUTHTOKEN       "CSAuthToken"
-- #define GRB_STR_PAR_CSAPIACCESSID     "CSAPIAccessID"
-- #define GRB_STR_PAR_CSAPISECRET       "CSAPISecret"
-- #define GRB_INT_PAR_CSBATCHMODE       "CSBatchMode"
-- #define GRB_STR_PAR_USERNAME          "Username"
-- #define GRB_STR_PAR_CSAPPNAME         "CSAppName"
-- #define GRB_INT_PAR_CSCLIENTLOG       "CSClientLog"
-- #define GRB_STR_PAR_WLSACCESSID       "WLSAccessID"
-- #define GRB_STR_PAR_WLSSECRET         "WLSSecret"
-- #define GRB_INT_PAR_WLSTOKENDURATION  "WLSTokenDuration"
-- #define GRB_DBL_PAR_WLSTOKENREFRESH   "WLSTokenRefresh"
-- #define GRB_STR_PAR_WLSTOKEN          "WLSToken"
-- #define GRB_INT_PAR_LICENSEID         "LicenseID"
-- #define GRB_STR_PAR_WLSPROXY          "WLSProxy"
-- #define GRB_STR_PAR_WLSCONFIG         "WLSConfig"
-- 
-- 
-- /* Other */
-- 
-- #define GRB_INT_PAR_AGGREGATE         "Aggregate"
-- #define GRB_INT_PAR_AGGFILL           "AggFill"
-- #define GRB_INT_PAR_CONCURRENTMIP     "ConcurrentMIP"
-- #define GRB_INT_PAR_CONCURRENTJOBS    "ConcurrentJobs"
-- #define GRB_INT_PAR_DISPLAYINTERVAL   "DisplayInterval"
-- #define GRB_INT_PAR_DISTRIBUTEDMIPJOBS "DistributedMIPJobs"
-- #define GRB_INT_PAR_DUALREDUCTIONS    "DualReductions"
-- #define GRB_DBL_PAR_FEASRELAXBIGM     "FeasRelaxBigM"
-- #define GRB_INT_PAR_IISMETHOD         "IISMethod"
-- #define GRB_INT_PAR_INFUNBDINFO       "InfUnbdInfo"
-- #define GRB_INT_PAR_JSONSOLDETAIL     "JSONSolDetail"
-- #define GRB_INT_PAR_LAZYCONSTRAINTS   "LazyConstraints"
-- #define GRB_STR_PAR_LOGFILE           "LogFile"
-- #define GRB_INT_PAR_LOGTOCONSOLE      "LogToConsole"
-- #define GRB_INT_PAR_MIQCPMETHOD       "MIQCPMethod"
-- #define GRB_INT_PAR_NONCONVEX         "NonConvex"
-- #define GRB_INT_PAR_NUMERICFOCUS      "NumericFocus"
-- #define GRB_INT_PAR_OUTPUTFLAG        "OutputFlag"
-- #define GRB_INT_PAR_PRECRUSH          "PreCrush"
-- #define GRB_INT_PAR_PREDEPROW         "PreDepRow"
-- #define GRB_INT_PAR_PREDUAL           "PreDual"
-- #define GRB_INT_PAR_PREPASSES         "PrePasses"
-- #define GRB_INT_PAR_PREQLINEARIZE     "PreQLinearize"
-- #define GRB_INT_PAR_PRESOLVE          "Presolve"
-- #define GRB_DBL_PAR_PRESOS1BIGM       "PreSOS1BigM"
-- #define GRB_DBL_PAR_PRESOS2BIGM       "PreSOS2BigM"
-- #define GRB_INT_PAR_PRESOS1ENCODING   "PreSOS1Encoding"
-- #define GRB_INT_PAR_PRESOS2ENCODING   "PreSOS2Encoding"
-- #define GRB_INT_PAR_PRESPARSIFY       "PreSparsify"
-- #define GRB_INT_PAR_PREMIQCPFORM      "PreMIQCPForm"
-- #define GRB_INT_PAR_QCPDUAL           "QCPDual"
-- #define GRB_INT_PAR_RECORD            "Record"
-- #define GRB_STR_PAR_RESULTFILE        "ResultFile"
-- #define GRB_INT_PAR_SEED              "Seed"
-- #define GRB_INT_PAR_SOLUTIONTARGET    "SolutionTarget"
-- #define GRB_INT_PAR_THREADS           "Threads"
-- #define GRB_INT_PAR_THREADLIMIT       "ThreadLimit"
-- #define GRB_DBL_PAR_TUNETIMELIMIT     "TuneTimeLimit"
-- #define GRB_INT_PAR_TUNERESULTS       "TuneResults"
-- #define GRB_INT_PAR_TUNECRITERION     "TuneCriterion"
-- #define GRB_INT_PAR_TUNETRIALS        "TuneTrials"
-- #define GRB_INT_PAR_TUNEOUTPUT        "TuneOutput"
-- #define GRB_INT_PAR_TUNEJOBS          "TuneJobs"
-- #define GRB_DBL_PAR_TUNECLEANUP       "TuneCleanup"
-- #define GRB_DBL_PAR_TUNETARGETMIPGAP  "TuneTargetMIPGap"
-- #define GRB_DBL_PAR_TUNETARGETTIME    "TuneTargetTime"
-- #define GRB_INT_PAR_TUNEMETRIC        "TuneMetric"
-- #define GRB_INT_PAR_TUNEDYNAMICJOBS   "TuneDynamicJobs"
-- #define GRB_INT_PAR_UPDATEMODE        "UpdateMode"
-- #define GRB_INT_PAR_OBJNUMBER         "ObjNumber"
-- #define GRB_INT_PAR_MULTIOBJMETHOD    "MultiObjMethod"
-- #define GRB_INT_PAR_MULTIOBJPRE       "MultiObjPre"
-- #define GRB_INT_PAR_SCENARIONUMBER    "ScenarioNumber"
-- #define GRB_INT_PAR_POOLSOLUTIONS     "PoolSolutions"
-- #define GRB_DBL_PAR_POOLGAP           "PoolGap"
-- #define GRB_DBL_PAR_POOLGAPABS        "PoolGapAbs"
-- #define GRB_INT_PAR_POOLSEARCHMODE    "PoolSearchMode"
-- #define GRB_INT_PAR_IGNORENAMES       "IgnoreNames"
-- #define GRB_INT_PAR_STARTNUMBER       "StartNumber"
-- #define GRB_INT_PAR_PARTITIONPLACE    "PartitionPlace"
-- #define GRB_INT_PAR_FUNCPIECES        "FuncPieces"
-- #define GRB_DBL_PAR_FUNCPIECELENGTH   "FuncPieceLength"
-- #define GRB_DBL_PAR_FUNCPIECEERROR    "FuncPieceError"
-- #define GRB_DBL_PAR_FUNCPIECERATIO    "FuncPieceRatio"
-- #define GRB_DBL_PAR_FUNCMAXVAL        "FuncMaxVal"
-- #define GRB_INT_PAR_FUNCNONLINEAR     "FuncNonlinear"
-- #define GRB_STR_PAR_DUMMY             "Dummy"
-- #define GRB_STR_PAR_JOBID             "JobID"
-- 
-- 
-- /* Parameter enumerations */
-- 
-- /* Cuts parameter values */
-- 
-- #define GRB_CUTS_AUTO          -1
-- #define GRB_CUTS_OFF            0
-- #define GRB_CUTS_CONSERVATIVE   1
-- #define GRB_CUTS_AGGRESSIVE     2
-- #define GRB_CUTS_VERYAGGRESSIVE 3
-- 
-- /* Presolve parameter values */
-- 
-- #define GRB_PRESOLVE_AUTO        -1
-- #define GRB_PRESOLVE_OFF          0
-- #define GRB_PRESOLVE_CONSERVATIVE 1
-- #define GRB_PRESOLVE_AGGRESSIVE   2
-- 
-- /* Method parameter values */
-- 
-- #define GRB_METHOD_NONE                            -1
-- #define GRB_METHOD_AUTO                            -1
-- #define GRB_METHOD_PRIMAL                           0
-- #define GRB_METHOD_DUAL                             1
-- #define GRB_METHOD_BARRIER                          2
-- #define GRB_METHOD_CONCURRENT                       3
-- #define GRB_METHOD_DETERMINISTIC_CONCURRENT         4
-- #define GRB_METHOD_DETERMINISTIC_CONCURRENT_SIMPLEX 5 /* Deprecated since v11 */
-- 
-- #define GRB_CONCURRENTMETHOD_AUTO                -1
-- #define GRB_CONCURRENTMETHOD_BARRIER_PRIMAL_DUAL  0
-- #define GRB_CONCURRENTMETHOD_BARRIER_DUAL         1
-- #define GRB_CONCURRENTMETHOD_BARRIER_PRIMAL       2
-- #define GRB_CONCURRENTMETHOD_PRIMAL_DUAL          3
-- 
-- /* BarHomogeneous parameter values */
-- 
-- #define GRB_BARHOMOGENEOUS_AUTO -1
-- #define GRB_BARHOMOGENEOUS_OFF   0
-- #define GRB_BARHOMOGENEOUS_ON    1
-- 
-- /* BarOrder parameter values */
-- 
-- #define GRB_BARORDER_AUTOMATIC       -1
-- #define GRB_BARORDER_AMD              0
-- #define GRB_BARORDER_NESTEDDISSECTION 1
-- 
-- /* MIPFocus parameter values */
-- 
-- #define GRB_MIPFOCUS_BALANCED    0
-- #define GRB_MIPFOCUS_FEASIBILITY 1
-- #define GRB_MIPFOCUS_OPTIMALITY  2
-- #define GRB_MIPFOCUS_BESTBOUND   3
-- 
-- /* SimplexPricing parameter values */
-- 
-- #define GRB_SIMPLEXPRICING_AUTO           -1
-- #define GRB_SIMPLEXPRICING_PARTIAL         0
-- #define GRB_SIMPLEXPRICING_STEEPEST_EDGE   1
-- #define GRB_SIMPLEXPRICING_DEVEX           2
-- #define GRB_SIMPLEXPRICING_STEEPEST_QUICK  3
-- 
-- /* VarBranch parameter values */
-- 
-- #define GRB_VARBRANCH_AUTO          -1
-- #define GRB_VARBRANCH_PSEUDO_REDUCED 0
-- #define GRB_VARBRANCH_PSEUDO_SHADOW  1
-- #define GRB_VARBRANCH_MAX_INFEAS     2
-- #define GRB_VARBRANCH_STRONG         3
-- 
-- /* PartitionPlace parameter values */
-- 
-- #define GRB_PARTITION_EARLY     16
-- #define GRB_PARTITION_ROOTSTART 8
-- #define GRB_PARTITION_ROOTEND   4
-- #define GRB_PARTITION_NODES     2
-- #define GRB_PARTITION_CLEANUP   1
-- 
-- /* Callback phase values */
-- 
-- #define GRB_PHASE_MIP_NOREL   0
-- #define GRB_PHASE_MIP_SEARCH  1
-- #define GRB_PHASE_MIP_IMPROVE 2
-- 
-- int __stdcall
--   GRBcheckmodel(GRBmodel *model);
foreign import stdcall safe "GRBcheckmodel" checkmodel
  :: Model -- ^ model
  -> IO ErrorCode

-- void __stdcall
--   GRBterminate(GRBmodel *model);
foreign import stdcall safe "GRBterminate" terminate
  :: Model -- ^ model
  -> IO ()

-- int __stdcall
--   GRBreplay(const char *filename);
foreign import stdcall safe "GRBreplay" replay
  :: CString -- ^ filename
  -> IO ErrorCode

-- int __stdcall
--   GRBsetobjective(GRBmodel *model, int sense, double constant,
--                   int lnz, int *lind, double *lval,
--                   int qnz, int *qrow, int *qcol, double *qval);
foreign import stdcall unsafe "GRBsetobjective" setobjective
  :: Model -- ^ model
  -> CInt -- ^ sense
  -> CDouble -- ^ constant
  -> CInt -- ^ lnz
  -> Ptr CInt -- ^ lind
  -> Ptr CDouble -- ^ lval
  -> CInt -- ^ qnz
  -> Ptr CInt -- ^ qrow
  -> Ptr CInt -- ^ qcol
  -> Ptr CDouble -- ^ qval
  -> IO ErrorCode

-- int __stdcall
--   GRBsetobjectiven(GRBmodel *model, int index, int priority, double weight,
--                    double abstol, double reltol, const char *name,
--                    double constant, int lnz, int *lind, double *lval);
foreign import stdcall unsafe "GRBsetobjectiven" setobjectiven
  :: Model -- ^ model
  -> CInt -- ^ index
  -> CInt -- ^ priority
  -> CDouble -- ^ weight
  -> CDouble -- ^ abstol
  -> CDouble -- ^ reltol
  -> CString -- ^ name
  -> CDouble -- ^ constant
  -> CInt -- ^ lnz
  -> Ptr CInt -- ^ lind
  -> Ptr CDouble -- ^ lval
  -> IO ErrorCode

-- void __stdcall
--   GRBclean2(int *lenP, int *ind, double *val);
foreign import stdcall safe "GRBclean2" clean2
  :: Ptr CInt -- ^ lenP
  -> Ptr CInt -- ^ ind
  -> Ptr CDouble -- ^ val
  -> IO ()

-- void __stdcall
--   GRBclean3(int *lenP, int *ind0, int *ind1, double *val);
foreign import stdcall safe "GRBclean3" clean3
  :: Ptr CInt -- ^ lenP
  -> Ptr CInt -- ^ ind0
  -> Ptr CInt -- ^ ind1
  -> Ptr CDouble -- ^ val
  -> IO ()

-- int __stdcall
--   GRBprintquality(GRBmodel *model);
foreign import stdcall safe "GRBprintquality" printquality
  :: Model -- ^ model
  -> IO ErrorCode

-- /* Logging */

-- void __stdcall
--   GRBmsg(GRBenv *env, const char *message);
foreign import stdcall safe "GRBmsg" msg
  :: Env -- ^ env
  -> CString -- ^ message
  -> IO ()

-- /* Parameter routines */

-- int __stdcall
--   GRBgetintparam(GRBenv *env, const char *paramname, int *valueP);
foreign import stdcall unsafe "GRBgetintparam" getintparam
  :: Env -- ^ env
  -> CString -- ^ paramname
  -> Ptr CInt -- ^ valueP
  -> IO ErrorCode

-- int __stdcall
--   GRBgetdblparam(GRBenv *env, const char *paramname, double *valueP);
foreign import stdcall unsafe "GRBgetdblparam" getdblparam
  :: Env -- ^ env
  -> CString -- ^ paramname
  -> Ptr CDouble -- ^ valueP
  -> IO ErrorCode

-- int __stdcall
--   GRBgetstrparam(GRBenv *env, const char *paramname, char *valueP);
foreign import stdcall unsafe "GRBgetstrparam" getstrparam
  :: Env -- ^ env
  -> CString -- ^ paramname
  -> Ptr CChar -- ^ valueP
  -> IO ErrorCode

-- int __stdcall
--   GRBgetlongstrparam(GRBenv *env, const char *paramname, char *valueP,
--                      int size, int *requiredlenP);
foreign import stdcall unsafe "GRBgetlongstrparam" getlongstrparam
  :: Env -- ^ env
  -> CString -- ^ paramname
  -> Ptr CChar -- ^ valueP
  -> CInt -- ^ size
  -> Ptr CInt -- ^ requiredlenP
  -> IO ErrorCode

-- int __stdcall
--   GRBgetintparaminfo(GRBenv *env, const char *paramname, int *valueP,
--                      int *minP, int *maxP, int *defP);
foreign import stdcall unsafe "GRBgetintparaminfo" getintparaminfo
  :: Env -- ^ env
  -> CString -- ^ paramname
  -> Ptr CInt -- ^ valueP
  -> Ptr CInt -- ^ minP
  -> Ptr CInt -- ^ maxP
  -> Ptr CInt -- ^ defP
  -> IO ErrorCode

-- int __stdcall
--   GRBgetdblparaminfo(GRBenv *env, const char *paramname, double *valueP,
--                      double *minP, double *maxP, double *defP);
foreign import stdcall unsafe "GRBgetdblparaminfo" getdblparaminfo
  :: Env -- ^ env
  -> CString -- ^ paramname
  -> Ptr CDouble -- ^ valueP
  -> Ptr CDouble -- ^ minP
  -> Ptr CDouble -- ^ maxP
  -> Ptr CDouble -- ^ defP
  -> IO ErrorCode

-- int __stdcall
--   GRBgetstrparaminfo(GRBenv *env, const char *paramname, char *valueP,
--                      char *defP);
foreign import stdcall unsafe "GRBgetstrparaminfo" getstrparaminfo
  :: Env -- ^ env
  -> CString -- ^ paramname
  -> Ptr CChar -- ^ valueP
  -> Ptr CChar -- ^ defP
  -> IO ErrorCode

-- int __stdcall
--   GRBgetparamflags(GRBenv *env, const char *parname, unsigned int *valueP);
foreign import stdcall unsafe "GRBgetparamflags" getparamflags
  :: Env -- ^ env
  -> CString -- ^ parname
  -> Ptr CUInt -- ^ valueP
  -> IO ErrorCode

-- int __stdcall
--   GRBsetparam(GRBenv *env, const char *paramname, const char *value);
foreign import stdcall unsafe "GRBsetparam" setparam
  :: Env -- ^ env
  -> CString -- ^ paramname
  -> CString -- ^ value
  -> IO ErrorCode

-- int __stdcall
--   GRBsetintparam(GRBenv *env, const char *paramname, int value);
foreign import stdcall unsafe "GRBsetintparam" setintparam
  :: Env -- ^ env
  -> CString -- ^ paramname
  -> CInt -- ^ value
  -> IO ErrorCode

-- int __stdcall
--   GRBsetdblparam(GRBenv *env, const char *paramname, double value);
foreign import stdcall unsafe "GRBsetdblparam" setdblparam
  :: Env -- ^ env
  -> CString -- ^ paramname
  -> CDouble -- ^ value
  -> IO ErrorCode

-- int __stdcall
--   GRBsetstrparam(GRBenv *env, const char *paramname, const char *value);
foreign import stdcall unsafe "GRBsetstrparam" setstrparam
  :: Env -- ^ env
  -> CString -- ^ paramname
  -> CString -- ^ value
  -> IO ErrorCode

-- int __stdcall
--   GRBgetparamtype(GRBenv *env, const char *paramname);
foreign import stdcall unsafe "GRBgetparamtype" getparamtype
  :: Env -- ^ env
  -> CString -- ^ paramname
  -> IO ErrorCode

-- int __stdcall
--   GRBresetparams(GRBenv *env);
foreign import stdcall unsafe "GRBresetparams" resetparams
  :: Env -- ^ env
  -> IO ErrorCode

-- int __stdcall
--   GRBcopyparams(GRBenv *dest, GRBenv *src);
foreign import stdcall unsafe "GRBcopyparams" copyparams
  :: Env -- ^ dest
  -> Env -- ^ src
  -> IO ErrorCode

-- int __stdcall
--   GRBwriteparams(GRBenv *env, const char *filename);
foreign import stdcall safe "GRBwriteparams" writeparams
  :: Env -- ^ env
  -> CString -- ^ filename
  -> IO ErrorCode

-- int __stdcall
--   GRBreadparams(GRBenv *env, const char *filename);
foreign import stdcall safe "GRBreadparams" readparams
  :: Env -- ^ env
  -> CString -- ^ filename
  -> IO ErrorCode

-- int __stdcall
--   GRBreadconcurrentsettings(GRBmodel *model, const char *filename);
foreign import stdcall safe "GRBreadconcurrentsettings" readconcurrentsettings
  :: Model -- ^ model
  -> CString -- ^ filename
  -> IO ErrorCode

-- int __stdcall
--   GRBreadmultiobjsettings(GRBmodel *model, const char *filename);
foreign import stdcall safe "GRBreadmultiobjsettings" readmultiobjsettings
  :: Model -- ^ model
  -> CString -- ^ filename
  -> IO ErrorCode

-- int __stdcall
--   GRBreadtunebasesettings(GRBenv *env, const char *filename);
foreign import stdcall safe "GRBreadtunebasesettings" readtunebasesettings
  :: Env -- ^ env
  -> CString -- ^ filename
  -> IO ErrorCode

-- int __stdcall
--   GRBgetnumparams(GRBenv *env);
foreign import stdcall unsafe "GRBgetnumparams" getnumparams
  :: Env -- ^ env
  -> IO ErrorCode

-- int __stdcall
--   GRBgetparamname(GRBenv *env, int parnum, char **paramnameP);
foreign import stdcall unsafe "GRBgetparamname" getparamname
  :: Env -- ^ env
  -> CInt -- ^ parnum
  -> Ptr CString -- ^ paramnameP
  -> IO ErrorCode

-- int __stdcall
--   GRBgetnumattributes(GRBmodel *model);
foreign import stdcall unsafe "GRBgetnumattributes" getnumattributes
  :: Model -- ^ model
  -> IO ErrorCode

-- int __stdcall
--   GRBgetattrname(GRBmodel *model, int i, char **attrnameP);
foreign import stdcall unsafe "GRBgetattrname" getattrname
  :: Model -- ^ model
  -> CInt -- ^ i
  -> Ptr CString -- ^ attrnameP
  -> IO ErrorCode

-- /* Environment routines */

-- #define GRBloadenv(envP, logfilename) GRBloadenvinternal(envP, logfilename, GRB_VERSION_MAJOR, GRB_VERSION_MINOR, GRB_VERSION_TECHNICAL)
loadenv envP logfilename = loadenvinternal envP logfilename
  (#const GRB_VERSION_MAJOR)
  (#const GRB_VERSION_MINOR)
  (#const GRB_VERSION_TECHNICAL)

-- #define GRBemptyenv(envP) GRBemptyenvinternal(envP, GRB_VERSION_MAJOR, GRB_VERSION_MINOR, GRB_VERSION_TECHNICAL)
emptyenv envP = emptyenvinternal envP
  (#const GRB_VERSION_MAJOR)
  (#const GRB_VERSION_MINOR)
  (#const GRB_VERSION_TECHNICAL)

-- int __stdcall
-- GRBloadenvinternal(GRBenv **envP, const char *logfilename, int major, int minor, int tech);
foreign import stdcall safe "GRBloadenvinternal" loadenvinternal
  :: Ptr Env -- ^ envP
  -> CString -- ^ logfilename
  -> CInt -- ^ major
  -> CInt -- ^ minor
  -> CInt -- ^ tech
  -> IO ErrorCode

-- int __stdcall
-- GRBemptyenvinternal(GRBenv **envP, int major, int minor, int tech);
foreign import stdcall safe "GRBemptyenvinternal" emptyenvinternal
  :: Ptr Env -- ^ envP
  -> CInt -- ^ major
  -> CInt -- ^ minor
  -> CInt -- ^ tech
  -> IO ErrorCode

-- int __stdcall
--   GRBstartenv(GRBenv *env);
foreign import stdcall safe "GRBstartenv" startenv
  :: Env -- ^ env
  -> IO ErrorCode

-- int __stdcall
--   GRBloadenvadv(GRBenv **envP, const char *logfilename,
--                 int apitype, int major, int minor, int tech,
--                 const char *server, const char *router,
--                 const char *password, const char *group,
--                 int priority, int idletimeout,
--                 const char *cloudaccessid, const char *cloudsecretkey,
--                 int (__stdcall *cb)(CB_ARGS), void *usrdata,
--                 int (__stdcall *logcb)(LOGCB_ARGS), void *logdata);
foreign import stdcall safe "GRBloadenvadv" loadenvadv
  :: Ptr Env -- ^ envP
  -> CString -- ^ logfilename
  -> CInt -- ^ apitype
  -> CInt -- ^ major
  -> CInt -- ^ minor
  -> CInt -- ^ tech
  -> CString -- ^ server
  -> CString -- ^ router
  -> CString -- ^ password
  -> CString -- ^ group
  -> CInt -- ^ priority
  -> CInt -- ^ idletimeout
  -> CString -- ^ cloudaccessid
  -> CString -- ^ cloudsecretkey
  -> FunPtr (CB a) -- ^ cb
  -> Ptr a -- ^ usrdata
  -> FunPtr (LogCB b) -- ^ logcb
  -> Ptr b -- ^ logdata
  -> IO ErrorCode

-- GRBenv *__stdcall
--   GRBgetenv(GRBmodel *model);
foreign import stdcall unsafe "GRBgetenv" getenv
  :: Model -- ^ model
  -> IO Env

-- GRBenv *__stdcall
--   GRBgetconcurrentenv(GRBmodel *model, int num);
foreign import stdcall unsafe "GRBgetconcurrentenv" getconcurrentenv
  :: Model -- ^ model
  -> CInt -- ^ num
  -> IO Env

-- void __stdcall
--   GRBdiscardconcurrentenvs(GRBmodel *model);
foreign import stdcall unsafe "GRBdiscardconcurrentenvs" discardconcurrentenvs
  :: Model -- ^ model
  -> IO ()

-- GRBenv *__stdcall
--   GRBgetmultiobjenv(GRBmodel *model, int num);
foreign import stdcall unsafe "GRBgetmultiobjenv" getmultiobjenv
  :: Model -- ^ model
  -> CInt -- ^ num
  -> IO Env

-- void __stdcall
--   GRBdiscardmultiobjenvs(GRBmodel *model);
foreign import stdcall unsafe "GRBdiscardmultiobjenvs" discardmultiobjenvs
  :: Model -- ^ model
  -> IO ()

-- GRBenv *__stdcall
--   GRBgettuneenv(GRBenv *env, int num);
foreign import stdcall unsafe "GRBgettuneenv" gettuneenv
  :: Env -- ^ env
  -> CInt -- ^ num
  -> IO Env

-- void __stdcall
--   GRBdiscardtuneenvs(GRBenv *env);
foreign import stdcall unsafe "GRBdiscardtuneenvs" discardtuneenvs
  :: Env -- ^ env
  -> IO ()

-- void __stdcall
--   GRBreleaselicense(GRBenv *env);
foreign import stdcall unsafe "GRBreleaselicense" releaselicense
  :: Env -- ^ env
  -> IO ()

-- void __stdcall
--   GRBfreeenv(GRBenv *env);
foreign import stdcall unsafe "GRBfreeenv" freeenv
  :: Env -- ^ env
  -> IO ()

-- const char * __stdcall
--   GRBgeterrormsg(GRBenv *env);
foreign import stdcall unsafe "GRBgeterrormsg" geterrormsg
  :: Env -- ^ env
  -> IO CString

-- const char * __stdcall
--   GRBgetmerrormsg(GRBmodel *model);
foreign import stdcall unsafe "GRBgetmerrormsg" getmerrormsg
  :: Model -- ^ model
  -> IO CString

-- void __stdcall
--   GRBgetcommstats(GRBenv *env, double *recvtimeP, double *recvbytesP,
--                   double *recvmsgsP, double *sendtimeP,
--                   double *sendbytesP, double *sendmsgsP);
foreign import stdcall unsafe "GRBgetcommstats" getcommstats
  :: Env -- ^ env
  -> Ptr CDouble -- ^ recvtimeP
  -> Ptr CDouble -- ^ recvbytesP
  -> Ptr CDouble -- ^ recvmsgsP
  -> Ptr CDouble -- ^ sendtimeP
  -> Ptr CDouble -- ^ sendbytesP
  -> Ptr CDouble -- ^ sendmsgsP
  -> IO ()

-- /* Version info */

-- void __stdcall
--   GRBversion(int *majorP, int *minorP, int *technicalP);
foreign import stdcall unsafe "GRBversion" version
  :: Ptr CInt -- ^ majorP
  -> Ptr CInt -- ^ minorP
  -> Ptr CInt -- ^ technicalP
  -> IO ()

-- void __stdcall
--   GRBgetdistro(char *str);
foreign import stdcall unsafe "GRBgetdistro" getdistro
  :: Ptr CChar -- ^ str
  -> IO ()

-- char * __stdcall
--   GRBplatform(void);
foreign import stdcall unsafe "GRBplatform" platform
  :: IO CString

-- char * __stdcall
--   GRBplatformext(void);
foreign import stdcall unsafe "GRBplatformext" platformext
  :: IO CString

-- int __stdcall
--   GRBlisttokens(void);
foreign import stdcall unsafe "GRBlisttokens" listtokens
  :: IO CInt

-- int __stdcall
--   GRBgetwlstokenlifespan(GRBenv *env, int *lifespanP);
foreign import stdcall safe "GRBgetwlstokenlifespan" getwlstokenlifespan
  :: Env -- ^ env
  -> Ptr CInt -- ^ lifespanP
  -> IO ErrorCode

-- /* Used in Matlab API */
-- void __stdcall
--   GRBsortIDi(int len, int *ind, double *val);
foreign import stdcall "GRBsortIDi" sortIDi
  :: CInt -- ^ len
  -> Ptr CInt -- ^ ind
  -> Ptr CDouble -- ^ val
  -> IO ()

-- /* batch-related routines */
-- int __stdcall
--   GRBabortbatch(GRBbatch *batch);
foreign import stdcall safe "GRBabortbatch" abortbatch
  :: Batch -- ^ batch
  -> IO ErrorCode

-- int __stdcall
--   GRBdiscardbatch(GRBbatch *batch);
foreign import stdcall safe "GRBdiscardbatch" discardbatch
  :: Batch -- ^ batch
  -> IO ErrorCode

-- int __stdcall
--   GRBretrybatch(GRBbatch *batch);
foreign import stdcall safe "GRBretrybatch" retrybatch
  :: Batch -- ^ batch
  -> IO ErrorCode

-- int __stdcall
--   GRBfreebatch(GRBbatch *batch);
foreign import stdcall safe "GRBfreebatch" freebatch
  :: Batch -- ^ batch
  -> IO ErrorCode

-- int __stdcall
--   GRBgetbatch(GRBenv *env, const char *batchID, GRBbatch **batchP);
foreign import stdcall safe "GRBgetbatch" getbatch
  :: Env -- ^ env
  -> CString -- ^ batchID
  -> Ptr Batch -- ^ batchP
  -> IO ErrorCode

-- int __stdcall
--   GRBgetbatchjsonsolution(GRBbatch *batch, char **jsonsolP);
foreign import stdcall safe "GRBgetbatchjsonsolution" getbatchjsonsolution
  :: Batch -- ^ batch
  -> Ptr CString -- ^ jsonsolP
  -> IO ErrorCode

-- int __stdcall
--   GRBgetbatchintattr(GRBbatch *batch, const char *attrname, int *valueP);
foreign import stdcall unsafe "GRBgetbatchintattr" getbatchintattr
  :: Batch -- ^ batch
  -> CString -- ^ attrname
  -> Ptr CInt -- ^ valueP
  -> IO ErrorCode

-- int __stdcall
--   GRBgetbatchstrattr(GRBbatch *batch, const char *attrname, char **valueP);
foreign import stdcall unsafe "GRBgetbatchstrattr" getbatchstrattr
  :: Batch -- ^ batch
  -> CString -- ^ attrname
  -> Ptr CString -- ^ valueP
  -> IO ErrorCode

-- int __stdcall
--   GRBgetbatchattrname(GRBenv *env, int n, char **attrnameP);
foreign import stdcall unsafe "GRBgetbatchattrname" getbatchattrname
  :: Env -- ^ env
  -> CInt -- ^ n
  -> Ptr CString -- ^ attrnameP
  -> IO ErrorCode

-- int __stdcall
--   GRBgetbatchattrflags(GRBbatch *batch, const char *attrname, unsigned *flagsP);
foreign import stdcall unsafe "GRBgetbatchattrflags" getbatchattrflags
  :: Batch -- ^ batch
  -> CString -- ^ attrname
  -> Ptr CUInt -- ^ flagsP
  -> IO ErrorCode

-- int __stdcall GRBgetbatchattrinfo(GRBbatch *batch, const char *attrname, int *datatypeP, int *settableP);
foreign import stdcall unsafe "GRBgetbatchattrinfo" getbatchattrinfo
  :: Batch -- ^ batch
  -> CString -- ^ attrname
  -> Ptr CInt -- ^ datatypeP
  -> Ptr CInt -- ^ settableP
  -> IO ErrorCode

-- int __stdcall
--   GRBupdatebatch(GRBbatch *batch);
foreign import stdcall safe "GRBupdatebatch" updatebatch
  :: Batch -- ^ batch
  -> IO ErrorCode

-- int __stdcall
--   GRBwritebatchjsonsolution(GRBbatch *batch, const char *filename);
foreign import stdcall safe "GRBwritebatchjsonsolution" writebatchjsonsolution
  :: Batch -- ^ batch
  -> CString -- ^ filename
  -> IO ErrorCode

-- int __stdcall
--   GRBgetnumbatchattributes(GRBenv *env);
foreign import stdcall "GRBgetnumbatchattributes" getnumbatchattributes
  :: Env -- ^ env
  -> IO ErrorCode

-- GRBenv *__stdcall
--   GRBgetbatchenv(GRBbatch *batch);
foreign import stdcall unsafe "GRBgetbatchenv" getbatchenv
  :: Batch -- ^ batch
  -> IO Env

-- /* dummy wrapper for free function */
-- void __stdcall
--   GRBfree(void *ptr);
foreign import stdcall unsafe "GRBfree" free
  :: Ptr a -- ^ ptr
  -> IO ()

-- /* Batch object status codes */
-- 
-- #define GRB_BATCH_STATUS_UNKNOWN 0
-- #define GRB_BATCH_CREATED        1
-- #define GRB_BATCH_SUBMITTED      2
-- #define GRB_BATCH_ABORTED        3
-- #define GRB_BATCH_FAILED         4
-- #define GRB_BATCH_COMPLETED      5


-- /* Async interface */

-- int __stdcall
--   GRBsync(GRBmodel *model);
foreign import stdcall safe "GRBsync" sync
  :: Model -- ^ model
  -> IO ErrorCode

-- int __stdcall
--   GRBpingserver(const char *server, const char *password);
foreign import stdcall safe "GRBpingserver" pingserver
  :: CString -- ^ server
  -> CString -- ^ password
  -> IO ErrorCode

-- /* pre-fetching attributes from Compute Server */

-- int __stdcall
--   GRBprefetchattr(GRBmodel *model, const char *attrname);
foreign import stdcall safe "GRBprefetchattr" prefetchattr
  :: Model -- ^ model
  -> CString -- ^ attrname
  -> IO ErrorCode

-- /* Tuning */

-- int __stdcall
--   GRBtunemodel(GRBmodel *model);
foreign import stdcall safe "GRBtunemodel" tunemodel
  :: Model -- ^ model
  -> IO ErrorCode

-- int __stdcall
--   GRBtunemodels(GRBenv *env, int nummodels, GRBmodel **models);
foreign import stdcall safe "GRBtunemodels" tunemodels
  :: Env -- ^ env
  -> CInt -- ^ nummodels
  -> Ptr (Ptr Model) -- ^ models
  -> IO ErrorCode

-- int __stdcall
--   GRBgettuneresult(GRBmodel *model, int i);
foreign import stdcall "safe GRBgettuneresult" gettuneresult
  :: Model -- ^ model
  -> CInt -- ^ i
  -> IO ErrorCode

-- int __stdcall
--   GRBgettunelog(GRBmodel *model, int i, char **logP);
foreign import stdcall safe "GRBgettunelog" gettunelog
  :: Model -- ^ model
  -> CInt -- ^ i
  -> Ptr CString -- ^ logP
  -> IO ErrorCode

-- int __stdcall
--   GRBwritetunelog(GRBmodel *model, int result, const char *filename);
foreign import stdcall safe "GRBwritetunelog" writetunelog
  :: Model -- ^ model
  -> CInt -- ^ result
  -> CString -- ^ filename
  -> IO ErrorCode

-- void __stdcall
--   GRBtuneparamsPrint(void);
foreign import stdcall safe "GRBtuneparamsPrint" tuneparamsPrint
  :: IO ()
