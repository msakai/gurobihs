{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Numeric.Gurobi.C where

#include <gurobi_c.h>

#define hsc_const_cstr(x...)                                      \
    {                                                             \
        const char *s = (x);                                      \
        hsc_printf ("(Ptr \"");                                   \
        while (*s != '\0')                                        \
        {                                                         \
            if (*s == '"' || *s == '\\')                          \
                hsc_printf ("\\%c", *s);                          \
            else if (*s >= 0x20 && *s <= 0x7E)                    \
                hsc_printf ("%c", *s);                            \
            else                                                  \
                hsc_printf ("\\%d%s",                             \
                        (unsigned char) *s,                       \
                        s[1] >= '0' && s[1] <= '9' ? "\\&" : ""); \
            ++s;                                                  \
        }                                                         \
        hsc_printf ("\"#)");                                      \
    }

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

c_MIN_ERROR
  , eRROR_OUT_OF_MEMORY
  , eRROR_NULL_ARGUMENT
  , eRROR_INVALID_ARGUMENT
  , eRROR_UNKNOWN_ATTRIBUTE
  , eRROR_DATA_NOT_AVAILABLE
  , eRROR_INDEX_OUT_OF_RANGE
  , eRROR_UNKNOWN_PARAMETER
  , eRROR_VALUE_OUT_OF_RANGE
  , eRROR_NO_LICENSE
  , eRROR_SIZE_LIMIT_EXCEEDED
  , eRROR_CALLBACK
  , eRROR_FILE_READ
  , eRROR_FILE_WRITE
  , eRROR_NUMERIC
  , eRROR_IIS_NOT_INFEASIBLE
  , eRROR_NOT_FOR_MIP
  , eRROR_OPTIMIZATION_IN_PROGRESS
  , eRROR_DUPLICATES
  , eRROR_NODEFILE
  , eRROR_Q_NOT_PSD
  , eRROR_QCP_EQUALITY_CONSTRAINT
  , eRROR_NETWORK
  , eRROR_JOB_REJECTED
  , eRROR_NOT_SUPPORTED
  , eRROR_EXCEED_2B_NONZEROS
  , eRROR_INVALID_PIECEWISE_OBJ
  , eRROR_UPDATEMODE_CHANGE
  , eRROR_CLOUD
  , eRROR_MODEL_MODIFICATION
  , eRROR_CSWORKER
  , eRROR_TUNE_MODEL_TYPES
  , eRROR_SECURITY
  , c_MAX_ERROR
  :: ErrorCode

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

lESS_EQUAL, gREATER_EUQAL, eQUAL :: CChar
lESS_EQUAL    = #const GRB_LESS_EQUAL
gREATER_EUQAL = #const GRB_GREATER_EQUAL
eQUAL         = #const GRB_EQUAL

-- /* Variable types */

cONTINUOUS, bInary, iNTEGER, sEMICONT, sEMIINT :: CChar
cONTINUOUS = #const GRB_CONTINUOUS
bInary     = #const GRB_BINARY
iNTEGER    = #const GRB_INTEGER
sEMICONT   = #const GRB_SEMICONT
sEMIINT    = #const GRB_SEMIINT

-- /* Objective sense */

mINIMIZE :: CInt
mINIMIZE = #const GRB_MINIMIZE

mAXIMIZE :: CInt
mAXIMIZE = #const GRB_MAXIMIZE

-- /* SOS types */

sOS_TYPE1 :: CInt
sOS_TYPE1 = #const GRB_SOS_TYPE1

sOS_TYPE2 :: CInt
sOS_TYPE2 = #const GRB_SOS_TYPE2

-- /* Numeric constants */

iNFINITY :: CInt
iNFINITY = #const GRB_INFINITY

uNDEFINED :: CInt
uNDEFINED = #const GRB_UNDEFINED

mAXINT :: CInt
mAXINT = #const GRB_MAXINT

-- /* Limits */

mAX_NAMELEN :: CInt
mAX_NAMELEN = #const GRB_MAX_NAMELEN

mAX_STRLEN :: CInt
mAX_STRLEN = #const GRB_MAX_STRLEN

mAX_TAGLEN :: CInt
mAX_TAGLEN = #const GRB_MAX_TAGLEN

mAX_CONCURRENT :: CInt
mAX_CONCURRENT = #const GRB_MAX_CONCURRENT

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

-- | # of constraints
iNT_ATTR_NUMCONSTRS :: String
iNT_ATTR_NUMCONSTRS = #const_str GRB_INT_ATTR_NUMCONSTRS

-- | # of constraints
iNT_ATTR_NUMCONSTRS_PTR :: CString
iNT_ATTR_NUMCONSTRS_PTR = #const_cstr GRB_INT_ATTR_NUMCONSTRS

-- | # of vars
iNT_ATTR_NUMVARS :: String
iNT_ATTR_NUMVARS = #const_str GRB_INT_ATTR_NUMVARS

-- | # of vars
iNT_ATTR_NUMVARS_PTR :: CString
iNT_ATTR_NUMVARS_PTR = #const_cstr GRB_INT_ATTR_NUMVARS

-- | # of sos constraints
iNT_ATTR_NUMSOS :: String
iNT_ATTR_NUMSOS = #const_str GRB_INT_ATTR_NUMSOS

-- | # of sos constraints
iNT_ATTR_NUMSOS_PTR :: CString
iNT_ATTR_NUMSOS_PTR = #const_cstr GRB_INT_ATTR_NUMSOS

-- | # of quadratic constraints
iNT_ATTR_NUMQCONSTRS :: String
iNT_ATTR_NUMQCONSTRS = #const_str GRB_INT_ATTR_NUMQCONSTRS

-- | # of quadratic constraints
iNT_ATTR_NUMQCONSTRS_PTR :: CString
iNT_ATTR_NUMQCONSTRS_PTR = #const_cstr GRB_INT_ATTR_NUMQCONSTRS

-- | # of nz in A
iNT_ATTR_NUMNZS :: String
iNT_ATTR_NUMNZS = #const_str GRB_INT_ATTR_NUMNZS

-- | # of nz in A
iNT_ATTR_NUMNZS_PTR :: CString
iNT_ATTR_NUMNZS_PTR = #const_cstr GRB_INT_ATTR_NUMNZS

-- | # of nz in A
dBL_ATTR_DNUMNZS :: String
dBL_ATTR_DNUMNZS = #const_str GRB_DBL_ATTR_DNUMNZS

-- | # of nz in A
dBL_ATTR_DNUMNZS_PTR :: CString
dBL_ATTR_DNUMNZS_PTR = #const_cstr GRB_DBL_ATTR_DNUMNZS

-- | # of nz in Q
iNT_ATTR_NUMQNZS :: String
iNT_ATTR_NUMQNZS = #const_str GRB_INT_ATTR_NUMQNZS

-- | # of nz in Q
iNT_ATTR_NUMQNZS_PTR :: CString
iNT_ATTR_NUMQNZS_PTR = #const_cstr GRB_INT_ATTR_NUMQNZS

-- | # of nz in q constraints
iNT_ATTR_NUMQCNZS :: String
iNT_ATTR_NUMQCNZS = #const_str GRB_INT_ATTR_NUMQCNZS

-- | # of nz in q constraints
iNT_ATTR_NUMQCNZS_PTR :: CString
iNT_ATTR_NUMQCNZS_PTR = #const_cstr GRB_INT_ATTR_NUMQCNZS

-- | # of integer vars
iNT_ATTR_NUMINTVARS :: String
iNT_ATTR_NUMINTVARS = #const_str GRB_INT_ATTR_NUMINTVARS

-- | # of integer vars
iNT_ATTR_NUMINTVARS_PTR :: CString
iNT_ATTR_NUMINTVARS_PTR = #const_cstr GRB_INT_ATTR_NUMINTVARS

-- | # of binary vars
iNT_ATTR_NUMBINVARS :: String
iNT_ATTR_NUMBINVARS = #const_str GRB_INT_ATTR_NUMBINVARS

-- | # of binary vars
iNT_ATTR_NUMBINVARS_PTR :: CString
iNT_ATTR_NUMBINVARS_PTR = #const_cstr GRB_INT_ATTR_NUMBINVARS

-- | model name
sTR_ATTR_MODELNAME :: String
sTR_ATTR_MODELNAME = #const_str GRB_STR_ATTR_MODELNAME

-- | model name
sTR_ATTR_MODELNAME_PTR :: CString
sTR_ATTR_MODELNAME_PTR = #const_cstr GRB_STR_ATTR_MODELNAME

-- | 1=min, -1=max
iNT_ATTR_MODELSENSE :: String
iNT_ATTR_MODELSENSE = #const_str GRB_INT_ATTR_MODELSENSE

-- | 1=min, -1=max
iNT_ATTR_MODELSENSE_PTR :: CString
iNT_ATTR_MODELSENSE_PTR = #const_cstr GRB_INT_ATTR_MODELSENSE

-- | Objective constant
dBL_ATTR_OBJCON :: String
dBL_ATTR_OBJCON = #const_str GRB_DBL_ATTR_OBJCON

-- | Objective constant
dBL_ATTR_OBJCON_PTR :: CString
dBL_ATTR_OBJCON_PTR = #const_cstr GRB_DBL_ATTR_OBJCON

-- | Is model a MIP?
iNT_ATTR_IS_MIP :: String
iNT_ATTR_IS_MIP = #const_str GRB_INT_ATTR_IS_MIP

-- | Is model a MIP?
iNT_ATTR_IS_MIP_PTR :: CString
iNT_ATTR_IS_MIP_PTR = #const_cstr GRB_INT_ATTR_IS_MIP

-- | Is model a QP/MIQP (without Q/NL constraints)?
iNT_ATTR_IS_QP :: String
iNT_ATTR_IS_QP = #const_str GRB_INT_ATTR_IS_QP

-- | Is model a QP/MIQP (without Q/NL constraints)?
iNT_ATTR_IS_QP_PTR :: CString
iNT_ATTR_IS_QP_PTR = #const_cstr GRB_INT_ATTR_IS_QP

-- | Model has quadratic constr?
iNT_ATTR_IS_QCP :: String
iNT_ATTR_IS_QCP = #const_str GRB_INT_ATTR_IS_QCP

-- | Model has quadratic constr?
iNT_ATTR_IS_QCP_PTR :: CString
iNT_ATTR_IS_QCP_PTR = #const_cstr GRB_INT_ATTR_IS_QCP

-- | Model has multiple objectives?
iNT_ATTR_IS_MULTIOBJ :: String
iNT_ATTR_IS_MULTIOBJ = #const_str GRB_INT_ATTR_IS_MULTIOBJ

-- | Model has multiple objectives?
iNT_ATTR_IS_MULTIOBJ_PTR :: CString
iNT_ATTR_IS_MULTIOBJ_PTR = #const_cstr GRB_INT_ATTR_IS_MULTIOBJ

-- | number of tagged elements in model
iNT_ATTR_NUMTAGGED :: String
iNT_ATTR_NUMTAGGED = #const_str GRB_INT_ATTR_NUMTAGGED

-- | number of tagged elements in model
iNT_ATTR_NUMTAGGED_PTR :: CString
iNT_ATTR_NUMTAGGED_PTR = #const_cstr GRB_INT_ATTR_NUMTAGGED

-- | fingerprint computed from the model data and attributes influencing the optimization process
iNT_ATTR_FINGERPRINT :: String
iNT_ATTR_FINGERPRINT = #const_str GRB_INT_ATTR_FINGERPRINT

-- | fingerprint computed from the model data and attributes influencing the optimization process
iNT_ATTR_FINGERPRINT_PTR :: CString
iNT_ATTR_FINGERPRINT_PTR = #const_cstr GRB_INT_ATTR_FINGERPRINT

-- /* Batch attributes */

iNT_ATTR_BATCHERRORCODE :: String
iNT_ATTR_BATCHERRORCODE = #const_str GRB_INT_ATTR_BATCHERRORCODE

iNT_ATTR_BATCHERRORCODE_PTR :: CString
iNT_ATTR_BATCHERRORCODE_PTR = #const_cstr GRB_INT_ATTR_BATCHERRORCODE

sTR_ATTR_BATCHID :: String
sTR_ATTR_BATCHID = #const_str GRB_STR_ATTR_BATCHID

sTR_ATTR_BATCHID_PTR :: CString
sTR_ATTR_BATCHID_PTR = #const_cstr GRB_STR_ATTR_BATCHID

iNT_ATTR_BATCHSTATUS :: String
iNT_ATTR_BATCHSTATUS = #const_str GRB_INT_ATTR_BATCHSTATUS

iNT_ATTR_BATCHSTATUS_PTR :: CString
iNT_ATTR_BATCHSTATUS_PTR = #const_cstr GRB_INT_ATTR_BATCHSTATUS

-- /* Variable attributes */

-- | Lower bound
dBL_ATTR_LB :: String
dBL_ATTR_LB = #const_str GRB_DBL_ATTR_LB

-- | Lower bound
dBL_ATTR_LB_PTR :: CString
dBL_ATTR_LB_PTR = #const_cstr GRB_DBL_ATTR_LB

-- | Upper bound
dBL_ATTR_UB :: String
dBL_ATTR_UB = #const_str GRB_DBL_ATTR_UB

-- | Upper bound
dBL_ATTR_UB_PTR :: CString
dBL_ATTR_UB_PTR = #const_cstr GRB_DBL_ATTR_UB

-- | Objective coeff
dBL_ATTR_OBJ :: String
dBL_ATTR_OBJ = #const_str GRB_DBL_ATTR_OBJ

-- | Objective coeff
dBL_ATTR_OBJ_PTR :: CString
dBL_ATTR_OBJ_PTR = #const_cstr GRB_DBL_ATTR_OBJ

-- | Integrality type
cHAR_ATTR_VTYPE :: String
cHAR_ATTR_VTYPE = #const_str GRB_CHAR_ATTR_VTYPE

-- | Integrality type
cHAR_ATTR_VTYPE_PTR :: CString
cHAR_ATTR_VTYPE_PTR = #const_cstr GRB_CHAR_ATTR_VTYPE

-- | MIP start value, depends on startnumber
dBL_ATTR_START :: String
dBL_ATTR_START = #const_str GRB_DBL_ATTR_START

-- | MIP start value, depends on startnumber
dBL_ATTR_START_PTR :: CString
dBL_ATTR_START_PTR = #const_cstr GRB_DBL_ATTR_START

-- | LP primal solution warm start
dBL_ATTR_PSTART :: String
dBL_ATTR_PSTART = #const_str GRB_DBL_ATTR_PSTART

-- | LP primal solution warm start
dBL_ATTR_PSTART_PTR :: CString
dBL_ATTR_PSTART_PTR = #const_cstr GRB_DBL_ATTR_PSTART

-- | Variable name
sTR_ATTR_VARNAME :: String
sTR_ATTR_VARNAME = #const_str GRB_STR_ATTR_VARNAME

-- | Variable name
sTR_ATTR_VARNAME_PTR :: CString
sTR_ATTR_VARNAME_PTR = #const_cstr GRB_STR_ATTR_VARNAME

-- | Convexity of variable PWL obj
iNT_ATTR_PWLOBJCVX :: String
iNT_ATTR_PWLOBJCVX = #const_str GRB_INT_ATTR_PWLOBJCVX

-- | Convexity of variable PWL obj
iNT_ATTR_PWLOBJCVX_PTR :: CString
iNT_ATTR_PWLOBJCVX_PTR = #const_cstr GRB_INT_ATTR_PWLOBJCVX

-- | variable hint value
dBL_ATTR_VARHINTVAL :: String
dBL_ATTR_VARHINTVAL = #const_str GRB_DBL_ATTR_VARHINTVAL

-- | variable hint value
dBL_ATTR_VARHINTVAL_PTR :: CString
dBL_ATTR_VARHINTVAL_PTR = #const_cstr GRB_DBL_ATTR_VARHINTVAL

-- | variable hint priority
iNT_ATTR_VARHINTPRI :: String
iNT_ATTR_VARHINTPRI = #const_str GRB_INT_ATTR_VARHINTPRI

-- | variable hint priority
iNT_ATTR_VARHINTPRI_PTR :: CString
iNT_ATTR_VARHINTPRI_PTR = #const_cstr GRB_INT_ATTR_VARHINTPRI

-- | user specified variable partition
iNT_ATTR_PARTITION :: String
iNT_ATTR_PARTITION = #const_str GRB_INT_ATTR_PARTITION

-- | user specified variable partition
iNT_ATTR_PARTITION_PTR :: CString
iNT_ATTR_PARTITION_PTR = #const_cstr GRB_INT_ATTR_PARTITION

-- | Ignore variable for solution identity check in solution pool
iNT_ATTR_POOLIGNORE :: String
iNT_ATTR_POOLIGNORE = #const_str GRB_INT_ATTR_POOLIGNORE

-- | Ignore variable for solution identity check in solution pool
iNT_ATTR_POOLIGNORE_PTR :: CString
iNT_ATTR_POOLIGNORE_PTR = #const_cstr GRB_INT_ATTR_POOLIGNORE

-- | variable tags
sTR_ATTR_VTAG :: String
sTR_ATTR_VTAG = #const_str GRB_STR_ATTR_VTAG

-- | variable tags
sTR_ATTR_VTAG_PTR :: CString
sTR_ATTR_VTAG_PTR = #const_cstr GRB_STR_ATTR_VTAG

-- /* Constraint attributes */

-- | linear constraint tags
sTR_ATTR_CTAG :: String
sTR_ATTR_CTAG = #const_str GRB_STR_ATTR_CTAG

-- | linear constraint tags
sTR_ATTR_CTAG_PTR :: CString
sTR_ATTR_CTAG_PTR = #const_cstr GRB_STR_ATTR_CTAG

-- | RHS
dBL_ATTR_RHS :: String
dBL_ATTR_RHS = #const_str GRB_DBL_ATTR_RHS

-- | RHS
dBL_ATTR_RHS_PTR :: CString
dBL_ATTR_RHS_PTR = #const_cstr GRB_DBL_ATTR_RHS

-- | LP dual solution warm start
dBL_ATTR_DSTART :: String
dBL_ATTR_DSTART = #const_str GRB_DBL_ATTR_DSTART

-- | LP dual solution warm start
dBL_ATTR_DSTART_PTR :: CString
dBL_ATTR_DSTART_PTR = #const_cstr GRB_DBL_ATTR_DSTART

-- | Sense ('<', '>', or '=')
cHAR_ATTR_SENSE :: String
cHAR_ATTR_SENSE = #const_str GRB_CHAR_ATTR_SENSE

-- | Sense ('<', '>', or '=')
cHAR_ATTR_SENSE_PTR :: CString
cHAR_ATTR_SENSE_PTR = #const_cstr GRB_CHAR_ATTR_SENSE

-- | Lazy constraint?
iNT_ATTR_LAZY :: String
iNT_ATTR_LAZY = #const_str GRB_INT_ATTR_LAZY

-- | Lazy constraint?
iNT_ATTR_LAZY_PTR :: CString
iNT_ATTR_LAZY_PTR = #const_cstr GRB_INT_ATTR_LAZY

-- /* Quadratic constraint attributes */

-- | quadratic constraint tags
sTR_ATTR_QCTAG :: String
sTR_ATTR_QCTAG = #const_str GRB_STR_ATTR_QCTAG

-- | quadratic constraint tags
sTR_ATTR_QCTAG_PTR :: CString
sTR_ATTR_QCTAG_PTR = #const_cstr GRB_STR_ATTR_QCTAG

-- | QC RHS
dBL_ATTR_QCRHS :: String
dBL_ATTR_QCRHS = #const_str GRB_DBL_ATTR_QCRHS

-- | QC RHS
dBL_ATTR_QCRHS_PTR :: CString
dBL_ATTR_QCRHS_PTR = #const_cstr GRB_DBL_ATTR_QCRHS

-- | QC name
sTR_ATTR_QCNAME :: String
sTR_ATTR_QCNAME = #const_str GRB_STR_ATTR_QCNAME

-- | QC name
sTR_ATTR_QCNAME_PTR :: CString
sTR_ATTR_QCNAME_PTR = #const_cstr GRB_STR_ATTR_QCNAME

-- /* General constraint attributes */

-- | Type of general constraint
iNT_ATTR_GENCONSTRTYPE :: String
iNT_ATTR_GENCONSTRTYPE = #const_str GRB_INT_ATTR_GENCONSTRTYPE

-- | Type of general constraint
iNT_ATTR_GENCONSTRTYPE_PTR :: CString
iNT_ATTR_GENCONSTRTYPE_PTR = #const_cstr GRB_INT_ATTR_GENCONSTRTYPE

-- | Name of general constraint
sTR_ATTR_GENCONSTRNAME :: String
sTR_ATTR_GENCONSTRNAME = #const_str GRB_STR_ATTR_GENCONSTRNAME

-- | Name of general constraint
sTR_ATTR_GENCONSTRNAME_PTR :: CString
sTR_ATTR_GENCONSTRNAME_PTR = #const_cstr GRB_STR_ATTR_GENCONSTRNAME

-- /* General function constraint attributes */

-- | An option for PWL translation
iNT_ATTR_FUNCPIECES :: String
iNT_ATTR_FUNCPIECES = #const_str GRB_INT_ATTR_FUNCPIECES

-- | An option for PWL translation
iNT_ATTR_FUNCPIECES_PTR :: CString
iNT_ATTR_FUNCPIECES_PTR = #const_cstr GRB_INT_ATTR_FUNCPIECES

-- | An option for PWL translation
dBL_ATTR_FUNCPIECEERROR :: String
dBL_ATTR_FUNCPIECEERROR = #const_str GRB_DBL_ATTR_FUNCPIECEERROR

-- | An option for PWL translation
dBL_ATTR_FUNCPIECEERROR_PTR :: CString
dBL_ATTR_FUNCPIECEERROR_PTR = #const_cstr GRB_DBL_ATTR_FUNCPIECEERROR

-- | An option for PWL translation
dBL_ATTR_FUNCPIECERATIO :: String
dBL_ATTR_FUNCPIECERATIO = #const_str GRB_DBL_ATTR_FUNCPIECERATIO

-- | An option for PWL translation
dBL_ATTR_FUNCPIECERATIO_PTR :: CString
dBL_ATTR_FUNCPIECERATIO_PTR = #const_cstr GRB_DBL_ATTR_FUNCPIECERATIO

-- | An option for PWL translation
iNT_ATTR_FUNCNONLINEAR :: String
iNT_ATTR_FUNCNONLINEAR = #const_str GRB_INT_ATTR_FUNCNONLINEAR

-- | An option for PWL translation
iNT_ATTR_FUNCNONLINEAR_PTR :: CString
iNT_ATTR_FUNCNONLINEAR_PTR = #const_cstr GRB_INT_ATTR_FUNCNONLINEAR

-- /* Model statistics */

-- | Max (abs) nz coeff in A
dBL_ATTR_MAX_COEFF :: String
dBL_ATTR_MAX_COEFF = #const_str GRB_DBL_ATTR_MAX_COEFF

-- | Max (abs) nz coeff in A
dBL_ATTR_MAX_COEFF_PTR :: CString
dBL_ATTR_MAX_COEFF_PTR = #const_cstr GRB_DBL_ATTR_MAX_COEFF

-- | Min (abs) nz coeff in A
dBL_ATTR_MIN_COEFF :: String
dBL_ATTR_MIN_COEFF = #const_str GRB_DBL_ATTR_MIN_COEFF

-- | Min (abs) nz coeff in A
dBL_ATTR_MIN_COEFF_PTR :: CString
dBL_ATTR_MIN_COEFF_PTR = #const_cstr GRB_DBL_ATTR_MIN_COEFF

-- | Max (abs) finite var bd
dBL_ATTR_MAX_BOUND :: String
dBL_ATTR_MAX_BOUND = #const_str GRB_DBL_ATTR_MAX_BOUND

-- | Max (abs) finite var bd
dBL_ATTR_MAX_BOUND_PTR :: CString
dBL_ATTR_MAX_BOUND_PTR = #const_cstr GRB_DBL_ATTR_MAX_BOUND

-- | Min (abs) var bd
dBL_ATTR_MIN_BOUND :: String
dBL_ATTR_MIN_BOUND = #const_str GRB_DBL_ATTR_MIN_BOUND

-- | Min (abs) var bd
dBL_ATTR_MIN_BOUND_PTR :: CString
dBL_ATTR_MIN_BOUND_PTR = #const_cstr GRB_DBL_ATTR_MIN_BOUND

-- | Max (abs) obj coeff
dBL_ATTR_MAX_OBJ_COEFF :: String
dBL_ATTR_MAX_OBJ_COEFF = #const_str GRB_DBL_ATTR_MAX_OBJ_COEFF

-- | Max (abs) obj coeff
dBL_ATTR_MAX_OBJ_COEFF_PTR :: CString
dBL_ATTR_MAX_OBJ_COEFF_PTR = #const_cstr GRB_DBL_ATTR_MAX_OBJ_COEFF

-- | Min (abs) obj coeff
dBL_ATTR_MIN_OBJ_COEFF :: String
dBL_ATTR_MIN_OBJ_COEFF = #const_str GRB_DBL_ATTR_MIN_OBJ_COEFF

-- | Min (abs) obj coeff
dBL_ATTR_MIN_OBJ_COEFF_PTR :: CString
dBL_ATTR_MIN_OBJ_COEFF_PTR = #const_cstr GRB_DBL_ATTR_MIN_OBJ_COEFF

-- | Max (abs) rhs coeff
dBL_ATTR_MAX_RHS :: String
dBL_ATTR_MAX_RHS = #const_str GRB_DBL_ATTR_MAX_RHS

-- | Max (abs) rhs coeff
dBL_ATTR_MAX_RHS_PTR :: CString
dBL_ATTR_MAX_RHS_PTR = #const_cstr GRB_DBL_ATTR_MAX_RHS

-- | Min (abs) rhs coeff
dBL_ATTR_MIN_RHS :: String
dBL_ATTR_MIN_RHS = #const_str GRB_DBL_ATTR_MIN_RHS

-- | Min (abs) rhs coeff
dBL_ATTR_MIN_RHS_PTR :: CString
dBL_ATTR_MIN_RHS_PTR = #const_cstr GRB_DBL_ATTR_MIN_RHS

-- | Max (abs) nz coeff in Q
dBL_ATTR_MAX_QCCOEFF :: String
dBL_ATTR_MAX_QCCOEFF = #const_str GRB_DBL_ATTR_MAX_QCCOEFF

-- | Max (abs) nz coeff in Q
dBL_ATTR_MAX_QCCOEFF_PTR :: CString
dBL_ATTR_MAX_QCCOEFF_PTR = #const_cstr GRB_DBL_ATTR_MAX_QCCOEFF

-- | Min (abs) nz coeff in Q
dBL_ATTR_MIN_QCCOEFF :: String
dBL_ATTR_MIN_QCCOEFF = #const_str GRB_DBL_ATTR_MIN_QCCOEFF

-- | Min (abs) nz coeff in Q
dBL_ATTR_MIN_QCCOEFF_PTR :: CString
dBL_ATTR_MIN_QCCOEFF_PTR = #const_cstr GRB_DBL_ATTR_MIN_QCCOEFF

-- | Max (abs) nz coeff in linear part of Q
dBL_ATTR_MAX_QCLCOEFF :: String
dBL_ATTR_MAX_QCLCOEFF = #const_str GRB_DBL_ATTR_MAX_QCLCOEFF

-- | Max (abs) nz coeff in linear part of Q
dBL_ATTR_MAX_QCLCOEFF_PTR :: CString
dBL_ATTR_MAX_QCLCOEFF_PTR = #const_cstr GRB_DBL_ATTR_MAX_QCLCOEFF

-- | Min (abs) nz coeff in linear part of Q
dBL_ATTR_MIN_QCLCOEFF :: String
dBL_ATTR_MIN_QCLCOEFF = #const_str GRB_DBL_ATTR_MIN_QCLCOEFF

-- | Min (abs) nz coeff in linear part of Q
dBL_ATTR_MIN_QCLCOEFF_PTR :: CString
dBL_ATTR_MIN_QCLCOEFF_PTR = #const_cstr GRB_DBL_ATTR_MIN_QCLCOEFF

-- | Max (abs) rhs of Q
dBL_ATTR_MAX_QCRHS :: String
dBL_ATTR_MAX_QCRHS = #const_str GRB_DBL_ATTR_MAX_QCRHS

-- | Max (abs) rhs of Q
dBL_ATTR_MAX_QCRHS_PTR :: CString
dBL_ATTR_MAX_QCRHS_PTR = #const_cstr GRB_DBL_ATTR_MAX_QCRHS

-- | Min (abs) rhs of Q
dBL_ATTR_MIN_QCRHS :: String
dBL_ATTR_MIN_QCRHS = #const_str GRB_DBL_ATTR_MIN_QCRHS

-- | Min (abs) rhs of Q
dBL_ATTR_MIN_QCRHS_PTR :: CString
dBL_ATTR_MIN_QCRHS_PTR = #const_cstr GRB_DBL_ATTR_MIN_QCRHS

-- /* Model solution attributes */

-- | Run time for optimization
dBL_ATTR_RUNTIME :: String
dBL_ATTR_RUNTIME = #const_str GRB_DBL_ATTR_RUNTIME

-- | Run time for optimization
dBL_ATTR_RUNTIME_PTR :: CString
dBL_ATTR_RUNTIME_PTR = #const_cstr GRB_DBL_ATTR_RUNTIME

-- | Work for optimization
dBL_ATTR_WORK :: String
dBL_ATTR_WORK = #const_str GRB_DBL_ATTR_WORK

-- | Work for optimization
dBL_ATTR_WORK_PTR :: CString
dBL_ATTR_WORK_PTR = #const_cstr GRB_DBL_ATTR_WORK

-- | Optimization status
iNT_ATTR_STATUS :: String
iNT_ATTR_STATUS = #const_str GRB_INT_ATTR_STATUS

-- | Optimization status
iNT_ATTR_STATUS_PTR :: CString
iNT_ATTR_STATUS_PTR = #const_cstr GRB_INT_ATTR_STATUS

-- | Solution objective
dBL_ATTR_OBJVAL :: String
dBL_ATTR_OBJVAL = #const_str GRB_DBL_ATTR_OBJVAL

-- | Solution objective
dBL_ATTR_OBJVAL_PTR :: CString
dBL_ATTR_OBJVAL_PTR = #const_cstr GRB_DBL_ATTR_OBJVAL

-- | Best bound on solution
dBL_ATTR_OBJBOUND :: String
dBL_ATTR_OBJBOUND = #const_str GRB_DBL_ATTR_OBJBOUND

-- | Best bound on solution
dBL_ATTR_OBJBOUND_PTR :: CString
dBL_ATTR_OBJBOUND_PTR = #const_cstr GRB_DBL_ATTR_OBJBOUND

-- | Continuous bound
dBL_ATTR_OBJBOUNDC :: String
dBL_ATTR_OBJBOUNDC = #const_str GRB_DBL_ATTR_OBJBOUNDC

-- | Continuous bound
dBL_ATTR_OBJBOUNDC_PTR :: CString
dBL_ATTR_OBJBOUNDC_PTR = #const_cstr GRB_DBL_ATTR_OBJBOUNDC

-- | Best bound on pool solution
dBL_ATTR_POOLOBJBOUND :: String
dBL_ATTR_POOLOBJBOUND = #const_str GRB_DBL_ATTR_POOLOBJBOUND

-- | Best bound on pool solution
dBL_ATTR_POOLOBJBOUND_PTR :: CString
dBL_ATTR_POOLOBJBOUND_PTR = #const_cstr GRB_DBL_ATTR_POOLOBJBOUND

-- | Solution objective, depends on solutionnumber
dBL_ATTR_POOLOBJVAL :: String
dBL_ATTR_POOLOBJVAL = #const_str GRB_DBL_ATTR_POOLOBJVAL

-- | Solution objective, depends on solutionnumber
dBL_ATTR_POOLOBJVAL_PTR :: CString
dBL_ATTR_POOLOBJVAL_PTR = #const_cstr GRB_DBL_ATTR_POOLOBJVAL

-- | MIP optimality gap
dBL_ATTR_MIPGAP :: String
dBL_ATTR_MIPGAP = #const_str GRB_DBL_ATTR_MIPGAP

-- | MIP optimality gap
dBL_ATTR_MIPGAP_PTR :: CString
dBL_ATTR_MIPGAP_PTR = #const_cstr GRB_DBL_ATTR_MIPGAP

-- | # of solutions found
iNT_ATTR_SOLCOUNT :: String
iNT_ATTR_SOLCOUNT = #const_str GRB_INT_ATTR_SOLCOUNT

-- | # of solutions found
iNT_ATTR_SOLCOUNT_PTR :: CString
iNT_ATTR_SOLCOUNT_PTR = #const_cstr GRB_INT_ATTR_SOLCOUNT

-- | Iters performed (simplex)
dBL_ATTR_ITERCOUNT :: String
dBL_ATTR_ITERCOUNT = #const_str GRB_DBL_ATTR_ITERCOUNT

-- | Iters performed (simplex)
dBL_ATTR_ITERCOUNT_PTR :: CString
dBL_ATTR_ITERCOUNT_PTR = #const_cstr GRB_DBL_ATTR_ITERCOUNT

-- | Iters performed (barrier)
iNT_ATTR_BARITERCOUNT :: String
iNT_ATTR_BARITERCOUNT = #const_str GRB_INT_ATTR_BARITERCOUNT

-- | Iters performed (barrier)
iNT_ATTR_BARITERCOUNT_PTR :: CString
iNT_ATTR_BARITERCOUNT_PTR = #const_cstr GRB_INT_ATTR_BARITERCOUNT

-- | Nodes explored (B&C)
dBL_ATTR_NODECOUNT :: String
dBL_ATTR_NODECOUNT = #const_str GRB_DBL_ATTR_NODECOUNT

-- | Nodes explored (B&C)
dBL_ATTR_NODECOUNT_PTR :: CString
dBL_ATTR_NODECOUNT_PTR = #const_cstr GRB_DBL_ATTR_NODECOUNT

-- |
--
-- 0, no basis,
-- 1, has basis, so can be computed
-- 2, available
iNT_ATTR_HASDUALNORM :: String
iNT_ATTR_HASDUALNORM = #const_str GRB_INT_ATTR_HASDUALNORM

-- |
--
-- 0, no basis,
-- 1, has basis, so can be computed
-- 2, available
iNT_ATTR_HASDUALNORM_PTR :: CString
iNT_ATTR_HASDUALNORM_PTR = #const_cstr GRB_INT_ATTR_HASDUALNORM

-- | method that solved LP using concurrent
iNT_ATTR_CONCURRENTWINMETHOD :: String
iNT_ATTR_CONCURRENTWINMETHOD = #const_str GRB_INT_ATTR_CONCURRENTWINMETHOD

-- | method that solved LP using concurrent
iNT_ATTR_CONCURRENTWINMETHOD_PTR :: CString
iNT_ATTR_CONCURRENTWINMETHOD_PTR = #const_cstr GRB_INT_ATTR_CONCURRENTWINMETHOD

-- /* Variable attributes related to the current solution */

-- | Solution value
dBL_ATTR_X :: String
dBL_ATTR_X = #const_str GRB_DBL_ATTR_X

-- | Solution value
dBL_ATTR_X_PTR :: CString
dBL_ATTR_X_PTR = #const_cstr GRB_DBL_ATTR_X

-- | Alternate MIP solution, depends on solutionnumber
dBL_ATTR_XN :: String
dBL_ATTR_XN = #const_str GRB_DBL_ATTR_XN

-- | Alternate MIP solution, depends on solutionnumber
dBL_ATTR_XN_PTR :: CString
dBL_ATTR_XN_PTR = #const_cstr GRB_DBL_ATTR_XN

-- | Best barrier primal iterate
dBL_ATTR_BARX :: String
dBL_ATTR_BARX = #const_str GRB_DBL_ATTR_BARX

-- | Best barrier primal iterate
dBL_ATTR_BARX_PTR :: CString
dBL_ATTR_BARX_PTR = #const_cstr GRB_DBL_ATTR_BARX

-- | Best barrier dual iterate
dBL_ATTR_BARPI :: String
dBL_ATTR_BARPI = #const_str GRB_DBL_ATTR_BARPI

-- | Best barrier dual iterate
dBL_ATTR_BARPI_PTR :: CString
dBL_ATTR_BARPI_PTR = #const_cstr GRB_DBL_ATTR_BARPI

-- | Reduced costs
dBL_ATTR_RC :: String
dBL_ATTR_RC = #const_str GRB_DBL_ATTR_RC

-- | Reduced costs
dBL_ATTR_RC_PTR :: CString
dBL_ATTR_RC_PTR = #const_cstr GRB_DBL_ATTR_RC

-- | Variable basis status
iNT_ATTR_VBASIS :: String
iNT_ATTR_VBASIS = #const_str GRB_INT_ATTR_VBASIS

-- | Variable basis status
iNT_ATTR_VBASIS_PTR :: CString
iNT_ATTR_VBASIS_PTR = #const_cstr GRB_INT_ATTR_VBASIS

-- /* Constraint attributes related to the current solution */

-- | Dual value
dBL_ATTR_PI :: String
dBL_ATTR_PI = #const_str GRB_DBL_ATTR_PI

-- | Dual value
dBL_ATTR_PI_PTR :: CString
dBL_ATTR_PI_PTR = #const_cstr GRB_DBL_ATTR_PI

-- | Dual value for QC
dBL_ATTR_QCPI :: String
dBL_ATTR_QCPI = #const_str GRB_DBL_ATTR_QCPI

-- | Dual value for QC
dBL_ATTR_QCPI_PTR :: CString
dBL_ATTR_QCPI_PTR = #const_cstr GRB_DBL_ATTR_QCPI

-- | Constraint slack
dBL_ATTR_SLACK :: String
dBL_ATTR_SLACK = #const_str GRB_DBL_ATTR_SLACK

-- | Constraint slack
dBL_ATTR_SLACK_PTR :: CString
dBL_ATTR_SLACK_PTR = #const_cstr GRB_DBL_ATTR_SLACK

-- | QC Constraint slack
dBL_ATTR_QCSLACK :: String
dBL_ATTR_QCSLACK = #const_str GRB_DBL_ATTR_QCSLACK

-- | QC Constraint slack
dBL_ATTR_QCSLACK_PTR :: CString
dBL_ATTR_QCSLACK_PTR = #const_cstr GRB_DBL_ATTR_QCSLACK

-- | Constraint basis status
iNT_ATTR_CBASIS :: String
iNT_ATTR_CBASIS = #const_str GRB_INT_ATTR_CBASIS

-- | Constraint basis status
iNT_ATTR_CBASIS_PTR :: CString
iNT_ATTR_CBASIS_PTR = #const_cstr GRB_INT_ATTR_CBASIS

-- /* Solution quality attributes */

dBL_ATTR_MAX_VIO :: String
dBL_ATTR_MAX_VIO = #const_str GRB_DBL_ATTR_MAX_VIO

dBL_ATTR_MAX_VIO_PTR :: CString
dBL_ATTR_MAX_VIO_PTR = #const_cstr GRB_DBL_ATTR_MAX_VIO

dBL_ATTR_BOUND_VIO :: String
dBL_ATTR_BOUND_VIO = #const_str GRB_DBL_ATTR_BOUND_VIO

dBL_ATTR_BOUND_VIO_PTR :: CString
dBL_ATTR_BOUND_VIO_PTR = #const_cstr GRB_DBL_ATTR_BOUND_VIO

dBL_ATTR_BOUND_SVIO :: String
dBL_ATTR_BOUND_SVIO = #const_str GRB_DBL_ATTR_BOUND_SVIO

dBL_ATTR_BOUND_SVIO_PTR :: CString
dBL_ATTR_BOUND_SVIO_PTR = #const_cstr GRB_DBL_ATTR_BOUND_SVIO

iNT_ATTR_BOUND_VIO_INDEX :: String
iNT_ATTR_BOUND_VIO_INDEX = #const_str GRB_INT_ATTR_BOUND_VIO_INDEX

iNT_ATTR_BOUND_VIO_INDEX_PTR :: CString
iNT_ATTR_BOUND_VIO_INDEX_PTR = #const_cstr GRB_INT_ATTR_BOUND_VIO_INDEX

iNT_ATTR_BOUND_SVIO_INDEX :: String
iNT_ATTR_BOUND_SVIO_INDEX = #const_str GRB_INT_ATTR_BOUND_SVIO_INDEX

iNT_ATTR_BOUND_SVIO_INDEX_PTR :: CString
iNT_ATTR_BOUND_SVIO_INDEX_PTR = #const_cstr GRB_INT_ATTR_BOUND_SVIO_INDEX

dBL_ATTR_BOUND_VIO_SUM :: String
dBL_ATTR_BOUND_VIO_SUM = #const_str GRB_DBL_ATTR_BOUND_VIO_SUM

dBL_ATTR_BOUND_VIO_SUM_PTR :: CString
dBL_ATTR_BOUND_VIO_SUM_PTR = #const_cstr GRB_DBL_ATTR_BOUND_VIO_SUM

dBL_ATTR_BOUND_SVIO_SUM :: String
dBL_ATTR_BOUND_SVIO_SUM = #const_str GRB_DBL_ATTR_BOUND_SVIO_SUM

dBL_ATTR_BOUND_SVIO_SUM_PTR :: CString
dBL_ATTR_BOUND_SVIO_SUM_PTR = #const_cstr GRB_DBL_ATTR_BOUND_SVIO_SUM

dBL_ATTR_CONSTR_VIO :: String
dBL_ATTR_CONSTR_VIO = #const_str GRB_DBL_ATTR_CONSTR_VIO

dBL_ATTR_CONSTR_VIO_PTR :: CString
dBL_ATTR_CONSTR_VIO_PTR = #const_cstr GRB_DBL_ATTR_CONSTR_VIO

dBL_ATTR_CONSTR_SVIO :: String
dBL_ATTR_CONSTR_SVIO = #const_str GRB_DBL_ATTR_CONSTR_SVIO

dBL_ATTR_CONSTR_SVIO_PTR :: CString
dBL_ATTR_CONSTR_SVIO_PTR = #const_cstr GRB_DBL_ATTR_CONSTR_SVIO

iNT_ATTR_CONSTR_VIO_INDEX :: String
iNT_ATTR_CONSTR_VIO_INDEX = #const_str GRB_INT_ATTR_CONSTR_VIO_INDEX

iNT_ATTR_CONSTR_VIO_INDEX_PTR :: CString
iNT_ATTR_CONSTR_VIO_INDEX_PTR = #const_cstr GRB_INT_ATTR_CONSTR_VIO_INDEX

iNT_ATTR_CONSTR_SVIO_INDEX :: String
iNT_ATTR_CONSTR_SVIO_INDEX = #const_str GRB_INT_ATTR_CONSTR_SVIO_INDEX

iNT_ATTR_CONSTR_SVIO_INDEX_PTR :: CString
iNT_ATTR_CONSTR_SVIO_INDEX_PTR = #const_cstr GRB_INT_ATTR_CONSTR_SVIO_INDEX

dBL_ATTR_CONSTR_VIO_SUM :: String
dBL_ATTR_CONSTR_VIO_SUM = #const_str GRB_DBL_ATTR_CONSTR_VIO_SUM

dBL_ATTR_CONSTR_VIO_SUM_PTR :: CString
dBL_ATTR_CONSTR_VIO_SUM_PTR = #const_cstr GRB_DBL_ATTR_CONSTR_VIO_SUM

dBL_ATTR_CONSTR_SVIO_SUM :: String
dBL_ATTR_CONSTR_SVIO_SUM = #const_str GRB_DBL_ATTR_CONSTR_SVIO_SUM

dBL_ATTR_CONSTR_SVIO_SUM_PTR :: CString
dBL_ATTR_CONSTR_SVIO_SUM_PTR = #const_cstr GRB_DBL_ATTR_CONSTR_SVIO_SUM

dBL_ATTR_CONSTR_RESIDUAL :: String
dBL_ATTR_CONSTR_RESIDUAL = #const_str GRB_DBL_ATTR_CONSTR_RESIDUAL

dBL_ATTR_CONSTR_RESIDUAL_PTR :: CString
dBL_ATTR_CONSTR_RESIDUAL_PTR = #const_cstr GRB_DBL_ATTR_CONSTR_RESIDUAL

dBL_ATTR_CONSTR_SRESIDUAL :: String
dBL_ATTR_CONSTR_SRESIDUAL = #const_str GRB_DBL_ATTR_CONSTR_SRESIDUAL

dBL_ATTR_CONSTR_SRESIDUAL_PTR :: CString
dBL_ATTR_CONSTR_SRESIDUAL_PTR = #const_cstr GRB_DBL_ATTR_CONSTR_SRESIDUAL

iNT_ATTR_CONSTR_RESIDUAL_INDEX :: String
iNT_ATTR_CONSTR_RESIDUAL_INDEX = #const_str GRB_INT_ATTR_CONSTR_RESIDUAL_INDEX

iNT_ATTR_CONSTR_RESIDUAL_INDEX_PTR :: CString
iNT_ATTR_CONSTR_RESIDUAL_INDEX_PTR = #const_cstr GRB_INT_ATTR_CONSTR_RESIDUAL_INDEX

iNT_ATTR_CONSTR_SRESIDUAL_INDEX :: String
iNT_ATTR_CONSTR_SRESIDUAL_INDEX = #const_str GRB_INT_ATTR_CONSTR_SRESIDUAL_INDEX

iNT_ATTR_CONSTR_SRESIDUAL_INDEX_PTR :: CString
iNT_ATTR_CONSTR_SRESIDUAL_INDEX_PTR = #const_cstr GRB_INT_ATTR_CONSTR_SRESIDUAL_INDEX

dBL_ATTR_CONSTR_RESIDUAL_SUM :: String
dBL_ATTR_CONSTR_RESIDUAL_SUM = #const_str GRB_DBL_ATTR_CONSTR_RESIDUAL_SUM

dBL_ATTR_CONSTR_RESIDUAL_SUM_PTR :: CString
dBL_ATTR_CONSTR_RESIDUAL_SUM_PTR = #const_cstr GRB_DBL_ATTR_CONSTR_RESIDUAL_SUM

dBL_ATTR_CONSTR_SRESIDUAL_SUM :: String
dBL_ATTR_CONSTR_SRESIDUAL_SUM = #const_str GRB_DBL_ATTR_CONSTR_SRESIDUAL_SUM

dBL_ATTR_CONSTR_SRESIDUAL_SUM_PTR :: CString
dBL_ATTR_CONSTR_SRESIDUAL_SUM_PTR = #const_cstr GRB_DBL_ATTR_CONSTR_SRESIDUAL_SUM

dBL_ATTR_DUAL_VIO :: String
dBL_ATTR_DUAL_VIO = #const_str GRB_DBL_ATTR_DUAL_VIO

dBL_ATTR_DUAL_VIO_PTR :: CString
dBL_ATTR_DUAL_VIO_PTR = #const_cstr GRB_DBL_ATTR_DUAL_VIO

dBL_ATTR_DUAL_SVIO :: String
dBL_ATTR_DUAL_SVIO = #const_str GRB_DBL_ATTR_DUAL_SVIO

dBL_ATTR_DUAL_SVIO_PTR :: CString
dBL_ATTR_DUAL_SVIO_PTR = #const_cstr GRB_DBL_ATTR_DUAL_SVIO

iNT_ATTR_DUAL_VIO_INDEX :: String
iNT_ATTR_DUAL_VIO_INDEX = #const_str GRB_INT_ATTR_DUAL_VIO_INDEX

iNT_ATTR_DUAL_VIO_INDEX_PTR :: CString
iNT_ATTR_DUAL_VIO_INDEX_PTR = #const_cstr GRB_INT_ATTR_DUAL_VIO_INDEX

iNT_ATTR_DUAL_SVIO_INDEX :: String
iNT_ATTR_DUAL_SVIO_INDEX = #const_str GRB_INT_ATTR_DUAL_SVIO_INDEX

iNT_ATTR_DUAL_SVIO_INDEX_PTR :: CString
iNT_ATTR_DUAL_SVIO_INDEX_PTR = #const_cstr GRB_INT_ATTR_DUAL_SVIO_INDEX

dBL_ATTR_DUAL_VIO_SUM :: String
dBL_ATTR_DUAL_VIO_SUM = #const_str GRB_DBL_ATTR_DUAL_VIO_SUM

dBL_ATTR_DUAL_VIO_SUM_PTR :: CString
dBL_ATTR_DUAL_VIO_SUM_PTR = #const_cstr GRB_DBL_ATTR_DUAL_VIO_SUM

dBL_ATTR_DUAL_SVIO_SUM :: String
dBL_ATTR_DUAL_SVIO_SUM = #const_str GRB_DBL_ATTR_DUAL_SVIO_SUM

dBL_ATTR_DUAL_SVIO_SUM_PTR :: CString
dBL_ATTR_DUAL_SVIO_SUM_PTR = #const_cstr GRB_DBL_ATTR_DUAL_SVIO_SUM

dBL_ATTR_DUAL_RESIDUAL :: String
dBL_ATTR_DUAL_RESIDUAL = #const_str GRB_DBL_ATTR_DUAL_RESIDUAL

dBL_ATTR_DUAL_RESIDUAL_PTR :: CString
dBL_ATTR_DUAL_RESIDUAL_PTR = #const_cstr GRB_DBL_ATTR_DUAL_RESIDUAL

dBL_ATTR_DUAL_SRESIDUAL :: String
dBL_ATTR_DUAL_SRESIDUAL = #const_str GRB_DBL_ATTR_DUAL_SRESIDUAL

dBL_ATTR_DUAL_SRESIDUAL_PTR :: CString
dBL_ATTR_DUAL_SRESIDUAL_PTR = #const_cstr GRB_DBL_ATTR_DUAL_SRESIDUAL

iNT_ATTR_DUAL_RESIDUAL_INDEX :: String
iNT_ATTR_DUAL_RESIDUAL_INDEX = #const_str GRB_INT_ATTR_DUAL_RESIDUAL_INDEX

iNT_ATTR_DUAL_RESIDUAL_INDEX_PTR :: CString
iNT_ATTR_DUAL_RESIDUAL_INDEX_PTR = #const_cstr GRB_INT_ATTR_DUAL_RESIDUAL_INDEX

iNT_ATTR_DUAL_SRESIDUAL_INDEX :: String
iNT_ATTR_DUAL_SRESIDUAL_INDEX = #const_str GRB_INT_ATTR_DUAL_SRESIDUAL_INDEX

iNT_ATTR_DUAL_SRESIDUAL_INDEX_PTR :: CString
iNT_ATTR_DUAL_SRESIDUAL_INDEX_PTR = #const_cstr GRB_INT_ATTR_DUAL_SRESIDUAL_INDEX

dBL_ATTR_DUAL_RESIDUAL_SUM :: String
dBL_ATTR_DUAL_RESIDUAL_SUM = #const_str GRB_DBL_ATTR_DUAL_RESIDUAL_SUM

dBL_ATTR_DUAL_RESIDUAL_SUM_PTR :: CString
dBL_ATTR_DUAL_RESIDUAL_SUM_PTR = #const_cstr GRB_DBL_ATTR_DUAL_RESIDUAL_SUM

dBL_ATTR_DUAL_SRESIDUAL_SUM :: String
dBL_ATTR_DUAL_SRESIDUAL_SUM = #const_str GRB_DBL_ATTR_DUAL_SRESIDUAL_SUM

dBL_ATTR_DUAL_SRESIDUAL_SUM_PTR :: CString
dBL_ATTR_DUAL_SRESIDUAL_SUM_PTR = #const_cstr GRB_DBL_ATTR_DUAL_SRESIDUAL_SUM

dBL_ATTR_INT_VIO :: String
dBL_ATTR_INT_VIO = #const_str GRB_DBL_ATTR_INT_VIO

dBL_ATTR_INT_VIO_PTR :: CString
dBL_ATTR_INT_VIO_PTR = #const_cstr GRB_DBL_ATTR_INT_VIO

iNT_ATTR_INT_VIO_INDEX :: String
iNT_ATTR_INT_VIO_INDEX = #const_str GRB_INT_ATTR_INT_VIO_INDEX

iNT_ATTR_INT_VIO_INDEX_PTR :: CString
iNT_ATTR_INT_VIO_INDEX_PTR = #const_cstr GRB_INT_ATTR_INT_VIO_INDEX

dBL_ATTR_INT_VIO_SUM :: String
dBL_ATTR_INT_VIO_SUM = #const_str GRB_DBL_ATTR_INT_VIO_SUM

dBL_ATTR_INT_VIO_SUM_PTR :: CString
dBL_ATTR_INT_VIO_SUM_PTR = #const_cstr GRB_DBL_ATTR_INT_VIO_SUM

dBL_ATTR_COMPL_VIO :: String
dBL_ATTR_COMPL_VIO = #const_str GRB_DBL_ATTR_COMPL_VIO

dBL_ATTR_COMPL_VIO_PTR :: CString
dBL_ATTR_COMPL_VIO_PTR = #const_cstr GRB_DBL_ATTR_COMPL_VIO

iNT_ATTR_COMPL_VIO_INDEX :: String
iNT_ATTR_COMPL_VIO_INDEX = #const_str GRB_INT_ATTR_COMPL_VIO_INDEX

iNT_ATTR_COMPL_VIO_INDEX_PTR :: CString
iNT_ATTR_COMPL_VIO_INDEX_PTR = #const_cstr GRB_INT_ATTR_COMPL_VIO_INDEX

dBL_ATTR_COMPL_VIO_SUM :: String
dBL_ATTR_COMPL_VIO_SUM = #const_str GRB_DBL_ATTR_COMPL_VIO_SUM

dBL_ATTR_COMPL_VIO_SUM_PTR :: CString
dBL_ATTR_COMPL_VIO_SUM_PTR = #const_cstr GRB_DBL_ATTR_COMPL_VIO_SUM

dBL_ATTR_KAPPA :: String
dBL_ATTR_KAPPA = #const_str GRB_DBL_ATTR_KAPPA

dBL_ATTR_KAPPA_PTR :: CString
dBL_ATTR_KAPPA_PTR = #const_cstr GRB_DBL_ATTR_KAPPA

dBL_ATTR_KAPPA_EXACT :: String
dBL_ATTR_KAPPA_EXACT = #const_str GRB_DBL_ATTR_KAPPA_EXACT

dBL_ATTR_KAPPA_EXACT_PTR :: CString
dBL_ATTR_KAPPA_EXACT_PTR = #const_cstr GRB_DBL_ATTR_KAPPA_EXACT

dBL_ATTR_N2KAPPA :: String
dBL_ATTR_N2KAPPA = #const_str GRB_DBL_ATTR_N2KAPPA

dBL_ATTR_N2KAPPA_PTR :: CString
dBL_ATTR_N2KAPPA_PTR = #const_cstr GRB_DBL_ATTR_N2KAPPA

-- /* LP sensitivity analysis */

dBL_ATTR_SA_OBJLOW :: String
dBL_ATTR_SA_OBJLOW = #const_str GRB_DBL_ATTR_SA_OBJLOW

dBL_ATTR_SA_OBJLOW_PTR :: CString
dBL_ATTR_SA_OBJLOW_PTR = #const_cstr GRB_DBL_ATTR_SA_OBJLOW

dBL_ATTR_SA_OBJUP :: String
dBL_ATTR_SA_OBJUP = #const_str GRB_DBL_ATTR_SA_OBJUP

dBL_ATTR_SA_OBJUP_PTR :: CString
dBL_ATTR_SA_OBJUP_PTR = #const_cstr GRB_DBL_ATTR_SA_OBJUP

dBL_ATTR_SA_LBLOW :: String
dBL_ATTR_SA_LBLOW = #const_str GRB_DBL_ATTR_SA_LBLOW

dBL_ATTR_SA_LBLOW_PTR :: CString
dBL_ATTR_SA_LBLOW_PTR = #const_cstr GRB_DBL_ATTR_SA_LBLOW

dBL_ATTR_SA_LBUP :: String
dBL_ATTR_SA_LBUP = #const_str GRB_DBL_ATTR_SA_LBUP

dBL_ATTR_SA_LBUP_PTR :: CString
dBL_ATTR_SA_LBUP_PTR = #const_cstr GRB_DBL_ATTR_SA_LBUP

dBL_ATTR_SA_UBLOW :: String
dBL_ATTR_SA_UBLOW = #const_str GRB_DBL_ATTR_SA_UBLOW

dBL_ATTR_SA_UBLOW_PTR :: CString
dBL_ATTR_SA_UBLOW_PTR = #const_cstr GRB_DBL_ATTR_SA_UBLOW

dBL_ATTR_SA_UBUP :: String
dBL_ATTR_SA_UBUP = #const_str GRB_DBL_ATTR_SA_UBUP

dBL_ATTR_SA_UBUP_PTR :: CString
dBL_ATTR_SA_UBUP_PTR = #const_cstr GRB_DBL_ATTR_SA_UBUP

dBL_ATTR_SA_RHSLOW :: String
dBL_ATTR_SA_RHSLOW = #const_str GRB_DBL_ATTR_SA_RHSLOW

dBL_ATTR_SA_RHSLOW_PTR :: CString
dBL_ATTR_SA_RHSLOW_PTR = #const_cstr GRB_DBL_ATTR_SA_RHSLOW

dBL_ATTR_SA_RHSUP :: String
dBL_ATTR_SA_RHSUP = #const_str GRB_DBL_ATTR_SA_RHSUP

dBL_ATTR_SA_RHSUP_PTR :: CString
dBL_ATTR_SA_RHSUP_PTR = #const_cstr GRB_DBL_ATTR_SA_RHSUP

-- /* IIS */

-- | Boolean: Is IIS Minimal?
iNT_ATTR_IIS_MINIMAL :: String
iNT_ATTR_IIS_MINIMAL = #const_str GRB_INT_ATTR_IIS_MINIMAL

-- | Boolean: Is IIS Minimal?
iNT_ATTR_IIS_MINIMAL_PTR :: CString
iNT_ATTR_IIS_MINIMAL_PTR = #const_cstr GRB_INT_ATTR_IIS_MINIMAL

-- | Boolean: Is var LB in IIS?
iNT_ATTR_IIS_LB :: String
iNT_ATTR_IIS_LB = #const_str GRB_INT_ATTR_IIS_LB

-- | Boolean: Is var LB in IIS?
iNT_ATTR_IIS_LB_PTR :: CString
iNT_ATTR_IIS_LB_PTR = #const_cstr GRB_INT_ATTR_IIS_LB

-- | Boolean: Is var UB in IIS?
iNT_ATTR_IIS_UB :: String
iNT_ATTR_IIS_UB = #const_str GRB_INT_ATTR_IIS_UB

-- | Boolean: Is var UB in IIS?
iNT_ATTR_IIS_UB_PTR :: CString
iNT_ATTR_IIS_UB_PTR = #const_cstr GRB_INT_ATTR_IIS_UB

-- | Boolean: Is constr in IIS?
iNT_ATTR_IIS_CONSTR :: String
iNT_ATTR_IIS_CONSTR = #const_str GRB_INT_ATTR_IIS_CONSTR

-- | Boolean: Is constr in IIS?
iNT_ATTR_IIS_CONSTR_PTR :: CString
iNT_ATTR_IIS_CONSTR_PTR = #const_cstr GRB_INT_ATTR_IIS_CONSTR

-- | Boolean: Is SOS in IIS?
iNT_ATTR_IIS_SOS :: String
iNT_ATTR_IIS_SOS = #const_str GRB_INT_ATTR_IIS_SOS

-- | Boolean: Is SOS in IIS?
iNT_ATTR_IIS_SOS_PTR :: CString
iNT_ATTR_IIS_SOS_PTR = #const_cstr GRB_INT_ATTR_IIS_SOS

-- | Boolean: Is QConstr in IIS?
iNT_ATTR_IIS_QCONSTR :: String
iNT_ATTR_IIS_QCONSTR = #const_str GRB_INT_ATTR_IIS_QCONSTR

-- | Boolean: Is QConstr in IIS?
iNT_ATTR_IIS_QCONSTR_PTR :: CString
iNT_ATTR_IIS_QCONSTR_PTR = #const_cstr GRB_INT_ATTR_IIS_QCONSTR

-- | Boolean: Is general constr in IIS?
iNT_ATTR_IIS_GENCONSTR :: String
iNT_ATTR_IIS_GENCONSTR = #const_str GRB_INT_ATTR_IIS_GENCONSTR

-- | Boolean: Is general constr in IIS?
iNT_ATTR_IIS_GENCONSTR_PTR :: CString
iNT_ATTR_IIS_GENCONSTR_PTR = #const_cstr GRB_INT_ATTR_IIS_GENCONSTR


-- | Force var LB to be (1) or to not be (0) in final IIS
iNT_ATTR_IIS_LBFORCE :: String
iNT_ATTR_IIS_LBFORCE = #const_str GRB_INT_ATTR_IIS_LBFORCE

-- | Force var LB to be (1) or to not be (0) in final IIS
iNT_ATTR_IIS_LBFORCE_PTR :: CString
iNT_ATTR_IIS_LBFORCE_PTR = #const_cstr GRB_INT_ATTR_IIS_LBFORCE

-- | Force var UB to be (1) or to not be (0) in final IIS
iNT_ATTR_IIS_UBFORCE :: String
iNT_ATTR_IIS_UBFORCE = #const_str GRB_INT_ATTR_IIS_UBFORCE

-- | Force var UB to be (1) or to not be (0) in final IIS
iNT_ATTR_IIS_UBFORCE_PTR :: CString
iNT_ATTR_IIS_UBFORCE_PTR = #const_cstr GRB_INT_ATTR_IIS_UBFORCE

-- | Force constr to be (1) or to not be (0) in final IIS
iNT_ATTR_IIS_CONSTRFORCE :: String
iNT_ATTR_IIS_CONSTRFORCE = #const_str GRB_INT_ATTR_IIS_CONSTRFORCE

-- | Force constr to be (1) or to not be (0) in final IIS
iNT_ATTR_IIS_CONSTRFORCE_PTR :: CString
iNT_ATTR_IIS_CONSTRFORCE_PTR = #const_cstr GRB_INT_ATTR_IIS_CONSTRFORCE

-- | Force SOS to be (1) or to not be (0) in final IIS
iNT_ATTR_IIS_SOSFORCE :: String
iNT_ATTR_IIS_SOSFORCE = #const_str GRB_INT_ATTR_IIS_SOSFORCE

-- | Force SOS to be (1) or to not be (0) in final IIS
iNT_ATTR_IIS_SOSFORCE_PTR :: CString
iNT_ATTR_IIS_SOSFORCE_PTR = #const_cstr GRB_INT_ATTR_IIS_SOSFORCE

-- | Force QConstr to be (1) or to not be (0) in final IIS
iNT_ATTR_IIS_QCONSTRFORCE :: String
iNT_ATTR_IIS_QCONSTRFORCE = #const_str GRB_INT_ATTR_IIS_QCONSTRFORCE

-- | Force QConstr to be (1) or to not be (0) in final IIS
iNT_ATTR_IIS_QCONSTRFORCE_PTR :: CString
iNT_ATTR_IIS_QCONSTRFORCE_PTR = #const_cstr GRB_INT_ATTR_IIS_QCONSTRFORCE

-- | Force general constr to be (1) or to not be (0) in final IIS
iNT_ATTR_IIS_GENCONSTRFORCE :: String
iNT_ATTR_IIS_GENCONSTRFORCE = #const_str GRB_INT_ATTR_IIS_GENCONSTRFORCE

-- | Force general constr to be (1) or to not be (0) in final IIS
iNT_ATTR_IIS_GENCONSTRFORCE_PTR :: CString
iNT_ATTR_IIS_GENCONSTRFORCE_PTR = #const_cstr GRB_INT_ATTR_IIS_GENCONSTRFORCE

-- /* Tuning */

iNT_ATTR_TUNE_RESULTCOUNT :: String
iNT_ATTR_TUNE_RESULTCOUNT = #const_str GRB_INT_ATTR_TUNE_RESULTCOUNT

iNT_ATTR_TUNE_RESULTCOUNT_PTR :: CString
iNT_ATTR_TUNE_RESULTCOUNT_PTR = #const_cstr GRB_INT_ATTR_TUNE_RESULTCOUNT

-- /* Advanced simplex features */

dBL_ATTR_FARKASDUAL :: String
dBL_ATTR_FARKASDUAL = #const_str GRB_DBL_ATTR_FARKASDUAL

dBL_ATTR_FARKASDUAL_PTR :: CString
dBL_ATTR_FARKASDUAL_PTR = #const_cstr GRB_DBL_ATTR_FARKASDUAL

dBL_ATTR_FARKASPROOF :: String
dBL_ATTR_FARKASPROOF = #const_str GRB_DBL_ATTR_FARKASPROOF

dBL_ATTR_FARKASPROOF_PTR :: CString
dBL_ATTR_FARKASPROOF_PTR = #const_cstr GRB_DBL_ATTR_FARKASPROOF

dBL_ATTR_UNBDRAY :: String
dBL_ATTR_UNBDRAY = #const_str GRB_DBL_ATTR_UNBDRAY

dBL_ATTR_UNBDRAY_PTR :: CString
dBL_ATTR_UNBDRAY_PTR = #const_cstr GRB_DBL_ATTR_UNBDRAY

iNT_ATTR_INFEASVAR :: String
iNT_ATTR_INFEASVAR = #const_str GRB_INT_ATTR_INFEASVAR

iNT_ATTR_INFEASVAR_PTR :: CString
iNT_ATTR_INFEASVAR_PTR = #const_cstr GRB_INT_ATTR_INFEASVAR

iNT_ATTR_UNBDVAR :: String
iNT_ATTR_UNBDVAR = #const_str GRB_INT_ATTR_UNBDVAR

iNT_ATTR_UNBDVAR_PTR :: CString
iNT_ATTR_UNBDVAR_PTR = #const_cstr GRB_INT_ATTR_UNBDVAR

-- /* Presolve attribute */

iNT_ATTR_VARPRESTAT :: String
iNT_ATTR_VARPRESTAT = #const_str GRB_INT_ATTR_VARPRESTAT

iNT_ATTR_VARPRESTAT_PTR :: CString
iNT_ATTR_VARPRESTAT_PTR = #const_cstr GRB_INT_ATTR_VARPRESTAT

dBL_ATTR_PREFIXVAL :: String
dBL_ATTR_PREFIXVAL = #const_str GRB_DBL_ATTR_PREFIXVAL

dBL_ATTR_PREFIXVAL_PTR :: CString
dBL_ATTR_PREFIXVAL_PTR = #const_cstr GRB_DBL_ATTR_PREFIXVAL

-- /* Multi objective attribute, controlled by parameter ObjNumber (= i)
--  *
--  * Note if you add an new attribute, adjust the const array
--  * OBJNUMATTRNAMES and define OBJNUMATTRS in attrcache.c.
--  */

-- | ith objective
dBL_ATTR_OBJN :: String
dBL_ATTR_OBJN = #const_str GRB_DBL_ATTR_OBJN

-- | ith objective
dBL_ATTR_OBJN_PTR :: CString
dBL_ATTR_OBJN_PTR = #const_cstr GRB_DBL_ATTR_OBJN

-- | Solution objective for Multi-objectives, also depends on solutionnumber
dBL_ATTR_OBJNVAL :: String
dBL_ATTR_OBJNVAL = #const_str GRB_DBL_ATTR_OBJNVAL

-- | Solution objective for Multi-objectives, also depends on solutionnumber
dBL_ATTR_OBJNVAL_PTR :: CString
dBL_ATTR_OBJNVAL_PTR = #const_cstr GRB_DBL_ATTR_OBJNVAL

-- | constant term
dBL_ATTR_OBJNCON :: String
dBL_ATTR_OBJNCON = #const_str GRB_DBL_ATTR_OBJNCON

-- | constant term
dBL_ATTR_OBJNCON_PTR :: CString
dBL_ATTR_OBJNCON_PTR = #const_cstr GRB_DBL_ATTR_OBJNCON

-- | weight
dBL_ATTR_OBJNWEIGHT :: String
dBL_ATTR_OBJNWEIGHT = #const_str GRB_DBL_ATTR_OBJNWEIGHT

-- | weight
dBL_ATTR_OBJNWEIGHT_PTR :: CString
dBL_ATTR_OBJNWEIGHT_PTR = #const_cstr GRB_DBL_ATTR_OBJNWEIGHT

-- | priority
iNT_ATTR_OBJNPRIORITY :: String
iNT_ATTR_OBJNPRIORITY = #const_str GRB_INT_ATTR_OBJNPRIORITY

-- | priority
iNT_ATTR_OBJNPRIORITY_PTR :: CString
iNT_ATTR_OBJNPRIORITY_PTR = #const_cstr GRB_INT_ATTR_OBJNPRIORITY

-- | relative tolerance
dBL_ATTR_OBJNRELTOL :: String
dBL_ATTR_OBJNRELTOL = #const_str GRB_DBL_ATTR_OBJNRELTOL

-- | relative tolerance
dBL_ATTR_OBJNRELTOL_PTR :: CString
dBL_ATTR_OBJNRELTOL_PTR = #const_cstr GRB_DBL_ATTR_OBJNRELTOL

-- | absolute tolerance
dBL_ATTR_OBJNABSTOL :: String
dBL_ATTR_OBJNABSTOL = #const_str GRB_DBL_ATTR_OBJNABSTOL

-- | absolute tolerance
dBL_ATTR_OBJNABSTOL_PTR :: CString
dBL_ATTR_OBJNABSTOL_PTR = #const_cstr GRB_DBL_ATTR_OBJNABSTOL

-- | name
sTR_ATTR_OBJNNAME :: String
sTR_ATTR_OBJNNAME = #const_str GRB_STR_ATTR_OBJNNAME

-- | name
sTR_ATTR_OBJNNAME_PTR :: CString
sTR_ATTR_OBJNNAME_PTR = #const_cstr GRB_STR_ATTR_OBJNNAME

-- /* Scenario attributes, controlled by parameter ScenarioNumber (= i)
--  *
--  * Note if you add an new attribute, adjust the const array
--  * SCENARIONUMATTRNAMES and define SCENARIONUMATTRS in attrcache.c.
--  */

-- | lower bound in scenario i
dBL_ATTR_SCENNLB :: String
dBL_ATTR_SCENNLB = #const_str GRB_DBL_ATTR_SCENNLB

-- | lower bound in scenario i
dBL_ATTR_SCENNLB_PTR :: CString
dBL_ATTR_SCENNLB_PTR = #const_cstr GRB_DBL_ATTR_SCENNLB

-- | upper bound in scenario i
dBL_ATTR_SCENNUB :: String
dBL_ATTR_SCENNUB = #const_str GRB_DBL_ATTR_SCENNUB

-- | upper bound in scenario i
dBL_ATTR_SCENNUB_PTR :: CString
dBL_ATTR_SCENNUB_PTR = #const_cstr GRB_DBL_ATTR_SCENNUB

-- | objective in scenario i
dBL_ATTR_SCENNOBJ :: String
dBL_ATTR_SCENNOBJ = #const_str GRB_DBL_ATTR_SCENNOBJ

-- | objective in scenario i
dBL_ATTR_SCENNOBJ_PTR :: CString
dBL_ATTR_SCENNOBJ_PTR = #const_cstr GRB_DBL_ATTR_SCENNOBJ

-- | right hand side in scenario i
dBL_ATTR_SCENNRHS :: String
dBL_ATTR_SCENNRHS = #const_str GRB_DBL_ATTR_SCENNRHS

-- | right hand side in scenario i
dBL_ATTR_SCENNRHS_PTR :: CString
dBL_ATTR_SCENNRHS_PTR = #const_cstr GRB_DBL_ATTR_SCENNRHS

-- | name of scenario i
sTR_ATTR_SCENNNAME :: String
sTR_ATTR_SCENNNAME = #const_str GRB_STR_ATTR_SCENNNAME

-- | name of scenario i
sTR_ATTR_SCENNNAME_PTR :: CString
sTR_ATTR_SCENNNAME_PTR = #const_cstr GRB_STR_ATTR_SCENNNAME

-- | solution value in scenario i
dBL_ATTR_SCENNX :: String
dBL_ATTR_SCENNX = #const_str GRB_DBL_ATTR_SCENNX

-- | solution value in scenario i
dBL_ATTR_SCENNX_PTR :: CString
dBL_ATTR_SCENNX_PTR = #const_cstr GRB_DBL_ATTR_SCENNX

-- | objective bound for scenario i
dBL_ATTR_SCENNOBJBOUND :: String
dBL_ATTR_SCENNOBJBOUND = #const_str GRB_DBL_ATTR_SCENNOBJBOUND

-- | objective bound for scenario i
dBL_ATTR_SCENNOBJBOUND_PTR :: CString
dBL_ATTR_SCENNOBJBOUND_PTR = #const_cstr GRB_DBL_ATTR_SCENNOBJBOUND

-- | objective value for scenario i
dBL_ATTR_SCENNOBJVAL :: String
dBL_ATTR_SCENNOBJVAL = #const_str GRB_DBL_ATTR_SCENNOBJVAL

-- | objective value for scenario i
dBL_ATTR_SCENNOBJVAL_PTR :: CString
dBL_ATTR_SCENNOBJVAL_PTR = #const_cstr GRB_DBL_ATTR_SCENNOBJVAL

-- | number of objectives
iNT_ATTR_NUMOBJ :: String
iNT_ATTR_NUMOBJ = #const_str GRB_INT_ATTR_NUMOBJ

-- | number of objectives
iNT_ATTR_NUMOBJ_PTR :: CString
iNT_ATTR_NUMOBJ_PTR = #const_cstr GRB_INT_ATTR_NUMOBJ

-- | number of scenarios
iNT_ATTR_NUMSCENARIOS :: String
iNT_ATTR_NUMSCENARIOS = #const_str GRB_INT_ATTR_NUMSCENARIOS

-- | number of scenarios
iNT_ATTR_NUMSCENARIOS_PTR :: CString
iNT_ATTR_NUMSCENARIOS_PTR = #const_cstr GRB_INT_ATTR_NUMSCENARIOS

-- | number of MIP starts
iNT_ATTR_NUMSTART :: String
iNT_ATTR_NUMSTART = #const_str GRB_INT_ATTR_NUMSTART

-- | number of MIP starts
iNT_ATTR_NUMSTART_PTR :: CString
iNT_ATTR_NUMSTART_PTR = #const_cstr GRB_INT_ATTR_NUMSTART

-- /* Memory consumption statistics */

-- | current amount of allocated memory (in GB) in master environment
dBL_ATTR_MEMUSED :: String
dBL_ATTR_MEMUSED = #const_str GRB_DBL_ATTR_MEMUSED

-- | current amount of allocated memory (in GB) in master environment
dBL_ATTR_MEMUSED_PTR :: CString
dBL_ATTR_MEMUSED_PTR = #const_cstr GRB_DBL_ATTR_MEMUSED

-- | maximum amount of allocated memory (in GB) in master environment
dBL_ATTR_MAXMEMUSED :: String
dBL_ATTR_MAXMEMUSED = #const_str GRB_DBL_ATTR_MAXMEMUSED

-- | maximum amount of allocated memory (in GB) in master environment
dBL_ATTR_MAXMEMUSED_PTR :: CString
dBL_ATTR_MAXMEMUSED_PTR = #const_cstr GRB_DBL_ATTR_MAXMEMUSED

-- /* Alternate define */

dBL_ATTR_Xn :: String
dBL_ATTR_Xn = #const_str GRB_DBL_ATTR_Xn

dBL_ATTR_Xn_PTR :: CString
dBL_ATTR_Xn_PTR = #const_cstr GRB_DBL_ATTR_Xn

-- /* General constraints */

gENCONSTR_MAX :: CInt
gENCONSTR_MAX = #const GRB_GENCONSTR_MAX

gENCONSTR_MIN :: CInt
gENCONSTR_MIN = #const GRB_GENCONSTR_MIN

gENCONSTR_ABS :: CInt
gENCONSTR_ABS = #const GRB_GENCONSTR_ABS

gENCONSTR_AND :: CInt
gENCONSTR_AND = #const GRB_GENCONSTR_AND

gENCONSTR_OR :: CInt
gENCONSTR_OR = #const GRB_GENCONSTR_OR

gENCONSTR_NORM :: CInt
gENCONSTR_NORM = #const GRB_GENCONSTR_NORM

gENCONSTR_NL :: CInt
gENCONSTR_NL = #const GRB_GENCONSTR_NL

gENCONSTR_INDICATOR :: CInt
gENCONSTR_INDICATOR = #const GRB_GENCONSTR_INDICATOR

gENCONSTR_PWL :: CInt
gENCONSTR_PWL = #const GRB_GENCONSTR_PWL

gENCONSTR_POLY :: CInt
gENCONSTR_POLY = #const GRB_GENCONSTR_POLY

gENCONSTR_EXP :: CInt
gENCONSTR_EXP = #const GRB_GENCONSTR_EXP

gENCONSTR_EXPA :: CInt
gENCONSTR_EXPA = #const GRB_GENCONSTR_EXPA

gENCONSTR_LOG :: CInt
gENCONSTR_LOG = #const GRB_GENCONSTR_LOG

gENCONSTR_LOGA :: CInt
gENCONSTR_LOGA = #const GRB_GENCONSTR_LOGA

gENCONSTR_POW :: CInt
gENCONSTR_POW = #const GRB_GENCONSTR_POW

gENCONSTR_SIN :: CInt
gENCONSTR_SIN = #const GRB_GENCONSTR_SIN

gENCONSTR_COS :: CInt
gENCONSTR_COS = #const GRB_GENCONSTR_COS

gENCONSTR_TAN :: CInt
gENCONSTR_TAN = #const GRB_GENCONSTR_TAN

gENCONSTR_LOGISTIC :: CInt
gENCONSTR_LOGISTIC = #const GRB_GENCONSTR_LOGISTIC

nUMGENCONSTYPES :: Integral a => a
nUMGENCONSTYPES = 19

-- /* Operation codes for genconstrNL */

oPCODE_CONSTANT :: CInt
oPCODE_CONSTANT = #const GRB_OPCODE_CONSTANT

oPCODE_VARIABLE :: CInt
oPCODE_VARIABLE = #const GRB_OPCODE_VARIABLE

oPCODE_PLUS :: CInt
oPCODE_PLUS = #const GRB_OPCODE_PLUS

oPCODE_MINUS :: CInt
oPCODE_MINUS = #const GRB_OPCODE_MINUS

oPCODE_MULTIPLY :: CInt
oPCODE_MULTIPLY = #const GRB_OPCODE_MULTIPLY

oPCODE_DIVIDE :: CInt
oPCODE_DIVIDE = #const GRB_OPCODE_DIVIDE

oPCODE_UMINUS :: CInt
oPCODE_UMINUS = #const GRB_OPCODE_UMINUS

oPCODE_SQUARE :: CInt
oPCODE_SQUARE = #const GRB_OPCODE_SQUARE

oPCODE_SQRT :: CInt
oPCODE_SQRT = #const GRB_OPCODE_SQRT

oPCODE_SIN :: CInt
oPCODE_SIN = #const GRB_OPCODE_SIN

oPCODE_COS :: CInt
oPCODE_COS = #const GRB_OPCODE_COS

oPCODE_TAN :: CInt
oPCODE_TAN = #const GRB_OPCODE_TAN

oPCODE_POW :: CInt
oPCODE_POW = #const GRB_OPCODE_POW

oPCODE_EXP :: CInt
oPCODE_EXP = #const GRB_OPCODE_EXP

oPCODE_LOG :: CInt
oPCODE_LOG = #const GRB_OPCODE_LOG

oPCODE_LOG2 :: CInt
oPCODE_LOG2 = #const GRB_OPCODE_LOG2

oPCODE_LOG10 :: CInt
oPCODE_LOG10 = #const GRB_OPCODE_LOG10

oPCODE_LOGISTIC :: CInt
oPCODE_LOGISTIC = #const GRB_OPCODE_LOGISTIC

-- /*
--    CALLBACKS
-- */

-- /* For callback */

cB_POLLING :: CInt
cB_POLLING = #const GRB_CB_POLLING

cB_PRESOLVE :: CInt
cB_PRESOLVE = #const GRB_CB_PRESOLVE

cB_SIMPLEX :: CInt
cB_SIMPLEX = #const GRB_CB_SIMPLEX

cB_MIP :: CInt
cB_MIP = #const GRB_CB_MIP

cB_MIPSOL :: CInt
cB_MIPSOL = #const GRB_CB_MIPSOL

cB_MIPNODE :: CInt
cB_MIPNODE = #const GRB_CB_MIPNODE

cB_MESSAGE :: CInt
cB_MESSAGE = #const GRB_CB_MESSAGE

cB_BARRIER :: CInt
cB_BARRIER = #const GRB_CB_BARRIER

cB_MULTIOBJ :: CInt
cB_MULTIOBJ = #const GRB_CB_MULTIOBJ

cB_IIS :: CInt
cB_IIS = #const GRB_CB_IIS

-- /* Supported names for callback */

cB_PRE_COLDEL :: CInt
cB_PRE_COLDEL = #const GRB_CB_PRE_COLDEL

cB_PRE_ROWDEL :: CInt
cB_PRE_ROWDEL = #const GRB_CB_PRE_ROWDEL

cB_PRE_SENCHG :: CInt
cB_PRE_SENCHG = #const GRB_CB_PRE_SENCHG

cB_PRE_BNDCHG :: CInt
cB_PRE_BNDCHG = #const GRB_CB_PRE_BNDCHG

cB_PRE_COECHG :: CInt
cB_PRE_COECHG = #const GRB_CB_PRE_COECHG

cB_SPX_ITRCNT :: CInt
cB_SPX_ITRCNT = #const GRB_CB_SPX_ITRCNT

cB_SPX_OBJVAL :: CInt
cB_SPX_OBJVAL = #const GRB_CB_SPX_OBJVAL

cB_SPX_PRIMINF :: CInt
cB_SPX_PRIMINF = #const GRB_CB_SPX_PRIMINF

cB_SPX_DUALINF :: CInt
cB_SPX_DUALINF = #const GRB_CB_SPX_DUALINF

cB_SPX_ISPERT :: CInt
cB_SPX_ISPERT = #const GRB_CB_SPX_ISPERT

cB_MIP_OBJBST :: CInt
cB_MIP_OBJBST = #const GRB_CB_MIP_OBJBST

cB_MIP_OBJBND :: CInt
cB_MIP_OBJBND = #const GRB_CB_MIP_OBJBND

cB_MIP_NODCNT :: CInt
cB_MIP_NODCNT = #const GRB_CB_MIP_NODCNT

cB_MIP_SOLCNT :: CInt
cB_MIP_SOLCNT = #const GRB_CB_MIP_SOLCNT

cB_MIP_CUTCNT :: CInt
cB_MIP_CUTCNT = #const GRB_CB_MIP_CUTCNT

cB_MIP_NODLFT :: CInt
cB_MIP_NODLFT = #const GRB_CB_MIP_NODLFT

cB_MIP_ITRCNT :: CInt
cB_MIP_ITRCNT = #const GRB_CB_MIP_ITRCNT

-- |
--
-- if single objective is an LP we
-- still do not have a "_OBJVAL", the
-- user can query the _OBJBST/_OBJBND
-- values instead
cB_MULTIOBJ_OBJBST :: CInt
cB_MULTIOBJ_OBJBST = #const GRB_CB_MULTIOBJ_OBJBST

cB_MULTIOBJ_OBJBND :: CInt
cB_MULTIOBJ_OBJBND = #const GRB_CB_MULTIOBJ_OBJBND

cB_MULTIOBJ_STATUS :: CInt
cB_MULTIOBJ_STATUS = #const GRB_CB_MULTIOBJ_STATUS

cB_MULTIOBJ_MIPGAP :: CInt
cB_MULTIOBJ_MIPGAP = #const GRB_CB_MULTIOBJ_MIPGAP

cB_MULTIOBJ_NODCNT :: CInt
cB_MULTIOBJ_NODCNT = #const GRB_CB_MULTIOBJ_NODCNT

cB_MULTIOBJ_NODLFT :: CInt
cB_MULTIOBJ_NODLFT = #const GRB_CB_MULTIOBJ_NODLFT

cB_MULTIOBJ_RUNTIME :: CInt
cB_MULTIOBJ_RUNTIME = #const GRB_CB_MULTIOBJ_RUNTIME

cB_MULTIOBJ_WORK :: CInt
cB_MULTIOBJ_WORK = #const GRB_CB_MULTIOBJ_WORK

{-
TODO maybe we should also support, think about in MIP/LP cases if not applicable

cB_MULTIOBJ_PRIMINF :: CInt
cB_MULTIOBJ_PRIMINF = #const GRB_CB_MULTIOBJ_PRIMINF

cB_MULTIOBJ_DUALINF :: CInt
cB_MULTIOBJ_DUALINF = #const GRB_CB_MULTIOBJ_DUALINF

cB_MULTIOBJ_ISPERT :: CInt
cB_MULTIOBJ_ISPERT = #const GRB_CB_MULTIOBJ_ISPERT
-}

cB_IIS_CONSTRMIN :: CInt
cB_IIS_CONSTRMIN = #const GRB_CB_IIS_CONSTRMIN

cB_IIS_CONSTRMAX :: CInt
cB_IIS_CONSTRMAX = #const GRB_CB_IIS_CONSTRMAX

cB_IIS_CONSTRGUESS :: CInt
cB_IIS_CONSTRGUESS = #const GRB_CB_IIS_CONSTRGUESS

cB_IIS_BOUNDMIN :: CInt
cB_IIS_BOUNDMIN = #const GRB_CB_IIS_BOUNDMIN

cB_IIS_BOUNDMAX :: CInt
cB_IIS_BOUNDMAX = #const GRB_CB_IIS_BOUNDMAX

cB_IIS_BOUNDGUESS :: CInt
cB_IIS_BOUNDGUESS = #const GRB_CB_IIS_BOUNDGUESS

-- /* FeasRelax method parameter values */

fEASRELAX_LINEAR :: CInt
fEASRELAX_LINEAR = #const GRB_FEASRELAX_LINEAR

fEASRELAX_QUADRATIC :: CInt
fEASRELAX_QUADRATIC = #const GRB_FEASRELAX_QUADRATIC

fEASRELAX_CARDINALITY :: CInt
fEASRELAX_CARDINALITY = #const GRB_FEASRELAX_CARDINALITY

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

-- typedef struct _GRBsvec
-- {
--   int     len; /* sparse vector length. -1: It is a dense vector. */
--   int    *ind; /* indices array of the sparse vector */
--   double *val; /* value array of the sparse vector */
-- } GRBsvec;

data SVec_
type SVec = Ptr SVec_

-- int __stdcall
--   GRBFSolve(GRBmodel *model, GRBsvec *b, GRBsvec *x);
foreign import stdcall safe "GRBFSolve" fsolve
  :: Model -- ^ model
  -> SVec -- ^ b
  -> SVec -- ^ x
  -> IO ErrorCode

-- int __stdcall
--   GRBBinvColj(GRBmodel *model, int j, GRBsvec *x);
foreign import stdcall safe "GRBBinvColj" binvColj
  :: Model -- ^ model
  -> SVec -- ^ j
  -> SVec -- ^ x
  -> IO ErrorCode

-- int __stdcall
--   GRBBinvj(GRBmodel *model, int j, GRBsvec *x);
foreign import stdcall safe "GRBBinvj" binvj
  :: Model -- ^ model
  -> SVec -- ^ j
  -> SVec -- ^ x
  -> IO ErrorCode

-- int __stdcall
--   GRBBSolve(GRBmodel *model, GRBsvec *b, GRBsvec *x);
foreign import stdcall safe "GRBBSolve" bsolve
  :: Model -- ^ model
  -> SVec -- ^ b
  -> SVec -- ^ x
  -> IO ErrorCode

-- int __stdcall
--   GRBBinvi(GRBmodel *model, int i, GRBsvec *x);
foreign import stdcall safe "GRBBinvi" binvi
  :: Model -- ^ model
  -> CInt -- ^ i
  -> SVec -- ^ x
  -> IO ErrorCode

-- int __stdcall
--   GRBBinvRowi(GRBmodel *model, int i, GRBsvec *x);
foreign import stdcall safe "GRBBinvRowi" binvRowi
  :: Model -- ^ model
  -> CInt -- ^ i
  -> SVec -- ^ x
  -> IO ErrorCode

-- int __stdcall
--   GRBgetBasisHead(GRBmodel *model, int *bhead);
foreign import stdcall safe "GRBgetBasisHead" getBasisHead
  :: Model -- ^ model
  -> CInt -- ^ bhead
  -> IO ErrorCode

-- int __stdcall
--   GRBcbstoponemultiobj(GRBmodel *model, void *cbdata, int objnum);
foreign import stdcall safe "GRBcbstoponemultiobj" cbstoponemultiobj
  :: Model -- ^ model
  -> CBData -- ^ cbdata
  -> CInt -- ^ objnum
  -> IO ErrorCode

-- int __stdcall
--   GRBsingularvectors(GRBmodel *model, double *left, double *right);
foreign import stdcall safe "GRBsingularvectors" singularvectors
  :: Model -- ^ model
  -> Ptr Double -- ^ left
  -> Ptr Double -- ^ right
  -> IO ErrorCode

-- /* Model status codes */

lOADED :: CInt
lOADED = #const GRB_LOADED

oPTIMAL :: CInt
oPTIMAL = #const GRB_OPTIMAL

iNFEASIBLE :: CInt
iNFEASIBLE = #const GRB_INFEASIBLE

iNF_OR_UNBD :: CInt
iNF_OR_UNBD = #const GRB_INF_OR_UNBD

uNBOUNDED :: CInt
uNBOUNDED = #const GRB_UNBOUNDED

cUTOFF :: CInt
cUTOFF = #const GRB_CUTOFF

iTERATION_LIMIT :: CInt
iTERATION_LIMIT = #const GRB_ITERATION_LIMIT

nODE_LIMIT :: CInt
nODE_LIMIT = #const GRB_NODE_LIMIT

tIME_LIMIT :: CInt
tIME_LIMIT = #const GRB_TIME_LIMIT

sOLUTION_LIMIT :: CInt
sOLUTION_LIMIT = #const GRB_SOLUTION_LIMIT

iNTERRUPTED :: CInt
iNTERRUPTED = #const GRB_INTERRUPTED

nUMERIC :: CInt
nUMERIC = #const GRB_NUMERIC

sUBOPTIMAL :: CInt
sUBOPTIMAL = #const GRB_SUBOPTIMAL

iNPROGRESS :: CInt
iNPROGRESS = #const GRB_INPROGRESS

uSER_OBJ_LIMIT :: CInt
uSER_OBJ_LIMIT = #const GRB_USER_OBJ_LIMIT

wORK_LIMIT :: CInt
wORK_LIMIT = #const GRB_WORK_LIMIT

mEM_LIMIT :: CInt
mEM_LIMIT = #const GRB_MEM_LIMIT

-- /* Basis status info */

bASIC :: CInt
bASIC = #const GRB_BASIC

nONBASIC_LOWER :: CInt
nONBASIC_LOWER = #const GRB_NONBASIC_LOWER

nONBASIC_UPPER :: CInt
nONBASIC_UPPER = #const GRB_NONBASIC_UPPER

sUPERBASIC :: CInt
sUPERBASIC = #const GRB_SUPERBASIC

-- /* Undocumented routines */

-- int __stdcall
--   GRBstrongbranch(GRBmodel *model, int num, int *cand,
--                   double *downobjbd, double *upobjbd, int *statusP);
foreign import stdcall safe "GRBstrongbranch" strongbranch
  :: Model -- ^ model
  -> CInt -- ^ num
  -> CInt -- ^ cand
  -> Ptr CDouble -- ^ downobjbd
  -> Ptr CDouble -- ^ upobjbd
  -> Ptr CInt -- ^ statusP
  -> IO ErrorCode

-- /**************/
-- /* Parameters */
-- /**************/

-- /* Termination */

iNT_PAR_BARITERLIMIT :: String
iNT_PAR_BARITERLIMIT = #const_str GRB_INT_PAR_BARITERLIMIT

iNT_PAR_BARITERLIMIT_PTR :: CString
iNT_PAR_BARITERLIMIT_PTR = #const_cstr GRB_INT_PAR_BARITERLIMIT

dBL_PAR_CUTOFF :: String
dBL_PAR_CUTOFF = #const_str GRB_DBL_PAR_CUTOFF

dBL_PAR_CUTOFF_PTR :: CString
dBL_PAR_CUTOFF_PTR = #const_cstr GRB_DBL_PAR_CUTOFF

dBL_PAR_ITERATIONLIMIT :: String
dBL_PAR_ITERATIONLIMIT = #const_str GRB_DBL_PAR_ITERATIONLIMIT

dBL_PAR_ITERATIONLIMIT_PTR :: CString
dBL_PAR_ITERATIONLIMIT_PTR = #const_cstr GRB_DBL_PAR_ITERATIONLIMIT

dBL_PAR_NODELIMIT :: String
dBL_PAR_NODELIMIT = #const_str GRB_DBL_PAR_NODELIMIT

dBL_PAR_NODELIMIT_PTR :: CString
dBL_PAR_NODELIMIT_PTR = #const_cstr GRB_DBL_PAR_NODELIMIT

iNT_PAR_SOLUTIONLIMIT :: String
iNT_PAR_SOLUTIONLIMIT = #const_str GRB_INT_PAR_SOLUTIONLIMIT

iNT_PAR_SOLUTIONLIMIT_PTR :: CString
iNT_PAR_SOLUTIONLIMIT_PTR = #const_cstr GRB_INT_PAR_SOLUTIONLIMIT

dBL_PAR_TIMELIMIT :: String
dBL_PAR_TIMELIMIT = #const_str GRB_DBL_PAR_TIMELIMIT

dBL_PAR_TIMELIMIT_PTR :: CString
dBL_PAR_TIMELIMIT_PTR = #const_cstr GRB_DBL_PAR_TIMELIMIT

dBL_PAR_WORKLIMIT :: String
dBL_PAR_WORKLIMIT = #const_str GRB_DBL_PAR_WORKLIMIT

dBL_PAR_WORKLIMIT_PTR :: CString
dBL_PAR_WORKLIMIT_PTR = #const_cstr GRB_DBL_PAR_WORKLIMIT

dBL_PAR_MEMLIMIT :: String
dBL_PAR_MEMLIMIT = #const_str GRB_DBL_PAR_MEMLIMIT

dBL_PAR_MEMLIMIT_PTR :: CString
dBL_PAR_MEMLIMIT_PTR = #const_cstr GRB_DBL_PAR_MEMLIMIT

dBL_PAR_SOFTMEMLIMIT :: String
dBL_PAR_SOFTMEMLIMIT = #const_str GRB_DBL_PAR_SOFTMEMLIMIT

dBL_PAR_SOFTMEMLIMIT_PTR :: CString
dBL_PAR_SOFTMEMLIMIT_PTR = #const_cstr GRB_DBL_PAR_SOFTMEMLIMIT

dBL_PAR_BESTOBJSTOP :: String
dBL_PAR_BESTOBJSTOP = #const_str GRB_DBL_PAR_BESTOBJSTOP

dBL_PAR_BESTOBJSTOP_PTR :: CString
dBL_PAR_BESTOBJSTOP_PTR = #const_cstr GRB_DBL_PAR_BESTOBJSTOP

dBL_PAR_BESTBDSTOP :: String
dBL_PAR_BESTBDSTOP = #const_str GRB_DBL_PAR_BESTBDSTOP

dBL_PAR_BESTBDSTOP_PTR :: CString
dBL_PAR_BESTBDSTOP_PTR = #const_cstr GRB_DBL_PAR_BESTBDSTOP

-- /* Tolerances */

dBL_PAR_FEASIBILITYTOL :: String
dBL_PAR_FEASIBILITYTOL = #const_str GRB_DBL_PAR_FEASIBILITYTOL

dBL_PAR_FEASIBILITYTOL_PTR :: CString
dBL_PAR_FEASIBILITYTOL_PTR = #const_cstr GRB_DBL_PAR_FEASIBILITYTOL

dBL_PAR_INTFEASTOL :: String
dBL_PAR_INTFEASTOL = #const_str GRB_DBL_PAR_INTFEASTOL

dBL_PAR_INTFEASTOL_PTR :: CString
dBL_PAR_INTFEASTOL_PTR = #const_cstr GRB_DBL_PAR_INTFEASTOL

dBL_PAR_MARKOWITZTOL :: String
dBL_PAR_MARKOWITZTOL = #const_str GRB_DBL_PAR_MARKOWITZTOL

dBL_PAR_MARKOWITZTOL_PTR :: CString
dBL_PAR_MARKOWITZTOL_PTR = #const_cstr GRB_DBL_PAR_MARKOWITZTOL

dBL_PAR_MIPGAP :: String
dBL_PAR_MIPGAP = #const_str GRB_DBL_PAR_MIPGAP

dBL_PAR_MIPGAP_PTR :: CString
dBL_PAR_MIPGAP_PTR = #const_cstr GRB_DBL_PAR_MIPGAP

dBL_PAR_MIPGAPABS :: String
dBL_PAR_MIPGAPABS = #const_str GRB_DBL_PAR_MIPGAPABS

dBL_PAR_MIPGAPABS_PTR :: CString
dBL_PAR_MIPGAPABS_PTR = #const_cstr GRB_DBL_PAR_MIPGAPABS

dBL_PAR_OPTIMALITYTOL :: String
dBL_PAR_OPTIMALITYTOL = #const_str GRB_DBL_PAR_OPTIMALITYTOL

dBL_PAR_OPTIMALITYTOL_PTR :: CString
dBL_PAR_OPTIMALITYTOL_PTR = #const_cstr GRB_DBL_PAR_OPTIMALITYTOL

dBL_PAR_PSDTOL :: String
dBL_PAR_PSDTOL = #const_str GRB_DBL_PAR_PSDTOL

dBL_PAR_PSDTOL_PTR :: CString
dBL_PAR_PSDTOL_PTR = #const_cstr GRB_DBL_PAR_PSDTOL

-- /* Simplex */

iNT_PAR_METHOD :: String
iNT_PAR_METHOD = #const_str GRB_INT_PAR_METHOD

iNT_PAR_METHOD_PTR :: CString
iNT_PAR_METHOD_PTR = #const_cstr GRB_INT_PAR_METHOD

iNT_PAR_CONCURRENTMETHOD :: String
iNT_PAR_CONCURRENTMETHOD = #const_str GRB_INT_PAR_CONCURRENTMETHOD

iNT_PAR_CONCURRENTMETHOD_PTR :: CString
iNT_PAR_CONCURRENTMETHOD_PTR = #const_cstr GRB_INT_PAR_CONCURRENTMETHOD

dBL_PAR_PERTURBVALUE :: String
dBL_PAR_PERTURBVALUE = #const_str GRB_DBL_PAR_PERTURBVALUE

dBL_PAR_PERTURBVALUE_PTR :: CString
dBL_PAR_PERTURBVALUE_PTR = #const_cstr GRB_DBL_PAR_PERTURBVALUE

dBL_PAR_OBJSCALE :: String
dBL_PAR_OBJSCALE = #const_str GRB_DBL_PAR_OBJSCALE

dBL_PAR_OBJSCALE_PTR :: CString
dBL_PAR_OBJSCALE_PTR = #const_cstr GRB_DBL_PAR_OBJSCALE

iNT_PAR_SCALEFLAG :: String
iNT_PAR_SCALEFLAG = #const_str GRB_INT_PAR_SCALEFLAG

iNT_PAR_SCALEFLAG_PTR :: CString
iNT_PAR_SCALEFLAG_PTR = #const_cstr GRB_INT_PAR_SCALEFLAG

iNT_PAR_SIMPLEXPRICING :: String
iNT_PAR_SIMPLEXPRICING = #const_str GRB_INT_PAR_SIMPLEXPRICING

iNT_PAR_SIMPLEXPRICING_PTR :: CString
iNT_PAR_SIMPLEXPRICING_PTR = #const_cstr GRB_INT_PAR_SIMPLEXPRICING

iNT_PAR_QUAD :: String
iNT_PAR_QUAD = #const_str GRB_INT_PAR_QUAD

iNT_PAR_QUAD_PTR :: CString
iNT_PAR_QUAD_PTR = #const_cstr GRB_INT_PAR_QUAD

iNT_PAR_NORMADJUST :: String
iNT_PAR_NORMADJUST = #const_str GRB_INT_PAR_NORMADJUST

iNT_PAR_NORMADJUST_PTR :: CString
iNT_PAR_NORMADJUST_PTR = #const_cstr GRB_INT_PAR_NORMADJUST

iNT_PAR_SIFTING :: String
iNT_PAR_SIFTING = #const_str GRB_INT_PAR_SIFTING

iNT_PAR_SIFTING_PTR :: CString
iNT_PAR_SIFTING_PTR = #const_cstr GRB_INT_PAR_SIFTING

iNT_PAR_SIFTMETHOD :: String
iNT_PAR_SIFTMETHOD = #const_str GRB_INT_PAR_SIFTMETHOD

iNT_PAR_SIFTMETHOD_PTR :: CString
iNT_PAR_SIFTMETHOD_PTR = #const_cstr GRB_INT_PAR_SIFTMETHOD

iNT_PAR_LPWARMSTART :: String
iNT_PAR_LPWARMSTART = #const_str GRB_INT_PAR_LPWARMSTART

iNT_PAR_LPWARMSTART_PTR :: CString
iNT_PAR_LPWARMSTART_PTR = #const_cstr GRB_INT_PAR_LPWARMSTART

iNT_PAR_NETWORKALG :: String
iNT_PAR_NETWORKALG = #const_str GRB_INT_PAR_NETWORKALG

iNT_PAR_NETWORKALG_PTR :: CString
iNT_PAR_NETWORKALG_PTR = #const_cstr GRB_INT_PAR_NETWORKALG

-- /* Barrier */

dBL_PAR_BARCONVTOL :: String
dBL_PAR_BARCONVTOL = #const_str GRB_DBL_PAR_BARCONVTOL

dBL_PAR_BARCONVTOL_PTR :: CString
dBL_PAR_BARCONVTOL_PTR = #const_cstr GRB_DBL_PAR_BARCONVTOL

iNT_PAR_BARCORRECTORS :: String
iNT_PAR_BARCORRECTORS = #const_str GRB_INT_PAR_BARCORRECTORS

iNT_PAR_BARCORRECTORS_PTR :: CString
iNT_PAR_BARCORRECTORS_PTR = #const_cstr GRB_INT_PAR_BARCORRECTORS

iNT_PAR_BARHOMOGENEOUS :: String
iNT_PAR_BARHOMOGENEOUS = #const_str GRB_INT_PAR_BARHOMOGENEOUS

iNT_PAR_BARHOMOGENEOUS_PTR :: CString
iNT_PAR_BARHOMOGENEOUS_PTR = #const_cstr GRB_INT_PAR_BARHOMOGENEOUS

iNT_PAR_BARORDER :: String
iNT_PAR_BARORDER = #const_str GRB_INT_PAR_BARORDER

iNT_PAR_BARORDER_PTR :: CString
iNT_PAR_BARORDER_PTR = #const_cstr GRB_INT_PAR_BARORDER

dBL_PAR_BARQCPCONVTOL :: String
dBL_PAR_BARQCPCONVTOL = #const_str GRB_DBL_PAR_BARQCPCONVTOL

dBL_PAR_BARQCPCONVTOL_PTR :: CString
dBL_PAR_BARQCPCONVTOL_PTR = #const_cstr GRB_DBL_PAR_BARQCPCONVTOL

iNT_PAR_CROSSOVER :: String
iNT_PAR_CROSSOVER = #const_str GRB_INT_PAR_CROSSOVER

iNT_PAR_CROSSOVER_PTR :: CString
iNT_PAR_CROSSOVER_PTR = #const_cstr GRB_INT_PAR_CROSSOVER

iNT_PAR_CROSSOVERBASIS :: String
iNT_PAR_CROSSOVERBASIS = #const_str GRB_INT_PAR_CROSSOVERBASIS

iNT_PAR_CROSSOVERBASIS_PTR :: CString
iNT_PAR_CROSSOVERBASIS_PTR = #const_cstr GRB_INT_PAR_CROSSOVERBASIS

-- /* MIP */

iNT_PAR_BRANCHDIR :: String
iNT_PAR_BRANCHDIR = #const_str GRB_INT_PAR_BRANCHDIR

iNT_PAR_BRANCHDIR_PTR :: CString
iNT_PAR_BRANCHDIR_PTR = #const_cstr GRB_INT_PAR_BRANCHDIR

iNT_PAR_DEGENMOVES :: String
iNT_PAR_DEGENMOVES = #const_str GRB_INT_PAR_DEGENMOVES

iNT_PAR_DEGENMOVES_PTR :: CString
iNT_PAR_DEGENMOVES_PTR = #const_cstr GRB_INT_PAR_DEGENMOVES

iNT_PAR_DISCONNECTED :: String
iNT_PAR_DISCONNECTED = #const_str GRB_INT_PAR_DISCONNECTED

iNT_PAR_DISCONNECTED_PTR :: CString
iNT_PAR_DISCONNECTED_PTR = #const_cstr GRB_INT_PAR_DISCONNECTED

dBL_PAR_HEURISTICS :: String
dBL_PAR_HEURISTICS = #const_str GRB_DBL_PAR_HEURISTICS

dBL_PAR_HEURISTICS_PTR :: CString
dBL_PAR_HEURISTICS_PTR = #const_cstr GRB_DBL_PAR_HEURISTICS

dBL_PAR_IMPROVESTARTGAP :: String
dBL_PAR_IMPROVESTARTGAP = #const_str GRB_DBL_PAR_IMPROVESTARTGAP

dBL_PAR_IMPROVESTARTGAP_PTR :: CString
dBL_PAR_IMPROVESTARTGAP_PTR = #const_cstr GRB_DBL_PAR_IMPROVESTARTGAP

dBL_PAR_IMPROVESTARTTIME :: String
dBL_PAR_IMPROVESTARTTIME = #const_str GRB_DBL_PAR_IMPROVESTARTTIME

dBL_PAR_IMPROVESTARTTIME_PTR :: CString
dBL_PAR_IMPROVESTARTTIME_PTR = #const_cstr GRB_DBL_PAR_IMPROVESTARTTIME

dBL_PAR_IMPROVESTARTNODES :: String
dBL_PAR_IMPROVESTARTNODES = #const_str GRB_DBL_PAR_IMPROVESTARTNODES

dBL_PAR_IMPROVESTARTNODES_PTR :: CString
dBL_PAR_IMPROVESTARTNODES_PTR = #const_cstr GRB_DBL_PAR_IMPROVESTARTNODES

iNT_PAR_INTEGRALITYFOCUS :: String
iNT_PAR_INTEGRALITYFOCUS = #const_str GRB_INT_PAR_INTEGRALITYFOCUS

iNT_PAR_INTEGRALITYFOCUS_PTR :: CString
iNT_PAR_INTEGRALITYFOCUS_PTR = #const_cstr GRB_INT_PAR_INTEGRALITYFOCUS

iNT_PAR_MINRELNODES :: String
iNT_PAR_MINRELNODES = #const_str GRB_INT_PAR_MINRELNODES

iNT_PAR_MINRELNODES_PTR :: CString
iNT_PAR_MINRELNODES_PTR = #const_cstr GRB_INT_PAR_MINRELNODES

iNT_PAR_MIPFOCUS :: String
iNT_PAR_MIPFOCUS = #const_str GRB_INT_PAR_MIPFOCUS

iNT_PAR_MIPFOCUS_PTR :: CString
iNT_PAR_MIPFOCUS_PTR = #const_cstr GRB_INT_PAR_MIPFOCUS

iNT_PAR_NLPHEUR :: String
iNT_PAR_NLPHEUR = #const_str GRB_INT_PAR_NLPHEUR

iNT_PAR_NLPHEUR_PTR :: CString
iNT_PAR_NLPHEUR_PTR = #const_cstr GRB_INT_PAR_NLPHEUR

sTR_PAR_NODEFILEDIR :: String
sTR_PAR_NODEFILEDIR = #const_str GRB_STR_PAR_NODEFILEDIR

sTR_PAR_NODEFILEDIR_PTR :: CString
sTR_PAR_NODEFILEDIR_PTR = #const_cstr GRB_STR_PAR_NODEFILEDIR

dBL_PAR_NODEFILESTART :: String
dBL_PAR_NODEFILESTART = #const_str GRB_DBL_PAR_NODEFILESTART

dBL_PAR_NODEFILESTART_PTR :: CString
dBL_PAR_NODEFILESTART_PTR = #const_cstr GRB_DBL_PAR_NODEFILESTART

iNT_PAR_NODEMETHOD :: String
iNT_PAR_NODEMETHOD = #const_str GRB_INT_PAR_NODEMETHOD

iNT_PAR_NODEMETHOD_PTR :: CString
iNT_PAR_NODEMETHOD_PTR = #const_cstr GRB_INT_PAR_NODEMETHOD

dBL_PAR_NORELHEURTIME :: String
dBL_PAR_NORELHEURTIME = #const_str GRB_DBL_PAR_NORELHEURTIME

dBL_PAR_NORELHEURTIME_PTR :: CString
dBL_PAR_NORELHEURTIME_PTR = #const_cstr GRB_DBL_PAR_NORELHEURTIME

dBL_PAR_NORELHEURWORK :: String
dBL_PAR_NORELHEURWORK = #const_str GRB_DBL_PAR_NORELHEURWORK

dBL_PAR_NORELHEURWORK_PTR :: CString
dBL_PAR_NORELHEURWORK_PTR = #const_cstr GRB_DBL_PAR_NORELHEURWORK

iNT_PAR_OBBT :: String
iNT_PAR_OBBT = #const_str GRB_INT_PAR_OBBT

iNT_PAR_OBBT_PTR :: CString
iNT_PAR_OBBT_PTR = #const_cstr GRB_INT_PAR_OBBT

iNT_PAR_PUMPPASSES :: String
iNT_PAR_PUMPPASSES = #const_str GRB_INT_PAR_PUMPPASSES

iNT_PAR_PUMPPASSES_PTR :: CString
iNT_PAR_PUMPPASSES_PTR = #const_cstr GRB_INT_PAR_PUMPPASSES

iNT_PAR_RINS :: String
iNT_PAR_RINS = #const_str GRB_INT_PAR_RINS

iNT_PAR_RINS_PTR :: CString
iNT_PAR_RINS_PTR = #const_cstr GRB_INT_PAR_RINS

sTR_PAR_SOLFILES :: String
sTR_PAR_SOLFILES = #const_str GRB_STR_PAR_SOLFILES

sTR_PAR_SOLFILES_PTR :: CString
sTR_PAR_SOLFILES_PTR = #const_cstr GRB_STR_PAR_SOLFILES

iNT_PAR_STARTNODELIMIT :: String
iNT_PAR_STARTNODELIMIT = #const_str GRB_INT_PAR_STARTNODELIMIT

iNT_PAR_STARTNODELIMIT_PTR :: CString
iNT_PAR_STARTNODELIMIT_PTR = #const_cstr GRB_INT_PAR_STARTNODELIMIT

iNT_PAR_SUBMIPNODES :: String
iNT_PAR_SUBMIPNODES = #const_str GRB_INT_PAR_SUBMIPNODES

iNT_PAR_SUBMIPNODES_PTR :: CString
iNT_PAR_SUBMIPNODES_PTR = #const_cstr GRB_INT_PAR_SUBMIPNODES

iNT_PAR_SYMMETRY :: String
iNT_PAR_SYMMETRY = #const_str GRB_INT_PAR_SYMMETRY

iNT_PAR_SYMMETRY_PTR :: CString
iNT_PAR_SYMMETRY_PTR = #const_cstr GRB_INT_PAR_SYMMETRY

iNT_PAR_VARBRANCH :: String
iNT_PAR_VARBRANCH = #const_str GRB_INT_PAR_VARBRANCH

iNT_PAR_VARBRANCH_PTR :: CString
iNT_PAR_VARBRANCH_PTR = #const_cstr GRB_INT_PAR_VARBRANCH

iNT_PAR_SOLUTIONNUMBER :: String
iNT_PAR_SOLUTIONNUMBER = #const_str GRB_INT_PAR_SOLUTIONNUMBER

iNT_PAR_SOLUTIONNUMBER_PTR :: CString
iNT_PAR_SOLUTIONNUMBER_PTR = #const_cstr GRB_INT_PAR_SOLUTIONNUMBER

iNT_PAR_ZEROOBJNODES :: String
iNT_PAR_ZEROOBJNODES = #const_str GRB_INT_PAR_ZEROOBJNODES

iNT_PAR_ZEROOBJNODES_PTR :: CString
iNT_PAR_ZEROOBJNODES_PTR = #const_cstr GRB_INT_PAR_ZEROOBJNODES

-- /* MIP cuts */

iNT_PAR_CUTS :: String
iNT_PAR_CUTS = #const_str GRB_INT_PAR_CUTS

iNT_PAR_CUTS_PTR :: CString
iNT_PAR_CUTS_PTR = #const_cstr GRB_INT_PAR_CUTS

iNT_PAR_CLIQUECUTS :: String
iNT_PAR_CLIQUECUTS = #const_str GRB_INT_PAR_CLIQUECUTS

iNT_PAR_CLIQUECUTS_PTR :: CString
iNT_PAR_CLIQUECUTS_PTR = #const_cstr GRB_INT_PAR_CLIQUECUTS

iNT_PAR_COVERCUTS :: String
iNT_PAR_COVERCUTS = #const_str GRB_INT_PAR_COVERCUTS

iNT_PAR_COVERCUTS_PTR :: CString
iNT_PAR_COVERCUTS_PTR = #const_cstr GRB_INT_PAR_COVERCUTS

iNT_PAR_FLOWCOVERCUTS :: String
iNT_PAR_FLOWCOVERCUTS = #const_str GRB_INT_PAR_FLOWCOVERCUTS

iNT_PAR_FLOWCOVERCUTS_PTR :: CString
iNT_PAR_FLOWCOVERCUTS_PTR = #const_cstr GRB_INT_PAR_FLOWCOVERCUTS

iNT_PAR_FLOWPATHCUTS :: String
iNT_PAR_FLOWPATHCUTS = #const_str GRB_INT_PAR_FLOWPATHCUTS

iNT_PAR_FLOWPATHCUTS_PTR :: CString
iNT_PAR_FLOWPATHCUTS_PTR = #const_cstr GRB_INT_PAR_FLOWPATHCUTS

iNT_PAR_GUBCOVERCUTS :: String
iNT_PAR_GUBCOVERCUTS = #const_str GRB_INT_PAR_GUBCOVERCUTS

iNT_PAR_GUBCOVERCUTS_PTR :: CString
iNT_PAR_GUBCOVERCUTS_PTR = #const_cstr GRB_INT_PAR_GUBCOVERCUTS

iNT_PAR_IMPLIEDCUTS :: String
iNT_PAR_IMPLIEDCUTS = #const_str GRB_INT_PAR_IMPLIEDCUTS

iNT_PAR_IMPLIEDCUTS_PTR :: CString
iNT_PAR_IMPLIEDCUTS_PTR = #const_cstr GRB_INT_PAR_IMPLIEDCUTS

iNT_PAR_PROJIMPLIEDCUTS :: String
iNT_PAR_PROJIMPLIEDCUTS = #const_str GRB_INT_PAR_PROJIMPLIEDCUTS

iNT_PAR_PROJIMPLIEDCUTS_PTR :: CString
iNT_PAR_PROJIMPLIEDCUTS_PTR = #const_cstr GRB_INT_PAR_PROJIMPLIEDCUTS

iNT_PAR_MIPSEPCUTS :: String
iNT_PAR_MIPSEPCUTS = #const_str GRB_INT_PAR_MIPSEPCUTS

iNT_PAR_MIPSEPCUTS_PTR :: CString
iNT_PAR_MIPSEPCUTS_PTR = #const_cstr GRB_INT_PAR_MIPSEPCUTS

iNT_PAR_MIRCUTS :: String
iNT_PAR_MIRCUTS = #const_str GRB_INT_PAR_MIRCUTS

iNT_PAR_MIRCUTS_PTR :: CString
iNT_PAR_MIRCUTS_PTR = #const_cstr GRB_INT_PAR_MIRCUTS

iNT_PAR_STRONGCGCUTS :: String
iNT_PAR_STRONGCGCUTS = #const_str GRB_INT_PAR_STRONGCGCUTS

iNT_PAR_STRONGCGCUTS_PTR :: CString
iNT_PAR_STRONGCGCUTS_PTR = #const_cstr GRB_INT_PAR_STRONGCGCUTS

iNT_PAR_MODKCUTS :: String
iNT_PAR_MODKCUTS = #const_str GRB_INT_PAR_MODKCUTS

iNT_PAR_MODKCUTS_PTR :: CString
iNT_PAR_MODKCUTS_PTR = #const_cstr GRB_INT_PAR_MODKCUTS

iNT_PAR_ZEROHALFCUTS :: String
iNT_PAR_ZEROHALFCUTS = #const_str GRB_INT_PAR_ZEROHALFCUTS

iNT_PAR_ZEROHALFCUTS_PTR :: CString
iNT_PAR_ZEROHALFCUTS_PTR = #const_cstr GRB_INT_PAR_ZEROHALFCUTS

iNT_PAR_NETWORKCUTS :: String
iNT_PAR_NETWORKCUTS = #const_str GRB_INT_PAR_NETWORKCUTS

iNT_PAR_NETWORKCUTS_PTR :: CString
iNT_PAR_NETWORKCUTS_PTR = #const_cstr GRB_INT_PAR_NETWORKCUTS

iNT_PAR_SUBMIPCUTS :: String
iNT_PAR_SUBMIPCUTS = #const_str GRB_INT_PAR_SUBMIPCUTS

iNT_PAR_SUBMIPCUTS_PTR :: CString
iNT_PAR_SUBMIPCUTS_PTR = #const_cstr GRB_INT_PAR_SUBMIPCUTS

iNT_PAR_INFPROOFCUTS :: String
iNT_PAR_INFPROOFCUTS = #const_str GRB_INT_PAR_INFPROOFCUTS

iNT_PAR_INFPROOFCUTS_PTR :: CString
iNT_PAR_INFPROOFCUTS_PTR = #const_cstr GRB_INT_PAR_INFPROOFCUTS

iNT_PAR_RLTCUTS :: String
iNT_PAR_RLTCUTS = #const_str GRB_INT_PAR_RLTCUTS

iNT_PAR_RLTCUTS_PTR :: CString
iNT_PAR_RLTCUTS_PTR = #const_cstr GRB_INT_PAR_RLTCUTS

iNT_PAR_RELAXLIFTCUTS :: String
iNT_PAR_RELAXLIFTCUTS = #const_str GRB_INT_PAR_RELAXLIFTCUTS

iNT_PAR_RELAXLIFTCUTS_PTR :: CString
iNT_PAR_RELAXLIFTCUTS_PTR = #const_cstr GRB_INT_PAR_RELAXLIFTCUTS

iNT_PAR_BQPCUTS :: String
iNT_PAR_BQPCUTS = #const_str GRB_INT_PAR_BQPCUTS

iNT_PAR_BQPCUTS_PTR :: CString
iNT_PAR_BQPCUTS_PTR = #const_cstr GRB_INT_PAR_BQPCUTS

iNT_PAR_PSDCUTS :: String
iNT_PAR_PSDCUTS = #const_str GRB_INT_PAR_PSDCUTS

iNT_PAR_PSDCUTS_PTR :: CString
iNT_PAR_PSDCUTS_PTR = #const_cstr GRB_INT_PAR_PSDCUTS

iNT_PAR_LIFTPROJECTCUTS :: String
iNT_PAR_LIFTPROJECTCUTS = #const_str GRB_INT_PAR_LIFTPROJECTCUTS

iNT_PAR_LIFTPROJECTCUTS_PTR :: CString
iNT_PAR_LIFTPROJECTCUTS_PTR = #const_cstr GRB_INT_PAR_LIFTPROJECTCUTS

iNT_PAR_MIXINGCUTS :: String
iNT_PAR_MIXINGCUTS = #const_str GRB_INT_PAR_MIXINGCUTS

iNT_PAR_MIXINGCUTS_PTR :: CString
iNT_PAR_MIXINGCUTS_PTR = #const_cstr GRB_INT_PAR_MIXINGCUTS

iNT_PAR_DUALIMPLIEDCUTS :: String
iNT_PAR_DUALIMPLIEDCUTS = #const_str GRB_INT_PAR_DUALIMPLIEDCUTS

iNT_PAR_DUALIMPLIEDCUTS_PTR :: CString
iNT_PAR_DUALIMPLIEDCUTS_PTR = #const_cstr GRB_INT_PAR_DUALIMPLIEDCUTS

iNT_PAR_CUTAGGPASSES :: String
iNT_PAR_CUTAGGPASSES = #const_str GRB_INT_PAR_CUTAGGPASSES

iNT_PAR_CUTAGGPASSES_PTR :: CString
iNT_PAR_CUTAGGPASSES_PTR = #const_cstr GRB_INT_PAR_CUTAGGPASSES

iNT_PAR_CUTPASSES :: String
iNT_PAR_CUTPASSES = #const_str GRB_INT_PAR_CUTPASSES

iNT_PAR_CUTPASSES_PTR :: CString
iNT_PAR_CUTPASSES_PTR = #const_cstr GRB_INT_PAR_CUTPASSES

iNT_PAR_GOMORYPASSES :: String
iNT_PAR_GOMORYPASSES = #const_str GRB_INT_PAR_GOMORYPASSES

iNT_PAR_GOMORYPASSES_PTR :: CString
iNT_PAR_GOMORYPASSES_PTR = #const_cstr GRB_INT_PAR_GOMORYPASSES

-- /* Distributed algorithms */

sTR_PAR_WORKERPOOL :: String
sTR_PAR_WORKERPOOL = #const_str GRB_STR_PAR_WORKERPOOL

sTR_PAR_WORKERPOOL_PTR :: CString
sTR_PAR_WORKERPOOL_PTR = #const_cstr GRB_STR_PAR_WORKERPOOL

sTR_PAR_WORKERPASSWORD :: String
sTR_PAR_WORKERPASSWORD = #const_str GRB_STR_PAR_WORKERPASSWORD

sTR_PAR_WORKERPASSWORD_PTR :: CString
sTR_PAR_WORKERPASSWORD_PTR = #const_cstr GRB_STR_PAR_WORKERPASSWORD

sTR_PAR_COMPUTESERVER :: String
sTR_PAR_COMPUTESERVER = #const_str GRB_STR_PAR_COMPUTESERVER

sTR_PAR_COMPUTESERVER_PTR :: CString
sTR_PAR_COMPUTESERVER_PTR = #const_cstr GRB_STR_PAR_COMPUTESERVER

sTR_PAR_TOKENSERVER :: String
sTR_PAR_TOKENSERVER = #const_str GRB_STR_PAR_TOKENSERVER

sTR_PAR_TOKENSERVER_PTR :: CString
sTR_PAR_TOKENSERVER_PTR = #const_cstr GRB_STR_PAR_TOKENSERVER

sTR_PAR_SERVERPASSWORD :: String
sTR_PAR_SERVERPASSWORD = #const_str GRB_STR_PAR_SERVERPASSWORD

sTR_PAR_SERVERPASSWORD_PTR :: CString
sTR_PAR_SERVERPASSWORD_PTR = #const_cstr GRB_STR_PAR_SERVERPASSWORD

iNT_PAR_SERVERTIMEOUT :: String
iNT_PAR_SERVERTIMEOUT = #const_str GRB_INT_PAR_SERVERTIMEOUT

iNT_PAR_SERVERTIMEOUT_PTR :: CString
iNT_PAR_SERVERTIMEOUT_PTR = #const_cstr GRB_INT_PAR_SERVERTIMEOUT

sTR_PAR_CSROUTER :: String
sTR_PAR_CSROUTER = #const_str GRB_STR_PAR_CSROUTER

sTR_PAR_CSROUTER_PTR :: CString
sTR_PAR_CSROUTER_PTR = #const_cstr GRB_STR_PAR_CSROUTER

sTR_PAR_CSGROUP :: String
sTR_PAR_CSGROUP = #const_str GRB_STR_PAR_CSGROUP

sTR_PAR_CSGROUP_PTR :: CString
sTR_PAR_CSGROUP_PTR = #const_cstr GRB_STR_PAR_CSGROUP

dBL_PAR_CSQUEUETIMEOUT :: String
dBL_PAR_CSQUEUETIMEOUT = #const_str GRB_DBL_PAR_CSQUEUETIMEOUT

dBL_PAR_CSQUEUETIMEOUT_PTR :: CString
dBL_PAR_CSQUEUETIMEOUT_PTR = #const_cstr GRB_DBL_PAR_CSQUEUETIMEOUT

iNT_PAR_CSPRIORITY :: String
iNT_PAR_CSPRIORITY = #const_str GRB_INT_PAR_CSPRIORITY

iNT_PAR_CSPRIORITY_PTR :: CString
iNT_PAR_CSPRIORITY_PTR = #const_cstr GRB_INT_PAR_CSPRIORITY

iNT_PAR_CSIDLETIMEOUT :: String
iNT_PAR_CSIDLETIMEOUT = #const_str GRB_INT_PAR_CSIDLETIMEOUT

iNT_PAR_CSIDLETIMEOUT_PTR :: CString
iNT_PAR_CSIDLETIMEOUT_PTR = #const_cstr GRB_INT_PAR_CSIDLETIMEOUT

iNT_PAR_CSTLSINSECURE :: String
iNT_PAR_CSTLSINSECURE = #const_str GRB_INT_PAR_CSTLSINSECURE

iNT_PAR_CSTLSINSECURE_PTR :: CString
iNT_PAR_CSTLSINSECURE_PTR = #const_cstr GRB_INT_PAR_CSTLSINSECURE

iNT_PAR_TSPORT :: String
iNT_PAR_TSPORT = #const_str GRB_INT_PAR_TSPORT

iNT_PAR_TSPORT_PTR :: CString
iNT_PAR_TSPORT_PTR = #const_cstr GRB_INT_PAR_TSPORT

sTR_PAR_CLOUDACCESSID :: String
sTR_PAR_CLOUDACCESSID = #const_str GRB_STR_PAR_CLOUDACCESSID

sTR_PAR_CLOUDACCESSID_PTR :: CString
sTR_PAR_CLOUDACCESSID_PTR = #const_cstr GRB_STR_PAR_CLOUDACCESSID

sTR_PAR_CLOUDSECRETKEY :: String
sTR_PAR_CLOUDSECRETKEY = #const_str GRB_STR_PAR_CLOUDSECRETKEY

sTR_PAR_CLOUDSECRETKEY_PTR :: CString
sTR_PAR_CLOUDSECRETKEY_PTR = #const_cstr GRB_STR_PAR_CLOUDSECRETKEY

sTR_PAR_CLOUDPOOL :: String
sTR_PAR_CLOUDPOOL = #const_str GRB_STR_PAR_CLOUDPOOL

sTR_PAR_CLOUDPOOL_PTR :: CString
sTR_PAR_CLOUDPOOL_PTR = #const_cstr GRB_STR_PAR_CLOUDPOOL

sTR_PAR_CLOUDHOST :: String
sTR_PAR_CLOUDHOST = #const_str GRB_STR_PAR_CLOUDHOST

sTR_PAR_CLOUDHOST_PTR :: CString
sTR_PAR_CLOUDHOST_PTR = #const_cstr GRB_STR_PAR_CLOUDHOST

sTR_PAR_CSMANAGER :: String
sTR_PAR_CSMANAGER = #const_str GRB_STR_PAR_CSMANAGER

sTR_PAR_CSMANAGER_PTR :: CString
sTR_PAR_CSMANAGER_PTR = #const_cstr GRB_STR_PAR_CSMANAGER

sTR_PAR_CSAUTHTOKEN :: String
sTR_PAR_CSAUTHTOKEN = #const_str GRB_STR_PAR_CSAUTHTOKEN

sTR_PAR_CSAUTHTOKEN_PTR :: CString
sTR_PAR_CSAUTHTOKEN_PTR = #const_cstr GRB_STR_PAR_CSAUTHTOKEN

sTR_PAR_CSAPIACCESSID :: String
sTR_PAR_CSAPIACCESSID = #const_str GRB_STR_PAR_CSAPIACCESSID

sTR_PAR_CSAPIACCESSID_PTR :: CString
sTR_PAR_CSAPIACCESSID_PTR = #const_cstr GRB_STR_PAR_CSAPIACCESSID

sTR_PAR_CSAPISECRET :: String
sTR_PAR_CSAPISECRET = #const_str GRB_STR_PAR_CSAPISECRET

sTR_PAR_CSAPISECRET_PTR :: CString
sTR_PAR_CSAPISECRET_PTR = #const_cstr GRB_STR_PAR_CSAPISECRET

iNT_PAR_CSBATCHMODE :: String
iNT_PAR_CSBATCHMODE = #const_str GRB_INT_PAR_CSBATCHMODE

iNT_PAR_CSBATCHMODE_PTR :: CString
iNT_PAR_CSBATCHMODE_PTR = #const_cstr GRB_INT_PAR_CSBATCHMODE

sTR_PAR_USERNAME :: String
sTR_PAR_USERNAME = #const_str GRB_STR_PAR_USERNAME

sTR_PAR_USERNAME_PTR :: CString
sTR_PAR_USERNAME_PTR = #const_cstr GRB_STR_PAR_USERNAME

sTR_PAR_CSAPPNAME :: String
sTR_PAR_CSAPPNAME = #const_str GRB_STR_PAR_CSAPPNAME

sTR_PAR_CSAPPNAME_PTR :: CString
sTR_PAR_CSAPPNAME_PTR = #const_cstr GRB_STR_PAR_CSAPPNAME

iNT_PAR_CSCLIENTLOG :: String
iNT_PAR_CSCLIENTLOG = #const_str GRB_INT_PAR_CSCLIENTLOG

iNT_PAR_CSCLIENTLOG_PTR :: CString
iNT_PAR_CSCLIENTLOG_PTR = #const_cstr GRB_INT_PAR_CSCLIENTLOG

sTR_PAR_WLSACCESSID :: String
sTR_PAR_WLSACCESSID = #const_str GRB_STR_PAR_WLSACCESSID

sTR_PAR_WLSACCESSID_PTR :: CString
sTR_PAR_WLSACCESSID_PTR = #const_cstr GRB_STR_PAR_WLSACCESSID

sTR_PAR_WLSSECRET :: String
sTR_PAR_WLSSECRET = #const_str GRB_STR_PAR_WLSSECRET

sTR_PAR_WLSSECRET_PTR :: CString
sTR_PAR_WLSSECRET_PTR = #const_cstr GRB_STR_PAR_WLSSECRET

iNT_PAR_WLSTOKENDURATION :: String
iNT_PAR_WLSTOKENDURATION = #const_str GRB_INT_PAR_WLSTOKENDURATION

iNT_PAR_WLSTOKENDURATION_PTR :: CString
iNT_PAR_WLSTOKENDURATION_PTR = #const_cstr GRB_INT_PAR_WLSTOKENDURATION

dBL_PAR_WLSTOKENREFRESH :: String
dBL_PAR_WLSTOKENREFRESH = #const_str GRB_DBL_PAR_WLSTOKENREFRESH

dBL_PAR_WLSTOKENREFRESH_PTR :: CString
dBL_PAR_WLSTOKENREFRESH_PTR = #const_cstr GRB_DBL_PAR_WLSTOKENREFRESH

sTR_PAR_WLSTOKEN :: String
sTR_PAR_WLSTOKEN = #const_str GRB_STR_PAR_WLSTOKEN

sTR_PAR_WLSTOKEN_PTR :: CString
sTR_PAR_WLSTOKEN_PTR = #const_cstr GRB_STR_PAR_WLSTOKEN

iNT_PAR_LICENSEID :: String
iNT_PAR_LICENSEID = #const_str GRB_INT_PAR_LICENSEID

iNT_PAR_LICENSEID_PTR :: CString
iNT_PAR_LICENSEID_PTR = #const_cstr GRB_INT_PAR_LICENSEID

sTR_PAR_WLSPROXY :: String
sTR_PAR_WLSPROXY = #const_str GRB_STR_PAR_WLSPROXY

sTR_PAR_WLSPROXY_PTR :: CString
sTR_PAR_WLSPROXY_PTR = #const_cstr GRB_STR_PAR_WLSPROXY

sTR_PAR_WLSCONFIG :: String
sTR_PAR_WLSCONFIG = #const_str GRB_STR_PAR_WLSCONFIG

sTR_PAR_WLSCONFIG_PTR :: CString
sTR_PAR_WLSCONFIG_PTR = #const_cstr GRB_STR_PAR_WLSCONFIG

-- /* Other */

iNT_PAR_AGGREGATE :: String
iNT_PAR_AGGREGATE = #const_str GRB_INT_PAR_AGGREGATE

iNT_PAR_AGGREGATE_PTR :: CString
iNT_PAR_AGGREGATE_PTR = #const_cstr GRB_INT_PAR_AGGREGATE

iNT_PAR_AGGFILL :: String
iNT_PAR_AGGFILL = #const_str GRB_INT_PAR_AGGFILL

iNT_PAR_AGGFILL_PTR :: CString
iNT_PAR_AGGFILL_PTR = #const_cstr GRB_INT_PAR_AGGFILL

iNT_PAR_CONCURRENTMIP :: String
iNT_PAR_CONCURRENTMIP = #const_str GRB_INT_PAR_CONCURRENTMIP

iNT_PAR_CONCURRENTMIP_PTR :: CString
iNT_PAR_CONCURRENTMIP_PTR = #const_cstr GRB_INT_PAR_CONCURRENTMIP

iNT_PAR_CONCURRENTJOBS :: String
iNT_PAR_CONCURRENTJOBS = #const_str GRB_INT_PAR_CONCURRENTJOBS

iNT_PAR_CONCURRENTJOBS_PTR :: CString
iNT_PAR_CONCURRENTJOBS_PTR = #const_cstr GRB_INT_PAR_CONCURRENTJOBS

iNT_PAR_DISPLAYINTERVAL :: String
iNT_PAR_DISPLAYINTERVAL = #const_str GRB_INT_PAR_DISPLAYINTERVAL

iNT_PAR_DISPLAYINTERVAL_PTR :: CString
iNT_PAR_DISPLAYINTERVAL_PTR = #const_cstr GRB_INT_PAR_DISPLAYINTERVAL

iNT_PAR_DISTRIBUTEDMIPJOBS :: String
iNT_PAR_DISTRIBUTEDMIPJOBS = #const_str GRB_INT_PAR_DISTRIBUTEDMIPJOBS

iNT_PAR_DISTRIBUTEDMIPJOBS_PTR :: CString
iNT_PAR_DISTRIBUTEDMIPJOBS_PTR = #const_cstr GRB_INT_PAR_DISTRIBUTEDMIPJOBS

iNT_PAR_DUALREDUCTIONS :: String
iNT_PAR_DUALREDUCTIONS = #const_str GRB_INT_PAR_DUALREDUCTIONS

iNT_PAR_DUALREDUCTIONS_PTR :: CString
iNT_PAR_DUALREDUCTIONS_PTR = #const_cstr GRB_INT_PAR_DUALREDUCTIONS

dBL_PAR_FEASRELAXBIGM :: String
dBL_PAR_FEASRELAXBIGM = #const_str GRB_DBL_PAR_FEASRELAXBIGM

dBL_PAR_FEASRELAXBIGM_PTR :: CString
dBL_PAR_FEASRELAXBIGM_PTR = #const_cstr GRB_DBL_PAR_FEASRELAXBIGM

iNT_PAR_IISMETHOD :: String
iNT_PAR_IISMETHOD = #const_str GRB_INT_PAR_IISMETHOD

iNT_PAR_IISMETHOD_PTR :: CString
iNT_PAR_IISMETHOD_PTR = #const_cstr GRB_INT_PAR_IISMETHOD

iNT_PAR_INFUNBDINFO :: String
iNT_PAR_INFUNBDINFO = #const_str GRB_INT_PAR_INFUNBDINFO

iNT_PAR_INFUNBDINFO_PTR :: CString
iNT_PAR_INFUNBDINFO_PTR = #const_cstr GRB_INT_PAR_INFUNBDINFO

iNT_PAR_JSONSOLDETAIL :: String
iNT_PAR_JSONSOLDETAIL = #const_str GRB_INT_PAR_JSONSOLDETAIL

iNT_PAR_JSONSOLDETAIL_PTR :: CString
iNT_PAR_JSONSOLDETAIL_PTR = #const_cstr GRB_INT_PAR_JSONSOLDETAIL

iNT_PAR_LAZYCONSTRAINTS :: String
iNT_PAR_LAZYCONSTRAINTS = #const_str GRB_INT_PAR_LAZYCONSTRAINTS

iNT_PAR_LAZYCONSTRAINTS_PTR :: CString
iNT_PAR_LAZYCONSTRAINTS_PTR = #const_cstr GRB_INT_PAR_LAZYCONSTRAINTS

sTR_PAR_LOGFILE :: String
sTR_PAR_LOGFILE = #const_str GRB_STR_PAR_LOGFILE

sTR_PAR_LOGFILE_PTR :: CString
sTR_PAR_LOGFILE_PTR = #const_cstr GRB_STR_PAR_LOGFILE

iNT_PAR_LOGTOCONSOLE :: String
iNT_PAR_LOGTOCONSOLE = #const_str GRB_INT_PAR_LOGTOCONSOLE

iNT_PAR_LOGTOCONSOLE_PTR :: CString
iNT_PAR_LOGTOCONSOLE_PTR = #const_cstr GRB_INT_PAR_LOGTOCONSOLE

iNT_PAR_MIQCPMETHOD :: String
iNT_PAR_MIQCPMETHOD = #const_str GRB_INT_PAR_MIQCPMETHOD

iNT_PAR_MIQCPMETHOD_PTR :: CString
iNT_PAR_MIQCPMETHOD_PTR = #const_cstr GRB_INT_PAR_MIQCPMETHOD

iNT_PAR_NONCONVEX :: String
iNT_PAR_NONCONVEX = #const_str GRB_INT_PAR_NONCONVEX

iNT_PAR_NONCONVEX_PTR :: CString
iNT_PAR_NONCONVEX_PTR = #const_cstr GRB_INT_PAR_NONCONVEX

iNT_PAR_NUMERICFOCUS :: String
iNT_PAR_NUMERICFOCUS = #const_str GRB_INT_PAR_NUMERICFOCUS

iNT_PAR_NUMERICFOCUS_PTR :: CString
iNT_PAR_NUMERICFOCUS_PTR = #const_cstr GRB_INT_PAR_NUMERICFOCUS

iNT_PAR_OUTPUTFLAG :: String
iNT_PAR_OUTPUTFLAG = #const_str GRB_INT_PAR_OUTPUTFLAG

iNT_PAR_OUTPUTFLAG_PTR :: CString
iNT_PAR_OUTPUTFLAG_PTR = #const_cstr GRB_INT_PAR_OUTPUTFLAG

iNT_PAR_PRECRUSH :: String
iNT_PAR_PRECRUSH = #const_str GRB_INT_PAR_PRECRUSH

iNT_PAR_PRECRUSH_PTR :: CString
iNT_PAR_PRECRUSH_PTR = #const_cstr GRB_INT_PAR_PRECRUSH

iNT_PAR_PREDEPROW :: String
iNT_PAR_PREDEPROW = #const_str GRB_INT_PAR_PREDEPROW

iNT_PAR_PREDEPROW_PTR :: CString
iNT_PAR_PREDEPROW_PTR = #const_cstr GRB_INT_PAR_PREDEPROW

iNT_PAR_PREDUAL :: String
iNT_PAR_PREDUAL = #const_str GRB_INT_PAR_PREDUAL

iNT_PAR_PREDUAL_PTR :: CString
iNT_PAR_PREDUAL_PTR = #const_cstr GRB_INT_PAR_PREDUAL

iNT_PAR_PREPASSES :: String
iNT_PAR_PREPASSES = #const_str GRB_INT_PAR_PREPASSES

iNT_PAR_PREPASSES_PTR :: CString
iNT_PAR_PREPASSES_PTR = #const_cstr GRB_INT_PAR_PREPASSES

iNT_PAR_PREQLINEARIZE :: String
iNT_PAR_PREQLINEARIZE = #const_str GRB_INT_PAR_PREQLINEARIZE

iNT_PAR_PREQLINEARIZE_PTR :: CString
iNT_PAR_PREQLINEARIZE_PTR = #const_cstr GRB_INT_PAR_PREQLINEARIZE

iNT_PAR_PRESOLVE :: String
iNT_PAR_PRESOLVE = #const_str GRB_INT_PAR_PRESOLVE

iNT_PAR_PRESOLVE_PTR :: CString
iNT_PAR_PRESOLVE_PTR = #const_cstr GRB_INT_PAR_PRESOLVE

dBL_PAR_PRESOS1BIGM :: String
dBL_PAR_PRESOS1BIGM = #const_str GRB_DBL_PAR_PRESOS1BIGM

dBL_PAR_PRESOS1BIGM_PTR :: CString
dBL_PAR_PRESOS1BIGM_PTR = #const_cstr GRB_DBL_PAR_PRESOS1BIGM

dBL_PAR_PRESOS2BIGM :: String
dBL_PAR_PRESOS2BIGM = #const_str GRB_DBL_PAR_PRESOS2BIGM

dBL_PAR_PRESOS2BIGM_PTR :: CString
dBL_PAR_PRESOS2BIGM_PTR = #const_cstr GRB_DBL_PAR_PRESOS2BIGM

iNT_PAR_PRESOS1ENCODING :: String
iNT_PAR_PRESOS1ENCODING = #const_str GRB_INT_PAR_PRESOS1ENCODING

iNT_PAR_PRESOS1ENCODING_PTR :: CString
iNT_PAR_PRESOS1ENCODING_PTR = #const_cstr GRB_INT_PAR_PRESOS1ENCODING

iNT_PAR_PRESOS2ENCODING :: String
iNT_PAR_PRESOS2ENCODING = #const_str GRB_INT_PAR_PRESOS2ENCODING

iNT_PAR_PRESOS2ENCODING_PTR :: CString
iNT_PAR_PRESOS2ENCODING_PTR = #const_cstr GRB_INT_PAR_PRESOS2ENCODING

iNT_PAR_PRESPARSIFY :: String
iNT_PAR_PRESPARSIFY = #const_str GRB_INT_PAR_PRESPARSIFY

iNT_PAR_PRESPARSIFY_PTR :: CString
iNT_PAR_PRESPARSIFY_PTR = #const_cstr GRB_INT_PAR_PRESPARSIFY

iNT_PAR_PREMIQCPFORM :: String
iNT_PAR_PREMIQCPFORM = #const_str GRB_INT_PAR_PREMIQCPFORM

iNT_PAR_PREMIQCPFORM_PTR :: CString
iNT_PAR_PREMIQCPFORM_PTR = #const_cstr GRB_INT_PAR_PREMIQCPFORM

iNT_PAR_QCPDUAL :: String
iNT_PAR_QCPDUAL = #const_str GRB_INT_PAR_QCPDUAL

iNT_PAR_QCPDUAL_PTR :: CString
iNT_PAR_QCPDUAL_PTR = #const_cstr GRB_INT_PAR_QCPDUAL

iNT_PAR_RECORD :: String
iNT_PAR_RECORD = #const_str GRB_INT_PAR_RECORD

iNT_PAR_RECORD_PTR :: CString
iNT_PAR_RECORD_PTR = #const_cstr GRB_INT_PAR_RECORD

sTR_PAR_RESULTFILE :: String
sTR_PAR_RESULTFILE = #const_str GRB_STR_PAR_RESULTFILE

sTR_PAR_RESULTFILE_PTR :: CString
sTR_PAR_RESULTFILE_PTR = #const_cstr GRB_STR_PAR_RESULTFILE

iNT_PAR_SEED :: String
iNT_PAR_SEED = #const_str GRB_INT_PAR_SEED

iNT_PAR_SEED_PTR :: CString
iNT_PAR_SEED_PTR = #const_cstr GRB_INT_PAR_SEED

iNT_PAR_SOLUTIONTARGET :: String
iNT_PAR_SOLUTIONTARGET = #const_str GRB_INT_PAR_SOLUTIONTARGET

iNT_PAR_SOLUTIONTARGET_PTR :: CString
iNT_PAR_SOLUTIONTARGET_PTR = #const_cstr GRB_INT_PAR_SOLUTIONTARGET

iNT_PAR_THREADS :: String
iNT_PAR_THREADS = #const_str GRB_INT_PAR_THREADS

iNT_PAR_THREADS_PTR :: CString
iNT_PAR_THREADS_PTR = #const_cstr GRB_INT_PAR_THREADS

iNT_PAR_THREADLIMIT :: String
iNT_PAR_THREADLIMIT = #const_str GRB_INT_PAR_THREADLIMIT

iNT_PAR_THREADLIMIT_PTR :: CString
iNT_PAR_THREADLIMIT_PTR = #const_cstr GRB_INT_PAR_THREADLIMIT

dBL_PAR_TUNETIMELIMIT :: String
dBL_PAR_TUNETIMELIMIT = #const_str GRB_DBL_PAR_TUNETIMELIMIT

dBL_PAR_TUNETIMELIMIT_PTR :: CString
dBL_PAR_TUNETIMELIMIT_PTR = #const_cstr GRB_DBL_PAR_TUNETIMELIMIT

iNT_PAR_TUNERESULTS :: String
iNT_PAR_TUNERESULTS = #const_str GRB_INT_PAR_TUNERESULTS

iNT_PAR_TUNERESULTS_PTR :: CString
iNT_PAR_TUNERESULTS_PTR = #const_cstr GRB_INT_PAR_TUNERESULTS

iNT_PAR_TUNECRITERION :: String
iNT_PAR_TUNECRITERION = #const_str GRB_INT_PAR_TUNECRITERION

iNT_PAR_TUNECRITERION_PTR :: CString
iNT_PAR_TUNECRITERION_PTR = #const_cstr GRB_INT_PAR_TUNECRITERION

iNT_PAR_TUNETRIALS :: String
iNT_PAR_TUNETRIALS = #const_str GRB_INT_PAR_TUNETRIALS

iNT_PAR_TUNETRIALS_PTR :: CString
iNT_PAR_TUNETRIALS_PTR = #const_cstr GRB_INT_PAR_TUNETRIALS

iNT_PAR_TUNEOUTPUT :: String
iNT_PAR_TUNEOUTPUT = #const_str GRB_INT_PAR_TUNEOUTPUT

iNT_PAR_TUNEOUTPUT_PTR :: CString
iNT_PAR_TUNEOUTPUT_PTR = #const_cstr GRB_INT_PAR_TUNEOUTPUT

iNT_PAR_TUNEJOBS :: String
iNT_PAR_TUNEJOBS = #const_str GRB_INT_PAR_TUNEJOBS

iNT_PAR_TUNEJOBS_PTR :: CString
iNT_PAR_TUNEJOBS_PTR = #const_cstr GRB_INT_PAR_TUNEJOBS

dBL_PAR_TUNECLEANUP :: String
dBL_PAR_TUNECLEANUP = #const_str GRB_DBL_PAR_TUNECLEANUP

dBL_PAR_TUNECLEANUP_PTR :: CString
dBL_PAR_TUNECLEANUP_PTR = #const_cstr GRB_DBL_PAR_TUNECLEANUP

dBL_PAR_TUNETARGETMIPGAP :: String
dBL_PAR_TUNETARGETMIPGAP = #const_str GRB_DBL_PAR_TUNETARGETMIPGAP

dBL_PAR_TUNETARGETMIPGAP_PTR :: CString
dBL_PAR_TUNETARGETMIPGAP_PTR = #const_cstr GRB_DBL_PAR_TUNETARGETMIPGAP

dBL_PAR_TUNETARGETTIME :: String
dBL_PAR_TUNETARGETTIME = #const_str GRB_DBL_PAR_TUNETARGETTIME

dBL_PAR_TUNETARGETTIME_PTR :: CString
dBL_PAR_TUNETARGETTIME_PTR = #const_cstr GRB_DBL_PAR_TUNETARGETTIME

iNT_PAR_TUNEMETRIC :: String
iNT_PAR_TUNEMETRIC = #const_str GRB_INT_PAR_TUNEMETRIC

iNT_PAR_TUNEMETRIC_PTR :: CString
iNT_PAR_TUNEMETRIC_PTR = #const_cstr GRB_INT_PAR_TUNEMETRIC

iNT_PAR_TUNEDYNAMICJOBS :: String
iNT_PAR_TUNEDYNAMICJOBS = #const_str GRB_INT_PAR_TUNEDYNAMICJOBS

iNT_PAR_TUNEDYNAMICJOBS_PTR :: CString
iNT_PAR_TUNEDYNAMICJOBS_PTR = #const_cstr GRB_INT_PAR_TUNEDYNAMICJOBS

iNT_PAR_UPDATEMODE :: String
iNT_PAR_UPDATEMODE = #const_str GRB_INT_PAR_UPDATEMODE

iNT_PAR_UPDATEMODE_PTR :: CString
iNT_PAR_UPDATEMODE_PTR = #const_cstr GRB_INT_PAR_UPDATEMODE

iNT_PAR_OBJNUMBER :: String
iNT_PAR_OBJNUMBER = #const_str GRB_INT_PAR_OBJNUMBER

iNT_PAR_OBJNUMBER_PTR :: CString
iNT_PAR_OBJNUMBER_PTR = #const_cstr GRB_INT_PAR_OBJNUMBER

iNT_PAR_MULTIOBJMETHOD :: String
iNT_PAR_MULTIOBJMETHOD = #const_str GRB_INT_PAR_MULTIOBJMETHOD

iNT_PAR_MULTIOBJMETHOD_PTR :: CString
iNT_PAR_MULTIOBJMETHOD_PTR = #const_cstr GRB_INT_PAR_MULTIOBJMETHOD

iNT_PAR_MULTIOBJPRE :: String
iNT_PAR_MULTIOBJPRE = #const_str GRB_INT_PAR_MULTIOBJPRE

iNT_PAR_MULTIOBJPRE_PTR :: CString
iNT_PAR_MULTIOBJPRE_PTR = #const_cstr GRB_INT_PAR_MULTIOBJPRE

iNT_PAR_SCENARIONUMBER :: String
iNT_PAR_SCENARIONUMBER = #const_str GRB_INT_PAR_SCENARIONUMBER

iNT_PAR_SCENARIONUMBER_PTR :: CString
iNT_PAR_SCENARIONUMBER_PTR = #const_cstr GRB_INT_PAR_SCENARIONUMBER

iNT_PAR_POOLSOLUTIONS :: String
iNT_PAR_POOLSOLUTIONS = #const_str GRB_INT_PAR_POOLSOLUTIONS

iNT_PAR_POOLSOLUTIONS_PTR :: CString
iNT_PAR_POOLSOLUTIONS_PTR = #const_cstr GRB_INT_PAR_POOLSOLUTIONS

dBL_PAR_POOLGAP :: String
dBL_PAR_POOLGAP = #const_str GRB_DBL_PAR_POOLGAP

dBL_PAR_POOLGAP_PTR :: CString
dBL_PAR_POOLGAP_PTR = #const_cstr GRB_DBL_PAR_POOLGAP

dBL_PAR_POOLGAPABS :: String
dBL_PAR_POOLGAPABS = #const_str GRB_DBL_PAR_POOLGAPABS

dBL_PAR_POOLGAPABS_PTR :: CString
dBL_PAR_POOLGAPABS_PTR = #const_cstr GRB_DBL_PAR_POOLGAPABS

iNT_PAR_POOLSEARCHMODE :: String
iNT_PAR_POOLSEARCHMODE = #const_str GRB_INT_PAR_POOLSEARCHMODE

iNT_PAR_POOLSEARCHMODE_PTR :: CString
iNT_PAR_POOLSEARCHMODE_PTR = #const_cstr GRB_INT_PAR_POOLSEARCHMODE

iNT_PAR_IGNORENAMES :: String
iNT_PAR_IGNORENAMES = #const_str GRB_INT_PAR_IGNORENAMES

iNT_PAR_IGNORENAMES_PTR :: CString
iNT_PAR_IGNORENAMES_PTR = #const_cstr GRB_INT_PAR_IGNORENAMES

iNT_PAR_STARTNUMBER :: String
iNT_PAR_STARTNUMBER = #const_str GRB_INT_PAR_STARTNUMBER

iNT_PAR_STARTNUMBER_PTR :: CString
iNT_PAR_STARTNUMBER_PTR = #const_cstr GRB_INT_PAR_STARTNUMBER

iNT_PAR_PARTITIONPLACE :: String
iNT_PAR_PARTITIONPLACE = #const_str GRB_INT_PAR_PARTITIONPLACE

iNT_PAR_PARTITIONPLACE_PTR :: CString
iNT_PAR_PARTITIONPLACE_PTR = #const_cstr GRB_INT_PAR_PARTITIONPLACE

iNT_PAR_FUNCPIECES :: String
iNT_PAR_FUNCPIECES = #const_str GRB_INT_PAR_FUNCPIECES

iNT_PAR_FUNCPIECES_PTR :: CString
iNT_PAR_FUNCPIECES_PTR = #const_cstr GRB_INT_PAR_FUNCPIECES

dBL_PAR_FUNCPIECELENGTH :: String
dBL_PAR_FUNCPIECELENGTH = #const_str GRB_DBL_PAR_FUNCPIECELENGTH

dBL_PAR_FUNCPIECELENGTH_PTR :: CString
dBL_PAR_FUNCPIECELENGTH_PTR = #const_cstr GRB_DBL_PAR_FUNCPIECELENGTH

dBL_PAR_FUNCPIECEERROR :: String
dBL_PAR_FUNCPIECEERROR = #const_str GRB_DBL_PAR_FUNCPIECEERROR

dBL_PAR_FUNCPIECEERROR_PTR :: CString
dBL_PAR_FUNCPIECEERROR_PTR = #const_cstr GRB_DBL_PAR_FUNCPIECEERROR

dBL_PAR_FUNCPIECERATIO :: String
dBL_PAR_FUNCPIECERATIO = #const_str GRB_DBL_PAR_FUNCPIECERATIO

dBL_PAR_FUNCPIECERATIO_PTR :: CString
dBL_PAR_FUNCPIECERATIO_PTR = #const_cstr GRB_DBL_PAR_FUNCPIECERATIO

dBL_PAR_FUNCMAXVAL :: String
dBL_PAR_FUNCMAXVAL = #const_str GRB_DBL_PAR_FUNCMAXVAL

dBL_PAR_FUNCMAXVAL_PTR :: CString
dBL_PAR_FUNCMAXVAL_PTR = #const_cstr GRB_DBL_PAR_FUNCMAXVAL

iNT_PAR_FUNCNONLINEAR :: String
iNT_PAR_FUNCNONLINEAR = #const_str GRB_INT_PAR_FUNCNONLINEAR

iNT_PAR_FUNCNONLINEAR_PTR :: CString
iNT_PAR_FUNCNONLINEAR_PTR = #const_cstr GRB_INT_PAR_FUNCNONLINEAR

sTR_PAR_DUMMY :: String
sTR_PAR_DUMMY = #const_str GRB_STR_PAR_DUMMY

sTR_PAR_DUMMY_PTR :: CString
sTR_PAR_DUMMY_PTR = #const_cstr GRB_STR_PAR_DUMMY

sTR_PAR_JOBID :: String
sTR_PAR_JOBID = #const_str GRB_STR_PAR_JOBID

sTR_PAR_JOBID_PTR :: CString
sTR_PAR_JOBID_PTR = #const_cstr GRB_STR_PAR_JOBID

-- /* Parameter enumerations */

-- /* Cuts parameter values */

cUTS_AUTO :: CInt
cUTS_AUTO = #const GRB_CUTS_AUTO

cUTS_OFF :: CInt
cUTS_OFF = #const GRB_CUTS_OFF

cUTS_CONSERVATIVE :: CInt
cUTS_CONSERVATIVE = #const GRB_CUTS_CONSERVATIVE

cUTS_AGGRESSIVE :: CInt
cUTS_AGGRESSIVE = #const GRB_CUTS_AGGRESSIVE

cUTS_VERYAGGRESSIVE :: CInt
cUTS_VERYAGGRESSIVE = #const GRB_CUTS_VERYAGGRESSIVE

-- /* Presolve parameter values */

pRESOLVE_AUTO :: CInt
pRESOLVE_AUTO = #const GRB_PRESOLVE_AUTO

pRESOLVE_OFF :: CInt
pRESOLVE_OFF = #const GRB_PRESOLVE_OFF

pRESOLVE_CONSERVATIVE :: CInt
pRESOLVE_CONSERVATIVE = #const GRB_PRESOLVE_CONSERVATIVE

pRESOLVE_AGGRESSIVE :: CInt
pRESOLVE_AGGRESSIVE = #const GRB_PRESOLVE_AGGRESSIVE

-- /* Method parameter values */

mETHOD_NONE :: CInt
mETHOD_NONE = #const GRB_METHOD_NONE

mETHOD_AUTO :: CInt
mETHOD_AUTO = #const GRB_METHOD_AUTO

mETHOD_PRIMAL :: CInt
mETHOD_PRIMAL = #const GRB_METHOD_PRIMAL

mETHOD_DUAL :: CInt
mETHOD_DUAL = #const GRB_METHOD_DUAL

mETHOD_BARRIER :: CInt
mETHOD_BARRIER = #const GRB_METHOD_BARRIER

mETHOD_CONCURRENT :: CInt
mETHOD_CONCURRENT = #const GRB_METHOD_CONCURRENT

mETHOD_DETERMINISTIC_CONCURRENT :: CInt
mETHOD_DETERMINISTIC_CONCURRENT = #const GRB_METHOD_DETERMINISTIC_CONCURRENT

-- |
--
-- Deprecated since v11
mETHOD_DETERMINISTIC_CONCURRENT_SIMPLEX :: CInt
mETHOD_DETERMINISTIC_CONCURRENT_SIMPLEX = #const GRB_METHOD_DETERMINISTIC_CONCURRENT_SIMPLEX

cONCURRENTMETHOD_AUTO :: CInt
cONCURRENTMETHOD_AUTO = #const GRB_CONCURRENTMETHOD_AUTO

cONCURRENTMETHOD_BARRIER_PRIMAL_DUAL :: CInt
cONCURRENTMETHOD_BARRIER_PRIMAL_DUAL = #const GRB_CONCURRENTMETHOD_BARRIER_PRIMAL_DUAL

cONCURRENTMETHOD_BARRIER_DUAL :: CInt
cONCURRENTMETHOD_BARRIER_DUAL = #const GRB_CONCURRENTMETHOD_BARRIER_DUAL

cONCURRENTMETHOD_BARRIER_PRIMAL :: CInt
cONCURRENTMETHOD_BARRIER_PRIMAL = #const GRB_CONCURRENTMETHOD_BARRIER_PRIMAL

cONCURRENTMETHOD_PRIMAL_DUAL :: CInt
cONCURRENTMETHOD_PRIMAL_DUAL = #const GRB_CONCURRENTMETHOD_PRIMAL_DUAL

-- /* BarHomogeneous parameter values */

bARHOMOGENEOUS_AUTO :: CInt
bARHOMOGENEOUS_AUTO = #const GRB_BARHOMOGENEOUS_AUTO

bARHOMOGENEOUS_OFF :: CInt
bARHOMOGENEOUS_OFF = #const GRB_BARHOMOGENEOUS_OFF

bARHOMOGENEOUS_ON :: CInt
bARHOMOGENEOUS_ON = #const GRB_BARHOMOGENEOUS_ON

-- /* BarOrder parameter values */

bARORDER_AUTOMATIC :: CInt
bARORDER_AUTOMATIC = #const GRB_BARORDER_AUTOMATIC

bARORDER_AMD :: CInt
bARORDER_AMD = #const GRB_BARORDER_AMD

bARORDER_NESTEDDISSECTION :: CInt
bARORDER_NESTEDDISSECTION = #const GRB_BARORDER_NESTEDDISSECTION

-- /* MIPFocus parameter values */

mIPFOCUS_BALANCED :: CInt
mIPFOCUS_BALANCED = #const GRB_MIPFOCUS_BALANCED

mIPFOCUS_FEASIBILITY :: CInt
mIPFOCUS_FEASIBILITY = #const GRB_MIPFOCUS_FEASIBILITY

mIPFOCUS_OPTIMALITY :: CInt
mIPFOCUS_OPTIMALITY = #const GRB_MIPFOCUS_OPTIMALITY

mIPFOCUS_BESTBOUND :: CInt
mIPFOCUS_BESTBOUND = #const GRB_MIPFOCUS_BESTBOUND

-- /* SimplexPricing parameter values */

sIMPLEXPRICING_AUTO :: CInt
sIMPLEXPRICING_AUTO = #const GRB_SIMPLEXPRICING_AUTO

sIMPLEXPRICING_PARTIAL :: CInt
sIMPLEXPRICING_PARTIAL = #const GRB_SIMPLEXPRICING_PARTIAL

sIMPLEXPRICING_STEEPEST_EDGE :: CInt
sIMPLEXPRICING_STEEPEST_EDGE = #const GRB_SIMPLEXPRICING_STEEPEST_EDGE

sIMPLEXPRICING_DEVEX :: CInt
sIMPLEXPRICING_DEVEX = #const GRB_SIMPLEXPRICING_DEVEX

sIMPLEXPRICING_STEEPEST_QUICK :: CInt
sIMPLEXPRICING_STEEPEST_QUICK = #const GRB_SIMPLEXPRICING_STEEPEST_QUICK

-- /* VarBranch parameter values */

vARBRANCH_AUTO :: CInt
vARBRANCH_AUTO = #const GRB_VARBRANCH_AUTO

vARBRANCH_PSEUDO_REDUCED :: CInt
vARBRANCH_PSEUDO_REDUCED = #const GRB_VARBRANCH_PSEUDO_REDUCED

vARBRANCH_PSEUDO_SHADOW :: CInt
vARBRANCH_PSEUDO_SHADOW = #const GRB_VARBRANCH_PSEUDO_SHADOW

vARBRANCH_MAX_INFEAS :: CInt
vARBRANCH_MAX_INFEAS = #const GRB_VARBRANCH_MAX_INFEAS

vARBRANCH_STRONG :: CInt
vARBRANCH_STRONG = #const GRB_VARBRANCH_STRONG

-- /* PartitionPlace parameter values */

pARTITION_EARLY :: CInt
pARTITION_EARLY = #const GRB_PARTITION_EARLY

pARTITION_ROOTSTART :: CInt
pARTITION_ROOTSTART = #const GRB_PARTITION_ROOTSTART

pARTITION_ROOTEND :: CInt
pARTITION_ROOTEND = #const GRB_PARTITION_ROOTEND

pARTITION_NODES :: CInt
pARTITION_NODES = #const GRB_PARTITION_NODES

pARTITION_CLEANUP :: CInt
pARTITION_CLEANUP = #const GRB_PARTITION_CLEANUP

-- /* Callback phase values */

pHASE_MIP_NOREL :: CInt
pHASE_MIP_NOREL = #const GRB_PHASE_MIP_NOREL

pHASE_MIP_SEARCH :: CInt
pHASE_MIP_SEARCH = #const GRB_PHASE_MIP_SEARCH

pHASE_MIP_IMPROVE :: CInt
pHASE_MIP_IMPROVE = #const GRB_PHASE_MIP_IMPROVE

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

bATCH_STATUS_UNKNOWN :: CInt
bATCH_STATUS_UNKNOWN = #const GRB_BATCH_STATUS_UNKNOWN

bATCH_CREATED :: CInt
bATCH_CREATED = #const GRB_BATCH_CREATED

bATCH_SUBMITTED :: CInt
bATCH_SUBMITTED = #const GRB_BATCH_SUBMITTED

bATCH_ABORTED :: CInt
bATCH_ABORTED = #const GRB_BATCH_ABORTED

bATCH_FAILED :: CInt
bATCH_FAILED = #const GRB_BATCH_FAILED

bATCH_COMPLETED :: CInt
bATCH_COMPLETED = #const GRB_BATCH_COMPLETED

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
