/* This include file is used to defined name of C function
  depending on the UNDERSCORE state */

#ifdef UNDERSCORE

#define DBMOPEN dbmopen_
#define DBMCLOSE dbmclose_
#define DBMSTORE dbmstore_
#define DBMFETCH dbmfetch_
#define DBMREM dbmrem_
#define DBMNXT dbmnxt_
#define DBMFRST dbmfrst_

#else
#ifdef F2C

#define DBMOPEN dbmopen_
#define DBMCLOSE dbmclose_
#define DBMSTORE dbmstore_
#define DBMFETCH dbmfetch_
#define DBMREM dbmrem_
#define DBMNXT dbmnxt_
#define DBMFRST dbmfrst_

#else

#define DBMOPEN dbmopen
#define DBMCLOSE dbmclose
#define DBMSTORE dbmstore
#define DBMFETCH dbmfetch
#define DBMREM dbmrem
#define DBMNXT dbmnxt
#define DBMFRST dbmfrst

#endif
#endif

