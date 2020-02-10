/*
*******************************************************************
*
*       pstruc.h
*
*       Copyright (c) 1986
*       BRUKER ANALYTISCHE MESSTECHNIK GMBH
*       D7512 Rheinstetten, Germany
*
*       All Rights Reserved
*
*       Date of last modification  : 88/06/13
*       Pathname of SCCS file      : /u/gen/h/S/s.pstruc.h
*       SCCS identification        : 1.6
*
*******************************************************************
*/
/************************************************/
/* Definitions used by Parameter Structures     */
/* B.Guigas, July 14, 1986                      */
/* NAME OF FILE:        pstruc.h                */
/*
   modifications: ge 861027:
                MISC=0X80; MODIFIED=0x200000

                bume 890103
                change of 'size' in PNAM structure
                from short to long type , introduction of
                a fill array in PNAM (alignement
                problems on sun4)               */
/************************************************/
#ifndef PSTRUC
#define PSTRUC

# define XPAR   8             /* max. # of chars. -1 for a param name */
# define MAX    sizeof(double)     /* max size of a param. in table 1 */
# define MAXARY 519                /* max. # of bytes of array for a 
                                    * param. in tbl 2 */
# define INT    MAX-sizeof(int)
# define FLOAT  MAX-sizeof(float)
# define CHAR   MAX-sizeof(char)
# define SHORT  MAX-sizeof(short)
# define DOUBLE MAX-sizeof(double)
# define EXPSIZ 44                 /* size of explanatory text */
# define MAXINT32   0X7FFFFFFF
# define MAXREAL32  1.0E38


/*** bit assignment in "parclass", struc PNAM ***/

 /* parameter classes, bits 0..7 */
# define SETUP      1
# define ACQU       2
# define PROC       4
# define PLOT       8
# define DISPL      0X10
# define CURD       0X20
# define OUTD       0X40
# define MISC       0X80
# define NOCLASS    ~(SETUP|ACQU|PROC|PLOT|DISPL|CURD|OUTD|MISC)
 /* unused bits 8..12            */
# define EDTSELECT  0X2000         /* bit 13 =1 : selectable item */
# define RELATED    0X4000         /* bit 14 : 1 */
# define SUBRANGE   0X8000         /* bit 15 : 1 */
 /* bit no      16 17 18 19 20   */
# define INTEGER32  0X10000        /* 1  0  0  0  0 */
# define INTEGER64  0X20000        /* 0  1  0  0  0 */
# define REAL32     0X30000        /* 1  1  0  0  0 */
# define REAL64     0X40000        /* 0  0  1  0  0 */
# define ENUM       0X50000        /* 1  0  1  0  0 */
# define BOOLEAN    0X60000        /* 0  1  1  0  0 */
# define COUNTER    0X170000       /* 1  1  1  0  1 */
# define CHAR8      0X80000        /* 0  0  0  1  0 */
# define TYPEMASK   ~(REAL32|REAL64|CHAR8)               /* 16 ..19:0 */
# define ARRAYT     0X100000            /* bit 20 : 1 */
# define MODIFIED   0x200000            /* bit 21 : 1 */
# define HOLDMODB   0x400000            /* bit 22 : 1 */
# define ACCESSED   0x800000            /* bit 23 : 1 */
 /* unused bits 24..31           */
/*** par. range, relation and consistency struct ***/

typedef struct pdeftyp {
  char    expl[EXPSIZ];              /* explanatory text to parameter */
  union {
    int     i[3];                   /* low -, high -, default value */
    float   f[3];                   /* low -, high -, default value */
    double  d[2];                   /* low -, default value */
    char    c[3];                   /* low -, high -, default value */
    int     bd;                     /* default boolean value */
    struct {                        /* enumeration types */
      int     e[3];                 /* size, ordinal of high, default */
      char   *en;                   /* pointer to enumeration */
      /* names   */
    }       enu;
    int     co[3];                  /* default initial, final, step */
  }       range;
  int     high;                /* highest index, 0 if no array */
  char   *relptr;              /* pointer to string containing relations
                                * and consistency conditions */
}       PDEF, *PDEFPTR;


/*** parameter name & type struct ***/

typedef struct pnamtyp {
  char    parnam[XPAR];        /* name of parameter */
  long    parsiz;              /* size of parameter in bytes */
  long    parclass;            /* parameter class */
  PDEFPTR def;                 /* pointer to PDEF structure */
  char    fill[4];             /* fill array for address alignement */
}       PNAM, *PNAMPTR;

typedef PNAMPTR(*PFPNAMPTR) ();      /* ptr to func returning PNAMPTR */

typedef struct pnamval {             /* value region, size is MAX */
  union {
    char    c[8];
    int     i[2];
    long    l[2];
    float   f[2];
    double  d;
  }       val;
}       PVAL, *PVALPTR;

typedef struct parameter {
  PNAM    pnam;
  PVAL    pval;
}       PARAM, *PARAMPTR;

#endif
