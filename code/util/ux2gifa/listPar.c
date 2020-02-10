/*********************************************************************
 * COPYRIGHT 1990 BRUKER INSTRUMENTS, INC.
 *
 *
 * Program to list UXNMR parameters in ASCII format.
 *
 * Program is intended as a sample program to access BRUKER 
 * UXNMR parameters.
 *
 * Note: This program contains a architecture dependent 
 * '#define BIG_ENDIAN' that must be set correctly, prior to 
 * compilation. The routine also assumes IEEE format floats and doubles.
 * For VAX systems, additional conversions are necessary in the
 * routine  readParValue().
 ********************************************************************/

#include <stdio.h>
#include "pstruc.h"

#define 		BIG_ENDIAN	/* for sun3, sun4, sgi */
/* #undef 		BIG_ENDIAN	/* for intel 80x86, ns32x32 */


#define CAST_BUF_SIZE	1024
#define TRUE 		1
#define FALSE 		0

/* type conversion buffer */
typedef union cast_t {
  double  d[CAST_BUF_SIZE / sizeof(double)];
  float   f[CAST_BUF_SIZE / sizeof(float)];
  int     i[CAST_BUF_SIZE / sizeof(int)];
  char    c[CAST_BUF_SIZE];
}       CAST_T;

void exit();

/* local functions  */
static PNAMPTR  readParName();
static int 	readParValue();
static void	printParValue();
static void 	reverse4ByteOrder();
static void 	reverse8ByteOrder();


/**********************************************************************/
main(argc, argv)
int     argc;
char    **argv;
{
        extern char *sys_errlist[];
        extern int  errno;

	int  status;
        FILE *f_in;
        PNAM p, *pn;
  	CAST_T buf;

        (void)fprintf(stderr,
		"\n%s Copyright 1990 BRUKER INSTRUMENTS, INC.\n\n",
		argv[0]);

        if (argc != 2) {
                (void) fprintf(stderr,
		"... wrong number of arguments\n");
                (void) fprintf(stderr,
                "USAGE: %s uxnmr_parameter_file\n", argv[0]);
                (void) fprintf(stderr,"EXIT\n\n");
                exit(1);
        } 

        if ((f_in = fopen(argv[1], "r")) == NULL) {
                (void) fprintf(stderr,
		"... cannot read %s: \n", argv[1]);
                (void) fprintf(stderr,
		"\nERROR %s\n", sys_errlist[errno]);
                (void) fprintf(stderr,"EXIT.\n\n");
                exit(1);
        } else {
                (void) fprintf(stderr,
		"... %s open for reading.\n", argv[1]);
        }

	/* check for dummy parameter in file */
	pn = readParName(f_in, &p);
	status = readParValue(&p, f_in, &buf);
	if (pn == NULL || strcmp(p.parnam, "A000")) {
                (void) fprintf(stderr,"\nERROR: unknown file type\n");
                (void) fprintf(stderr,"EXIT.\n\n");
 		(void) fclose(f_in);
                exit(1);
 	}
	while (status != EOF && pn != NULL) {
		pn = readParName(f_in, &p);
		status = readParValue(&p, f_in, &buf);
		if (status == TRUE) {
			/* print value */
			printParValue(pn, &buf);
 		}
        }

	return 0;
}


/**********************************************************************/
static
int
readParValue(pn, f, pbuf)
PNAMPTR pn;
FILE   *f;
CAST_T *pbuf;
/*----------------------------------------------------------------------
Copy parameter to memory starting at pbuf.
Return FALSE if dummy value (array in first table). 
Return TRUE if pbuf contains value. 
Return EOF on end of file.
----------------------------------------------------------------------*/
{
  int     cnt, size;

  /* read 8 bytes */
  cnt = fread(pbuf->c, sizeof(char), MAX, f);
  if (cnt != MAX)
    return EOF;                        /* eof */

  /* check for dummy parameter "ZZ00" */

  if (strcmp(pn->parnam, "ZZ00") == 0) {
    return FALSE;
  } else if ((pn->parclass & ARRAYT) && (strcmp(pbuf->c, "ZZ00") == 0)){
    return FALSE;
  } else {

    /* read remaining bytes if parsiz > 8 */
    if (pn->parsiz > MAX) {
      if (pn->parsiz > CAST_BUF_SIZE) {
        (void) fprintf(stderr,
	"\nERROR: internal buffer CAST_BUF_SIZE to small.\n");
        (void) fprintf(stderr,
 	"... Parameter `%s' size: %d bytes, buffer: %d bytes\n", 
	pn->parnam, pn->parsiz, CAST_BUF_SIZE);
        (void) fprintf(stderr,"EXIT.\n\n");
 	(void) fclose(f);
        exit(1);
      }
      size = pn->parsiz - MAX;
      cnt = fread(&(pbuf->c[MAX]), sizeof(char), size, f);
      if (cnt != size)
        return EOF;                    /* eof */
    }  /* if parsiz > 8 */
  }  /* if ARRAYT & ZZ00 for 2nd table */

#ifdef BIG_ENDIAN
  /* if on a sun then swap integer bytes */
  /* convert read bytes to sun format according to parameter type */

  switch (pn->parclass & ~TYPEMASK) {
  case INTEGER32:
  case ENUM:
  case BOOLEAN:
  case (COUNTER & TYPEMASK):
  case REAL32:
    reverse4ByteOrder((int) (pn->parsiz / sizeof(int)), 
	(char *) &(pbuf->i[0]));
    break;
  case REAL64:
    reverse8ByteOrder((int) (pn->parsiz / sizeof(double)), 
	(char *) &(pbuf->i[0]));
    break;
  }
#endif

  return TRUE;
}


/**********************************************************************/
static
PNAMPTR
readParName(f, pn)
FILE   *f;
PNAMPTR pn;
/*----------------------------------------------------------------------
Read parameter header from file (x32 format) and convert it.
----------------------------------------------------------------------*/
{
  int     cnt;

  /* read parameter name */
  cnt = fread(pn->parnam, sizeof(char), XPAR, f);
  if (cnt != XPAR)
    return NULL;

  /* read parameter size, class and pointer */
  cnt = fread((char *) &(pn->parsiz), sizeof(int), 3, f);
  if (cnt != 3)
    return NULL;

#ifdef BIG_ENDIAN
  reverse4ByteOrder((int)2, (char *) &(pn->parsiz));
#endif

  /* set pdef field to zero */
  pn->def = NULL;
  return pn;
}


/**********************************************************************/
static
void
printParValue(pn, pbuf)
PNAMPTR pn;
CAST_T *pbuf;
/*----------------------------------------------------------------------
Print parameter value.
----------------------------------------------------------------------*/
{
  int i;

  /* print name */
  (void)printf("%s ", pn->parnam);

  switch (pn->parclass & ~TYPEMASK) {
  case INTEGER32:
  case ENUM:
  case BOOLEAN:
  case (COUNTER & TYPEMASK):
    for ( i = 0; i < pn->parsiz / sizeof (int); i++) {
    	(void)printf("%d ", pbuf->i[i]);
    }
    break;
  case REAL32:
    for ( i = 0; i < pn->parsiz / sizeof (float); i++) {
    	(void)printf("%g ", pbuf->f[i]);
    }
    break;
  case REAL64:
    for ( i = 0; i < pn->parsiz / sizeof (double); i++) {
    	(void)printf("%lg ", pbuf->d[i]);
    }
    break;
  case CHAR8:
    (void)printf("%s ", pbuf->c);
    break;
  }
  (void)printf("\n");
}


/**********************************************************************/
static void
reverse4ByteOrder(nele, dtaptr)
int     nele;                          /* specify number of elements to
                                        * convert */
char    *dtaptr;                       /* specify and return conversion
                                        * data    */
/*----------------------------------------------------------------------
Reverse byte order in 4 byte word (inplace).
----------------------------------------------------------------------*/
{
  register int *ip;
  register char *ca;
  register int m;
  union {
    char    ch[4];
    int     i;
  }       bs;

  ca = (char *) dtaptr;
  ip = (int *) dtaptr;
  for (m = 0; m < nele; m++) {
    bs.ch[3] = *ca++;
    bs.ch[2] = *ca++;
    bs.ch[1] = *ca++;
    bs.ch[0] = *ca++;
    *ip++ = bs.i;
  }

}                                      /* reverse4ByteOrder() */


/**********************************************************************/
static
void
reverse8ByteOrder(nele, dtaptr)
int     nele;                /* specify number of elements to convert */
char	*dtaptr;              /* specify and return conversion data */
/*----------------------------------------------------------------------
Reverse byte order in 8 byte double word (inplace).
----------------------------------------------------------------------*/
{
  register int *ip;
  register char *ca;
  register int m;
  union {
    char    ch[8];
    int     i[2];
  }       bs;

  ca = (char *) dtaptr;
  ip = (int *) dtaptr;
  for (m = 0; m < nele; m++) {
    bs.ch[7] = *ca++;
    bs.ch[6] = *ca++;
    bs.ch[5] = *ca++;
    bs.ch[4] = *ca++;
    bs.ch[3] = *ca++;
    bs.ch[2] = *ca++;
    bs.ch[1] = *ca++;
    bs.ch[0] = *ca++;
    *ip++ = bs.i[0];
    *ip++ = bs.i[1];
  }
}  /* reverse8ByteOrder() */


