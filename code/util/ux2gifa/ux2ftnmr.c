/***********************************************************************
Copyright 1989 BRUKER INSTRUMENTS, INC.

ux2ftnmr - convert uxnmr x32 integer data to ftnmr floating point data


USAGE:
	ux2ftnmr uxnmr_source_file TD ftnmr_output_file
	
	uxnmr_source_file:
		binary format uxnmr x32 fid or ser file.
	TD:	time domain size: number of data points per fid.
	ftnmr_output_file:
		ftnmr format output file name.

***********************************************************************/

#include <stdio.h>
#include <malloc.h>
#include <sys/types.h>
#include <sys/stat.h>

#define BIG_ENDIAN     /* big endian architecture (e.g. SGI Iris,
			* SUN Sparc, Sun MC68000 based).
			* Undefine BIG_ENDIAN for sall endian 
			* architecture (e.g. VAX, INTEL, NS based 
			* systems). */

/**********************************************************************/
main(argc, argv)
int	argc;
char	**argv;
{
	extern char *sys_errlist[];
	extern int  errno;

	FILE *f_in;
	FILE *f_out;
	struct stat buf;
	int  td, td1;

	(void)printf("\n%s Copyright BRUKER 1989\n\n",argv[0]);

	if (argc != 4) {
		(void) printf("... wrong number of arguments\n");
		(void) printf(
		"USAGE: %s uxnmr_source_file TD ftnmr_output_file\n",
		argv[0]);
		(void) printf("EXIT\n\n");
		exit(1);
	} 
	
	td = atoi(argv[2]);
	if (td <= (int)0) {
		(void) printf(
		"ERROR in commad line argument: TD = %d\n", td);
		(void) printf("EXIT\n\n");
		exit(1);
	}
	(void) printf("... TD %d\n", td);


	if (stat(argv[1], &buf) == -1) {
		(void) printf("... cannot access %s: \n", argv[1]);
		(void) printf("    ERROR %s\n", sys_errlist[errno]);
		(void) printf("EXIT\n\n");
		exit(1);
	} else {
		if (buf.st_size < td * sizeof(int)) {
		  (void) printf("... wrong input file size: %d bytes\n",
				buf.st_size);
		  (void) printf("    expected %d bytes\n", 
				td * sizeof(int));
		  (void) printf("EXIT\n\n");
		  exit(1);
		}
	}

	if ((f_in = fopen(argv[1], "r")) == NULL) {
		(void) printf("... cannot read %s: \n", argv[1]);
		(void) printf("    ERROR %s\n", sys_errlist[errno]);
		(void) printf("EXIT\n\n");
		exit(1);
	} else {
		(void) printf("... %s open for read, %d bytes\n", 
		argv[1], buf.st_size);
	}

	if ((f_out = fopen(argv[3], "w")) == NULL) {
		(void) printf("... cannot write to %s: \n", argv[3]);
		(void) printf("    ERROR %s\n", sys_errlist[errno]);
		(void) printf("EXIT\n\n");
		exit(1);
	} else {
		(void) printf("... %s open for write\n", argv[3]);
	}


	td1 = convert(f_in, td, f_out);
	(void) printf("... td1 = %d, converted %d bytes.\n", td1,
		td*td1*sizeof(int));
	
	(void)fclose(f_in);
	(void)fclose(f_out);

	if (buf.st_size == td * td1 * sizeof(int)) {
		(void) printf("... no errors.\n");
	}
	(void) printf("\ndone %s.\n\n", argv[0]);
	exit(0);
}


/**********************************************************************/
static int
convert (f_in, td, f_out)
FILE 	*f_in;
int	td;
FILE	*f_out;
/*----------------------------------------------------------------------
Convert integer file to ftnmr format file.
Ftnmr format (f77 unformatted):
fields:		header - count -    ... data ...      - tail
type:		int    - int   -        float         - int
bits:		32     - 32    -        32            - 32
meaning:        bytes  - TD/2  - re1 im1 .. reTD imTD - bytes
		(where 'bytes' are the number of data bytes excluding
		header and tail).
----------------------------------------------------------------------*/
{
/*	char 	*malloc(); */
	void 	int2float();
	void	reverse4ByteOrder();

	int 	*ip;
	int	cnt, i;
	int	bytes;
	int	n;

	ip = (int*) malloc((unsigned)(td * sizeof(int)));
	if (ip == NULL) {
		(void) printf("... no memory (%d bytes)\n", 
			td * sizeof(int));
		(void) printf("EXIT\n\n");
		exit(1);
	}

	bytes = sizeof(int) + td * sizeof(int) ;
	n = td/2;
	i = 0;
	(void) printf("\n");
	do {
		cnt = fread((char*)ip,sizeof(int),td,f_in);
		if (cnt == td) {
			(void) printf("row %4d\r",++i);
#ifdef BIG_ENDIAN
			reverse4ByteOrder(td, (char*)ip);
#endif
			int2float(td, (char*)ip);
			(void)fwrite((char*)&bytes,sizeof(int),1,f_out);
			(void)fwrite((char*)&n,sizeof(int),1,f_out);
			(void)fwrite((char*)ip,sizeof(int),td,f_out);
			(void)fwrite((char*)&bytes,sizeof(int),1,f_out);
		}
	} while (cnt == td);

	if (cnt != 0) {
	  (void) printf(
	  "... %d bytes remaining, incomplete conversion (?).\n",cnt);
	}

	return i;
}


/**********************************************************************/
static void
reverse4ByteOrder(nele, dtaptr)
int     nele;			       /* specify number of elements to
				        * convert */
char 	*dtaptr;		       /* specify and return conversion
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

}				       /* reverse4ByteOrder() */


/**********************************************************************/
static void
int2float(nele, dtaptr)
int     nele;			       /* specify number of elements to
				        * convert */
char 	*dtaptr;		       /* specify and return conversion
				        * data    */
/*----------------------------------------------------------------------
Inplace conversion of binary 32 bit integer format to 32 bit floating 
format.
----------------------------------------------------------------------*/
{
  register float *fl;
  register int m;
  char *ca;
  int     i;

  ca = (char *) dtaptr;
  fl = (float *) dtaptr;
  for (m = 0; m < nele; m++) {
    i = *((int*)ca);
    ca += sizeof(int);
    *fl++ = (float) i;
  }

}				       /* int2float() */

