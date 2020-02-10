/*
This file is a part of the GIFA program
This software has been developped by the NMR Group in CBS/Montpellier

     Authors :       M.A.Delsuc
                     C.B.S. Fac de Pharmacie
                     34000 Montpellier

This software cannot be used unless the user have been fully
licensed to do so form the above laboratory.
This file cannot be copied, duplicated or used in any other program,
without written permission from the authors.

*/

/*
This implements direct access to file from Gifa
/*
      april-2002 M.A.D. first attempt
*/

#include <stdio.h>
#include "sizebasec.h"
#include <string.h>

#define MAX_CHAR 256

FILE  *fname = NULL;

/**********************************************************************************/
#ifdef UNDERSCORE
#  define OPEN_RAW open_raw_
#else
#ifdef F2C
#  define OPEN_RAW open_raw__
#else
#  define OPEN_RAW open_raw
#endif
#endif

void OPEN_RAW(char *name, int *len, int *error)

{
   char lname[MAX_CHAR];

   if (fname != NULL) {
	  *error=2;
	  return;
   }

   convert_string(lname,name,*len);

   if ( (fname=fopen(lname,"r")) == NULL )
     {
        perror(NULL);
        *error=1;
     }
}

/**********************************************************************************/
#ifdef UNDERSCORE
#  define CLOSE_RAW close_raw_
#else
#ifdef F2C
#  define CLOSE_RAW close_raw__
#else
#  define CLOSE_RAW close_raw
#endif
#endif

void CLOSE_RAW(int *error)

{
   if (fname != NULL) {
      if (fclose(fname) != 0) {
	  *error=2;
	  perror(NULL);
       }
	 fname = NULL;
   } else {
       *error=1;
   }
}


/**********************************************************************************/
#ifdef UNDERSCORE
#  define READ_RAW read_raw_
#else
#ifdef F2C
#  define READ_RAW read_raw__
#else
#  define READ_RAW read_raw
#endif
#endif

void READ_RAW(float *column, int *size, int *offset, int *type, int *swab, int *error)
{
   int        *ibuffer;
   float      *buffer;
   short int  *sibuffer;
   double     *dbuffer;
   long int   loffset, lbsize;
   int        k,bsize;
   size_t     bsize_t;

   loffset = *offset;

   if (fname == NULL) {
	  *error=5;
	  return;
   }

   switch (*type) {   // switch on type  tells which kind of data we're reading
                      // computes sizes
// code is    : 1 float   2 long float   3 int   4 short int
     case 1:       // float
        loffset = 4*(*offset);
        lbsize = 4*(*size);
        break;
	 
     case 2:       // long float
        loffset = 8*(*offset);
        lbsize = 8*(*size);
	 break;

     case 3:     // int
        loffset = 4*(*offset);
       lbsize = 4*(*size);
	 break;

     case 4:   // short int
       loffset = 2*(*offset);
       lbsize = 2*(*size);
   }       // end of switch

// fseek
    if (0 !=
	  fseek(fname, loffset, SEEK_SET)) {       // seek
	  perror("error with fseek"); *error=1; return;
    }
// malloc
    if ( NULL ==
		(buffer = (float *) malloc(lbsize) ) ) {    
		perror("error with malloc"); *error=2; return;
    }
// fread
    if (1 !=
	  fread(buffer,lbsize,1,fname)) {                  
	  perror("error with fread"); *error=3; return;
    }
// swap byte if needed
    if (*swab == 1) {
      if (*type != 4) {
    		bsize = lbsize;
		swapbyte((char *) buffer, bsize);
	} else {
    		bsize = lbsize;
		sswapbyte((char *) buffer, bsize);
	}
    }

// copy to main buffer
     switch (*type) {
     case 1:
        for (k=0; k<(*size) ; k++) { column[k] = buffer[k];}
	  break;
     case 2:
        dbuffer = (double *) buffer;
        for (k=0; k<(*size) ; k++) { column[k] = dbuffer[k];}
	  break;
     case 3:
        ibuffer = (int *) buffer;
        for (k=0; k<(*size) ; k++) { column[k] = ibuffer[k];}
	  break;
     case 4:
        sibuffer = (short int *) buffer;
        for (k=0; k<(*size) ; k++) { column[k] = sibuffer[k];}
	  break;
    }
// free
     free(buffer);
}

