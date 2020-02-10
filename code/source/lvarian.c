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

/* This function will read VNMR (Varian) NMR files
  from VNMR V3.1 and higher */
/*
   15-june-1992 M.A.D. first attempt
      fev -1996 M.A.D. small corrections
      mar -1997 M.A.D. again, for float and shortint data
      july-1997 M.A.D. added Linux code for swapbyte
      apri-1998 M.A.D. moved swapbyte / sswapbyte to external file
*/

#include <stdio.h>
#include "sizebasec.h"


struct datafilehead
{
   long int nblock;
   long int ntraces;
   long int np;
   long int ebytes;
   long int tbytes;
   long int bbytes;
   short int vers_id;
   short int status;
   long int nbheaders;
};
struct datablockhead
{
   short int scale;
   short int status;
   short int index;
   short int mode;
   long int ctcount;
   long int lpval;
   long int rpval;
   long int lvl;
   long int tlt;
};
struct hypercmplxbhead
{
   short int s_spare1;
   short int status;
   short int s_spare2;
   short int s_spare3;
   long int  l_spare1;
   float lpval1;
   float rpval1;
   float f_spare1;
   float f_spare2;
 };


#ifdef UNDERSCORE
#  define LVARIAN lvarian_
#else
#ifdef F2C
#  define LVARIAN lvarian_
#else
#  define LVARIAN lvarian
#endif
#endif

void LVARIAN(column,image,si1im,si2im,name,len,dim,itype,error)

float *column,*image;
int   *si1im,*si2im,*len,*dim,*itype,*error;
char  *name;

{
   struct datafilehead block;
   struct datablockhead block2;
   struct hypercmplxbhead block3;
   FILE  *f;
   int *buffer2;
   float *buffer3;  /* used for int conversion if needed */
   short int *buffer4;
   long int i,j,k,type;
   char lname[MAX_CHAR];

   convert_string(lname,name,*len);

#ifdef DEBUG
   printf("File-name is: %s\n",lname);
#endif

   if ( (f=fopen(lname,"r")) == NULL )
     { printf("*** Cannot open file %s \n",lname);
       *error=1; return; }

/* read file header */
   if (fread(&block,sizeof(struct datafilehead),1,f) != 1)
      { printf("*** read error 1 \n");  *error=1; return;}

/* swap byte if on a big endian machine (PC)  symbol is defined outside */
#ifdef is_big_endian
     swapbyte((char *) &block, 6*4);
     sswapbyte((char *) &block.vers_id, 2*2);
     swapbyte((char *) &block.nbheaders, 4);
#endif

#ifdef DEBUG
   printf(" nblock= %d, size=%d \n",block.nblock,block.np);
#endif


   if  ( block.nblock == 1)
     { *dim = 1; } else { *dim = 2; }
   if (block.nblock*block.np > SMXTOT )
   {
      block.nblock = SMXTOT / block.np;
      printf(" truncated to %d  blocks",block.nblock);
   }

   if ( block.status & 0x8) {    /*  float */
     type = 0;
   } else {
     if (block.status & 0x4) {
       type = 1;                 /* int */
     } else {
       type = 2;                 /* short int */
     }
   }

/* check itype */
   if ( block.status & 0x10) { *itype = 1; } else { *itype = 0; }
   if ( block.status & 0x20) { *itype = *itype+2; }

#ifdef DEBUG
   printf (" status = %x  type = %d  itype = %d \n",block.status, type, *itype);
#endif

   
/* allocate transfer buffers */
   if ((buffer2 = (int *) malloc(block.np*block.ebytes)) == NULL)
     { printf("read error 4 \n");  *error=1; return; }
   
/************************ for all the block in the file ****************/
   for(j=1;j<=block.nblock;j++)
   {

/* read first header */
     if (fread(&block2,sizeof(struct datablockhead),1,f) != 1) {
        if (j == 1) {
          printf("read error 2.0 \n");  *error=1; return;
        } else {
          printf("Encountered early file termination - assuming truncated dataset\n");
          j = j-1;
          break;
        }
      }
     /* block2 is not used, no need to swab it */

/* read second header */
     if (block.nbheaders == 2)
         { if (fread(&block3,sizeof(struct hypercmplxbhead),1,f) != 1 )
            { printf("read error 2.1 \n");  *error=1; return;}
     /* block3 is not used, no need to swab it */
         }

/* read data block header */
     {  if (fread(buffer2,block.np*block.ebytes, 1, f) != 1 )
           { printf("read error 3 %d\n",j); *error=1; return; }
#ifdef is_big_endian
     /*un swap for Linux */
     if (type == 2) {
       sswapbyte((char *) buffer2, block.np*block.ebytes);
     } else {
       swapbyte((char *) buffer2, block.np*block.ebytes);
     }
#endif
     }

     switch (type) {
     case 0:                  /* means   real type */
       buffer3 = (float *) buffer2;
       if (*dim == 1)
	 {
	   for (k=0;k<=block.np;k++) { column[k] = buffer3[k];}
	 } else
	 {
	   for (k=0;k<=block.np;k++) { image[(j-1)*block.np+k] = buffer3[k];}
	 }
       break;
     case 1:                    /* means  integer */
       if (*dim == 1)
	 {
	   for (k=0;k<=block.np;k++) { column[k] = buffer2[k];}
	 } else
	 {
	   for (k=0;k<=block.np;k++) { image[(j-1)*block.np+k] = buffer2[k];}
	 }
       break;
     case 2:                    /* means  short integer */
       buffer4 = (short int *) buffer2;
       if (*dim == 1)
	 {
	   for (k=0;k<=block.np;k++) { column[k] = buffer4[k];}
	 } else
	 {
	   for (k=0;k<=block.np;k++) { image[(j-1)*block.np+k] = buffer4[k];}
	 }
       break;
     }
   }

   *si1im = j-1;
   *si2im = block.np;

   fclose(f);

}

