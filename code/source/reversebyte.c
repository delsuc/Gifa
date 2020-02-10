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

/* This file implement byte swapping functions for BIG_ENDIAN machines */

/* used by lvarian.c and cache_mad.c */


void swapbyte(char *buffer, int size)
/* double swap byte for big_endian machines */

{
   char  *chr, chr0, chr1, chr2, chr3;
   int k;

    chr =  buffer;
    for (k=0; k<size/4; k++)
    {
      chr0 = *chr;
      chr1 = *(chr+1);
      chr2 = *(chr+2);
      chr3 = *(chr+3);
      *chr++ = chr3;
      *chr++ = chr2;
      *chr++ = chr1;
      *chr++ = chr0;
    }
}

void sswapbyte(char *buffer, int size)
/* single swap byte for big_endian machines */

{
   char  *chr, chr0, chr1;
   int k;

    chr =  buffer;
    for (k=0; k<size/2; k++)
    {
      chr0 = *chr;
      chr1 = *(chr+1);
      *chr++ = chr1;
      *chr++ = chr0;
    }
}

