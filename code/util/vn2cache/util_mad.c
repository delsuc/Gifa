/*

This file contains certain entry points which are needed for the cache_mad
library.
These entries mimick entry points which are found in the complete
Gifa environment, in many cases, these entries realize trivial things.

*/

#include <stdio.h>
#include "util.h"

/*
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     cconvert_string(st2,st1,l)                                         C
C     Function to convert a C string into a fortran string.              C
C                                                                        C
C     st1 = string to convert. (C)                                       C
C     st2 = converted string.  (fortran)                                 C
C     l = length of st.                                                  C
C                                                                        C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
*/
void cconvert_string(st2,st1,l)

#ifdef VMS
/*  ATTENTION   A REECRIRE */
char st2[];
struct dsc$descriptor_s *st1;
int l;
{
   strncpy(st2,st1->dsc$a_pointer,l);
   st2[l] = '\0';
}
#else
char *st1,*st2;
int *l;
{
  *l = strlen(st1);
  strcpy(st2,st1);
}
#endif

/*
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     convert_string(st2,st1,l)                                          C
C     Function to convert a fortran string into a C string.              C
C                                                                        C
C     st1 = string to convert. (Fortran)                                 C
C     st2 = converted string.  (C)                                       C
C     l = length of st.                                                  C
C                                                                        C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
*/

void convert_string(st2,st1,l)

#ifdef VMS
char st2[];
struct dsc$descriptor_s *st1;
int l;
{
   strncpy(st2,st1->dsc$a_pointer,l);
   st2[l] = '\0';
}
#else
char *st1,*st2;
int l;
{
  strncpy(st2,st1,l);
  st2[l] = '\0';
}
#endif


/************************************************************************/
void GIFAOUTC(st)
char st;
{
  printf("%s\n",st);
}

/************************************************************************/
void GIFA_EXIT( int *i)
{
   exit(*i);
 }


