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

#include <stdio.h>
#include "sizebasec.h"


void cux1d(lname, ln, column, sizeimage1d, error)

char *lname;
int *ln, *sizeimage1d, *error;
float *column;

{
  char cname[MAX_CHAR];
  FILE *file;
  int i;

  convert_string(cname, lname, *ln);

  if ((file = fopen(cname, "wb")) == NULL) {
    *error = 1;
    return;
  }

  if ( fwrite(column, sizeof(float), *sizeimage1d, file) != *sizeimage1d) {
    *error = 1;
    return;
  }

  if ( fclose(file) != 0) {
      *error = 1;
      return;
    }
  return;
}
