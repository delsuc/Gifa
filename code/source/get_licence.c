
/* give a static licence protection
 a  file (licence_file) holds the following entries :

A long line describing the licence (software name, laboratory)
A_key_in_octal

The key is obtaining by hashing the licence string with an internal key

use mklicence to compute the key

if the licence file is incorrect, the string NON-VALID is returned,
otherwise the content of the licence string is returned.
*/


#include "util.h"
#include <stdio.h>

#define MAX_CHAR 256
#define LICENCE_FILE "/usr/local/gifa/licence"

void GET_LICENCE(lic,ilic)
char lic[MAX_CHAR];
int  *ilic;

{
  char key[64];
  unsigned long hash, ih, j;
  char string[MAX_CHAR], check[MAX_CHAR];
  FILE *fin;

/* key is the coding key, keep it secret */
  strcpy(key,"Put the name of your App here");

/* open licence_file and read licence */

  if ((fin = fopen(LICENCE_FILE,"r")) == NULL) {
    strcpy(lic,"NON-VALID");
    *ilic = strlen(lic);
    return;
  }

  if (fgets(lic, MAX_CHAR, fin) == NULL) {
    strcpy(lic,"NON-VALID");
    *ilic = strlen(lic);
    return;
  }  

/* compute key */
  hash = 12345;
  ih = 0;
  j = 0;
  while (lic[j] != '\n') {
    hash = hash + (hash%54321) * key[ih] + lic[j++];
    if ( key[ih++] == '\0') { ih = 0; }
  }

/* readin key */
  if (fgets(string, MAX_CHAR, fin) == NULL) {
    strcpy(lic,"NON-VALID");
    *ilic = strlen(lic);
    return;
  }

/* compare */
  sprintf(check,"%lo\n",hash);
  if (strcmp(check,string) != 0) {
    strcpy(lic,"NON-VALID");
    *ilic = strlen(lic);
    return;
  }
  *ilic = strlen(lic);
  return;

}

