/*
This file is a part of the GIFA program
This software has been developped by the NMR Group in CBS/Montpellier

     Authors :       T.E.Malliavin
                     C.B.S. Fac de Pharmacie
                     34000 Montpellier

This software cannot be used unless the user have been fully
licensed to do so form the above laboratory.
This file cannot be copied, duplicated or used in any other program,
without written permission from the authors.

*/

#include <stdio.h>

#ifdef TOTO
#ifndef MACHTEN
#   include <malloc.h>
#else
#   include <sys/malloc.h>
#endif
#endif

#ifdef LINUX
#   define dbm_open gdbm_open
#   define dbm_close gdbm_close
#   define dbm_store gdbm_store
#   define dbm_fetch gdbm_fetch
#   define dbm_delete gdbm_delete
#   define dbm_firstkey gdbm_firstkey
#   define dbm_nextkey gdbm_nextkey
#endif

#ifdef SPEC1
#   include <bsd/ndbm.h>
#else
#   include <ndbm.h>
#endif

#include <fcntl.h>
#include <string.h>
#include "sizebasec.h"
#include "dbmdef.h"

#define SIZE_BUFFER MAX_CHAR

/* static variables for reuse in the code */
char keyn[SIZE_BUFFER], valuen[SIZE_BUFFER];
datum  key_loc, value_loc;


void DBMOPEN(name,len_db,ist,error)

int *error,*len_db,*ist;
char name[SIZE_BUFFER];
/* to open a DBM file  */

{

int mode;
char file[MAX_CHAR];
DBM *datab;

   convert_string(file,name,*len_db);
   mode = 0666;
   if ( NULL == (datab = dbm_open(file,O_RDWR|O_CREAT,mode)))
        {
	  if ( NULL == (datab = dbm_open(file,O_RDONLY,mode)))
	    {
	    *error = 1;
	    }
	}
   *ist = (int ) datab;
#ifdef TOTO
#ifdef LINUX        /* assumes that LINUX uses gdbm */
   /* and set mode to fast */

   mode = (1==1);
   *error = gdbm_setopt( datab, GDBM_FASTMODE, &mode, sizeof(int));

#endif
#endif

}

void DBMCLOSE(ist)

int *ist;
/* to close a DBM file */

{

DBM *datab;

   datab = (DBM *) *ist;
   dbm_close(datab);

}

void DBMSTORE(ist,key,len_key,value,len_val,error)

int *error,*ist,*len_key,*len_val;
char key[SIZE_BUFFER],value[SIZE_BUFFER];
/* to store a element (key,value) in a DBM file  */

{

datum key_loc,value_loc;
int flg;
DBM *datab;

   convert_string(keyn,key,*len_key);
   convert_string(valuen,value,*len_val);

   key_loc.dsize = strlen(keyn);   
   key_loc.dptr = keyn;    
   value_loc.dsize = strlen(valuen);      
   value_loc.dptr = valuen;
   
   datab = (DBM *) *ist;   
   flg = DBM_REPLACE;
   *error = dbm_store(datab,key_loc,value_loc,flg);

   if (*error < 0) 
	{
	printf("Error %i during data storage !\n",*error);
	*error = 1 ;
	}

}

void DBMFETCH(ist,key,value,len_key,len_val,error)

int *ist,*len_key,*error,*len_val;
char key[SIZE_BUFFER],value[SIZE_BUFFER];
/* to fetch a element value from a DBM file  */

{

datum key_loc,value_loc;
DBM *datab;

   convert_string(keyn,key,*len_key); 

   key_loc.dsize = strlen(keyn);      
   key_loc.dptr = keyn;
   
   datab = (DBM *) *ist;   
   value_loc = dbm_fetch(datab,key_loc);
   if (value_loc.dptr != NULL) {        /* found */
      *len_val = value_loc.dsize;
      strncpy(value,value_loc.dptr,*len_val);  
   } else {                            /* not found */
      *error = 1; 
   }
   
}

void DBMREM(ist,key,len_key,error)

int *error,*ist,*len_key;
char key[SIZE_BUFFER];
/* to delete a element in a DBM file  */

{

datum key_loc;
DBM *datab;

   convert_string(keyn,key,*len_key); 

   key_loc.dsize = strlen(keyn);   
   key_loc.dptr = keyn;    
   
   datab = (DBM *) *ist;      
   *error = dbm_delete(datab,key_loc);

   if (*error != 0)
	{
	printf("Error %i during key deletion !\n",*error);
	*error = 1 ;
	}

}

void DBMNXT(ist,next_key,len_next_key,error,error_end)

int *error,*ist,*len_next_key,*error_end;
char next_key[SIZE_BUFFER];
/* to find the next key in a DBM file  */

{

DBM *datab;

   datab = (DBM *) *ist;      
   key_loc = dbm_nextkey(datab);

   if (key_loc.dptr != NULL) {        /* found */
      *len_next_key = key_loc.dsize;
      strncpy(next_key, key_loc.dptr,*len_next_key);  
   } else {                            /* not found */
      *error_end = 1;
   }

}

void DBMFRST(ist,first_key,len_first_key,error)

int *error,*ist,*len_first_key;
char first_key[SIZE_BUFFER];
/* to find the first key in a DBM file  */

{

DBM *datab;

   datab = (DBM *) *ist;      
  key_loc = dbm_firstkey(datab);

   if (key_loc.dptr != NULL) {        /* found */
      *len_first_key = key_loc.dsize;
      strncpy(first_key, key_loc.dptr,*len_first_key);  
   } else {                            /* not found */
      *error = 1;
      printf("Error during looking for the first key\n");
   }

}








