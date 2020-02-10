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

The cache library for the GIFA program
Version 2.0 M.A.Delsuc (mad@cbs.univ-montp1.fr) jan-1995
code in Ansi C

History...
M.A.Delsuc first version jan 1995
M.A.Delsuc modified 22 may 1995
         to add REFABSMAX code
M.A.Delsuc modified 20 sep 1995
         to correct c_readblock c_writeblock for pdata.pread instead of pdata (!)
M.A.Delsuc modified 13 dec 1995
         to correct for a floating exception on very asymmetric 2D and 3D
         to correct for absmax0 correctly propagated
M.A.Delsuc modified 18 jan 1996
         in putheader to accept parameter entry with more than 12 char (1 blank was missing)
M.A.Delsuc modified 11 march 1996
         in c_putparam, param matching was incorrectly done. xy was matching xxyy !
M.A.Delsuc modified 23 oct 1996
         added the OPEN_HAS_2 piece of code
M.A.Delsuc modified 10 mar 1997
         added code for DMIN, DMAX and DFACTOR in standard parameters in CH_GTPARBAS
M.A.Delsuc modified 10 dec 1997
         modified gt_param for a nasty bug, left from the 11 march 1996 modif
         added the code for Big_endian / little_endian
K.Gehring 2 dec 1998
         added the c_errlist function to return error codes
M.A.Delsuc 10 dec 1998
         removed an error, probably introduced in dec 1995
         inhibiting the possibility to change the Absmax header parameter
M.A.Delsuc 20 jan 2002
	 corrected error handling in CH_OPEN CH_CREATE
M.A.Delsuc 1 mar 2002
       added the EROFS flag in CH_OPEN, was preventing from joining on a CD-ROM


It is a complete rewrite of the version 1.0 from A.Rouh
This version is compatible in functionalities and file with version 1.0
but not in API (function calls).

This library implements a cache memory that permits to access NMR files(1D,2D,3D)
files consists of a header in ASCII, of a fixed size, filled with the 0 character.
Then the data are in float
1D are stored sequentially
2D are 3D are stored as submatrices, size of the blocks are defined in the header
(or assumed if it is a version 1.0 file)

The entries in UPPERCASE are the main entry points. In regular use there is no need to use
the other entries. The call syntax permit to use the library from FORTRAN
(all address passing, string as pair (pointer, length) )

All return call are done by passing a *perror, which is not touched if there is no error
but set to an error code (see cache_mad.h) if there is one, it is a bit different from usual.
Some examples given :

DO

status = 0;
routine(parameters, ... , &status);
if (status != 0) { 
    fprintf(stderr, "%s\n", c_errlist(status));    -- print error code
    ....                                           -- then handle it
    status = 0;                                    -- finally clear it
    }

If you're lazzy, you can do

status = 0;
routine1(parameters, ... , &status);
routine2(parameters, ... , &status);
if (status != 0) { .... 
    }


error code will be kept (are overwritten) by routine2, but NEVER cleared.

DO NOT
status = 0;
routine1(parameters, ... , &status);
status = 0;                              -- never clear without checking 
routine2(parameters, ... , &status);
if (&status != 0) { .... 
    }


Typical use are :
misc.
CH_INITIALISE to be called once to initialise the library
CH_DUMP dump the cache (for debugging)

opening :
a file is handle thru a pointer DATASET *pid (in fact DATASET **ppid for fortran)
a file is opened with a MODE : CF_READ, CF_WRITE (write only), CF_READ_WRITE
CH_CREATE create an empty shell for a file
CH_SETUP set-up the parameters for a file just created with CH_CREATE
CH_OPEN first connection to an already existing file
CH_CLOSE closes the file, flushes the dirty blocks and forgets every thing
CH_JOIN returns the descriptor of a file from a file-name (if opened)
CH_PRINTFILES print all currently opened files.
CH_FLUSH put to disk all the dirty blocks associated with a file (done by CH_CLOSE)
CH_FLUSHALL put to disk all the dirty blocks of the cache
c_errlist / CH_ERRLIST returns the text of a given error code (C / fortran version )

accessing and setting parameters :
a parameter appears in the header as :
param_name   = value_in_ascii \n
CH_GTPARAM reads an untyped parameters (string) from the header
CH_GTPARAM_TYPE reads a typed parameters (int, float, double) from the header
CH_PUTPARAM puts an untyped parameters (string) into the header
CH_PUTPARAM_TYPE puts a typed parameters (int, float, double) into the header
CH_GTPARBAS reads from the header the basic GIFA parameters (for convenience)

accessing data :
data are accessed with a mode : CB_READ, CB_WRITE, CB_LOOK
CB_LOOK is a special mode were the data are internally packed on 8bits instead of 32, thus
taking a fourth of the room internally. This mode is fine for just looking to the data
CH_AC1DAREA access a part of a 1D data
CH_AC2DAREA access a part of a 2D data
CH_AC3DAREA access a part of a 3D data


see in CH_JOIN and in c_getblock for the compatibilities of the different modes

The basic parameters of the data-set are loaded on memory at the opening of the file
in the structure DATASET (see cache_mad.h)
Parameters are : dimension (1,2 or3); sizes; size and number of block; and absmax0

Absmax0 is the value of the largest point of the data-set in absolute 
value, (used for display). value 0.0 means : unknown.
If REFABSMAX is defined (just below), Absmax0 is checked each time a 
block is written to disk. This insures an exact Absmax0, but slows a 
little the writting.
*/

#define REFABSMAX
 
/* the following is to be defined if open() accepts only 2 arguments
I had to do it once on a SUN running SunOS 4.1.3
*/

/* #define OPEN_HAS_2 */


/*
Known bugs
mode CB_LOOK does not work, code is not finished !
files are recognized as name, so if you join, cd some_where_else
and join another file with the same name : collision

To Do list :
mettre le path absolue dans CH_JOIN et CH_OPEN
mettre le noms des manips jointes en $var GIFA


*/
/* cache library */

/*
TAB at 4 char
*/

#include <stdio.h>

#if defined DARWIN
#    include <stdlib.h>
#elif defined MACHTEN
#  include <sys/malloc.h>
#else
#    include <malloc.h>
#endif

#include <fcntl.h>
#include "cache_mad.h"
#include "util.h"
#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <sys/file.h>
#include <math.h>
#include <sys/types.h>
#include <sys/stat.h>
#ifdef DARWIN
#  include <unistd.h>
#endif

/*************************************************/
/* STATIC VARIABLES */
/* the hash table */
BLOCK *tbhash_block[HASHSIZE];

/* the dataset list */
DATASET *p_file_opened;	/* hold the entry in the list */
DATASET *p_last_file;    /* marks the end */

/* the LRU list */
BLOCK *p_firstlru;
BLOCK *p_lastlru;

/* the maximum number of data stored in memory (in byte) */
int		MAXDATA;

/* in memory so far (in byte) (should not go over MAXDATA) */
int		data_so_far;

/* the read buffer for LOOKing and size (in bytes) currently allocated*/
float	*vpread;
int		szpread;

/* the lookup table for LOOK access mode */
float	lookup[256];		/* one entry for each unsigned char */


/*************************************************/
void CH_INITIALISE()

/*
  initialises all the data structures associated to the cache
  should be called once before using the library
*/

{ 	int		i;
	float	lalpha;

	p_file_opened = NULL;

/* initialize lru list with empty blocks on each side */

	if ( (p_firstlru = (BLOCK *) malloc (sizeof(BLOCK))) == NULL) {
	   printf("CACHE : Could not malloc initial memory\n");
	   i = 1;
	   GIFA_EXIT(&i);
	   }
	if ( (p_lastlru  = (BLOCK *) malloc (sizeof(BLOCK))) == NULL) {
	   printf("CACHE : Could not malloc initial memory\n");
	   i = 2;
	   GIFA_EXIT(&i);
	   }
        p_firstlru->pnextlru = p_lastlru;
        p_firstlru->pprevlru = p_firstlru;
        p_lastlru->pprevlru = p_firstlru;
        p_lastlru->pnextlru = p_lastlru;

/* then hash table */
	for (i=0;i<HASHSIZE;i++) {
		tbhash_block[i] = NULL;
		}
	data_so_far = 0;

	MAXDATA = 0;
/* open file list */
	if ( (p_file_opened = (DATASET *) malloc (sizeof(DATASET))) == NULL) {
	   printf("CACHE : Could not malloc initial memory\n");
	   i = 3;
	   GIFA_EXIT(&i);
	 }
	if ( (p_last_file = (DATASET *) malloc (sizeof(DATASET))) == NULL) {
	   printf("CACHE : Could not malloc initial memory\n");
	   i = 4;
	   GIFA_EXIT(&i);
	 }
	p_file_opened->pnext = p_last_file;
	p_last_file->pnext = p_last_file;

	szpread = BLOCKIO;
	vpread = malloc(szpread);
	if (vpread == NULL) {
		fprintf(stderr, "CACHE : could not allocate initial memory ! \n");
		i = 5;
		GIFA_EXIT(&i);
		}

/*
  create a lookup table such that lookup[i] = (alpha) at the power i
  alpha is such as 127 entries cover ~ 1.0E6 (MADABSMAX)  dynamic
  i.e. alpha ~ 1.1150  which is a pretty good resolution

  will be used as value[k] = lookup[ code[k] ]
*/
/* verifier l'ordre des entree */
	lookup[0] = 1.0/MADABSMAX;
	lalpha = log(MADABSMAX)/127;
	for (i=1; i<128; i++) {
		lookup[i] = exp(i*lalpha) / MADABSMAX;
		lookup[i+128] = -lookup[i];
		}
	}

/*************************************************/
void CH_JOIN(	DATASET **ppid,char *path,int  *plen,int  *mode,int  *perror)

/* 
	given a file name (char *path,int  *plen)
        returns the internal descriptor (DATASET **ppid) for this file,
	if the file has already been opened with CH_OPEN (or CH_CREATE)
	
	error if not already opened, or if wrong mode

	mode is :
	CF_READ         access to file in read_only in the file system
	CF_WRITE        for creating a new file - used only by CH_CREATE -
	CF_READ_WRITE   for normal access (JOIN)

    error conditions if joining a file already opened with another mode
    in which case, one should close and reopen

		acc :	R	W	RW
	prev:	R	=	x	x		= Ok	x impossible
		W	x	=	x
		RW	=	=	=
		
*/
{	int		i;
	DATASET *prun;
	char	lpath[NAMEMAX];
	
/* convert from Fortran */
	i = min(*plen, NAMEMAX);
	convert_string(lpath,path, i);

/* check if already there */
	prun = p_file_opened->pnext;
	while (prun != p_last_file) {
		if (strcmp(prun->name, lpath) == 0 ) {		/* found */
	/* reject wrong mode */
			if (  (prun->mode == CF_READ && *mode != CF_READ)
				|| (prun->mode == CF_WRITE && *mode != CF_WRITE)) {
				*perror = WRONG_MODE;
				ppid = NULL;
				return;
			      }
			else {
			  *ppid = prun;
			  return;
			}
		      }
		prun = prun->pnext;
	      }
/* if there - not found */
	*perror = NOT_OPENED;
	}

/*************************************************/
void CH_OPEN(DATASET **ppid,char *path,	int  *plen,int  *mode,int  *error)
/*
				DATASET **ppid,	 returned id 
				char *path,		 the name of the file to open 
				int  *plen,		 length of path 
				int  *mode, 	 CF_READ, CF_WRITE, CF_READ_WRITE
				int  *error)	 returned status

	opens a file into the cache,
	loads the header
	returns error status

jan 2002 MAD - correction to close opened file if error during set-up

*/
{
	int		flag,i,update;
	int		local_id;
	DATASET *pdata, *prun, localdata;
#ifdef SPEC1
	int		ch_mod;
#else
	mode_t		ch_mod;
#endif
	char	*pheader;
	int		loc_type, loc_int, headsize, status;
	char	lpath[NAMEMAX], localparam[32];

/* convert from Fortran */
	i = min(*plen, NAMEMAX);
	convert_string(lpath,path,i);

	if (*mode == CF_READ) {
		flag = O_RDONLY;
		}
	else {
		flag = O_RDWR;
		}

	ch_mod = 00640;

#ifdef OPEN_HAS_2
	if ( (local_id = open(lpath,flag)) == -1) {
#else
	if ( (local_id = open(lpath,flag,ch_mod)) == -1) {
#endif
		switch (errno) {
		case EMFILE:
		case ENFILE:
			*error = TOO_MANY_FILES;
			break;
		case EACCES:
            case EROFS:
			*error = WRONG_MODE;
			break;
		default:
                perror(NULL);
			*error = COULD_NOT_OPEN;
			}
		local_id = 0;
		return;
		}

	strcpy(localdata.name,lpath);
	localdata.mode = *mode;
	localdata.id = local_id;

/* reads (and allocates) header */
	c_loadheader(local_id, &pheader, &headsize, error);
	if (*error != 0) {
		close(local_id);
		return;
	}
	localdata.phead = pheader;	/* will be the private area for that dataset */
	localdata.headsz = headsize;
	
/* and loads basic parameters (default value if missing) */
	loc_type = H_INT;

        status = 0;
	c_gtparam_type(pheader, (void *) &(localdata.dimension), "Dim", loc_type, &status);
	if (status != 0) {
		*error = DIM_MISSING;
		goto free_and_exit;
		}
	c_gtparam_type(pheader, (void *) &(localdata.cacheversion), "Cacheversion", loc_type, &status);
	if (status != 0) { status = 0; localdata.cacheversion = 1;}

	c_gtparam_type(pheader, (void *) &(localdata.cacherelease), "Cacherelease", loc_type, &status);
	if (status != 0) { status = 0; localdata.cacherelease = 0;}

	if ((localdata.cacheversion*100 + localdata.cacherelease) > CACHEVERSION*100 + CACHERELEASE) {
		*error = VERSION_UNK;
		goto free_and_exit;
		}

	localdata.size3 = localdata.size2 = 1;
	localdata.nbblock3 = localdata.nbblock2 = 1;

	switch (localdata.dimension) {
	case 3:
	  c_gtparam_type(pheader, (void *) &(localdata.size3), "Dim3", loc_type, &status);
	  if (status != 0) {
		*error = SIZE_MISSING;
		goto free_and_exit;
		}
	  c_gtparam_type(pheader, (void *) &(localdata.nbblock3), "Nbblock3", loc_type, &status);
	  if (status != 0) {
		*error = BLKDEF_MISSING;
		goto free_and_exit;
		}
	case 2:
	  c_gtparam_type(pheader, (void *) &(localdata.size2), "Dim2", loc_type, &status);
	  if (status != 0) {
		*error = SIZE_MISSING;
		goto free_and_exit;
		}
	  c_gtparam_type(pheader, (void *) &(localdata.nbblock2), "Nbblock2", loc_type, &status);
	  if (status != 0) {
		*error = BLKDEF_MISSING;
		goto free_and_exit;
		}
	case 1:
	  c_gtparam_type(pheader, (void *) &(localdata.size1), "Dim1", loc_type, &status);
	  if (status != 0) {
		*error = SIZE_MISSING;
		goto free_and_exit;
		}
	  c_gtparam_type(pheader, (void *) &(localdata.nbblock1), "Nbblock1", loc_type, &status);
	  if (status != 0) {
		*error = BLKDEF_MISSING;
		goto free_and_exit;
		}
	}

	c_gtparam(pheader, localparam, "Byteorder", &status);
	if (status != 0 || strcmp(localparam,"big_endian") !=0) {
	 	status = 0; localdata.big_endian = FALSE;
		} else {
		localdata.big_endian = TRUE;     
		}

	c_gtparam_type(pheader, (void *) &(localdata.type), "Type", loc_type, &status);
	if (status != 0) { status = 0; localdata.type = 0;}

	loc_type = H_FLOAT;
	c_gtparam_type(pheader, (void *) &(localdata.absmax0), "Absmax", loc_type, &status);
	if (status != 0 || localdata.absmax0 <= 0.0) { status = 0; localdata.absmax0 = 0.0;}

	if (localdata.cacheversion == 1 ) {
/* blocks are hard-wired in version 1 */
		switch (localdata.dimension) {
			case 1:
				localdata.szblk1 = 4096;
				localdata.szblk2 = 1;
				localdata.szblk3 = 1;
				break;
			case 2:
				localdata.szblk1 = 64;
				localdata.szblk2 = 64;
				localdata.szblk3 = 1;
				break;
			case 3:
				localdata.szblk1 = 16;
				localdata.szblk2 = 16;
				localdata.szblk3 = 16;
				break;
			}
		}
	else {
	  localdata.szblk3 = localdata.szblk2 = 1;

	  loc_type = H_INT;
	  switch (localdata.dimension) {
	  case 3:
		c_gtparam_type(pheader, (void *) &(localdata.szblk3), "Szblk3", loc_type, &status);
		if (status != 0) {
			*error = BLKDEF_MISSING;
			goto free_and_exit;
			}
	  case 2:
		c_gtparam_type(pheader, (void *) &(localdata.szblk2), "Szblk2", loc_type, &status);
		if (status != 0) {
			*error = BLKDEF_MISSING;
			goto free_and_exit;
			}
	  case 1:
		c_gtparam_type(pheader, (void *) &(localdata.szblk1), "Szblk1", loc_type, &status);
		if (status != 0) {
			*error = BLKDEF_MISSING;
			goto free_and_exit;
		      }
	      }
	}

	localdata.szblk_tot = localdata.szblk1 * localdata.szblk2 * localdata.szblk3;

	localdata.head_dirty = FALSE;

	localdata.there = 1;
		

/* make a new entry for the list of opened file */
	if ( (pdata = malloc( sizeof(DATASET))) == NULL ) {
		*error = COULD_NOT_MALLOC;
		goto free_and_exit;
		}

	*pdata = localdata;

/* then insert in list */
	pdata->pnext = p_file_opened->pnext;
	p_file_opened->pnext = pdata;

	*ppid = pdata;
/* update MAXDATA such as to be able to store at least one complete 1D line */
/* or one complete plane in 3D */

	
	MAXDATA = MAXDATA + c_comp_update(pdata)*sizeof(float);


	return;				/* normal exit */

free_and_exit:	/* error exit */
	free(pheader);
	close(local_id);
	return;

}		


/*************************************************/
void CH_CREATE(DATASET **ppid,char *path, int *plen,int *mode,int *perror)
/*
		DATASET **ppid,	 returned id 
		char *path,		 the name of the file to open 
		int  *plen,		 length of path 
		int  *mode, 	 CF_WRITE only
		int  *perror)	 returned status

	creates a new file and put it into the cache,
	creates the header
	returns error status

jan 2002 MAD - correction to close opened file if error during set-up

*/
{
	int		flag,i,update;
	int		local_id;
	DATASET *pdata, *prun, localdata;
#ifdef SPEC1
	int		ch_mod;
#else
	mode_t		ch_mod;
#endif
	char	*pheader;
	int		loc_type, loc_int, headsize, status;
	char	lpath[NAMEMAX];

/* convert from Fortran */
	i = min(*plen, NAMEMAX);
	convert_string(lpath,path,i);

	if (*mode != CF_WRITE) {
		*perror = WRONG_MODE;
		return;
		}
	flag = O_WRONLY | O_CREAT;

	ch_mod = 0640;

#ifdef OPEN_HAS_2
	if ( (local_id = open(lpath,flag)) == -1) {
#else
	if ( (local_id = open(lpath,flag,ch_mod)) == -1) {
#endif
		switch (errno) {
		case EMFILE:
		case ENFILE:
			*perror = TOO_MANY_FILES;
			break;
		default:
			*perror = COULD_NOT_OPEN;
			}
		local_id = 0;
		return;
		}

#ifdef OPEN_HAS_2
        if (fchmod(local_id, ch_mod) == -1) {
		close(local_id);
                *perror = WRONG_MODE;
		return;
        }
#endif

	strcpy(localdata.name,lpath);
	localdata.mode = *mode;
	localdata.id = local_id;

/*set default values */
	if( (pheader = malloc(BLOCKIO+1)) == NULL) {
		*perror = COULD_NOT_MALLOC;
		close(local_id);
		return;
		}
	for (i=0; i<BLOCKIO; pheader[i++] = '0')  ;
	pheader[BLOCKIO]='\0';

	localdata.phead = pheader;
	localdata.headsz = BLOCKIO;
	loc_type = H_INT;
	c_putparam_type(&localdata,&(localdata.headsz), "HeaderSize", loc_type, perror);
	localdata.head_dirty = TRUE;
	localdata.cacheversion = CACHEVERSION;
	c_putparam_type(&localdata,&(localdata.cacheversion), "Cacheversion", loc_type, perror);
	localdata.cacherelease = CACHERELEASE;
	c_putparam_type(&localdata,&(localdata.cacherelease), "Cacherelease", loc_type, perror);

/* if on a BIG_ENDIAN machine, dataset is created big_endian (defined outside)
   else, nothing is done in the file, since little_endian is default */
#ifdef is_big_endian
        localdata.big_endian = TRUE;
	c_putparam(&localdata,"big_endian","Byteorder", perror);
#else
	localdata.big_endian = FALSE;
#endif

	localdata.dimension = 0;
	localdata.size1 = 0;
	localdata.size2 = 0;
	localdata.size3 = 0;
	localdata.nbblock1 = 0;
	localdata.nbblock2 = 0;
	localdata.nbblock3 = 0;
	localdata.szblk1 = 0;
	localdata.szblk2 = 0;
	localdata.szblk3 = 0;
	localdata.szblk_tot = 0;
	localdata.absmax0 = 0.0;
	localdata.type = 0;
	localdata.there = 0;
	localdata.szblk_tot = localdata.szblk1 * localdata.szblk2 * localdata.szblk3;

	localdata.there = 1;


/* make a new entry for the list of opened file */
	if ( (pdata = malloc( sizeof(DATASET))) == NULL ) {
		*perror = COULD_NOT_MALLOC;
		goto free_and_exit;
		}

	*pdata = localdata;

/* then insert in list */
	pdata->pnext = p_file_opened->pnext;
	p_file_opened->pnext = pdata;

	*ppid = pdata;

	return;				/* normal exit */

free_and_exit:	/* error exit */
	free(pheader);
	close(local_id);
	return;

}		


/*************************************************/
void CH_SETUP(DATASET **ppid, int *dim, int *si1, int *si2, int *si3,int  *perror)
/*
	used to set-up a file the very first time (dimension and sizes).
	These parameters should not be changed with c_putparam
	- typically called after CH_CREATE -
*/
{	int		loc_type,n1,n2,n3;
	float	sz12;
	loc_type = H_INT;

/* set-up basic params */
	(*ppid)->dimension = *dim;
	c_putparam_type(*ppid,dim, "Dim", loc_type, perror);

	(*ppid)->size1 = *si1;
	c_putparam_type(*ppid, si1, "Dim1", loc_type, perror);

	if (*dim > 1) {
		(*ppid)->size2 = *si2;
		c_putparam_type(*ppid, si2, "Dim2", loc_type, perror);
		}
	if (*dim > 2) {
		(*ppid)->size3 = *si3;
		c_putparam_type(*ppid, si3, "Dim3", loc_type, perror);
		}
/*
	choose here the size of blocks, chosen such that the ratio of the
	sizes of the block matches the ratio of the sizes of the data-set
*/

	switch (*dim) {
		case 1:		/* no need here */
			n1 = (*ppid)->szblk1 = BLOCKIO / sizeof(float);
			c_putparam_type(*ppid, &n1, "Szblk1", loc_type, perror);
			(*ppid)->szblk2 = 1;
			(*ppid)->szblk3 = 1;
			(*ppid)->nbblock1 = (*si1 -1)/ n1 + 1;
			c_putparam_type(*ppid, &((*ppid)->nbblock1), "Nbblock1", loc_type, perror);			
			(*ppid)->nbblock2 = 1;
			(*ppid)->nbblock3 =1;
			break;
		case 2:
			sz12 = (float) *si2 / (float) *si1;
			n2 = 1;
			n1 = BLOCKIO / sizeof(float);
			while ( ( ((float) n2/ (float) n1) < sz12) && (n1 > 1 ) ) {
				n1 = n1/2;	n2 = n2*2;
				}
			(*ppid)->szblk1 = n1;
			c_putparam_type(*ppid, &n1, "Szblk1", loc_type, perror);
			(*ppid)->szblk2 = n2;
			c_putparam_type(*ppid, &n2, "Szblk2", loc_type, perror);
			(*ppid)->szblk3 = 1;

			(*ppid)->nbblock1 = (*si1 -1)/ n1 + 1;
			c_putparam_type(*ppid, &((*ppid)->nbblock1), "Nbblock1", loc_type, perror);			
			(*ppid)->nbblock2 = (*si2 -1)/ n2 + 1;
			c_putparam_type(*ppid, &((*ppid)->nbblock2), "Nbblock2", loc_type, perror);			
			(*ppid)->nbblock3 =1;
			break;
		case 3:
			sz12 = (float) (*si2 * *si3) / (float) (*si1 * *si1);
			n1 = BLOCKIO / sizeof(float);
			n2 = 1;	n3 = 1;
			while ( (((float)(n2*n3) / (float) (n1*n1)) < sz12 ) && (n1 > 1) ) {
				n1 = n1/2;	n2 = n2*2;
				}
			sz12 = (float) *si3 / (float) *si2;
			while ( (((float) n3/ (float) n2) < sz12 ) && (n2 > 1) ) {
				n2 = n2/2;	n3 = n3*2;
				}
			(*ppid)->szblk1 = n1;
			c_putparam_type(*ppid, &n1, "Szblk1", loc_type, perror);
			(*ppid)->szblk2 = n2;
			c_putparam_type(*ppid, &n2, "Szblk2", loc_type, perror);
			(*ppid)->szblk3 = n3;
			c_putparam_type(*ppid, &n3, "Szblk3", loc_type, perror);

			(*ppid)->nbblock1 = (*si1 -1)/ n1 + 1;
			c_putparam_type(*ppid, &((*ppid)->nbblock1), "Nbblock1", loc_type, perror);			
			(*ppid)->nbblock2 = (*si2 -1)/ n2 + 1;
			c_putparam_type(*ppid, &((*ppid)->nbblock2), "Nbblock2", loc_type, perror);			
			(*ppid)->nbblock3 = (*si3 -1)/ n3 + 1;
			c_putparam_type(*ppid, &((*ppid)->nbblock3), "Nbblock3", loc_type, perror);			
			break;
		}
	(*ppid)->szblk_tot = (*ppid)->szblk1 * (*ppid)->szblk2 * (*ppid)->szblk3;

/* then compute update on MAXDATA */

	MAXDATA = MAXDATA + c_comp_update(*ppid)*sizeof(float);


	}

/*************************************************/
void CH_CLOSE(DATASET **ppid, int  *perror)


/*	closes an opened cache file
	flushes and forgets all blocks that need to
	double pointers because of FORTRAN
*/
{	DATASET *prev, *next;
	int		update;

	c_remdataset(*ppid, perror);
	if (*perror != 0) return;

	if ((*ppid)->head_dirty) {
		c_writeheader(*ppid, perror);
		}
	if (*perror != 0) return;

/*remove from list */
	next = p_file_opened;
	while ( next != p_last_file ) {
	   if (next->pnext == *ppid) {
	      next->pnext = next->pnext->pnext;
	      goto found;
	      }
	   next = next->pnext;
	   }
	*perror = NOT_OPENED;
	return;

found :
/* close file */
	close((*ppid)->id);

/* update MAXDATA */

	update = c_comp_update(*ppid)*sizeof(float);
	MAXDATA = max(MAXDATA - update,0);

/* free memory */
#ifdef DEBUG
	memset((*ppid)->phead,0xA0A0,(*ppid)->headsz);	/* clear memory */
#endif
	free((*ppid)->phead);
#ifdef DEBUG
	memset(*ppid,0xA0A0,sizeof(DATASET));
#endif
	free(*ppid);
	*ppid = 0;
}
	
/*************************************************/
void CH_PRINTFILES()
/*
print all the currently joined file
*/
{
	DATASET *prun;

	if( p_file_opened->pnext == p_last_file) {
		GIFAOUTC("No currently JOINed file");
		}
	else {
		for (prun = p_file_opened->pnext;
		     prun != p_last_file;
		     prun = prun->pnext) {
			GIFAOUTC(prun->name);
		}
	}		
}

/*************************************************/
void CH_FLUSH(DATASET **ppdata, int	*perror)
/*
	flushes all the blocks from a given dataset
*/
{
	BLOCK *pthru;

	if ((*ppdata)->mode == CF_READ ) return;

/* then data */
	for( pthru = p_firstlru->pnextlru;
	     pthru != p_lastlru;
	     pthru = pthru->pnextlru) {
		if (pthru->pdataset == *ppdata) {	/* if found one */
			if (pthru->dirty) {		/* write it if dirty */
				c_writeblock(pthru,perror);
				if (*perror != 0) return;
				pthru->dirty = FALSE;
			      }
		      }
	      }
/* write header */
	if ((*ppdata)->head_dirty) {
	  c_writeheader(*ppdata,perror);
	  if (*perror != 0) return;
	}
	 
      }

/*************************************************/
void CH_FLUSHALL(int *perror)
/*
	flushes all the blocks in the cache
*/
{
	DATASET *pthru;

	for (pthru = p_file_opened->pnext;
	     pthru != p_last_file;
	     pthru=pthru->pnext) {
		CH_FLUSH(&pthru,perror);
		if (*perror != 0) return;
		}
	}

/*************************************************/
void CH_AC1DAREA(float	*parea, DATASET	**ppdata, int  *plow, int  *pup, int *mode, int  *perror)
/*
	access a portion of a 1D, a sector with coordinates
	*plow,  *pup  "1 based" ;
	*mode can be	CB_READ : copy from dataset to parea
			CB_WRITE : copy from parea to dataset
			CB_LOOK : copy a 8bit coded version of dataset to parea
*/
{	int szb,size,ind,i,ia,ii,k,tt,end,off;
	BLOCK	*pbl;


	if ( !is_there(**ppdata)) {
		*perror = NOT_OPENED;
		return;
		}
	if ((*ppdata)->dimension != 1) {
		*perror = WRONG_DIM;
		return;
		}
	szb = (*ppdata)->szblk1;
	size = *plow - *pup + 1;

	if (   ((*plow) > (*ppdata)->size1)		/* check border */
		|| ((*plow) < 1)
	    || ((*pup) > (*ppdata)->size1)
		|| ((*pup) < 1)
		|| ((*plow) > (*pup)) ) {
		*perror = OUTSIDE;
		return;
		}

	i = *plow-1;
	while (i <= *pup-1) {
		ind = comp_ind_1D(i, **ppdata);
		pbl = c_getblock(*ppdata, ind, *mode, perror);
		if (pbl == NULL) { return; }
		ii = i + szb;
		ii = (ii/szb)*szb -1;
		ii = min(*pup-1, ii);

		off = i % szb;
		end = ii % szb;
		tt = end - off + 1;
		ia = i - *plow + 1;

/* block at index ind
off, end, relative to block
i, ii relative to dataset 
ia relative to parea
	          <--------szb--------->
              ----**************----
                 i|          ii|
               off|         end|
                  <-- tt ------>
*/
		switch ( *mode) {
		case CB_READ:
			memcpy( parea + ia, (pbl->pdata.pread) + off, tt * sizeof(float));
			break;
		case CB_WRITE:
			memcpy( (pbl->pdata.pread) + off, parea + ia, tt * sizeof(float));
			break;
		case CB_LOOK:
			for (k=0; k<tt; k++) {
				parea[ia+k] = (*ppdata)->absmax0 * lookup[ (pbl->pdata.plook)[off+k] ];
				}
			break;
		default:
			*perror = WRONG_MODE;
			return;
			}
		i = ii+1;
		}
	}

/*************************************************/
void CH_AC2DAREA(float	*parea, DATASET	**ppdata, int  *plf1, int  *plf2, int  *pur1,  int  *pur2, int *mode, int  *perror)

/*
	access a portion of a 2D, a rectangle with coordinates
	*plf1, *plf2, *pur1, *pur2 (lowerleft, upperright  "1 based" ;
	 	1 is for "dim1, vertical" )
	parea is supposed to point to a buffer of size exactly (ur1-lf1+1)*(ur2-lf2+1)
	*mode can be	CB_READ : copy from dataset to parea
		       	CB_WRITE : copy from parea to dataset
			CB_LOOK : copy a 8bit coded version of dataset to parea
*/
{
	int ind, size2, szb1, szb2, i, j, k, ii, jj, ia, ja, cpy,
	    off1, off2, end1, end2, tt1, tt2, from, to;
	BLOCK	*pbl;

	if ( !is_there(**ppdata)) {
		*perror = NOT_OPENED;
		return;
		}
	if ((*ppdata)->dimension != 2) {
		*perror = WRONG_DIM;
		return;
		}
	szb1 = (*ppdata)->szblk1;
	szb2 = (*ppdata)->szblk2;
	size2 = *pur2 - *plf2 + 1;

	if (   ((*plf1) > (*ppdata)->size1)		/* check border */
		|| ((*plf1) < 1)
	    || ((*pur1) > (*ppdata)->size1)
		|| ((*pur1) < 1)
		|| ((*plf1) > (*pur1))
	    || ((*plf2) > (*ppdata)->size2)
	    || ((*plf2) < 1)
	    || ((*pur2) > (*ppdata)->size2)
	    || ((*pur2) < 1)
	    || ((*plf2) > (*pur2)) ) {
		*perror = OUTSIDE;
		return;
		}

	i = *plf1-1;
	while (i <= *pur1-1) {
		ii = i + szb1;
		ii = (ii/szb1)*szb1 -1;
		ii = min(*pur1-1, ii);
		off1 = i % szb1;
		end1 = ii % szb1;
		tt1 = end1 - off1 + 1;
		ia = i - *plf1 + 1;

		j = *plf2-1;
		while (j <= *pur2-1) {
			ind = comp_ind_2D(i, j, **ppdata);
			pbl = c_getblock(*ppdata, ind, *mode, perror);
			if (pbl == NULL) { return; }
			jj = j + szb2;
			jj = (jj/szb2)*szb2 -1;
			jj = min(*pur2-1, jj);
			off2 = j % szb2;
			end2 = jj % szb2;
			tt2 = end2 - off2+1;

			ja = j - *plf2 + 1;
/* block at index ind
             <--------szb2-------->
              --------------------  ^
   ^ ii end1-|     -------------  | |
   |         |    |to be copied | | szb1
             |    |             | | |
  tt1        |    |             | |	|	off1, off2, end1, end2 relative to block
   |         |    |             | |	|	i, j, ii, jj relative to dataset         
   v  i off1-|     -------------  | |	ia, ja relative to parea
             |                    | |
              --------------------  v
                 j|           jj|
              off2|         end2|
                  <-- tt2 ------>
*/
			switch ( *mode) {
			case CB_READ:
				for (k=0; k<tt1; k++) {
					from = (off1+k)*szb2 + off2;
					to = (ia+k)*size2 + ja;
					memcpy( parea + to, (pbl->pdata.pread) + from, tt2 * sizeof(float));
					}
				break;
		        case CB_WRITE:
				for (k=0; k<tt1; k++) {
					to = (off1+k)*szb2 + off2;
					from = (ia+k)*size2 + ja;
					memcpy( (pbl->pdata.pread) + to, parea + from, tt2 * sizeof(float));
					}
				break;
			case CB_LOOK:
				for (k=0; k<tt1; k++) {
					from = (off1+k)*szb2 + off2;
					to = (ia+k)*size2 + ja;
					for (cpy=0; cpy<tt2; cpy++) {
						parea[to+cpy] = (*ppdata)->absmax0 * lookup[ (pbl->pdata.plook)[from+cpy] ];
						}
					}
				break;
			default:
				*perror = WRONG_MODE;
				return;
				}
			j = jj+1;
			}
		i = ii+1;
		}
	}


/*************************************************/
void CH_AC3DAREA(float	*parea, DATASET	**ppdata, int  *plf1, int  *plf2, int  *plf3, int  *pur1,  int  *pur2, int  *pur3, int *mode, int  *perror)

/*
	access a portion of a 3D, a subcube with coordinates
	*plf1, *plf2, *plf2, *pur1, *pur2, *pur3 (lowerleft, upperright  "1 based" )

	parea is supposed to point to a buffer of size exactly (ur1-lf1+1)*(ur2-lf2+1)*(ur3-lf3+1)
	*mode can be	CB_READ : copy from dataset to parea
			CB_WRITE : copy from parea to dataset
			CB_LOOK : copy a 8bit coded version of dataset to parea
*/
{
	int ind, size2, size3, szb1, szb2, szb3, i, j, k, m1, m2, ii, jj, kk, ia, ja, ka, cpy,
	    off1, off2, off3, end1, end2, end3, tt1, tt2, tt3, from, to;
	BLOCK	*pbl;
#ifdef DEBUG
   int sizet;
#endif

	if ( !is_there(**ppdata)) {
		*perror = NOT_OPENED;
		return;
		}
	if ((*ppdata)->dimension != 3) {
		*perror = WRONG_DIM;
		return;
		}
	szb1 = (*ppdata)->szblk1;
	szb2 = (*ppdata)->szblk2;
	szb3 = (*ppdata)->szblk3;
	size2 = *pur2 - *plf2 + 1;
	size3 = *pur3 - *plf3 + 1;
#ifdef DEBUG
        sizet = (*pur1 - *plf1 +1)*size2*size3;
#endif

	if (   ((*plf1) > (*ppdata)->size1)		/* check border */
		|| ((*plf1) < 1)
	    || ((*pur1) > (*ppdata)->size1)
		|| ((*pur1) < 1)
		|| ((*plf1) > (*pur1))
	    || ((*plf2) > (*ppdata)->size2)
	    || ((*plf2) < 1)
	    || ((*pur2) > (*ppdata)->size2)
	    || ((*pur2) < 1)
	    || ((*plf2) > (*pur2))
	    || ((*plf3) > (*ppdata)->size3)
	    || ((*plf3) < 1)
	    || ((*pur3) > (*ppdata)->size3)
	    || ((*pur3) < 1)
	    || ((*plf3) > (*pur3)) ) {
		*perror = OUTSIDE;
		return;
		}

	i = *plf1-1;
	while (i <= *pur1-1) {
		ii = i + szb1;
		ii = (ii/szb1)*szb1 -1;
		ii = min(*pur1-1, ii);
		off1 = i % szb1;
		end1 = ii % szb1;
		tt1 = end1 - off1 + 1;
		ia = i - *plf1 + 1;

		j = *plf2-1;
		while (j <= *pur2-1) {
		  jj = j + szb2;
		  jj = (jj/szb2)*szb2 -1;
		  jj = min(*pur2-1, jj);
		  off2 = j % szb2;
		  end2 = jj % szb2;
		  tt2 = end2 - off2 + 1;
		  ja = j - *plf2 + 1;

		  k = *plf3-1;
		  while (k <= *pur3-1) {
			ind = comp_ind_3D(i, j, k, **ppdata);
			pbl = c_getblock(*ppdata, ind, *mode, perror);
			if (pbl == NULL) { return; }
			kk = k + szb3;
			kk = (kk/szb3)*szb3 -1;
			kk = min(*pur3-1, kk);
			off3 = k % szb3;
			end3 = kk % szb3;
			tt3 = end3 - off3+1;

			ka = k - *plf3 + 1;
#if (1 == 0)
  printf ("...ac_3darea : i,j,k : %d %d %d\n",i,j,k);
#endif
/* same algo as in 2D */
			switch ( *mode) {
			case CB_READ:
			  for (m1=0; m1<tt1; m1++) for (m2=0; m2<tt2; m2++) {
			    from = (off1+m1)*szb2*szb3 + (off2+m2)*szb3 + off3;
			    to = (ia+m1)*size2*size3 + (ja+m2)*size3 + ka;
#ifdef DEBUG
   if (  (from + tt3 * sizeof(float)) > pbl->size
       ||(to + tt3 * sizeof(float)) > sizet*sizeof(float) ) { 
          printf ("Outside of memory in ac_3darea/READ, at i,j,k :%d %d %d m1,m2:%d %d\n",i,j,k,m1,m2);
	}
#endif
			    memcpy( parea + to, (pbl->pdata.pread) + from, tt3 * sizeof(float));
			  }
			  break;
			case CB_WRITE:
			  for (m1=0; m1<tt1; m1++) for (m2=0; m2<tt2; m2++) {
			    to = (off1+m1)*szb2*szb3 + (off2+m2)*szb3 + off3;
			    from = (ia+m1)*size2*size3 + (ja+m2)*size3 + ka;
#ifdef DEBUG
   if (  (from + tt3 * sizeof(float)) > sizet*sizeof(float)
       ||(to + tt3 * sizeof(float)) > pbl->size  ) { 
          printf ("Outside of memory in ac_3darea/WRITE, at i,j,k :%d %d %d m1,m2:%d %d\n",i,j,k,m1,m2);
	}
#endif
			    memcpy( (pbl->pdata.pread) + to, parea + from, tt3 * sizeof(float));
			  }
			  break;
			case CB_LOOK:
			  for (m1=0; m1<tt1; m1++) for (m2=0; m2<tt2; m2++) {
			    from = (off1+m1)*szb2*szb3 + (off2+m2)*szb3 + off3;
			    to = (ia+m1)*size2*size3 + (ja+m2)*size3 + ka;
			    for (cpy=0; cpy<tt3; cpy++) {
			      parea[to+cpy] = (*ppdata)->absmax0 * lookup[ (pbl->pdata.plook)[from+cpy] ];
			    }
			  }
			  break;
			default:
				*perror = WRONG_MODE;
				return;
				}
			k = kk+1;
		      }
		  j = jj+1;
		}
		i = ii+1;
	      }
      }







/*************************************************/
void CH_GTPARBAS(DATASET **ppdt,int *c_dimmanip,int *c_sizef1,int *c_sizef2,
	int *c_sizef3,int *c_type,float *c_specwf1,float *c_specwf2,float *c_specwf3,
	double *c_freq,double *c_freq1,double *c_freq2,double *c_freq3,
	float *c_offsf1,float *c_offsf2,float *c_offsf3, 
        float *dmin, float *dmax, float *dfactor, float *c_max0)
/*
gets from the file the GIFA basic parameters associated to the file
*/
{	int status;

	*c_dimmanip = (*ppdt)->dimension;
	*c_sizef1 = (*ppdt)->size1;
	*c_sizef2 = (*ppdt)->size2;
	*c_sizef3 = (*ppdt)->size3;
	*c_type = (*ppdt)->type;

	status = 0;
	c_gtparam_type((*ppdt)->phead, c_specwf1, "Specw1", H_FLOAT, &status);
	if (status != 0) { *c_specwf1 = 2000*3.1415;	status = 0;}
	c_gtparam_type((*ppdt)->phead, c_specwf2, "Specw2", H_FLOAT, &status);
	if (status != 0) { *c_specwf2 = 2000*3.1415;	status = 0;}
	c_gtparam_type((*ppdt)->phead, c_specwf3, "Specw3", H_FLOAT, &status);
	if (status != 0) { *c_specwf3 = 2000*3.1415;	status = 0;}

	c_gtparam_type((*ppdt)->phead, c_offsf1, "Offset1", H_FLOAT, &status);
	if (status != 0) { *c_offsf1 = 0.0;	status = 0;}
	c_gtparam_type((*ppdt)->phead, c_offsf2, "Offset2", H_FLOAT, &status);
	if (status != 0) { *c_offsf2 = 0.0;	status = 0;}
	c_gtparam_type((*ppdt)->phead, c_offsf3, "Offset3", H_FLOAT, &status);
	if (status != 0) { *c_offsf3 = 0.0;	status = 0;}

	c_gtparam_type((*ppdt)->phead, c_freq, "Frequency", H_DOUBLE, &status);
	if (status != 0) { *c_freq = 400.0;	status = 0;}
	c_gtparam_type((*ppdt)->phead, c_freq1, "Freq1", H_DOUBLE, &status);
	if (status != 0) { *c_freq1 = 400.0;	status = 0;}
	c_gtparam_type((*ppdt)->phead, c_freq2, "Freq2", H_DOUBLE, &status);
	if (status != 0) { *c_freq2 = 400.0;	status = 0;}
	c_gtparam_type((*ppdt)->phead, c_freq3, "Freq3", H_DOUBLE, &status);
	if (status != 0) { *c_freq3 = 400.0;	status = 0;}

	c_gtparam_type((*ppdt)->phead, dmin, "Dmin", H_FLOAT, &status);
	if (status != 0) { status = 0;}
	c_gtparam_type((*ppdt)->phead, dmax, "Dmax", H_FLOAT, &status);
	if (status != 0) { status = 0;}
	c_gtparam_type((*ppdt)->phead, dfactor, "Dfactor", H_FLOAT, &status);
	if (status != 0) { status = 0;}

	c_gtparam_type((*ppdt)->phead, c_max0, "Absmax", H_FLOAT, &status);
	if (status != 0) { *c_max0 = 0.0;	status = 0;}

	}
/*************************************************/
void CH_GTPARAM_TYPE(DATASET **pdt,void *val, char *param, int *len, int  *h_type, int  *status)

/*	find the value of "param" into "dt->header"
	with type "h_type"
	Types can be : H_INT H_FLOAT H_DOUBLE
	can be called from fortran
*/
{	char stl[STMAX];

	convert_string(stl, param, *len);

	c_gtparam_type((*pdt)->phead, val, stl, *h_type, status);

}
/*************************************************/
void CH_GTPARAM(DATASET **pdt,char *val, int *vlen,  char *param, int *plen, int *status)
/*	find the value (string) of "param" into "dt->phead"
	entries are of the form :
	param       = value \n
	can be called from fortran
*/
{	char stl[STMAX], vall[STMAX];

	convert_string(stl, param, *plen);

	c_gtparam((*pdt)->phead, vall, stl, status);

	cconvert_string(val, vall, vlen);
	}
/*************************************************/
void CH_PUTPARAM_TYPE(DATASET **pdt,void *val,char *param, int *len, int *h_type, int *status)
/* put the typed value of param into header of *pdt
	can be called from fortran
*/
{	char stl[STMAX];

	convert_string(stl, param, *len);

	c_putparam_type(*pdt, val, stl, *h_type, status);
	}

/*************************************************/
void CH_PUTPARAM(DATASET **pdt, char *val, int *vlen, char *param, int *plen, int *status)
/*
	insert a parameter named "param" into dt->phead,
	will be of the form : param      = val\n

	can be called from fortran
*/
{	char vall[STMAX], stl[STMAX];
	convert_string(stl, param, *plen);
	convert_string(vall, val, *vlen);

	c_putparam(*pdt,vall,stl,status);
	}

/*************************************************/
/*         LOCAL entries                         */
/*************************************************/
int c_comp_update(DATASET *pdata)
/*
Computes the size of the update for MAXDATA
*/
{
  int  update;
	switch (pdata->dimension) {
	case 1:
	case 2:
	  update = (pdata->nbblock1)*(pdata->szblk_tot) ;
	  update = max(update, (pdata->nbblock2)*(pdata->szblk_tot)) ;
	  update = max(update, (pdata->nbblock3)*(pdata->szblk_tot)) ;
	  break;
	case 3:
	  update = (pdata->nbblock1)*(pdata->nbblock2)*(pdata->szblk_tot) ;
	  update = max(update, (pdata->nbblock2)*(pdata->nbblock3)*(pdata->szblk_tot)) ;
	  update = max(update, (pdata->nbblock1)*(pdata->nbblock3)*(pdata->szblk_tot)) ;
	}
  return(update);
}
/*************************************************************/
void c_unplug_lru(BLOCK *pb)
/*
  unplug a block from lru
*/
{
	BLOCK *prev, *next;

		prev = pb->pprevlru;
		next = pb->pnextlru;
		prev->pnextlru = next;
		next->pprevlru = prev;

}
/*************************************************/
void c_unplug_hash(BLOCK *pb)

/*
  unplug a block from hash
*/
{
	BLOCK *next;

/* have to find it first */
	next = tbhash_block[pb->key];
	if (next == pb) {           /* need special treatment */
	  tbhash_block[pb->key] = pb->pnexthash;
	  return;
	  }
	while ( next != NULL ) {
	   if (next->pnexthash == pb) {
	      next->pnexthash = next->pnexthash->pnexthash;
	      return;
	      }
	   next = next->pnexthash;
	   }
	assert( 0 == 1);

}
/*************************************************/
void c_remblock(BLOCK *pb, int *perror)
/*
	remove a block from memory
*/
{
	BLOCK *prev, *next;

/* if dirty, has to write it */
	if (pb->dirty) {
		c_writeblock(pb,perror);
		if (*perror != 0) return ;
		}
/* plug out from LRU and hash */
	c_unplug_lru(pb);
	c_unplug_hash(pb);

/* then free */
	data_so_far = data_so_far - pb->size;
#ifdef DEBUG
	memset(pb->pdata.pread,0xA0A0,pb->size);	/* clear memory */
#endif
	free(pb->pdata.pread);
#ifdef DEBUG

	memset(pb,0xA0A0,sizeof(BLOCK));
#endif
	free(pb);
	}

/*************************************************/
void c_remdataset(DATASET *pdata, int *perror)
/*
	frees all the blocks from a given dataset
	flushes them if needed
*/
{
	BLOCK *pthru, *next;

	pthru = p_firstlru->pnextlru;
	while (pthru != p_lastlru) {
		if (pthru->pdataset == pdata) {		/* if found one */
			next = pthru->pnextlru;
			c_remblock(pthru, perror);
			if (*perror != 0) return;
			pthru = next;
			}
		else {
			pthru = pthru->pnextlru;
			}
		}
      }
/*************************************************/
BLOCK *c_getblock(DATASET *pdtset,int index,int mode,int *perror)
/*
	accesses the block from file "pdtset" at "index", with "mode" (CB_READ, CB_WRITE, CB_LOOK)

	a pointer to the block is returned (NULL if error)
	if mode is CB_LOOK or CB_READ or (CB_WRITE, but file access is CF_READ_WRITE) ,
            data are or retrieved in the
            currently set of blocks in memory
            or fetch from disk and put into the pool of blocks
            and then put at the end of LRU list
	if mode is CB_WRITE  the block is set as dirty
	if mode is CB_WRITE and file mode is CF_WRITE
           only allocation is done - no test on previous content	!!risky!!
	if mode is CB_LOOK the data are transformed from float to unsigned char
		in a logarithmic coding, thus compressing by a factor 4
	if a block is accessed in CB_READ or CB_WRITE and already in memory in CB_LOOK, the old
          block is discarded and reloaded.

	possible access modes depend on file mode : (see CH_JOIN):
		= ok; x:impossible; 

		file_mode	CF_R	CF_W	CF_RW
	acces mode	CB_R	=	x	=
			CB_W    x	=	=
			CB_L	=	x	=

	if x : close and reopen file with CF_RW mode

*/
{
	unsigned	hh;
	BLOCK	*ptb, *prev;

#ifdef toto
#ifdef DEBUG
	printf("c_getblock(dataset = %s, index = %d, mode = %d, *perror = %d\n",
                              pdtset->name,    index,        mode,         *perror);
	CH_DUMP();
#endif
#endif

/*check access mode */
        if ((pdtset->mode == CF_READ && mode == CB_WRITE) ||
	    (pdtset->mode == CF_WRITE && mode != CB_WRITE)) {
	  *perror = WRONG_MODE;
	  return(NULL);
	}

	hh = c_hash(pdtset, index);
	ptb = tbhash_block[hh];
	while (ptb != NULL) {
		if (ptb->pdataset == pdtset  &&  ptb->index == index) {		/* found ! */
		               /* then compare mode */
		  if ( ptb->mode == CB_LOOK && mode != CB_LOOK) {
		    c_remblock(ptb,perror);
		    goto not_found;
		  } else {
		    goto found;
		  }
		}
		ptb = ptb->pnexthash;
	      }

not_found:	/*if here, block was not found : has to allocate */
	while (data_so_far > MAXDATA) {	/* has to throw a block first */
		ptb = p_firstlru->pnextlru;
		c_remblock(ptb,perror);	/* remove it */
		if (*perror != 0)  { return(NULL); }
	      }
/* create a new BLOCK */
	ptb = c_creatblock(pdtset, index, mode, perror);
	if (*perror != 0)  return NULL;

/* read according to mode */
	switch (mode) {
	case CB_READ:
		c_readblock(ptb,perror);	/* read it */
		break;
	case CB_LOOK:
		c_lookblock(ptb, pdtset->absmax0, perror);	/* look at it */
		break;
	case CB_WRITE:
		if (pdtset->mode == CF_READ_WRITE) {
		  c_readblock(ptb,perror);
		  break;
		}
	      }

found:		/* succesfull return */
	if (mode == CB_WRITE) ptb->dirty = TRUE;
/* put it last in LRU */
	prev = p_lastlru->pprevlru;
	if (prev != ptb) {             /* would be already last */
	  c_unplug_lru(ptb);
	  ptb->pnextlru = p_lastlru;
	  ptb->pprevlru = prev;
	  prev->pnextlru = ptb;
	  p_lastlru->pprevlru = ptb;
	}

	return ptb;
      }

/*************************************************/
BLOCK	*c_creatblock(DATASET *pdtset,int index,int mode,int *perror)
/*
	create a new block and insert it in hash and in LRU (as last entry),
	typically called by c_getblock
*/
{
	unsigned	hh;
	BLOCK	*ptb, *prev;

/* create */
	ptb = malloc(sizeof(BLOCK));
	if (ptb == NULL) {
		*perror = COULD_NOT_MALLOC;
		return NULL;
		}
	ptb->pdataset = pdtset;
	ptb->index = index;
	ptb->mode = mode;
	ptb->dirty = FALSE;
	if (mode != CB_LOOK) {
		ptb->size = sizeof(float)*pdtset->szblk_tot;
		}
	else {
		ptb->size = pdtset->szblk_tot;
		}
	ptb->pdata.pread = malloc(ptb->size);
	if (ptb->pdata.pread == NULL) {
		free(ptb);
		*perror = COULD_NOT_MALLOC;
		return NULL;
		}
	data_so_far = data_so_far + ptb->size;

/* then insert in front in hash */
	hh = c_hash(pdtset, index);
	ptb->pnexthash = tbhash_block[hh];
	tbhash_block[hh] = ptb;
	ptb->key = hh;

/* and last in lru */
	prev = p_lastlru->pprevlru;
	ptb->pnextlru = p_lastlru;
	ptb->pprevlru = prev;
	prev->pnextlru = ptb;
	p_lastlru->pprevlru = ptb;

	return ptb;

	}

/*************************************************/
void c_writeblock(BLOCK *ptb,int  *perror)
/*
writes the block ptb
*/
{
int		local_id, index, size, hoff, i;

#ifdef REFABSMAX
float           x;
#endif

	local_id = (ptb->pdataset)->id;
	hoff = (ptb->pdataset)->headsz;
	index = ptb->index;
	size = ptb->size;

#ifdef REFABSMAX
/* if REFABSMAX, check the value of the largest point for that block */
	for (i=0; i<(ptb->pdataset)->szblk_tot; i++) {
	  x = (ptb->pdata.pread)[i];
	  x = (x>=0) ? x : -x;          /* I did not manage to have fabs() working !!! */
	  if (x > (ptb->pdataset)->absmax0) {
	    (ptb->pdataset)->absmax0 = x;
	    (ptb->pdataset)->head_dirty = TRUE;
	  }
	}
#endif

#ifdef is_big_endian
	if (!(ptb->pdataset)->big_endian) {
#else
	if ((ptb->pdataset)->big_endian) {
#endif
		swapbyte( (unsigned char *) ptb->pdata.pread, size);
		}

	if ( lseek(local_id, index*size+hoff, L_SET) == -1) {
		*perror = COULD_NOT_WRITE;
		return;
		}
	if (write(local_id, ptb->pdata.pread, size) != size) {
		*perror = COULD_NOT_WRITE;
		return;
		}
	}
/*************************************************/
void c_readblock(BLOCK *ptb,int  *perror)
/*
reads the bloc ptb
if dataset is big_endian and the machine is not (or dataset is not but machine is)
 then the data is byte swapped

*/
{
int		local_id, index, size,hoff;

	local_id = (ptb->pdataset)->id;
	hoff = (ptb->pdataset)->headsz;
	index = ptb->index;
	size = ptb->size;

	if ( lseek(local_id, index*size+hoff, L_SET) == -1) {
		*perror = COULD_NOT_READ;
		return;
		}
	if (read(local_id, ptb->pdata.pread, size) != size) {
		*perror = COULD_NOT_READ;
		return;
		}
#ifdef is_big_endian
	if (!(ptb->pdataset)->big_endian) {
#else
	if ((ptb->pdataset)->big_endian) {
#endif
		swapbyte( (unsigned char *) ptb->pdata.pread, size);
		}
	}

/*************************************************/
void c_lookblock(BLOCK *ptb, float max0, int  *perror)
/*
looks to the block ptb
*/
{
int		local_id, index, size, i, zz;
float	*temp, z;
static	float	lalpha;
#define eps 1.0E-7

	local_id = (ptb->pdataset)->id;
	index = ptb->index;
	size = ptb->size;

	if (szpread < size) {			/* make room for one byte per float*/
		temp = realloc(vpread,size);
		if (temp == NULL) {
			*perror = COULD_NOT_MALLOC;
			return;
			}
		vpread = temp;
		szpread = size;
		}

	if ( lseek(local_id, index*size, L_SET) == -1) {
		*perror = COULD_NOT_READ;
		return;
		}
	if (read(local_id, vpread, size) != size) {
		*perror = COULD_NOT_READ;
		return;
		}
#ifdef is_big_endian
	if (!(ptb->pdataset)->big_endian) {
#else
	if ((ptb->pdataset)->big_endian) {
#endif
		swapbyte( (unsigned char *) ptb->pdata.pread, size);
		}

/* make lookup
	computes i such as	alpha^zz ~ z 
				ie 		zz = log(z) / log(alpha)
*/
	lalpha = log(MADABSMAX)/127;
	
	for(i=0; i< (size / sizeof(float));i++) {
		z = abs(vpread[i])/max0 + eps;
		zz = log(z) / lalpha;
		zz = min(zz,127);
		if (vpread[i] < 0) zz = zz + 128;
		(ptb->pdata.plook)[i] = (unsigned char) zz;
		}
	}

/*************************************************/
void c_loadheader(int  fdes, char **ppheader,int *psize, int  *perror)
/*
reads in the header of the cache file fdes.
allocates and return a block holding the header in its current state.
will set an error if the file is invalid.
The header block will then be accessed with ad-hoc functions
*/
{
	char	*entry, *lblock;
	int		size, red, szh;
	int		done;
	char ENTRY[] = "HeaderSize";
	
/* makes room for header */
	if ( (lblock = malloc(BLOCKIO+1)) == NULL) {
		*perror = COULD_NOT_MALLOC;
		return;
		}
	lblock[BLOCKIO]='\0';

	done = FALSE;
	size = BLOCKIO;
/* load it */
	while (!done) {						/* find header size */
		if (size < 32) {		/* assume file cannot be less than 32 bytes */
			*perror = ACCESS;
			free(lblock);
			return;
			}
		if ( lseek(fdes, 0, L_SET) == -1) {			/* rewind */
			*perror = ACCESS;
			free(lblock);
			return;
			}
		red = read(fdes, lblock, size);
		if (red != size) {
			if (red == 0) {						/* passed beyond EOF */
				size = size/2;
				}
			else {
				*perror = ACCESS;
				free(lblock);
				return;
				}
			}
		else {
			done = TRUE;
			}
		}
/* find headersize entry */
#ifdef toto
A VERIF SI il y a \0 a la fin du header
#endif
	c_gtparam_type(lblock, &szh, ENTRY, H_INT, perror);
/* exit if error */
	if (*perror != 0 ) {
				*perror = WRONG_FILE;
				free(lblock);
				return;
				}
/* if headersize is 1 or 2, it is 16k blocks, otherwise it is in byte */
	if (szh <= 2) szh = szh * 16*1024;
	if (szh < 32) {		/* would be an impossible size */
				*perror = ACCESS;
				free(lblock);
				return;
				}
	if (szh != BLOCKIO) {
		lblock = realloc(lblock,szh+1);
		if (lblock == NULL) {
				*perror = COULD_NOT_MALLOC;
				return;
				}
		lblock[szh]='\0';
		}

	if ( lseek(fdes, 0, L_SET) == -1) {			/* rewind */
		*perror = ACCESS;
		free(lblock);
		return;
		}
/* loads complete header */
	if ( szh != read(fdes, lblock, szh)) {
		*perror = ACCESS;
		free(lblock);
		return;
		}

	*ppheader = lblock;
	*psize = szh;
	}

/*************************************************/
void c_writeheader(DATASET *dt, int  *perror)
/*	writes on file a dataset header */
{
#ifdef REFABSMAX
/* if REFABSMAX, absmax0 may have changed, who knows */
	c_putparam_type(dt,(void *) &(dt->absmax0),"Absmax", H_FLOAT, perror);
	if (*perror != 0) return;
#endif

	if ( lseek(dt->id, 0, L_SET) == -1) {
		*perror = COULD_NOT_WRITE;
		return;
		}
	if (write(dt->id, dt->phead, dt->headsz) != dt->headsz) {
		*perror = COULD_NOT_WRITE;
		return;
		}
	dt->head_dirty = FALSE;
	
	}
/*************************************************/
void c_gtparam(char *header, char *val,char *param, int  *status)
/*	find the value (string) of "param" into "dt->phead"
	entries are of the form :
	param       = value \n

	local version of CH_GTPARAM
*/
{	char	*entry, *cp, lparam[260];
	int		i;

/* find entry */
        strcpy(lparam,param);
        strcat(lparam," ");

  /* search for lparam at the beginning of a line (or of the header) */
	for (entry = strstr(header, lparam);
	     entry != NULL;
	     entry = strstr(entry+1, lparam) )
		{
		if (entry==header || entry[-1] == '\n')  break;
		}
	
	if (entry==NULL) { *status = PARAM_NOT_FOUND; return; }

/* skip trailing blanks and = sign */
	entry = entry + strlen(lparam) - 1;
	while (*++entry == ' ');

	if (*entry++ != '=') {
		*status = PARAM_NOT_FOUND;
		return;
		}

/* skip first blank */
	if (*entry++ == '\n') {
	        strcpy(val,"");
		return;
		}

/* then copy found value into val */
	cp = val;
	while( *entry != '\n') *cp++ = *entry++ ;
	*cp = '\0';

	}
/*************************************************/
void c_gtparam_type(char *header,void *val, char *param, int  h_type, int  *status)

/*	find the value of "param" into "dt->header"
	with type "h_type"
	uses c_gtparam
	
	version local de CH_GTPARAM_TYPE
*/
{
	char	stl[STMAX];


	c_gtparam(header, stl, param, status);	/* get param string */

	if (*status != 0) return;
	
	switch (h_type) {			/* and parse it */
	case H_INT:
		*(int *)val = atoi(stl);
		break;
	case H_FLOAT:
		*(float *)val = (float) atof(stl);
		break;
	case H_DOUBLE:
		*(double *)val = atof(stl);
		break;
	default:
		*status = WRONG_PARAM;
		break;
		}
	}
/*************************************************/
void c_putparam(DATASET *dt,char *val,char *param, int *status)
/*
	insert a parameter named "param" into dt->phead,
	will be of the form : param      = val\n

	error if file mode is CF_READ

	local version of CH_PUTPARAM
*/
{	char	*entry, *end, *endhead, term[]="\n0000", lparam[260];
	int		lp,lv, i;

	if (dt->mode == CF_READ) {
	  *status = WRONG_MODE;
	  return;
	}
	
	endhead = dt->phead + dt->headsz;	/* locates end */

/* is this a blank header */
	if ( strncmp(dt->phead,"0000",4)==0) {	/* yes */
		entry = dt->phead;
		}
	else {		      			/* else go to the end */
/* search if already there and remove it */
	  strcpy(lparam,"\n");
	  strcat(lparam,param);
	  strcat(lparam," ");

	  if ( (entry = strstr(dt->phead,lparam)) != NULL) {
	    entry++;                            /* skip leading \n */
	    end = strstr(entry, "\n");
	    if (end == NULL) {
				*status = WRONG_HEADER;
				return;
				}
	    end++;	/* param is between entry and end */
	    for ( ; end < endhead; *entry++ = *end++) ;
	    for ( ; entry < endhead; *entry++ = '0') ;
	  }
	  entry = strstr(dt->phead,term);		/* locates end */
	  if (entry == NULL) {
			*status = WRONG_HEADER;
			return;
			}
	  entry++;	/* points to end of valid data */
	}
/* entry now points where to insert */
	lp = strlen(param);
	lv = strlen(val);
	if (lp*lv <= 0) {
		*status = WRONG_PARAM;
		return;
		}
	if (entry + max(12,lp) + lv + 7 >= endhead) {		/* room for param  = val\n0000 */
		*status = NO_ROOM_HEAD;
		return;
		}

	for (i=0; i < lp; i++) entry[i] = param[i];  			/* copy param */
        entry[i++] = ' ';                        /* at least one blank */
	for (; i < 13; entry[i++] = ' ') ;   /* pad with blanks */
	entry = entry+i;
	*entry++ = '='; *entry++ = ' ';
	strcpy(entry, val); entry = entry+lv;
	*entry = '\n';

	dt->head_dirty = TRUE;

/* check if we're changing special header parameters */
	if (strcmp(param,"Type")==0)   {
		dt->type = atoi(val);
	} else if (strcmp(param,"Absmax")==0)  {
		dt->absmax0 = atof(val);
	}
}

/*************************************************/
void c_putparam_type(DATASET *dt,void *val,char *param, int  h_type, int *status)
/* local version of CH_PUTPARAM_TYPE */
{	char stl[STMAX];
	int i;

	switch (h_type) {			/* parse */
	case H_INT:
		sprintf(stl, "%d", *(int *)val);
		break;
	case H_FLOAT:
		sprintf(stl, "%.10g", *(float *)val);
		break;
	case H_DOUBLE:
		sprintf(stl, "%.15lg", *(double *)val);
		break;
	default:
		*status = WRONG_PARAM;
		return;
		}
	
	c_putparam(dt,stl,param, status);
	}
/*************************************************/
unsigned c_hash(DATASET *pdata, unsigned index)
{	unsigned val;

	val = ((unsigned) pdata + (HASHSIZE-1) )* (index+1);
	
	return val % HASHSIZE;
	}
/*************************************************/
void CH_ERRLIST (int *perror, char *st, int *pst)
{
  char *lst;
  lst = c_errlist(*perror);
  cconvert_string(st,lst,pst);
}
/*************************************************/

char *c_errlist (int error)
{	char *error_string;
	char line[STMAX];

error_string=line;  /*reserve space!!*/

/* codes for cache error conditions */
if (error==COULD_NOT_MALLOC) sprintf(error_string,"COULD NOT MALLOC MEMORY");	/* could not malloc */
if (error==COULD_NOT_OPEN) sprintf(error_string,"COULD NOT OPEN FILE");	/*for some reason (does not exist, etc...) could not open */
if (error==TOO_MANY_FILES) sprintf(error_string,"TOO MANY FILES OPENED"); 	/* too many file opened */
if (error==DIM_MISSING) sprintf(error_string,"DIM MISSING IN FILE HEADER"); 	/* definition of dimensionnality missing in header */
if (error==SIZE_MISSING) sprintf(error_string,"SIZE MISSING IN FILE HEADER"); 		/* definition of size missing in header */
if (error==VERSION_UNK) sprintf(error_string,"UNKNOWN CACHE VERSION DEFINED IN FILE HEADER");/* cache version icompatible with this version */
if (error==BLKDEF_MISSING) sprintf(error_string,"BLOCKSIZE DEFINITION MISSING IN FILE HEADER");/*definition of blocks missing in header */
if (error==WRONG_DIM) sprintf(error_string,"WRONG DIMENSIONALITY" );		/* access impossible with that dimensionnality */
if (error==ACCESS) sprintf(error_string,"ACCESS IMPOSSIBLE");		/* for some reason, access is impossible */
if (error==WRONG_MODE) sprintf(error_string,"WRONG MODE FOR READ OR WRITE");	/* trying to access a file in an invalid mode -or- access right conflict*/
if (error==NOT_OPENED) sprintf(error_string,"DATASET NOT OPENED"	);	/* DATASET not valid */
if (error==OUTSIDE) sprintf(error_string,"ACCESS OUTSIDE DATASET AREA");			/* accessing outside the data set */
if (error==WRONG_FILE) sprintf(error_string,"BAD HEADER, NOT A CACHE FILE");		/* not a "cache" file */
if (error==COULD_NOT_WRITE) sprintf(error_string,"COULD NOT WRITE FILE" );/* error in writing the file */
if (error==COULD_NOT_READ) sprintf(error_string,"COULD NOT READ FILE");	/* error in reading the file */
if (error==READ_VS_LOOK) sprintf(error_string,"READ VS LOOK");		/* returns READ instead of LOOK as asked */
if (error==WRONG_HEADER) sprintf(error_string,"WRONG HEADER (UNDECIPHERABLE)");		/* if undecipherable header */
if (error==WRONG_PARAM) sprintf(error_string,"WRONG PARAM SENT TO HEADER");		/* if wrong parameter send to header */
if (error==NO_ROOM_HEAD) sprintf(error_string,"NO ROOM HEAD LEFT IN HEADER");		/* if no room left in header */
if (error==PARAM_NOT_FOUND) sprintf(error_string,"PARAMETER NOT FOUND IN HEADER");	/* param not found in header */
if (error==INTERNAL_BUG) sprintf(error_string,"INTERNAL BUG IN GIFA");	/* should never occur (!) */

	return error_string;
	}
/*************************************************/
void printdt(DATASET *dt)
/*
	used for debugging
*/
{
		printf("name         : %s\n",dt->name);
		printf("id           : %d\n",dt->id);
		switch (dt->mode) {
			case CF_READ:
				printf("mode         : READ\n");
				break;
			case CF_WRITE:
				printf("mode         : WRITE\n");
				break;
			case CF_READ_WRITE:
				printf("mode         : READ_WRITE\n");
				break;
			default:
				printf("mode         : Unknown\n");
				}
		printf("headsz       : %d\n",dt->headsz);
		if (dt->head_dirty) {
		printf("head_dirty   : TRUE\n");
		} else {
		printf("head_dirty   : FALSE\n");
		}
		printf("cacheversion : %d\n",dt->cacheversion);
		printf("cacherelease : %d\n",dt->cacherelease);
		printf("dimension    : %d\n",dt->dimension);
		printf("size1        : %d\n",dt->size1);
		printf("size2        : %d\n",dt->size2);
		printf("size3        : %d\n",dt->size3);
		printf("nbblock1     : %d\n",dt->nbblock1);
		printf("nbblock2     : %d\n",dt->nbblock2);
		printf("nbblock3     : %d\n",dt->nbblock3);
		printf("szblk1       : %d\n",dt->szblk1);
		printf("szblk2       : %d\n",dt->szblk2);
		printf("szblk3       : %d\n",dt->szblk3);
		printf("szblk_tot    : %d\n",dt->szblk_tot);
		printf("absmax0      : %f\n",dt->absmax0);
		printf("type         : %d\n",dt->type);
		printf("there        : %d\n",dt->there);
		printf("endian       : %d\n",dt->big_endian);
	}
		
/*************************************************/
void CH_DUMP()
/*
	dumps the contents of the cache, used for debugging
*/
{
	DATASET *prun;
	BLOCK *pthru;

	printf("Allocated so far : %d ko   / (Max: %d ko) \n\n",
	       data_so_far/1024,MAXDATA/1024);
	if( p_file_opened->pnext == p_last_file) {
		printf("Cache Empty!\n");
		}
	else {
	  for (prun = p_file_opened->pnext;
	       prun != p_last_file;
	       prun = prun->pnext) {
	    printf("---------------------------------\n");
	    printdt(prun);
	    printf("\n");
	    for( pthru = p_firstlru->pnextlru;
		pthru != p_lastlru;
		pthru = pthru->pnextlru) {
	      if (pthru->pdataset == prun) {
		printf("Block # %d  ",pthru->index);
		printf("size %d  ",pthru->size);
		switch (pthru->mode) {
		case CB_LOOK:
		  printf("mode LOOK\n");
		  break;
		case CB_READ:
		  printf("mode READ\n");
		  break;
		case CB_WRITE:
		  printf("mode WRITE\n");
		  break;
		}
		if (pthru->dirty) { printf("  dirty\n"); }
	      }
	    }
	  }
	}
	printf("\nLRU list :\n");
	printf("1st -> ");
	pthru = p_firstlru->pnextlru;
	while (pthru != p_lastlru) {
	  printf("%d -> ",pthru->index);
	  pthru = pthru->pnextlru;
	}
	printf("last\n");
      }

