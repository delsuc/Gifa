#ifndef CACHE_INC

#define CACHE_INC
/* include file for cache library */
/* should always be synchronized with cachev.inc */

#define CACHEVERSION 2
#define CACHERELEASE 0

/*************************************************/
/* first, platform dependent entry points */
#ifdef UNDERSCORE
#define MAXDATA maxdata_
#define CH_JOIN ch_join_
#define CH_OPEN ch_open_
#define CH_CREATE ch_create_
#define CH_SETUP ch_setup_
#define CH_CLOSE ch_close_
#define CH_GTPARBAS ch_gtparbas_
#define CH_GTPARAM ch_gtparam_
#define CH_GTPARAM_TYPE ch_gtparam_type_
#define CH_PUTPARAM ch_putparam_
#define CH_PUTPARAM_TYPE ch_putparam_type_
#define CH_INITIALISE ch_initialise_
#define CH_FLUSH ch_flush_
#define CH_FLUSHALL ch_flushall_
#define CH_AC1DAREA ch_ac1darea_
#define CH_AC2DAREA ch_ac2darea_
#define CH_AC3DAREA ch_ac3darea_
#define CH_PRINTFILES ch_printfiles_
#define CH_DUMP ch_dump_
#define CH_ERRLIST ch_errlist_

#elif F2C
#define MAXDATA maxdata_
#define CH_JOIN ch_join__
#define CH_OPEN ch_open__
#define CH_CREATE ch_create__
#define CH_SETUP ch_setup__
#define CH_CLOSE ch_close__
#define CH_GTPARBAS ch_gtparbas__
#define CH_GTPARAM ch_gtparam__
#define CH_GTPARAM_TYPE ch_gtparam_type__
#define CH_PUTPARAM ch_putparam__
#define CH_PUTPARAM_TYPE ch_putparam_type__
#define CH_INITIALISE ch_initialise__
#define CH_FLUSH ch_flush__
#define CH_FLUSHALL ch_flushall__
#define CH_AC1DAREA ch_ac1darea__
#define CH_AC2DAREA ch_ac2darea__
#define CH_AC3DAREA ch_ac3darea__
#define CH_PRINTFILES ch_printfiles__
#define CH_DUMP ch_dump__
#define CH_ERRLIST ch_errlist__

#else
#define MAXDATA maxdata
#define CH_JOIN ch_join
#define CH_OPEN ch_open
#define CH_CREATE ch_create
#define CH_SETUP ch_setup
#define CH_CLOSE ch_close
#define CH_GTPARBAS ch_gtparbas
#define CH_GTPARAM ch_gtparam
#define CH_GTPARAM_TYPE ch_gtparam_type
#define CH_PUTPARAM ch_putparam
#define CH_PUTPARAM_TYPE ch_putparam_type
#define CH_INITIALISE ch_initialise
#define CH_FLUSH ch_flush
#define CH_FLUSHALL ch_flushall
#define CH_AC1DAREA ch_ac1darea
#define CH_AC2DAREA ch_ac2darea
#define CH_AC3DAREA ch_ac3darea
#define CH_PRINTFILES ch_printfiles
#define CH_DUMP ch_dump
#define CH_ERRLIST ch_errlist
#endif

/* codes for cache error conditions */
#define COULD_NOT_MALLOC 2	/* could not malloc */
#define COULD_NOT_OPEN 3	/* for some reason (does not exist, etc...) could not open */
#define TOO_MANY_FILES 4	/* too many file opened */
#define DIM_MISSING 5		/* definition of dimensionnality missing in header */
#define SIZE_MISSING 6		/* definition of size missing in header */
#define VERSION_UNK 7		/* cache version icompatible with this version */
#define BLKDEF_MISSING 8	/* definition of blocks missing in header */
#define WRONG_DIM 9			/* access impossible with that dimensionnality */
#define ACCESS 10			/* for some reason, access is impossible */
#define WRONG_MODE 11		/* trying to access a file in an invalid mode -or- access right conflict*/
#define NOT_OPENED 12		/* DATASET not valid */
#define OUTSIDE 13			/* accessing outside the data set */
#define WRONG_FILE 14		/* not a "cache" file */
#define COULD_NOT_WRITE 15	/* error in writing the file */
#define COULD_NOT_READ 16	/* error in reading the file */
#define READ_VS_LOOK 17		/* returns READ instead of LOOK as asked */
#define WRONG_HEADER 18		/* if undecipherable header */
#define WRONG_PARAM 19		/* if wrong parameter send to header */
#define NO_ROOM_HEAD 20		/* if no room left in header */
#define PARAM_NOT_FOUND	21	/* param not found in header */
#define INTERNAL_BUG	22	/* should never occur (!) */

/* codes for access mode */
/* for open mode / file */
#define CF_READ 1
#define CF_WRITE 2
#define CF_READ_WRITE 3

/* for access mode / block */
#define CB_READ 11
#define CB_WRITE 12
#define CB_LOOK 13

/* for parameters */
#define H_INT 1
#define H_FLOAT 2
#define H_DOUBLE 3

/* length of the largest strings */
#include "sizebasec.h"
#define STMAX MAX_CHAR

/*
length of standard I/O block in byte
	- assumed to correspond to at least one physical disk access.
Optimum block sizes are computed from it.
*/
#define BLOCKIO 4096

/* dynamic of the look mode */
#define MADABSMAX 1.0E6		/* merci pilou ! */

/*****************************************************/
/* several macros */

/*
comp_ind_1D(i, data)
	compute index (0 based) of the block holding
   the entry (i) (0 based) in the 1D data		*/
#define comp_ind_1D(i, data) ( i/((data).szblk1) )
/*
comp_ind_2D(i1, i2, data)
	compute index (0 based) of the block holding
   the entry (i1,i2) (0 based) in the 2D data		*/
#define comp_ind_2D(i1, i2, data) ((i1 / ((data).szblk1))*((data).nbblock2) + i2/((data).szblk2) )
/*
comp_ind_3D(i1, i2, i3, data)
	compute index (0 based) of the block holding
   the entry (i1,i2,i3) (0 based) in the 3D data		*/
#define comp_ind_3D(i1, i2, i3, data) ((i1 / ((data).szblk1))*((data).nbblock2)*((data).nbblock3) + i2/((data).szblk2)*((data).nbblock3) + i3/((data).szblk3) )

/*
is_there(DATASET)
	returns trueif DATASET is known */
#define is_there(dataset) ((dataset).there == 1)
/*
min(x ,y)
max(x ,y)
*/
#define min(x,y) (x < y ? x : y)
#define max(x,y) (x > y ? x : y)

#define LOGICAL unsigned char
#define TRUE 1
#define FALSE 0


/*****************************************************/
/* FOR EACH FILE */
/*	each file has a DATASET struct to describe its contents and properties
	all DATASET are linked in a double list for going thru and removing
	this list is entried from p_file_opened

	each DATASET has a header associated which holds bunch of parameters
	obtained with c_loadheader()

*/

#define NAMEMAX 256


/* holds the file description */
typedef struct _DATASET {
	struct _DATASET	*pnext;	/* in the list of opened file */
	char	name[NAMEMAX];
	int		id;
	int		mode;				/* access mode  READ or READ_WRITE */
	int		headsz;				/* size (in bytes) of the file-header */
	char	*phead;				/* the header table */
	int		head_dirty;			/* true if header to be written on disk */
	int		cacheversion;
	int 	cacherelease;
	int		dimension;
	int		size1,size2,size3;	/* the sizes of the experiment */
	int		nbblock1,nbblock2,nbblock3;  /* the number of blocks on each axes */
	int		szblk1, szblk2, szblk3;	/* the sizes of each data block (in floats) */
	int		szblk_tot;			/* equal to szblk1 * szblk2 * szblk3  */
	float	absmax0;			/* the largets absolute value in the data-set */
	int		type;				/* ITYPE of the dataset (real/imaginary) */
	int		there;				/* holds one if a valid entry */
	LOGICAL		big_endian;		/* true if data set is Big_endian */
	} DATASET;


/*****************************************************/
/* FOR BLOCKS */
/*	each bit of data from a file is held into a BLOCK
	BLOCKs are tiling the dataset in 1D, 2D or 3D
	when accessed the data is stored in a series of BLOCKs

	BLOCKs can be accessed in READ READ_WRITE or LOOK
	if the BLOCK is accessed in LOOK mode, the data poins are compressed from int
	to (unsigned char) with a logarithmic coding

	BLOCKs are accessed thru a hash table, indexed with DATASET/index as key 
		tbhash_block[key] points to the first entry for each key
		of a double list of BLOCKs.

	BLOCKs are also in a LRU double chained list
		the block least recently used is put at the front (p_firstlru)
		          most recently used is put at the back (p_lastlru)

	BLOCK are allocated and freed at need.

double chained list :
                DATA        DATA               DATA
     1st ---->  next---->   next---> (    )--> next---> NULL
     NULL   <---prev  <-----prev <-- (    )<---prev  <--- last

*/

/* holds the block description */
typedef struct _BLOCK {
	DATASET *pdataset;			/* DATASET parent of this block */
	int		index;				/* offset in the file */
	unsigned	key;			/* the hash key */
	struct _BLOCK *pnexthash;	/*in the hash table */
/*	struct _BLOCK *pprevhash;
*/
	struct _BLOCK *pnextlru;	/*in the LRU table */
	struct _BLOCK *pprevlru;
	int		mode;				/* a LOOK READ WRITE or READ_WRITE */
	LOGICAL	dirty;				/* if to be put back on dsk */
	union {
		float	*pread;			/* hold the data (mallocated) */
		unsigned char *plook;	/* plook is for look mode */
		} pdata;
	int		size;				/* size mallocated (in byte) */
	} BLOCK;

/* the hash table */
#define HASHSIZE 1000

/* the minimum number of data stored in memory (in byte) */
#define		MINIDATA  32*1024;


/*************************************************/
/* prototype */
void CH_INITIALISE();
void CH_JOIN(DATASET **ppid, char *path,int *plen,int *mode,int *perror);
void CH_OPEN(DATASET **ppid,char *path,	int *plen,int *mode,int *perror);
void CH_CREATE(DATASET **ppid,char *path, int *plen,int *mode,int *perror);
void CH_SETUP(DATASET **ppid, int *dim, int *si1, int *si2, int *si3,int *perror);
void CH_CLOSE(DATASET **ppid, int *perror);
void CH_PRINTFILES();
void CH_FLUSH(DATASET **ppdata, int *perror);
void CH_FLUSHALL(int *perror);
void CH_AC1DAREA(float *parea, DATASET **ppdata, int *plf1, int *pur1, int *mode, int *perror);
void CH_AC2DAREA(float *parea, DATASET **ppdata, int *plf1, int *plf2, int *pur1, int *pur2, int *mode, int *perror);
void CH_AC3DAREA(float *parea, DATASET **ppdata, int *plf1, int *plf2, int *plf3, int *pur1, int *pur2, int *pur3, int *mode, int *perror);
void CH_GTPARBAS(DATASET **pdt,int *c_dimmanip,int *c_sizef1,int *c_sizef2,
	int *c_sizef3,int *c_type,float *c_specwf1,float *c_specwf2,float *c_specwf3,
	double *c_freq,double *c_freq1,double *c_freq2,double *c_freq3,
	float *c_offsf1,float *c_offsf2,float *c_offsf3, 
        float *dmin, float *dmax, float *dfactor, float *absmax);
void CH_GTPARAM_TYPE(DATASET **pdt,void *val, char *param, int *len, int *h_type, int *status);
void CH_GTPARAM(DATASET **pdt,char *val, int *vlen, char *param, int *plen, int *status);
void CH_PUTPARAM_TYPE(DATASET **pdt,void *val,char *param, int *len, int *h_type, int *status);
void CH_PUTPARAM(DATASET **pdt, char *val, int *vlen, char *param, int *plen, int *status);
void CH_DUMP();
int c_comp_update(DATASET *pid);
void c_unplug_lru(BLOCK *pb);
void c_unplug_hash(BLOCK *pb);
void c_remblock(BLOCK *pb, int *perror);
void c_remdataset(DATASET *pdata, int *perror);
BLOCK *c_getblock(DATASET	*pdtset,int index,int mode,int *perror);
BLOCK *c_creatblock(DATASET	*pdtset,int index,int mode,int *perror);
void c_writeblock(BLOCK *ptb,int *perror);
void c_readblock(BLOCK *ptb,int *perror);
void c_lookblock(BLOCK *ptb, float max0, int *perror);
void c_loadheader(int fdes, char **ppheader,int *psize, int *perror);
void c_writeheader(DATASET *dt,int *perror);
void c_gtparam(char *header, char *val,char *st, int *status);
void c_gtparam_type(char *header, void *val, char *param, int h_type, int *status);
void c_putparam(DATASET *dt,char *val,char *param, int *status);
void c_putparam_type(DATASET *dt,void *val,char *param, int h_type, int *status);
unsigned c_hash(DATASET *pdata, unsigned index);
void GIFAOUTC(char *);
void swapbyte(unsigned char *dtaptr, int nelem);
void CH_ERRLIST(int *,char *, int *);
char *c_errlist(int);

#endif
