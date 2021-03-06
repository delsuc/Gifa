/*********************************************
ux2cache - convert uxnmr x32 integer data to Gifa v4.0 data

History :
Adapted from a Bruker program by Therese Malliavin, 1993
Heavily rewritten by M.A.Delsuc to port to Gifav4.0  feb-1995
debugged + byteorda M.A.Delsuc - may 1995
added BIG_ENDIAN flag M.A.D.  1997
added siz_buf computation for exotic buffer sizes M.A.D. oct 1998
corrected syntax M.A.D. jan 2000
changed BIG_ENDIAN to is_big_endian as everywhere else in Gifa fev 2000 MAD
added DECIM stuff   oct 2001 MAD

USAGE:
	ux2cach uxnmr_source_file byteorda cache_output_file \
                 DIM ITYPE FREQ SZ1 SW1 FREQ1               \
	         SZ2 SW2 FREQ2 SZ3 SW3 FREQ3 [ DECIM DSPFVS DSPFIRM ]

	uxnmr_source_file: binary format uxnmr x32/spec1/sgi fid or ser file.
        byteorda:          0 means X32-like data -> will reorder bytes
		           1 means DMX-like data -> will not reorder
	cache_output_file: Gifa format output file name.
	DIM:               dimension of experiment.
	ITYPE:             data type (cf gifa).
	FREQ:              spectrometer frequency in MHz
	SZI (I=1,2,3):     data size in dimension in number of (real) points
	SWI (I=1,2,3):     spectral widthes in dimension i, in Hz
	FREQI (I=1,2,3):   frequency in dimension i, in MHz
        DECIM DSPFVS DSPFIRM optionnal parameters, used for digital filter processing


***********************************************************************/
/* this one is redundant, but I suffered a lot with this ! */
#ifdef __i386__
#  define is_big_endian 1
#endif

#include <stdio.h>
#if defined __APPLE__
#    include <stdlib.h>
#else
#    include <malloc.h>
#endif
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include "cache_mad.h"
#ifdef _HPUX_SOURCE
  extern char *sys_errlist[];
#endif

/**********************************************************************/
/* prototyp */

void convert( FILE *f_in, DATASET **datasetid, int dime,int siz_fid,int siz_plan,int nb_plan,int byt); 
/**********************************************************************/
main(int argc, char *argv[])

{
	FILE *f_in;
  	FILE *f_out;
	struct stat buf;

  	DATASET *id;
	char name_out[256];
  	int mode ,longu, longv, itype, direction, modefid, option;
        int decim, dspfirm, dspfvs;
	int siz_fid,siz_plan,nb_plan;
  	double specw1,specw2,offset1,offset2;
  	double freq1,freq2,freq,freq3 ;
	double specw3,offset3;
  	int size1d,size2d,size3d,dime,byteorda;
	float ff;

	int status;

	printf("ux2cach 11-oct-2001 \n\n");


/* Get parameters *****************************/
        if (argc != 16 && argc != 19) {
	
		fprintf(stderr,"... wrong number of arguments\n\n");
fprintf(stderr,"USAGE:\n");
fprintf(stderr,"	ux2cach uxnmr_in_file byteorda gifa_out_file \\\n");
fprintf(stderr,"                 DIM ITYPE FREQ \\\n");
fprintf(stderr,"                 SZ1 SW1 FREQ1 \\\n");
fprintf(stderr,"	         SZ2 SW2 FREQ2 \\\n");
fprintf(stderr,"	         SZ3 SW3 FREQ3 \\\n");
fprintf(stderr,"	         [ DECIM DSPFVS DSPFIRM ] \n");
fprintf(stderr,"with :\n");
fprintf(stderr,"	uxnmr_in_file:\n");
fprintf(stderr,"		binary format uxnmr x32/spec1/sgi fid or ser file.\n");
fprintf(stderr,"        byteorda:\n");
fprintf(stderr,"	        0 means X32-like data -> will reorder bytes\n");
fprintf(stderr,"		1 means DMX-like data -> will not reorder\n");
fprintf(stderr,"	gifa_out_file:\n");
fprintf(stderr,"		Gifa format output file name.\n");
fprintf(stderr,"	DIM:\n");
fprintf(stderr,"		dimension of experiment. (i.e. 1, 2 or 3)\n");
fprintf(stderr,"	ITYPE:\n");
fprintf(stderr,"		data type (cf manual) (could be 0).\n");
fprintf(stderr,"	FREQ:\n");
fprintf(stderr,"		spectrometer frequency. (i.e. main freq, e.g. 400)\n");
fprintf(stderr,"	SZI (I=1,2,3):\n");
fprintf(stderr,"		data size in dimension i. (in number of points)\n");
fprintf(stderr,"	SWI (I=1,2,3):\n");
fprintf(stderr,"		spectral widthes in dimension i. (in Hz)\n");
fprintf(stderr,"	FREQI (I=1,2,3):\n");
fprintf(stderr,"		frequency in dimension i. (in MHz e.g. 100.13)\n");
fprintf(stderr,"        DECIM DSPFVS DSPFIRM\n");
fprintf(stderr,"                optionnal parameters, used for digital filter processing\n");

		exit(1);
	} 

/* basic ones : byteorda, dim, itype, freq */
	byteorda = atoi(argv[2]);
	if (byteorda < 0 || byteorda > 1 ) {
		fprintf(stderr,"ux2cach - ERROR in command line argument: byteorda = %d\n", byteorda);
		exit(1);
	}

	dime = atoi(argv[4]);
	if (dime <= 0 || dime > 3 ) {
		fprintf(stderr, "ux2cach - ERROR in command line argument: DIM = %d\n", dime);
		exit(1);
	}

	itype = atoi(argv[5]); 
	if (itype < 0) {
		fprintf(stderr, "ux2cach - ERROR in command line argument: ITYPE = %d\n", itype);
		exit(1);
	}

	freq = atof(argv[6]);

/*
   parameters are as follow :
   in 1D F1 (F2 - F3)
   in 2D F1 F2 (F3)   i.e. t1 - acq
   in 3D F1 F2 F3     i.e. t1 - t2 - acq
*/

/* first set for F1 */
	size1d = atoi(argv[7]);
	if (size1d <= 0) {
		fprintf(stderr, "ux2cach - ERROR in command line argument: SZ1D = %d\n", size1d);
		exit(1);
	}

	specw1 = atof(argv[8]);

	freq1 = atof(argv[9]);

	size2d = atoi(argv[10]);
	
	if (size2d <= 0) {
		fprintf(stderr, "ux2cach - ERROR in command line argument: SZ2D = %d\n", size2d);
		exit(1);
	      }
	specw2 = atof(argv[11]);

	freq2 = atof(argv[12]);

	size3d = atoi(argv[13]);
	if (size3d <= 0) {
		fprintf(stderr, "ux2cach - ERROR in command line argument: SZ3D = %d\n", size3d);
		exit(1);
	      }

	specw3 = atof(argv[14]);

	freq3 = atof(argv[15]);
        
        if (argc == 19) {
            decim = atoi(argv[16]);
            dspfvs = atoi(argv[17]);
            dspfirm = atoi(argv[18]);
        }

	if (stat(argv[1], &buf) == -1) {
		fprintf(stderr,"... cannot access %s: \n", argv[1]);
		perror("ux2cach - ERROR");
		exit(1);
	} else {
		if (buf.st_size < size1d * size2d*size3d * sizeof(int)) {
		  fprintf(stderr,"... wrong input file size: %d bytes\n", buf.st_size);
		  fprintf(stderr,"    expected %d bytes\n", size1d * size2d*size3d*sizeof(int));
		  exit(1);
		}
	      }

	if ((f_in = fopen(argv[1], "r")) == NULL) {
		fprintf(stderr,"... cannot read %s: \n", argv[1]);
		perror("ux2cach - ERROR");
		exit(1);
	} else {
		printf("... %s open for read, %d bytes\n", argv[1], buf.st_size);
	}

	    strcpy(name_out,argv[3]);
	    longu=strlen(name_out);
	    
/* OPEN cache file ********************************/

	    CH_INITIALISE();
	    mode = CF_WRITE;

            status = 0;
	    CH_CREATE(&id,name_out,&longu,&mode,&status);
	    if (status != 0) {
	      fprintf(stderr,"Error #: %d in opening file %s\n",status,name_out);
	      exit(1);
	    }	  


	    switch(dime) {
	      case 1:
		siz_fid=size1d;
		siz_plan=1;
		nb_plan=1;
		break;
	      case 2:
		siz_fid=size2d;
		siz_plan=size1d;
		nb_plan=1;
		break;
	      case 3:
		siz_fid=size3d;
		siz_plan=size2d;
		nb_plan=size1d;
		break;
	      }
	    CH_SETUP(&id, &dime, &size1d, &size2d, &size3d, &status);
	    if (status != 0) {
	      fprintf(stderr,"Error #: %d in setting-up file %s\n",status,name_out);
	      exit(1);
	    }

/* WRITE parameters into header **********************/
	    option = H_INT;
            longu = strlen("Type");
	    CH_PUTPARAM_TYPE(&id, &itype, "Type", &longu, &option, &status);
	    option = H_DOUBLE;
            longu = strlen("Frequency");
	    CH_PUTPARAM_TYPE(&id, &freq, "Frequency", &longu, &option, &status);
	    if (status != 0) {
	      fprintf(stderr,"Error #: %d in writing parameters \n",status);
	      exit(1);
	    }

	    switch(dime)
	    {
	    case 3 :
	      longu = strlen("Specw3");
	      CH_PUTPARAM_TYPE(&id, &specw3, "Specw3", &longu, &option, &status);
	      longu = strlen("Freq3");
	      CH_PUTPARAM_TYPE(&id, &freq3, "Freq3", &longu, &option, &status);

	    case 2 :
	      longu = strlen("Specw2");
	      CH_PUTPARAM_TYPE(&id, &specw2, "Specw2", &longu, &option, &status);
	      longu = strlen("Freq2");
	      CH_PUTPARAM_TYPE(&id, &freq2, "Freq2", &longu, &option, &status);

	    case 1 :
	      longu = strlen("Specw1");
	      CH_PUTPARAM_TYPE(&id, &specw1, "Specw1", &longu, &option, &status);
	      longu = strlen("Freq1");
	      CH_PUTPARAM_TYPE(&id, &freq1, "Freq1", &longu, &option, &status);
	    }
            
	    if (status != 0) {
	      fprintf(stderr,"Error #: %d in writing parameters \n",status);
	      exit(1);
	    }

            longu = strlen("Spectrometer");
            longv = strlen("BRUKER");
            CH_PUTPARAM(&id, "BRUKER", &longv, "Spectrometer", &longu, &status);

            if (argc == 19) {		/* if parameters were given */
                option = H_INT;
                longu = strlen("BRUKER_DECIM");
                CH_PUTPARAM_TYPE(&id, &decim, "BRUKER_DECIM", &longu, &option, &status);
                longu = strlen("BRUKER_DSPFVS");
                CH_PUTPARAM_TYPE(&id, &dspfvs, "BRUKER_DSPFVS", &longu, &option, &status);
                longu = strlen("BRUKER_DSPFIRM");
                CH_PUTPARAM_TYPE(&id, &dspfirm, "BRUKER_DSPFIRM", &longu, &option, &status);
            }

	    if (status != 0) {
	      fprintf(stderr,"Error #: %d in writing parameters \n",status);
	      exit(1);
	    }

	convert(f_in, &id, siz_fid, siz_plan, nb_plan, dime, byteorda);

	fclose(f_in);

	CH_CLOSE(&id,&status);

	printf ("\ndone %s.\n\n", argv[0]);
	exit(0);

      }


/**********************************************************************/
void convert(f_in, datasetid, siz_fid, siz_plan, nb_plan, dime, byt)
FILE 	*f_in;
DATASET **datasetid;
int dime,siz_fid,siz_plan,nb_plan,byt;

{
	void 	int2float();
	void	reverse4ByteOrder();

	int 	*ip;
	int	cnt,ind_row,ind_plan;
	int	siz_buf, from, mode, error;


	error = 0;

/* computes size of buffer, = siz_fid rounded to the next multiple of 256 */
        if ((siz_fid % 256) != 0) {
           siz_buf = ( ((siz_fid/256)+1) * 256);
        } else {
          siz_buf = siz_fid;
        }

printf("Size of input buffer: %d\n",siz_buf);

	ip = (int*) malloc((siz_buf * sizeof(int)));
	if (ip == NULL) {
		(void) fprintf(stderr,"... no memory available\n");
		exit(1);
	}

	printf("\n");
	for (ind_plan = 1;  ind_plan <= nb_plan; ind_plan++)
	   {
	   if (dime == 3 && ((ind_plan%10)*10 == ind_plan))
	     printf("plane %4d\n",ind_plan);
	   for (ind_row = 1;  ind_row <= siz_plan; ind_row++)
 	       {
	        printf("row %4d\r",ind_row);
		cnt = fread((char*)ip,sizeof(int),siz_buf,f_in);
		if (cnt != siz_buf) {
		  fprintf (stderr, "Plan %d: error in reading row %d : size %d\n",
			   ind_plan,ind_row,cnt);
		  CH_CLOSE(datasetid,&error);
		  exit(1);
		}
#ifdef is_big_endian
		if (byt == 1) {             /* from spec1 / INDY  */
		  reverse4ByteOrder(siz_fid, (char*)ip);
		}
#else
		if (byt == 0) {             /* from X32  */
		  reverse4ByteOrder(siz_fid, (char*)ip);
		}
#endif
		int2float(siz_fid, ip);

		from = 1;
		mode = CB_WRITE;
		switch(dime) {
		   case 1 :
		     CH_AC1DAREA((float *) ip,datasetid,&from,&siz_fid,&mode,&error);
		     break;
		   case 2 :
		     CH_AC2DAREA((float *) ip,datasetid,&ind_row,&from,
				 &ind_row,&siz_fid,&mode,&error);
		     break;
		   case 3 :
		     CH_AC3DAREA((float *) ip,datasetid,&ind_plan,&ind_row,&from,
				 &ind_plan,&ind_row,&siz_fid,&mode,&error);
		     break;
		   }
		   if (error != 0) {
		      fprintf(stderr,"Error #: %d in writing row %d\n",error,ind_row);
		      CH_CLOSE(datasetid,&error);
		      exit(1);
		      } 
	      }
	 }

      }


/**********************************************************************/
/* static void */
void reverse4ByteOrder(nele, dtaptr)
int     nele;			       /* specify number of elements to
				        * convert */
char 	*dtaptr;		       /* specify and return conversion
				        * data    */
/*----------------------------------------------------------------------
Reverse byte order in 4 byte word (inplace).
----------------------------------------------------------------------*/
{
/*
  register int *ip;
  register char *ca;
  register int m;
*/
  int *ip;
  char *ca;
  int m;
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
/*
printf(" %d %d %d %d \n",(int)bs.ch[0],(int)bs.ch[1],(int)bs.ch[2],(int)bs.ch[3]);
*/
  }

}				       /* reverse4ByteOrder() */


/**********************************************************************/
void int2float(int size, int *buff)
/*----------------------------------------------------------------------
Inplace conversion of binary 32 bit integer format to 32 bit floating 
format.

Rewritten from Bruker.
----------------------------------------------------------------------*/
{
  int i;
  float *f;
  
  for (i=0;i<size;i++) {
    f = (float *) &buff[i];
    *f = (float) buff[i];
  }

}


