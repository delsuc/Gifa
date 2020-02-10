/* uxnmr 2rr files to Gifa/cache files
 partially adapted from uxnmr_to_xeasy by Ralf W. Glaser
 written by M.A.Delsuc   july 1998

usage 2rr_to_cache <bruker proc directory> gifa-file


*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include "cache_mad.h"

#ifdef i386
#  define BIG_ENDIAN
#endif

/******************************************************************/  

typedef struct 
{ /* data from bruker files */
  int dims;			/* 2 or 3 dimensions */
  int ux_subm_size[3];		/* submatrix size of uxnmr file */
  int size[3];			/* size of spectrum */
  float sf[3];			/* spec frequency */
  float p0[3];			/* offset */
  float sw_hz[3];		/* sweep width in Hz */
  /* derived data */
  float sw_ppm[3];		/* sweep width in ppm */
  char iname[3][8];		/* identifier for dimension (e.g."HN", "Ctoc", "Hnoe") */
  float max_sf;
  int i_min;
  int i_max;			/* min and max intensities in UXNMR file */
  float max_int;		/* maximum intensity of UXNMR file integers */
  float ffm;
  int fim;			/* factors to reduce intensity. use the one that is not zero */
  int xe_subm_size[3];
  int perm_ux[3];
  int fold[3];
  int points;
  int bytordp;
}conv_data;


/******************************************************************/  
/* Get parameters from procxx files */
/* extract information from file */

void get_float(FILE *F, char * ID, char * ask_for, float *var)
{ char line[256];
  int read;
  read=0;
  if(F)
  { rewind(F);
    while(fgets(line, 255, F))
      if(!strncmp(ID, line, strlen(ID)))
        if(read=sscanf(line+strlen(ID),"%f",var))break;
  }
  while(read!=1)
  { printf("%s\n",ask_for);
    read=scanf("%f",var);
    if(!read)printf("invalid entry\n");
  }
}

void get_int(FILE *F, char * ID, char * ask_for, int *var)
{ char line[256];
  int read;
  read=0;
  if(F)
  { rewind(F);
    while(fgets(line, 255, F))
      if(!strncmp(ID, line, strlen(ID)))
        if(read=sscanf(line+strlen(ID),"%d",var))break;
  }
  while(read!=1)
  { printf("%s\n",ask_for);
    read=scanf("%d",var);
    if(!read)printf("invalid entry\n");
  }
}


void get_data(conv_data * d, FILE * F, char * Fx, int dim)
{ char ask[100];
 
  sprintf(ask, "spectral size in %s", Fx);
  get_int(F, "##$SI=", ask, &(d->size[dim]));
  sprintf(ask, "sub-matrix size in %s", Fx);
  get_int(F, "##$XDIM=", ask, &(d->ux_subm_size[dim]));
  sprintf(ask, "offset (max. ppm) in %s", Fx);
  get_float(F, "##$OFFSET=", ask, &(d->p0[dim]));
  sprintf(ask, "spectral width [in Hz] in %s", Fx);
  get_float(F, "##$SW_p=", ask, &(d->sw_hz[dim]));
  sprintf(ask, "Frequency in %s", Fx);
  get_float(F, "##$SF=", ask, &(d->sf[dim]));
}


void extract(conv_data * d, char *filename)
{ char all_files[256];
  char *p;
  FILE *F;

  (void)strncpy(all_files, filename, 240);
  
  p = all_files + strlen(all_files);
  sprintf(p,"/procs"); F=fopen(all_files,"r");
  if(!F) { sprintf(p,"/proc"); F=fopen(all_files,"r"); }

  if(d->dims==2) get_data(d,F,"F2",0);
  else get_data(d,F,"F3",0);
  get_int(F,"##$YMAX_p=","maximum intensity of uxnmr file",&d->i_max);
  get_int(F,"##$YMIN_p=","minimum intensity of uxnmr file",&d->i_min);
  get_int(F, "##$BYTORDP=", "Byte order, 0/1", &d->bytordp);
  d->max_int=d->i_max>-d->i_min?d->i_max:-d->i_min;
  if(F)fclose(F);
  sprintf(p,"/proc2s"); F=fopen(all_files,"r");
  if(!F) { sprintf(p,"/proc2"); F=fopen(all_files,"r"); }
  if(d->dims==2) get_data(d,F,"F1",1);
  else get_data(d,F,"F2",1);
  if(F)fclose(F);
  if(d->dims==3)
  { sprintf(p,"/proc3s"); F=fopen(all_files,"r");
    if(!F) { sprintf(p,"/proc3"); F=fopen(all_files,"r"); }
    get_data(d,F,"F1",2);
    if(F)fclose(F);
  }
  d->points=d->size[0]*d->size[1];
  if(d->dims==3) d->points*=d->size[2];
}

/*************************************************/
void dump_conv_data(conv_data *d)
{
  printf("dims:%d\n",d->dims);
  printf("ux_subm_size[0]:%d\n", d->ux_subm_size[0]);
  printf("ux_subm_size[1]:%d\n", d->ux_subm_size[1]);
  printf("ux_subm_size[2]:%d\n", d->ux_subm_size[2]);
  printf("size[0]:%d\n", d->size[0]);
  printf("size[1]:%d\n", d->size[1]);
  printf("size[2]:%d\n", d->size[2]);
  printf("sf[0]:%f\n",d->sf[0]);
  printf("sf[1]:%f\n",d->sf[1]);
  printf("sf[2]:%f\n",d->sf[2]);
  printf("sw_hz[0]:%f\n",d->sw_hz[0]);
  printf("sw_hz[1]:%f\n",d->sw_hz[1]);
  printf("sw_hz[2]:%f\n",d->sw_hz[2]);
  printf("bytordp:%d\n",d->bytordp);
}

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

/******************************************************************/  
void convert(FILE *FIN, DATASET ** datasetid, conv_data * d)

{

	int 	*ip;
	int	i, j, ii, jj, siz_fid, cnt;
	int	from, mode, error;


	error = 0;

	if (d->dims  == 2) {
		siz_fid = d->ux_subm_size[0] * d->ux_subm_size[1];
	} else {
		(void) fprintf(stderr,"Sorry, only 2D so far\n");
		exit(-1);
	}
			
	
	ip = (int*) malloc((siz_fid * sizeof(float)));
	if (ip == NULL) {
		(void) fprintf(stderr,"... no memory available\n");
		exit(1);
	}

	printf("\n");
	for (i = 1;  i <= d->size[1]; i = i + d->ux_subm_size[1])
	   {
	   	ii = i + d->ux_subm_size[1]-1;
	    printf("i %d\n",i);
		for (j = 1;  j <= d->size[0]; j = j + d->ux_subm_size[0])
 	       {
		   	jj = j + d->ux_subm_size[0]-1;
			cnt = fread((char*)ip,sizeof(float),siz_fid,FIN);
			if (cnt != siz_fid) {
		  	fprintf (stderr, "error in reading %d %d %d %d\n",i,j,cnt,siz_fid);
		  	CH_CLOSE(datasetid,&error);
		  	exit(1);
			}

#ifdef BIG_ENDIAN
		if (d->bytordp == 1) {             /* from spec1 / INDY  */
		  swapbyte((char*)ip, siz_fid);
		}
#else
		if (d->bytordp == 0) {             /* from X32  */
		  swapbyte((char*)ip, siz_fid);
		}
#endif


	int2float(siz_fid, ip);
	

		from = 1;
		mode = CB_WRITE;
		CH_AC2DAREA((float *) ip,datasetid,
			&i,&j,&ii,&jj,&mode,&error);
		   if (error != 0) {
		      fprintf (stderr, "error in writing %d %d\n",i,j);
		      CH_CLOSE(datasetid,&error);
		      exit(1);
		      } 
	      }
	 }

      }


/******************************************************************/
/******************************************************************/  
/******************************************************************/  

void main(int argc, char ** argv)
{
	FILE  *FIN, *FOUT;
	conv_data d;
	DATASET *id;
	char name_in[256];
  	int mode,status,longu,i;
  	int option,size1,size2,size3;
	
   
   printf("2rr_to_cache version 0.1\n");
  if(argc<3) 
  { printf("USAGE: 2rr_to_cache <bruker proc directory> gifa-file\n"); 
    exit(-1); 
  }

  
/* GET spectral parameters */

   d.dims = 2;           /* works only for 2D so far */


   extract(&d, argv[1]);
   dump_conv_data(&d);
   	
   
/* OPEN input file ********************************/
	strcpy(name_in,argv[1]);
	strcat(name_in,"/2rr");

        if ((FIN = fopen(name_in, "r")) == NULL) {
                (void) fprintf(stderr,"... cannot read %s: \n", name_in);
                (void) fprintf(stderr,"    ERROR %s\n", sys_errlist[errno]);
                exit(1);
        } else {
                printf("Input file: %s\n", name_in);
        }

/* OPEN cache file ********************************/

	    CH_INITIALISE();
	    mode = CF_WRITE;

        status = 0;
		longu = strlen(argv[2]);
        CH_CREATE(&id,argv[2],&longu,&mode,&status);
	    if (status != 0) {
	      fprintf(stderr,"Error #: %d in opening file %s\n",status,argv[2]);
	      exit(1);
	    }	  


	    size1 = d.size[0];
	    size2 = d.size[1];
	    size3 = 1;
	    
	    CH_SETUP(&id, &(d.dims), &size1, &size2, &size3, &status);

	    if (status != 0) {
	      fprintf(stderr,"Error #: %d in setting-up file %s\n",status,argv[2]);
	      exit(1);
	    }

/* WRITE parameters into header **********************/
	    option = H_INT;
            longu = strlen("Type");
        i = 0;
	    CH_PUTPARAM_TYPE(&id, &i, "Type", &longu, &option, &status);
	    option = H_FLOAT;
            longu = strlen("Frequency");
	    CH_PUTPARAM_TYPE(&id, &(d.sf[0]), "Frequency", &longu, &option, &status);
	    if (status != 0) {
	      fprintf(stderr,"Error #: %d in writing parameters \n",status);
	      exit(1);
	    }

	    switch(d.dims)
	    {
	    case 3 :

	    case 2 :
	      longu = strlen("Specw2");
	      CH_PUTPARAM_TYPE(&id, &(d.sw_hz[0]), "Specw2", &longu, &option, &status);
	      longu = strlen("Freq2");
	      CH_PUTPARAM_TYPE(&id, &(d.sf[0]), "Freq2", &longu, &option, &status);

	    case 1 :
	      longu = strlen("Specw1");
	      CH_PUTPARAM_TYPE(&id, &(d.sw_hz[1]), "Specw1", &longu, &option, &status);
	      longu = strlen("Freq1");
	      CH_PUTPARAM_TYPE(&id, &(d.sf[1]), "Freq1", &longu, &option, &status);
	    }
	    if (status != 0) {
	      fprintf(stderr,"Error #: %d in writing parameters \n",status);
	      exit(1);
	    }

	convert(FIN, &id, &d);

	fclose(FIN);

	CH_CLOSE(&id,&status);

	printf ("\ndone %s.\n\n", argv[0]);
	exit(0);

}


  
  
