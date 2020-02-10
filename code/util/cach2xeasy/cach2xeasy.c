/*********************************************************************
cach2xeasy - convert Gifa v4.0 cache data to Xeasy v.1.2 data

History :
Written by Kalle Gehring, Nov. 1998.
Adapted from ux2cach.c (M.A.Delsuc), uxnmr_to_xeasy.c (R.W.Glaser),
   spec_interface.c and related programs (C.Bartels).

USAGE:
	cach2xeasy cache_input_file xeasy_output_file

	cache_input_file:
		Gifa cache format input file name

	xeasy_output_file:
		base name for xeasy data and param files

***********************************************************************/

#include <stdio.h>
#include <malloc.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include "cache_mad.h"

#include <math.h>	/* from uxnmr_to_xeasy program */
#include <string.h>	/* from uxnmr_to_xeasy program */

#define MAX_XEASY 2097152 /* 2 power 21  - from uxnmr_to_xeasy program */

int reverse[96];	/* Global variable - look up table for floattox16 */

typedef struct		/* from uxnmr_to_xeasy program */
{ /* data from bruker files */
  int dims;                     /* 2 or 3 dimensions */
  int ux_subm_size[3];          /* submatrix size of uxnmr file */
  int size[3];                  /* size of spectrum */
  float sf[3];                  /* spec frequency */
  float p0[3];                  /* offset */
  float sw_hz[3];               /* sweep width in Hz */
  /* derived data */
  float sw_ppm[3];              /* sweep width in ppm */
  char iname[3][8];             /* identifier for dimension (e.g."HN", "Ctoc", "Hnoe") */
  float max_sf;
  int i_min;
  int i_max;                    /* min and max intensities in UXNMR file */
  float max_int;                /* maximum intensity of UXNMR file integers */
  float ffm;
  int fim;                      /* factors to reduce intensity. use the one that is not zero */
  int xe_subm_size[3];
  int perm_ux[3];
  int fold[3];
  int points;
  int bytordp;
}conv_data;

/* forward declaration of subroutines defined after main */
void floattox16();
float xexp10(float x) { return (float)(exp(x*log(10.0))); }
void from_spec_param_constructor();
void int2float();
int create_param_file();
char *param_file_name();
char *spec_file_name();
void get_inames();
void convert();
void reverse4ByteOrder(int, char*);


/**********************************************************************/
main(int argc, char *argv[])

{
  	FILE *f_out_data;
  	FILE *f_out_param;
	struct stat buf;

  	DATASET *id;
	char *data_name_out;
	char *param_name_out;
	char cach_name_in[256];
  	int mode,itype;
	int length_cach_name;
	float specw1,specw2,offset1,offset2;
  	float freq1,freq2,freq,freq3 ;
	float specw3,offset3;
	float absmax;
  	int size1d,size2d,size3d,dime;

	int status, i;

	conv_data d;				/* from uxnmr_to_xeasy.c */

	printf("cach2xeasy 1-nov-1998 \n\n");


/* Get command line parameters *****************************/

	if (argc != 3) {
		fprintf(stderr,"... wrong number of arguments\n\n");
fprintf(stderr,"USAGE:\n");
fprintf(stderr,"	cach2xeasy gifa_in_file xeasy_out_file \n");
fprintf(stderr,"with :\n");
fprintf(stderr,"	gifa_in_file:\n");
fprintf(stderr,"		Gifa cache format input file name.\n");
fprintf(stderr,"        xeasy_out_file:\n");
fprintf(stderr,"                base name for xeasy data and param files.\n");
		exit(1);
	} 

	if (stat(argv[1], &buf) == -1) {
		(void) fprintf(stderr,"... cannot access %s: \n", argv[1]);
		(void) fprintf(stderr,"    ERROR %s\n", sys_errlist[errno]);
		exit(1);
	} 

	strcpy(cach_name_in,argv[1]);
	length_cach_name=strlen(cach_name_in);

	param_name_out=param_file_name(argv[2]);	/* add ".3D.param" to basename */
	data_name_out=spec_file_name(argv[2]);		/* add ".3D.16" to basename    */

/* INITIALISE the cache system ********************************/

	CH_INITIALISE();
	mode = CF_READ_WRITE;

/* OPEN cache file ********************************/

	status=0;
	CH_OPEN(&id,cach_name_in,&length_cach_name,&mode,&status);
	if (status != 0) {
		fprintf(stderr,"Error #: %d in opening file %s\n",status,cach_name_in);
		exit(1);
	}	  
	
/* JOIN cache file ********************************/

	status=0;
	CH_JOIN(&id,cach_name_in,&length_cach_name,&mode,&status);
	if (status != 0) {
		fprintf(stderr,"Error #: %d in joining file %s\n",status,cach_name_in);
		exit(1);
	}	  
	if (id->dimension!=2 && id->dimension!=3) {
		fprintf(stderr,"Dimension must be 2 or 3. Exiting...");
		exit(1);
	}
	
/* Fetch variables from cache data file *************************************/

	status=0;
	CH_GTPARBAS(&id,&dime,&size1d,&size2d,&size3d,&itype,
		&specw1,&specw2,&specw3,&freq,&freq1,&freq2,&freq3,
		&offset1,&offset2,&offset3,&absmax);

	
/* Print variables from cache data structure ********************************/
/****************
	printf("dimension %d\n",id->dimension);
	printf("size1 %d\n",id->size1);
	printf("size2 %d\n",id->size2);
	printf("size3 %d\n",id->size3);
	printf("type %d\n",id->type);
	printf("nbblock1 %d\n",id->nbblock1);
	printf("nbblock2 %d\n",id->nbblock2);
	printf("nbblock3 %d\n",id->nbblock3);
	printf("szblk1 %d\n",id->szblk1);
	printf("szblk2 %d\n",id->szblk2);
	printf("szblk3 %d\n",id->szblk3);
	printf("szblk_tot %d\n",id->szblk_tot);
	printf("absmax0 %f\n",id->absmax0);
	printf("specw1 %f\n",specw1);
	printf("specw2 %f\n",specw2);
	printf("specw3 %f\n",specw3);
	printf("offset1 %f\n",offset1);
	printf("offset2 %f\n",offset2);
	printf("offset3 %f\n",offset3);
*********************/

/* Copy Gifa parameters into xeasy data structure ********************************/

	d.dims=id->dimension;
	d.sf[0]=freq1;
	d.sf[1]=freq2;
	d.sf[2]=freq3;
	if (freq1==0) d.sw_ppm[0]=0; else d.sw_ppm[0]=specw1/freq1;
	if (freq2==0) d.sw_ppm[1]=0; else d.sw_ppm[1]=specw2/freq2;
	if (freq3==0) d.sw_ppm[2]=0; else d.sw_ppm[2]=specw3/freq3;
	if (freq1==0) d.p0[0]=0; else d.p0[0]=(offset1+specw1)/freq1;
	if (freq2==0) d.p0[1]=0; else d.p0[1]=(offset2+specw2)/freq2;
	if (freq3==0) d.p0[2]=0; else d.p0[2]=(offset3+specw3)/freq3;
	d.size[0]=id->size1;
	d.size[1]=id->size2;
	d.size[2]=id->size3;
	switch(dime) {
	   case 2:
		d.xe_subm_size[0]=id->size1;   	/* sub_matrix is full 2D size */
		d.xe_subm_size[1]=id->size2;	
		d.xe_subm_size[2]=1;
		/*break;*/			/* REMOVE COMMENTS to make   */ 
						/* xeasy submatrices full size */
	   case 3:
		d.xe_subm_size[0]=id->szblk1;   /* use same sub_matrices as Gifa */
		d.xe_subm_size[1]=id->szblk2;	
		d.xe_subm_size[2]=id->szblk3;
	}	
	for (i=0;i<dime;++i) d.perm_ux[i]=dime-i-1;
	for (i=0;i<dime;++i) d.fold[i]=0;
	strcpy(d.iname[0],"Dim1");	/* seven characters maximum length */
	strcpy(d.iname[1],"Dim2");
	strcpy(d.iname[2],"Dim3");
	get_inames(&d);			/* calculate nuclei types and add to iname[i] */

/* Create xeasy param file ********************************/

	if(!create_param_file(param_name_out, &d))
	{	printf("\nFailed to create "); exit(-1); }
	else printf("\nCreated ");
	printf("parameter file %s\n",param_name_out);

/* Open xeasy data (spectrum) file  ********************************/

	f_out_data=fopen(data_name_out,"wb+");
	if (f_out_data==0) {
		fprintf (stderr,"Problems opening %s\n",data_name_out); exit(-1);
	}else{
		printf("\nData file %s open for writing.\n",data_name_out);
	}


/* Convert and write the xeasy data file  ***************************/

	convert(f_out_data,id,d.xe_subm_size[2],d.xe_subm_size[1],d.xe_subm_size[0]);


/* Close xeasy data file  ***************************/

	fclose(f_out_data);
        printf("Data file %s closed.\n", data_name_out );


/* Close cache data file ********************************/

	status=0;
	CH_CLOSE(&id,&status);
	if (status != 0) {
		fprintf(stderr,"Error #: %d in CH_CLOSE\n",status);
	}

	printf ("\ndone %s.\n\n", argv[0]);
	exit(0);

} /* End mainline */


/**********************************************************************/
/**********************************************************************/
/**********************************************************************/
void convert(f_out, datasetid, subm_size2, subm_size1, subm_size0)
	FILE 	*f_out;
	DATASET *datasetid;
	int  subm_size2, subm_size1, subm_size0;
{
	int 	dime;
	int	size_width,size_height,size_depth;
	int	sub_width, sub_height, sub_depth;
	float 	*ip;
	float 	*ip_head;
	float	fr,fr_zero,scale_down;
	unsigned char b[2], b_zero[2];
	int	mode, error;
	int	width, height,depth;
	int	upper_width, upper_height, upper_depth;
	int 	k,j,i;

/* Set up data buffer sizes ****************************/
	
	dime=datasetid->dimension;
	switch(dime) {
	   case 2:
		sub_width=subm_size1;
		sub_height=subm_size0;
		sub_depth=1;
		size_width=datasetid->size2;
		size_height=datasetid->size1;
		size_depth=1;
		break;
	
	   case 3:
		sub_width=subm_size2;
		sub_height=subm_size1;
		sub_depth=subm_size0;
		size_width=datasetid->size3;
		size_height=datasetid->size2;
		size_depth=datasetid->size1;
		break;
	}	

/* Set up conversion lookup table, reverse[i] ************/

	from_spec_param_constructor();

	scale_down= (datasetid->absmax0)/MAX_XEASY ;
	printf("\nScaling down data by  %f\n",scale_down);


/* Allocate memory ************/

	error = 0;
	ip = (float *)malloc((unsigned)sub_height * sub_width * sub_depth  * sizeof(float));
	if (ip == NULL) {
		(void) fprintf(stderr,"... no memory available\n");
		exit(1);
	}

/* Preset variables */

        ip_head=ip;	/* save position of ip pointer */
	fr_zero=0;      
        floattox16(&fr_zero,&b_zero);
	mode = CB_READ;

/* Write submatrices */    
/***********************************
* Gifa subroutine calls don't include explicit submatrices. 
* Submatrices are set by the subroutine calls CH_AC2DAREA and 
* CH_AC3DAREA.  Calls must respect the data size boundries 
* (ie size_width, size_height, size_depth).  The submatrix is 
* stored in the float data array, ip. 				
***************************************/

        for(depth=1; depth<=size_depth; depth += sub_depth) {
	   upper_depth=depth+sub_depth-1;
           if (upper_depth>size_depth) upper_depth=size_depth;
           for(height=1; height<=size_height; height += sub_height) {
	      upper_height=height+sub_height-1;
              if (upper_height>size_height) upper_height=size_height;
              for(width=1; width<=size_width; width += sub_width) {
	         upper_width=width+sub_width-1;
                 if (upper_width>size_width) upper_width=size_width;

/* Read submatrix from Gifa data file **/
		 ip=ip_head;
		 error = 0;
		 if (dime==2) { 
		      	CH_AC2DAREA((float *) ip,&datasetid,&height,&width,
		 	   &upper_height,&upper_width,&mode,&error);
	   	    }else{
	     		CH_AC3DAREA((float *) ip,&datasetid,&depth,&height,&width,
                           &upper_depth,&upper_height,&upper_width,&mode,&error);
                 }
                 if (error != 0) {
   fprintf(stderr,"Error #: %d in reading submatrix at %d %d %d to %d %d %d \n",
           error,depth,height,width,upper_depth,upper_height,upper_width);
                        CH_CLOSE(&datasetid,&error);
                        exit(1);
                 } 

/***********************************
* Xeasy doesn't use variable size submatrices so that padding
* of the xeasy data file is necessary if the full data matrix
* doesn't divide evenly into submatrices.  Data is converted
* word by word in "floattox16" before writing.
**************************************/

/*fprintf(stdout,"going to write  %d %d %d to %d %d %d\n",
depth,height,width,upper_depth,upper_height,upper_width);*/

/** Write submatrix to Xeasy data file **/
                 for (k = 0; k<sub_depth; k++) {
                    for (j = 0; j<sub_height; j++) {
   		       /*printf("row %4d\r",j);*/
	               for (i = 0; i<sub_width; i++) {	/* if pointers are */
			  if ((depth+k>size_depth) ||   /* out of data limits*/ 
			      (height+j>size_height) ||
			      (width+i>size_width)) {
			         fwrite(&b_zero,2,1,f_out);	/* pad file */
			     }else{				  /* else */	
			    	 /*cnt++;*/
				 fr=*(ip++)/scale_down;	
			     	 floattox16(&fr,&b);	     /* convert data */
    			     	 fwrite(&b, 2, 1, f_out);	/* and write */
			     }
		       }
/*************************** DEBUGGING ***************
		       if ( cnt != (upper_width-width+1) && 
			  	    (depth+k<=size_depth) && 
				    (height+j<=size_height)) {
   fprintf (stdout, "Error in writing submatrix at %d %d %d.  Count is %d of %d\n",
				 depth,height,width,cnt,(upper_width-width+1)); 
			     CH_CLOSE(&datasetid,&error);
			     exit(1);
		       }
*******************************************************/
		    }
		 }
		 fprintf(stdout,"Submatrix at %d %d %d written.\r",depth,height,width);
  	      }
	   }
	}
	if (dime==2) printf("Xeasy 2D matrix written with %d %d points.\n",
			size_height,size_width);
	if (dime==3) printf("Xeasy 3D matrix written with %d %d %d points.\n",
			size_depth,size_height,size_width);
}

/**********************************************************************/
/**********************************************************************/
/**********************************************************************/
/* add suffix to xeasy spectrum data file name (if not there already) */
char *spec_file_name(const char *name)
{ char * p;
  char * r;
  int base;
  if(p=strstr(name,".3D.param")) base=p-name;
  else if(p=strstr(name,".3D.16")) base=p-name;
  else base=strlen(name);
  r=(char*)malloc(base+7);
  strncpy(r,name,base); r[base]=0;
  strcat(r,".3D.16");
  return r;
}

/**********************************************************************/
/**********************************************************************/
/**********************************************************************/
/* add suffix to xeasy parameter file name (if not there already)     */
char *param_file_name(const char *name)
{ char * p;
  char * r;
  int base;
  if(p=strstr(name,".3D.param")) base=p-name;
  else if(p=strstr(name,".3D.16")) base=p-name;
  else base=strlen(name);
  r=(char*)malloc(base+10);
  strncpy(r,name,base); r[base]=0;
  strcat(r,".3D.param");
  return r;
}

/**********************************************************************/
/**********************************************************************/
/**********************************************************************/
int create_param_file(char *filename, conv_data * d)
{ FILE * F;
  int i;
  F=fopen(filename,"w");
  if(!F)return 0;
  fprintf(F,"Version ....................... 1\n");
  fprintf(F,"Number of dimensions .......... %d\n",d->dims);
  fprintf(F,"16 or 8 bit file type ......... 16\n");
  for(i=0; i<d->dims; i++)
    fprintf(F,"Spectrometer frequency in w%d .. %f\n", i+1, d->sf[i]);
  for(i=0; i<d->dims; i++)
    fprintf(F,"Spectral sweep width in w%d .... %f\n", i+1, d->sw_ppm[i]);
  for(i=0; i<d->dims; i++)
    fprintf(F,"Maximum chemical shift in w%d .. %f\n", i+1, d->p0[i]);
  for(i=0; i<d->dims; i++)
    fprintf(F,"Size of spectrum in w%d ........ %d\n", i+1, d->size[i]);
  for(i=0; i<d->dims; i++)
    fprintf(F,"Submatrix size in w%d .......... %d\n", i+1, d->xe_subm_size[i]);
  for(i=0; i<d->dims; i++)
    fprintf(F,"Permutation for w%d ............ %d\n", i+1, d->perm_ux[i]+1);
  for(i=0; i<d->dims; i++)
    if(d->fold[i]==1)
      fprintf(F,"Folding in w%d ................. TPPI\n", i+1);
    else if(d->fold[i]==2)
      fprintf(F,"Folding in w%d ................. RSH\n", i+1);
    else
      fprintf(F,"Folding in w%d ................. NO\n", i+1);
  fprintf(F,  "Type of spectrum .............. %s\n","?");
  for(i=0; i<d->dims; i++)
    fprintf(F,"Identifier for dimension w%d ... %s\n", i+1, d->iname[i]);
  fclose(F);
  return 1;
}

/**********************************************************************/
/**********************************************************************/
/**********************************************************************/
void get_inames(conv_data * d)
{ int i;
  d->max_sf=0;
  for(i=0; i<d->dims; i++)
    d->max_sf=d->max_sf<d->sf[i]?d->sf[i]:d->max_sf;
  for(i=0; i<d->dims; i++) {
    if((d->sf[i]/d->max_sf>0.999)&&(d->sf[i]/d->max_sf<1.001))strcat(d->iname[i],"-H");
    if((d->sf[i]/d->max_sf>0.251)&&(d->sf[i]/d->max_sf<0.252))strcat(d->iname[i],"-C");
    if((d->sf[i]/d->max_sf>0.101)&&(d->sf[i]/d->max_sf<0.102))strcat(d->iname[i],"-N");
    if((d->sf[i]/d->max_sf>0.404)&&(d->sf[i]/d->max_sf<0.405))strcat(d->iname[i],"-P");
  }
}

/**********************************************************************/
/**********************************************************************/
/**********************************************************************/
void from_spec_param_constructor()
{  int i;
   for(i = 1; i <= 47; i++)
      reverse[i] = (int)(xexp10((float)(i - 1)/(float)6.6438562)+(float)0.5);
   for(i = 48; i <= 95; i++)
      reverse[i] = -(int)(xexp10((float)(95 - i)/(float)6.6438562)+(float)0.5);
   reverse[0] = 1;
}
/* from filegen2d.c */
/* Calculate look up table for exponentials (base root 2) *********/
/*
        for(i = 1; i <= 64; i++)
           expon[i] = (int)(xexp10((float)i/6.6438562)+0.5);

        for(i = 1; i <= 47; i++)
           reverse[i] = (int)(xexp10((float)(i - 1)/6.6438562)+0.5);
        for(i = 48; i <= 95; i++)
           reverse[i] = -(int)(xexp10((float)(95 - i)/6.6438562)+0.5);
        reverse[0] = 1;
*/

/**********************************************************************/
/**********************************************************************/
/**********************************************************************/
void floattox16(float *realin, unsigned char *lo16)
{ 
  	int lo, hi; 
  	float real=*realin;
  	int sign;
	
  	if(real<0){sign=-1; real=-real; } else sign=1;
	
  	if(real<0.5) { 
		*lo16=(unsigned char)0; 
		*(lo16+1)=(unsigned char)0; 
		return; 
  	}
 	 
  	hi=(int)(log(real)*(float)2.8853902+1.414);
	
  	if(hi<0) hi=0; 
  	if(hi>47) hi=47;
	
  	lo=(int)(712*real/reverse[hi]-615);
	
  	if(lo<0)lo=0; if(lo>255)lo=255;
  	if((sign<0) && hi) hi=96-hi;
	
  	*lo16=(unsigned char)lo;
  	*(lo16+1)=(unsigned char)hi;
}

