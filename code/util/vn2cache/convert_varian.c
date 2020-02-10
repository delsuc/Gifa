/***********************************************************************/
/* This function will read VNMR (Varian) NMR files
   from VNMR V3.1 and higher and write a gifa cache file. */

/*Modification of lvarian.c and ux2cache.c to read a varian 
file and write it out as a Gifa cache file ****************/

/**** can also write 3d data sets *************************/

/* TS 28.7.99 */
/**** Modfied to handle 2 types of varian 3d data sets*****/
/* TS 13.12.99 */ 

/***********************************************************************/

#include <stdio.h>
#ifndef DARWIN
#  include <malloc.h>
#endif
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "cache_mad.h"
#include "sizebasec.h"
#include <string.h>

/*#define DEBUG 1 */

struct datafilehead
{
   long int nblock;
   long int ntraces;
   long int np;
   long int ebytes;
   long int tbytes;
   long int bbytes;
   short int vers_id;
   short int status;
   long int nbheaders;
};
struct datablockhead
{
   short int scale;
   short int status;
   short int index;
   short int mode;
   long int ctcount;
   long int lpval;
   long int rpval;
   long int lvl;
   long int tlt;
};
struct hypercmplxbhead
{
   short int s_spare1;
   short int status;
   short int s_spare2;
   short int s_spare3;
   long int  l_spare1;
   float lpval1;
   float rpval1;
   float f_spare1;
   float f_spare2;
 };


/**********************************************************************/
main (int argc,char *argv[])

{
#ifndef DARWIN
        extern char *sys_errlist[];
#endif
        extern int  errno;

	FILE *f_in;

        DATASET *id;

        char name_out[256];

        int statusc, type, itype, option, modev;
        int size1d,size2d,size3d,dime,size1dc,size2dc,size3dc;
        int modec,longu,errorc,from;
        int siz_fid,ind_row,ind_plan,nb_plan,siz_plan;
	int *error;
	int *buffer2;

	short int *buffer4;

	long int i,j,k,l,m,v,cnt,cntv,loop,loopv;
       
	double specw1,specw2,specw3,offset1,offset2,offset3;
        double freq1,freq2,freq,freq3;
	
	float *ip;
	float *buffer3;  /* used for int conversion if needed */

        struct stat buf;
	struct datafilehead block;
	struct datablockhead block2;
	struct hypercmplxbhead block3;
	
	errorc=0;

	printf("vn2cache 28.7.99 \n\n");

/* read in parameters from vn2cach.sh or user input ****************/

if (argc != 19) {

  fprintf(stderr,"... wrong number of arguments\n\n");
  fprintf(stderr,"USAGE:\n");
  fprintf(stderr,"        tsreadv vnnmr_in_file gifa_out_file \\\n");
  fprintf(stderr,"                 DIM ITYPE FREQ \\\n");
  fprintf(stderr,"                 SZ1 SW1 FREQ1 OFFSET1\\\\n");
  fprintf(stderr,"                 SZ2 SW2 FREQ2 OFFSET2\\\\n");
  fprintf(stderr,"                 SZ3 SW3 FREQ3 OFFSET3\\\\n");
  fprintf(stderr,"                 VARIAN_MODE\n");
  fprintf(stderr,"with :\n");
  fprintf(stderr,"        vnnmr_in_file:\n");
  fprintf(stderr,"                varian fid file.\n");
  fprintf(stderr,"        gifa_out_file:\n");
  fprintf(stderr,"                Gifa format output file name.\n");
  fprintf(stderr,"        DIM:\n");
  fprintf(stderr,"                dimension of experiment. (i.e. 1, 2 or 3)\n");  
  fprintf(stderr,"        ITYPE:\n");
  fprintf(stderr,"                data type (cf manual) (could be 0).\n");
  fprintf(stderr,"        FREQ:\n");
  fprintf(stderr,"                spectrometer frequency. (i.e. main freq, e.g. 400)\n");
  fprintf(stderr,"        SZI (I=1,2,3):\n");
  fprintf(stderr,"                data size in dimension i. (in number of points)\n");
  fprintf(stderr,"		  (real+imaginary ie ni*2)\n");
  fprintf(stderr,"        SWI (I=1,2,3):\n");
  fprintf(stderr,"                spectral widthes in dimension i. (in Hz)\n");
  fprintf(stderr,"        FREQI (I=1,2,3):\n");
  fprintf(stderr,"                frequency in dimension i. (in MHz e.g. 100.13)\n");
  fprintf(stderr,"	  OFFSETI (I=1,2,3):\n");
  fprintf(stderr,"		  offset from 0ppm at upfield edge (in Hz)\n");
  fprintf(stderr,"        VARIAN_MODE (1 or 2)\n");
  fprintf(stderr,"		  1 for 1d or 2d data\n");
  fprintf(stderr,"		  1 for r1,p1,r1,p2 3d data\n");
  fprintf(stderr,"		  2 for r1,p1,r2,p2 3d data\n");


  exit(1);
                 }


/** get values from from user input **/
	
	dime = atoi(argv[3]);
	itype = atoi(argv[4]);
        freq = atof(argv[5]);
        size1d = atoi(argv[6]);
        specw1 = atof(argv[7]);
        freq1 = atof(argv[8]);
        offset1 = atof(argv[9]);
        size2d = atoi(argv[10]);
        specw2 = atof(argv[11]);
        freq2 = atof(argv[12]);
        offset2 = atof(argv[13]);
        size3d = atoi(argv[14]);
        specw3 = atof(argv[15]);
        freq3 = atof(argv[16]);
        offset3 = atof(argv[17]);
 	modev = atoi(argv[18]);
        strcpy(name_out,argv[2]);

#ifdef DEBUG
        printf("dime=%d\n",dime);
        printf("itype=%d\n",itype);
        printf("freq=%f\n",freq);
        printf("size1d=%d\n",size1d);
        printf("size2d=%d\n",size2d);
        printf("size3d=%d\n",size3d);
        printf("specw1=%f\n",specw1);
        printf("specw2=%f\n",specw2);
        printf("specw3=%f\n",specw3);
        printf("freq1=%f\n",freq1);
        printf("freq2=%f\n",freq2);
        printf("freq3=%f     \n",freq3);
	printf("modev=%d\n",modev);
        printf("out file is: %s\n",name_out);
        printf("File-name is: %s\n",argv[1]);
#endif

/****************** get values from fid header ********/

if (stat(argv[1], &buf) == -1) {

  (void) fprintf(stderr,"... cannot access %s: \n", argv[1]);
  (void) fprintf(stderr,"    ERROR %s\n", sys_errlist[errno]);

  exit(1);			}
 

#ifdef DEBUG
	printf ("buf.st_size=%d\n",buf.st_size);
#endif
  

if ((f_in = fopen(argv[1], "r")) == NULL) {

  (void) fprintf(stderr,"... cannot read %s: \n", argv[1]);
  (void) fprintf(stderr,"    ERROR %s\n", sys_errlist[errno]);

  exit(1);
                                           }
else {

   printf("...%s open for read, %d bytes\n", argv[1], buf.st_size);

     }
     
     
/* read file header */
   if (fread(&block,sizeof(struct datafilehead),1,f_in) != 1)
      { printf("*** read error 1 \n");  *error=1; return;}

/* swap byte if on a big endian machine (PC)  symbol is defined outside */
#ifdef is_big_endian
     swapbyte((char *) &block, 6*4);
     sswapbyte((char *) &block.vers_id, 2*2);
     swapbyte((char *) &block.nbheaders, 4);
#endif

#ifdef DEBUG
   printf("printing value of variable 'block' of type 'datafilehead'\n");
   printf("     block.nblock=%d\n",block.nblock);
   printf("     block.ntraces=%d\n",block.ntraces);
   printf("     block.np=%d\n",block.np);
   printf("     block.ebytes=%d\n",block.ebytes);
   printf("     block.tbytes=%d\n",block.tbytes);
   printf("     block.bbytes=%d\n",block.bbytes);
   printf("     block.vers_id=%d\n",block.vers_id);
   printf("     block.status=%d\n",block.status);
   printf("     block.nbheaders=%d\n\n",block.nbheaders);
#endif
     

   if ( block.status & 0x8) {    /*  float */
     type = 0;              }
     else {
     if (block.status & 0x4) {
       type = 1;             }    /* int */
       else {
       type = 2;                 /* short int */
            }
           }

#ifdef DEBUG
    printf(" type = %d\n",type);
#endif

/*** set sizes of dimensions for use in writing cache file and reading fid ***/

  switch(dime) {

        case 1:
          siz_fid=block.np;
          siz_plan=1;
          nb_plan=1;
          size1dc=siz_fid;
          size2dc=1;
          size3dc=1;
          break;

        case 2:
          siz_fid=block.np;
          siz_plan=size1d;
          nb_plan=1;
          size1dc=siz_plan;
          size2dc=siz_fid;
          size3dc=1;
          break;
          
	case 3:
	  siz_fid=block.np;
          siz_plan=size2d;
          nb_plan=size1d;
          size1dc=nb_plan;
          size2dc=siz_plan;
          size3dc=siz_fid;
          break;
                }
                
#ifdef DEBUG
   if(dime==1) {
   	printf("size1d= %d size2d=%d size3d=%d\n",size1dc,size2dc,size3dc);
   } else { if(dime==2) {printf("size1d= %d size2d=%d size3d=%d\n",size1dc,size2dc,size3dc);}
   	    else {printf("size1d= %d size2d=%d size3d=%d\n",size1dc,size2dc,size3dc);
   	  }
   }
#endif

/*** check sizes and agreement with fid header ***************/
 
        if (size1d <= 0) {
                (void) fprintf(stderr,
                "ERROR in command line argument: SZ1D = %d\n", size1d);
                exit(1);
		          }

        if (size2d <= 0) {
                (void) fprintf(stderr,
                "ERROR in command line argument: SZ2D = %d\n", size2d);
                exit(1);
	                  }
	                  
	if (size3d <= 0) {
                (void) fprintf(stderr,
                "ERROR in command line argument: SZ3D = %d\n", size3d);
                exit(1);
	                  }
	                  
	if (siz_plan*nb_plan != block.nblock) {
		 (void) fprintf(stderr,
                "ERROR with sizes or dime: SZ1D or SZ1D*SZ2D = %d not nblock = %d\n",siz_plan*nb_plan,block.nblock);
                exit(1);                 
					      }

#ifdef DEBUG
	printf ("buf.st_size=%d\n",buf.st_size);
        printf ("size1d * size2d * size3d * sizeof(int)=%d\n",(siz_fid*siz_plan*nb_plan*sizeof(int)));
#endif

/*** allocate transfer buffers ********************************/

   if ((buffer2 = (int *) malloc(block.np*block.ebytes)) == NULL)
     { printf("read error 4 \n");  *error=1; return; }
     
#ifdef DEBUG
   printf ("\nALLOCATED MEMORY FOR buffer2 OF SIZE %d BYTES\n\n", block.np*block.ebytes);
#endif

if (block.ebytes!=4) {

   if ((ip = (float *) malloc(block.nblock*block.np*sizeof(float))) == NULL)
     { printf("read error 4 \n");  *error=1; return; }
     
#ifdef DEBUG
   printf ("\nALLOCATED MEMORY FOR ip OF SIZE %d BYTES\n\n", block.np*sizeof (float));
#endif
}

/*** create cache file ****************************************/

        CH_INITIALISE();
        modec = CF_WRITE;

        statusc = 0;
        longu=strlen(name_out);
        CH_CREATE(&id,name_out,&longu,&modec,&statusc);

  if (statusc != 0) {

     fprintf(stderr,"Error #: %d in opening file %s\n",statusc,name_out);
     exit(1);
                   }

        CH_SETUP(&id, &dime, &size1dc, &size2dc, &size3dc, &statusc);

  if (statusc != 0)  {

     fprintf(stderr,"Error #: %d in setting-up file %s\n",statusc,name_out);
     exit(1);
                     }

#ifdef DEBUG
	printf("created file %s\n",name_out);
#endif

        option = H_INT;
        longu = strlen("Type");
        CH_PUTPARAM_TYPE(&id, &itype, "Type", &longu, &option, &statusc);
        option = H_DOUBLE;
        longu = strlen("Frequency");
        CH_PUTPARAM_TYPE(&id, &freq, "Frequency", &longu, &option, &statusc);

  if (statusc != 0) {

     fprintf(stderr,"Error #: %d in writing parameters \n",statusc);
     exit(1);
                    }

  switch(dime) {

        case 3 :
          longu = strlen("Specw3");
          CH_PUTPARAM_TYPE(&id, &specw3, "Specw3", &longu, &option, &statusc);
          longu = strlen("Freq3");
          CH_PUTPARAM_TYPE(&id, &freq3, "Freq3", &longu, &option, &statusc);
	  longu = strlen("Offset3");
          CH_PUTPARAM_TYPE(&id, &offset3, "Offset3", &longu, &option, &statusc);
        case 2 :
          longu = strlen("Specw2");
          CH_PUTPARAM_TYPE(&id, &specw2, "Specw2", &longu, &option, &statusc);
          longu = strlen("Freq2");
          CH_PUTPARAM_TYPE(&id, &freq2, "Freq2", &longu, &option, &statusc);
	  longu = strlen("Offset2");
          CH_PUTPARAM_TYPE(&id, &offset2, "Offset2", &longu, &option, &statusc); 
        case 1 :
          longu = strlen("Specw1");
          CH_PUTPARAM_TYPE(&id, &specw1, "Specw1", &longu, &option, &statusc);
          longu = strlen("Freq1");
          CH_PUTPARAM_TYPE(&id, &freq1, "Freq1", &longu, &option, &statusc);
          longu = strlen("Offset1");
          CH_PUTPARAM_TYPE(&id, &offset1, "Offset1", &longu, &option, &statusc);
               }

  if (statusc != 0) {

     fprintf(stderr,"Error #: %d in writing parameters \n",statusc);
     exit(1);
                    }

#ifdef DEBUG
	printf("wrote header\n");
#endif


#ifdef DEBUG
   printf("out file is: %s\n",name_out);
#endif

/****************check that everything is still OK***********************/

#ifdef DEBUG
   printf("printing value of variable 'block' of type 'datafilehead'\n");
   printf("     block.nblock=%d\n",block.nblock);
   printf("     block.ntraces=%d\n",block.ntraces);
   printf("     block.np=%d\n",block.np);
   printf("     block.ebytes=%d\n",block.ebytes);
   printf("     block.tbytes=%d\n",block.tbytes);
   printf("     block.bbytes=%d\n",block.bbytes);
   printf("     block.vers_id=%d\n",block.vers_id);
   printf("     block.status=%d\n",block.status);
   printf("     block.nbheaders=%d\n\n",block.nbheaders);
#endif

#ifdef DEBUG
   printf ("status=%x  type = %d  itype = %d dime = %d \n",block.status, type, itype, dime);
#endif


/*************** read in and convert and write out the data *************/
/******************** for all the blocks in the file ********************/
/*									*/
/* m is block counter, l is loop counter- loop=2 for 3d, 1 for 1d or 2d */
/* v is loop counter for 3d data-loopv=1 for 1d,2d			*/
/*				 loopv=1 for 3d data of type r1,p1,r1,p2*/
/*				 loopv=2 for 3d data of type r1,p1,r2,p1*/
/************************************************************************/
 
m=1;


if(dime==3){cnt=nb_plan/2-1;
	   loop=2;}
else {cnt=0;
      loop=1;}
      
      
if(modev==2){cntv=siz_plan/2-1;
	     loopv=2;}
else {cntv=siz_plan-1;
      loopv=1;}
            

for(i=0;i<=cnt;i++)
   {
    #ifdef DEBUG
      printf("reading data\n");
      printf("i=%d\n",i);
    #endif
    for(j=0;j<=cntv;j++)
       {
	#ifdef DEBUG
	  printf("reading data\n");
	  printf("j=%d\n",j);
	#endif
	for(l=1;l<=loop;l++)
	   {
	   
	   for(v=1;v<=loopv;v++)
	   {
	   /*** read in block m and convert it to floating-point data ***/
	   /*** need to read headers before each block is loaded ********/
	   
	   /* read first header */
	    if (fread(&block2,sizeof(struct datablockhead),1,f_in) != 1) {
        	if (j == 1) {
          	printf("read error 2.0 \n");  *error=1; return;
            } else {
          	printf("Encountered early file termination - assuming truncated dataset\n");
          	j = j-1;
            break;
	           }
      	    }
	   /* block2 is not used, no need to swab it */

	   #ifdef DEBUG
   		printf("printing value of variable 'block2' of type 'datablockhead'\n");
   		printf("     block2.scale=%d\n",block2.scale);
   		printf("     block2.status=%d\n",block2.status);
   		printf("     block2.index=%d\n",block2.index);
   		printf("     block2.mode=%d\n",block2.mode);
   		printf("     block2.ctcount=%d\n",block2.ctcount);
   		printf("     block2.lpval=%d\n",block2.lpval);
   		printf("     block2.rpval=%d\n",block2.rpval);
   		printf("     block2.lvl=%d\n",block2.lvl);
   		printf("     block2.tlt=%d\n\n",block2.tlt);
	   #endif

	   /* read second header */
            if (block.nbheaders == 2)
         	{ if (fread(&block3,sizeof(struct hypercmplxbhead),1,f_in) != 1 )
            	{ printf("read error 2.1 \n");  *error=1; return;}
            }
	   /* block3 is not used, no need to swab it */
	   #ifdef DEBUG
   		printf("printing value of variable 'block3' of type 'hypercmplxbhead'\n");
   		printf("     block3.s_spare1=%d\n",block3.s_spare1);
   		printf("     block3.status=%d\n",block3.status);
   		printf("     block3.s_spare2=%d\n",block3.s_spare2);
   		printf("     block3.s_spare3=%d\n",block3.s_spare3);
   		printf("     block3.l_spare1=%d\n",block3.l_spare1);
   		printf("     block3.lpval1=%f\n",block3.lpval1);
   		printf("     block3.rpval1=%f\n",block3.rpval1);
   		printf("     block3.f_spare1=%f\n",block3.f_spare1);
   		printf("     block3.f_spare2=%f\n\n",block3.f_spare2);
	   #endif
	   
	   /* read data block header */ 
            {if (fread(buffer2,block.np*block.ebytes, 1, f_in) != 1 )   
           	{printf("read error 3 %d\n",j); *error=1; return; }
	    #ifdef is_big_endian
		/*un swap for Linux */
     		if (type == 2) {
       		sswapbyte((char *) buffer2, block.np*block.ebytes);
     		} else {
       		swapbyte((char *) buffer2, block.np*block.ebytes);
     		}
	    #endif
            }
            
            /*** switch type: fid is real or integer or short int ********/


	    switch (type) {

     	      case 0:		/* real */
     
     		#ifdef DEBUG
		  printf("switch type\n");
     		#endif
	                  
       		buffer3 = (float *) buffer2;
       
           	for (k=0;k<=block.np;k++) { ip[k] = buffer3[k];
           			      	if(modev==2) {ind_row=j*2+v;}
           			      	else {ind_row=j+1;}
           			      	ind_plan=i*2+l;
           			      	#ifdef DEBUG
        				 printf("buffer3[%d]=%d\n",k,buffer3[k]);
        				 printf("ip[%d]=%f\n",k,ip[k]);
	   				#endif
           			    	 }
           			      	cache_write (ip,id,siz_fid,ind_row,ind_plan,dime,from,modec,longu,errorc,name_out);
           			      	m=m+1;
           
		#ifdef DEBUG
       		  printf("read data with type=0\n");
		#endif

       		break;
       
     	      case 1:		/* integer */
     		
     		#ifdef DEBUG
		  printf("switch type\n");
     		#endif
     		
           	ip = (float *) buffer2;
           	
          	for (k=0;k<=block.np;k++) { ip[k] = buffer2[k];
           			       	if(modev==2) {ind_row=j*2+v;}
           			      	else {ind_row=j+1;}
           			       	ind_plan=i*2+l;
           				#ifdef DEBUG
        				 printf("ind_plan= %d ind_row= %d m=%d\n",ind_plan,ind_row,m);
        				 /*printf("buffer2[%d]=%d\n",k,buffer2[k]);*/
        				 printf("ip[%d]=%f\n",k,ip[k]);
	   				#endif
           			           }
           			      	cache_write (ip,id,siz_fid,ind_row,ind_plan,dime,from,modec,longu,errorc,name_out);
           			      	m=m+1;
           
		#ifdef DEBUG
        	  printf("read data with type=1\n");
		#endif

       		break;
       
     	      case 2:		/* short integer */
     	      
     	      	#ifdef DEBUG
		  printf("switch type\n");
     		#endif
     	      
       	   	buffer4 = (short int *) buffer2;
       	   
           	for (k=0;k<=block.np;k++) { ip[k] = buffer4[k];
    				       	if(modev==2) {ind_row=j*2+v;}
           			      	else {ind_row=j+1;}
           			       	ind_plan=i*2+l;
           				#ifdef DEBUG
        				 printf("buffer4[%d]=%d\n",k,buffer4[k]);
        				 printf("ip[%d]=%f\n",k,ip[k]);
 	   				#endif
           			     	  }
           			      	cache_write (ip,id,siz_fid,ind_row,ind_plan,dime,from,modec,longu,errorc,name_out);
           			      	m=m+1;
          
		#ifdef DEBUG
        	  printf("read data with type=2\n");
		#endif

       		break;
	     }
	    } 
	   }
	  }
	 } 
	 
#ifdef DEBUG
	printf("looped through all j and i\n");
	printf("m is %d\n",m);
#endif

   fclose(f_in);

#ifdef DEBUG
        printf("closed read file\n");
#endif

   CH_CLOSE(&id, &statusc);

#ifdef DEBUG
        printf("closed write file\n");
#endif

   exit(0);
}


/***** subroutine to write block m into cache file **************************/

cache_write (ip,id,siz_fid,ind_row,ind_plan,dime,from,modec,longu,errorc,name_out)

DATASET *id;
float *ip;
int siz_fid,ind_row,ind_plan,dime;
int from,modec,longu,errorc;
char name_out[256];

{
	from = 1;
	modec = CB_WRITE;
	longu=strlen(name_out);
	errorc=0;

switch(dime)	{ 
                 
	case 1 :

	   CH_AC1DAREA((float *) ip,&id,&from,&siz_fid,&modec,&errorc);
	   #ifdef DEBUG
		printf("writing data\n");
	   #endif
           break;

	case 2 :
           CH_AC2DAREA((float *) ip,&id,&ind_row,&from,
                      &ind_row,&siz_fid,&modec,&errorc);
           #ifdef DEBUG
		printf("writing data\n");
	   #endif           
           break;

	case 3 :

           CH_AC3DAREA((float *) ip,&id,&ind_plan,&ind_row,&from,
                      &ind_plan,&ind_row,&siz_fid,&modec,&errorc);
           #ifdef DEBUG
		printf("writing data\n");
	   #endif
           break;
                }

  if (errorc != 0) {
                     
	fprintf(stderr,"Error #: %d in writing row %d\n",errorc,ind_row);
        CH_CLOSE(&id,&errorc);
        exit(1);

                   }
}
/*****************************************************************************/
