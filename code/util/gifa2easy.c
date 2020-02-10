#include <errno.h>
#include <stdio.h>
#include <math.h>

/**************************************************************************    
*                                                                         *
*                          gifa2easy.c                                    *
*                                                                         *
* Program to generate the 8 bit files required by the NMR program easy    *
* Input file can be text or standard compact format of gifa.              *
* All output data is in SER format.                                       *
*                                                                         *
*                                       A. Dautant, G. Comberton MAY 92   *
* Laboratoire de Cristallographie, Bordeaux I                             *
*************************************************************************** 
*
*
* COMBERTON G avril 96 MODIFS POUR SGI  
*
*
***************************************************************************/

#define MAX_STR 200

#define SFORM	"s"  /* Format  codes */
#define TFORM 	"t"

/* Local subroutines *****************/

int eight_bit_conversion();
int logr2();
int nbuf;
/*
float spec;
*/

/* Array to store NMR data ****/
float xexp10(x)
float x;
{ return (exp(x*log(10.0))); }

  
/* Global variables **********************/
 
FILE *fp_input,*fp_8;

char fn_input[MAX_STR];          /* input filename                        */
char fn_8[MAX_STR];              /* 8 bit filename                        */
char filename[MAX_STR];          /* Basic filename (no extension)         */
char format;                  /* format name s or t                    */

int bits;                      /* Number of bits data is coded in        */
int w1_points,w2_points;       /* Number of data points in w1 and w2     */
int header_length;             /* Length of header in SMX file           */
int sub_width,sub_height;      /* Submatrix width and height in SMX file */
int scale_factor;              /* Power of 2 by which to divide data     */
int nombre_points;

/******************************* Main program ********************************/


main()
{
   int i;
   
/* Get some information from the user ****************************************/

   printf("\n\n## 2D file generation for EASY from a gifa file ### MAY 90 ##\n");

   printf("\nEnter full file name of the gifa file ................... ");
   scanf("%s",filename);
   
   strcpy(fn_input,filename);


   do {
      printf("Is the data are in S[s] or T[t] format .................. ");
      scanf("%s",&format);
   }
      while(format != 's' && format != 't' ); 

	do {
         printf("Power of 2 to scale data down by (8 is good) ............ ");
         scanf("%d",&scale_factor);
	   }
      while(scale_factor < 0 || scale_factor > 31); 


   printf("\n");

/* Define the file names ****************************************************/

   strcpy(fn_8,filename);
   strcat(fn_8,".8");			/* 8 bit file      (fp_8)    */

   printf("\n\n--------------- Converting file %s ----------------\n",filename);

   if(eight_bit_conversion()) exit(0);


   printf("\n\n-------------------------------------------------------");
   printf("\n\nFile generation completed\n");
}

/*****************************************************************************
*             Convert the data to 8 bit form                                 *
*****************************************************************************/

/*
int
eight_bit_conversion()
*/
 int  eight_bit_conversion()

{
   register int i,j,k,l;
   int dim,itype;
   char toto,nom;
   int byte_0,byte_1,byte_2,byte_3;
   char byte1,byte2;
   int col_skip,sub_skip;
   int address;
   int idata,ki;
   int expon[64];
   float data;
   char compact[8];
   char bufdata;
/*
  float const,fact,max0;
   char sign;
*/
       int          espec[6];
       int          p,q,csi;
	int nres;
       int err;
   float aconst;
 float gege;
   float fact;
   float max0;
   char asign;
 float  dspec ;


/* Check file status *****************************************/
	nres=80;


   if((fp_8 = fopen(fn_8,"w")) == 0)
   {
      printf("\nProblems opening '8' bit file\n\n");
      return(1);
   }

   if((fp_input = fopen(fn_input,"r")) == 0)
   {
      printf("\nProblems opening 'input' file\n\n");
      return(1);
   }
   fscanf(fp_input,"%s",&toto);
   fscanf(fp_input,"%d",&dim);
	printf("dim = %d \n",dim);
   if ( dim == 1 ) exit(0);
   fscanf(fp_input,"%s",&toto);
   fscanf(fp_input,"%s",&nom);
	printf("nom = %s \n",&nom);
   fscanf(fp_input,"%s",&toto);
   fscanf(fp_input,"%d",&w1_points);
   	printf("Number of data points in w1 dimension = %d \n",w1_points);
   fscanf(fp_input,"%s",&toto);
   fscanf(fp_input,"%d",&w2_points);
   	printf("Number of data points in w2 dimension = %d \n",w2_points);
   fscanf(fp_input,"%s",&toto);
   fscanf(fp_input,"%d",&itype);
	printf("itype = %d \n",itype);
   fscanf(fp_input,"%s",&toto);
   fscanf(fp_input,"%s",&toto);
   fscanf(fp_input,"%s",&toto);
   fscanf(fp_input,"%s",&toto);
   fscanf(fp_input,"%s",&toto);
   fscanf(fp_input,"%s",&toto);
   fscanf(fp_input,"%s",&toto);
   fscanf(fp_input,"%s",&toto);
     if (format == 's') {
   fscanf(fp_input,"%s",&toto);
   fscanf(fp_input,"%f",&max0);
	printf("max0  = %f \n",max0);
   fscanf(fp_input,"%s",&toto);
   fscanf(fp_input,"%f",&aconst);
	printf("normalisation = %f \n",aconst);
       fact=max0/aconst;
       fact = 1.0;
     printf("fact %f \n",fact);
      }
   fscanf(fp_input,"%s",&toto);

/* Write standard raster file header to 8 bit file ****************/

   putc(0x59,fp_8);                  
   putc(0xa6,fp_8);
   putc(0x6a,fp_8);
   putc(0x95,fp_8);
   putw(w2_points,fp_8);            
   putw(w1_points,fp_8);           
   putw(8,fp_8);                  
   putw(w1_points*w2_points,fp_8);
   putw(1,fp_8);
   putw(1,fp_8);
   putw(0x300,fp_8);          
   for(i = 0; i < 768; i++)
      putc(0,fp_8);
/* Calculate look up table for exponentials (base root 2) *********/

   for(i = 0; i <= 64; i++)
{
/*COMBER
      expon[i] = nint(exp10((float)i/6.6438562));
*/
/*
     expon[i] = (int) (exp((float)i/6.6438562));
*/
      expon[i] = (int)(xexp10((float)i/6.6438562)+0.5);
}

 
   printf("\nGIFA to EASY conversion in progress");
   printf("\n8 bit file being generated ...... \n");

/* Loop over data *******************************/
     if (format == 't') {
   for(j = 0; j < (w1_points*w2_points); j++)
   {
/* read formated data */
     fscanf(fp_input,"%f",&data);

     idata=(int)data;
     idata = idata /  scale_factor;
/* Write out in serial form ********************************************/

	       putc((char)logr2(idata,expon),fp_8);
   }
	}
   else
/* read compacted data */
	{
     nombre_points = (w1_points*w2_points)-1;
printf("%s %d","nb de points=",nombre_points);
   for(j = 0; j < nombre_points; j++)
/*
printf("%s","COMBER");
*/
   {
 
	k=0;
 

    do
	{
     fscanf(fp_input,"%c",&bufdata);
          if ( bufdata != ' ' && bufdata != '\n')
       {
/*COMBER*/
	compact[k]=bufdata;
	k++;
       }
	}
    while ( bufdata != '+' && bufdata != '-');


       asign =bufdata ;
        compact[k]='\0';
        k--;
        q=0;
  if ( k == 1 ) idata = 0;
  if ( k != 1 )
    {
        for ( p = 0 ; p <k ; p++)
		{
         err=0;
         b10(compact[p],err);
	espec[p]=nbuf;
	if (err == 0 ) q++;
		}
        csi=q-1;
        base10(fact,asign,espec,csi,&dspec);
          idata=(int) dspec;
/* Write out in serial form ********************************************/
}

	     putc((char)logr2(idata,expon),fp_8); 
   }
	}


   fclose(fp_8);
   fclose(fp_input);

   return(0);
}


/*****************************************************************************
*  Calculate the logarithm of each data point and map onto the color scale   *
*  uses a successive approximation technique.                                *
*****************************************************************************/

int
logr2(num,expon)

register num;
register *expon;

{
   register i;
   register cnt;

   i = 31;   /* Start guess root2^31             */
   cnt = 16; /* number to add to guess  root2^16 */

   if(num >= 1) /* Positive number */
   {
      while(num < expon[i] || num >= expon[i+1])
      {
	 if(num > expon[i])
	   i += cnt;
	 else
	   i -= cnt;
	 cnt >>= 1;
      }

      if((expon[i+1] - num) <= (num - expon[i]))
	 i++;

      i++;	
      if(i > 47) i = 47;
   }
   else if(num <= -1) /* Negative number */
   {
      num = -num;

      while(num < expon[i] || num >= expon[i+1])
      {
	 if(num > expon[i])
	   i += cnt;
	 else
	   i -= cnt;
	 cnt >>= 1;
      }

      if((expon[i+1] - num) <= (num - expon[i]))
	 i++;

      if(i > 47) i = 47;
      i = 95 - i;
   }
   else if(num == 0)
      i = 1;

   return(i);
}
   
b10(nb,err)
       char nb;
	int err;
{
	{
       if ( (nb >= '0') && (nb <= '9'))
        nbuf=(int)(nb)-(int)('0');
       else if (nb >= 'A'  &&  nb <= 'Z') 
        nbuf=(int)(nb)-(int)('A')+10;
       else if (nb >= 'a'  &&  nb <= 'z') 
        nbuf=(int)(nb)-(int)('a')+36;
       else if (nb == ',') 
        nbuf=62;
       else if (nb == '.') 
        nbuf=63;
       else
       {
      err=1;
         printf("%c error in data \n",nb);
       }
	}
}

base10(fact,sign1,espec1,csi1,dspec1)
       int     csi1,espec1[6];
       float       fact;
       float *dspec1;
       char sign1;
 {
       int m;

        float spec;
	float temp;
       temp=0;
/*
   for ( m = csi1 ; m <= 0    ; m--)
        temp=temp*64+ espec1[m];
*/
       m=csi1;

/*
       printf ("%d ",csi1);
*/
       do
          {
        temp=temp*64+ espec1[m];
       m = m -1 ;
       }
         while (m  != 0); 
        
       if (sign1 == '-') 
        spec=(-1)*fact*temp;
       else
        spec=fact*temp;
         *dspec1=spec;
	}