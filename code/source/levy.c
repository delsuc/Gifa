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

/* UNDERSCORE should be defined for systems on which an underscore must be
appended to the C function name to be called from Fortran, typically most
UNIX machines, IBM Risc 6000/AIX being the most notable exception */

/* this prog reads G.levy data into internal GIFA representation 
   it creates a parameter file which will be used later on for
   writing back a G.levy file

  It is based on the Guang Zhu program...

  partially rewritten by M.A.D. 29-feb-1996   (yes it was a bisextil year !)

  This is a first try, Tell me if I can do better */

/* I've found info about parameter storage in levy's file, I'll
   try to use them, tell me if it's working */

/* called from Fortran (GIFA main) so pointer passing and _ in name */


#include <stdio.h>

#ifdef F2C
#  define WLEVY wlevy_
#  define LLEVY llevy_

#else
#ifdef UNDERSCORE
#  define WLEVY wlevy_
#  define LLEVY llevy_

#else
#  define WLEVY wlevy
#  define LLEVY llevy
#endif
#endif

void LLEVY(nin,lin,npar,lpar,data1d,data2d,si1,si2,itype,specw1,specw2,dim,error)

char *nin,*npar;
float *data1d, *data2d, *specw1, *specw2;
int *lin, *lpar, *si1, *si2, *itype, *dim, *error;
{
	float temp[513];  /* room for the parameter buffer */
	int i,j,f1_size,f2_size,t_otal;
	FILE *fin,*fh;
	char namein[256], namepar[256];

/* convert from FORTRAN */
	convert_string(namein,nin,*lin);
	convert_string(namepar,npar,*lpar);


/* open files */
	if (NULL == (fin=fopen(namein,"r"))) { *error = 1; return; }

 /* copy parameters */
        if (512 != fread(temp+1,sizeof(float),512,fin)) { *error = 2; return; }

	f2_size = temp[100];    /* let's try find out parameters */
	f1_size = temp[220];
	t_otal=f1_size*f2_size;
	*si2 = 2*temp[100];
	*si1 = temp[220];
	*specw2 = temp[101];
	*specw1 = temp[230];
	*itype = 0;

        if (f1_size == 1) {
	  if (2*t_otal != fread(data1d,sizeof(float),t_otal*2,fin)) { *error = 2; return; }
	  *dim = 1;
	} else {
	  if (2*t_otal != fread(data2d,sizeof(float),t_otal*2,fin)) { *error = 2; return; }
	  *dim = 2;
	}

/* do param file */

	if (NULL == (fh=fopen(namepar,"w"))) { *error = 1; return; }
	if (512 != fwrite(temp+1,sizeof(float),512,fh)) { *error = 3; return; }

/* then close */
	fclose(fin);fclose(fh);
}
/************************************************************************/
        

/* this one will write the levy file instead */

void WLEVY(nin,lin,npar,lpar,data,si1,si2,itype,specw1,specw2,error)

char *nin, *npar;
float *data,*specw1,*specw2;
int *lin, *lpar, *si1,*si2,*itype, *error;
{
        char nameout[256],namepar[256];
	int t_otal;
	float temp[513];
	FILE *fout,*fhd;

/* convert from FORTRAN */
	convert_string(nameout,nin,*lin);
	convert_string(namepar,npar,*lpar);

	 
	if (NULL == (fout=fopen(nameout,"w")))  { *error = 1; return; }
	if (NULL == (fhd=fopen(namepar,"r")))  { *error = 1; return; }

	if (512 != fread(temp+1,sizeof(float),512,fhd))  { *error = 2; return; }
	temp[100] = (*si2)/2;    /* let's try find out parameters */
	temp[220] = *si1;
	t_otal = (*si1)*(*si2);
	temp[101] = *specw2; 
	temp[230] = *specw1;
	temp[221]=1.0;
	temp[223]=1.0;

	if (512 != fwrite(temp+1,sizeof(float),512,fout))  { *error = 3; return; }

	if (t_otal != fwrite(data,sizeof(float),t_otal,fout))  { *error = 3; return; }
        fclose(fout);fclose(fhd);
}
