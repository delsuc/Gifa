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

#include <fcntl.h>
#include <string.h>
#include "ambdef.h"

/* variable de depart de la liste chainee */
AMB deb_amb, fin_amb;

/*************************************************/
/* tableau d'adresses des amibes
array amoeba;
valeurs dx des pixels de l'amibe
int *pixelx;
valeurs dy des pixels de l'amibe
int *pixely;
*/

void CREAT_AMB(int *error)
/*
   create the beginning and the end of the chained list of amoeba
*/

{	
   deb_amb.nxt = &fin_amb;
   deb_amb.indpk = -1;
   fin_amb.nxt = &fin_amb;
   fin_amb.indpk = -1; 
}

void CHECK_AMB(int *ipk_no, int *check)
/*
	cheak if the amoeba of a given peak is defined 
     number *ipk_no 
        *check = 0; not belongs
        *check = 1; belongs
	*ipk_no,  peak number;
        tableau d'adresses des amibes *iamoeba (tableau statique Fortran)
        valeurs dx par rapport aux coord du pic des pixels de l'amibe 
     *pixelx (pointeur C)
        valeurs dy par rapport aux coord du pic des pixels de l'amibe
     *pixely (pointeur C)
*/

{	
   AMB *amoeba;
/* get the adress of the initial pointeur of the amoeba chained list */
   amoeba = &deb_amb;
   while (amoeba->nxt != amoeba) {
     amoeba = amoeba->nxt;
/* look for the amoeba of the given peak */
     if (amoeba->indpk == *ipk_no) {
       *check = 1;
       break;
       } 
   }
}

void INI_AMB(int *ipk_no, int *ipkx, int *ipky, int *error)
/*
	initialize the amoeba of peak *ipk_no
	*ipk_no,  peak number;
        tableau d'adresses des amibes *amoeba (tableau statique Fortran)
        valeurs dx par rapport aux coord du pic des pixels de l'amibe 
     *pixelx (pointeur C)
        valeurs dy par rapport aux coord du pic des pixels de l'amibe
     *pixely (pointeur C)
*/

{	
AMB *amoeba,*ambnxt;
PIXEL *pixl;
/* get the adress of the initial pointeur of the amoeba chained list */
   amoeba = &deb_amb;
/* add the amoeba at the beginning of the chained list */
/* check for memory */
   if ((ambnxt = (AMB *)malloc(sizeof(AMB))) == NULL)
     {
       printf ("No more memory for amoeba !");
       *error = 1;
     }
   else
     { 
#ifdef DEBUG
       memset(ambnxt,0xA0A0,sizeof(AMB));
#endif
/* connect the first amoeba to the new amoeba */
       ambnxt->nxt = amoeba->nxt ;
/* connect the new amoeba to the amoeba following the first */
       amoeba->nxt = ambnxt;
       ambnxt->indpk = *ipk_no;
       ambnxt->pkx = *ipkx;
       ambnxt->pky = *ipky;
/* initialize the chained list of amoeba pixels */
       if ((pixl = (PIXEL *)malloc(sizeof(PIXEL))) == NULL)
         {
          printf ("No more memory for amoeba pixel !");
          *error = 1;
         }
         else
         { 
#ifdef DEBUG
       memset(pixl,0xA0A0,sizeof(PIXEL));
#endif
/* initialize the chained list of amoeba pixels */
         ambnxt->pix = pixl;
         pixl->dx = -1;
         pixl->dy = -1;
         pixl->nxt = pixl;
         }   
     }      
}

void DEL_AMB(int *ipk_no)
/*
	delete the amoeba of peak *ipk_no
	*ipk_no,  peak number;
        tableau d'adresses des amibes *iamoeba (tableau statique Fortran)
        valeurs dx par rapport aux coord du pic des pixels de l'amibe 
     *pixelx (pointeur C)
        valeurs dy par rapport aux coord du pic des pixels de l'amibe
     *pixely (pointeur C)
*/

{	
PIXEL *pixl,*pixprv;
AMB *amoeba,*ambprv;
/* get the adress of the initial pointeur of the amoeba chained list */
   amoeba = &deb_amb;
   while (amoeba->nxt != amoeba) {
     ambprv = amoeba;
     amoeba = amoeba->nxt;
/* look for the amoeba of the given peak */
     if (amoeba->indpk == *ipk_no) {
        pixl = amoeba->pix;
        while (amoeba->pix->nxt != amoeba->pix) {
/* remove all the pixels and free the corresponding memory */
           pixprv = pixl;    
           pixl = pixl->nxt;
           amoeba->pix = pixl;
           free(pixprv);
#ifdef DEBUG
           memset(pixprv,0xA0A0,sizeof(PIXEL));
#endif
	}
     }
     free(amoeba->pix);
     ambprv->nxt = amoeba->nxt;
     free(amoeba);
#ifdef DEBUG
     memset(amoeba,0xA0A0,sizeof(AMB));
#endif
   }
}

void SUM_AMB(int *ipk_no, int *total, float *sum, float *ftab, 
int *size1, int *size2)
/*
	calculate the value and the error of the integral of the amoeba of peak *ipk_no
        *ipkx,*ipky: peak coordinates
        *total: number of pixels in amoeba
        *sum: integral
	*ipk_no,  peak number;
        tableau d'adresses des amibes *iamoeba (tableau statique Fortran)
        valeurs dx par rapport aux coord du pic des pixels de l'amibe 
     *pixelx (pointeur C)
        valeurs dy par rapport aux coord du pic des pixels de l'amibe
     *pixely (pointeur C)
        tab: 2D experiment array
*/

{	
   PIXEL *pixl;
   AMB *amoeba;
   float sum_loc;
   int crdx,crdy,ipkx,ipky,adr,total_loc;
   sum_loc = 0.0;
   total_loc = 0;
/* get the adress of the initial pointeur of the amoeba chained list */
   amoeba = &deb_amb;
   while (amoeba->nxt != amoeba) {
     amoeba = amoeba->nxt;
/* look for the amoeba of the given peak */
     if (amoeba->indpk == *ipk_no) {
       ipkx = amoeba->pkx;
       ipky = amoeba->pky;
       total_loc++;
       adr = ((ipky-1)*(*size2)) + ipkx-1;  
       sum_loc = sum_loc + (ftab[adr]);
       pixl = amoeba->pix;
       while (pixl->nxt != pixl) {
/* calculate the sum of amoeba from each pixel */
          total_loc++;
          crdx = pixl->dx + ipkx;
          crdy = pixl->dy + ipky;
          adr = ((crdy-1)*(*size2)) + crdx-1;  
          sum_loc = sum_loc + (ftab[adr]);
          pixl = pixl->nxt;
       }
       *sum = sum_loc;
       *total = total_loc;
       break;
     }
   }
}

void GET_AMB(int *ipk_no, int *liste, int *nbpixel)
/*
	get the information corrresponding to the amoeba of peak *ipk_no
        *ipkx,*ipky: peak coordinates
        *total: number of pixels in amoeba
        *sum: integral
	*ipk_no,  peak number;
        tableau d'adresses des amibes *iamoeba (tableau statique Fortran)
        valeurs dx par rapport aux coord du pic des pixels de l'amibe 
     *pixelx (pointeur C)
        valeurs dy par rapport aux coord du pic des pixels de l'amibe
     *pixely (pointeur C)
*/

{	
   PIXEL *pixl;
   AMB *amoeba;
   int i,ipkx,ipky;
   i = 0;
/* get the adress of the initial pointeur of the amoeba chained list */
   amoeba = &deb_amb;
   while (amoeba->nxt != amoeba) {
     amoeba = amoeba->nxt;
/* look for the amoeba of the given peak */
     if (amoeba->indpk == *ipk_no) {
       ipkx = amoeba->pkx;
       ipky = amoeba->pky;
       i++;
       liste[i] = ipkx;
       i++;
       liste[i] = ipky;
       pixl = amoeba->pix;
       while (pixl->nxt != pixl) {
/* calculate the sum of amoeba from each pixel */
          i++;
          liste[i] = pixl->dx;
          i++;
          liste[i] = pixl->dy;
          pixl = pixl->nxt;
       }
     *nbpixel = i/2;
     break;
     }
   }
}

void PUT_AMB(int *ipk_no, int *liste, int *nbpixel, int *error)
/*
	put a given list of pixels into the amoeba of peak *ipk_no
        *ipkx,*ipky: peak coordinates
        *total: number of pixels in amoeba
        *sum: integral
	*ipk_no,  peak number;
        tableau d'adresses des amibes *iamoeba (tableau statique Fortran)
        valeurs dx par rapport aux coord du pic des pixels de l'amibe 
     *pixelx (pointeur C)
        valeurs dy par rapport aux coord du pic des pixels de l'amibe
     *pixely (pointeur C)
 */

{	
   PIXEL *pixl,*pixnxt;
   AMB *amoeba,*ambnxt,*ambtmp;
   int i;
/* get the adress of the initial pointeur of the amoeba chained list */
   amoeba = &deb_amb;
/* add the amoeba at the beginning of the chained list */
/* check for memory */
   if ((ambnxt = (AMB *)malloc(sizeof(AMB))) == NULL)
     {
       printf ("No more memory for amoeba !");
       *error = 1;
       goto fin;
     }
   else
     { 
#ifdef DEBUG
       memset(ambnxt,0xA0A0,sizeof(AMB));
#endif
/* connect the first amoeba to the new amoeba */
       ambnxt->nxt = amoeba->nxt ;
/* connect the new amoeba to the amoeba following the first */
       amoeba->nxt = ambnxt;
       ambnxt->indpk = *ipk_no;
       ambnxt->pkx = liste[0];
       ambnxt->pky = liste[1];
     }
/* initialize the chained list of amoeba pixels */
   if ((pixl = (PIXEL *)malloc(sizeof(PIXEL))) == NULL)
     {
       printf ("No more memory for amoeba pixel !");
       *error = 1;
       goto fin;
     }
   else
     { 
#ifdef DEBUG
       memset(pixl,0xA0A0,sizeof(PIXEL));
#endif
/* initialize the chained list of amoeba pixels */
       ambnxt->pix = pixl;
       pixl->dx = -1;
       pixl->dy = -1;     
       pixl->nxt = pixl;
     }   
/* add the pixels from the array liste to the amoeba */
   for (i = 1; i <= (*nbpixel)-1; i++) {
/* add the pixel at the beginnig of the chained list */
       if ((pixnxt = (PIXEL *)malloc(sizeof(PIXEL))) == NULL)
        {
        printf ("No more memory for pixel amoeba !");
        *error = 1;
        goto fin;
        }
        else
        { 
#ifdef DEBUG
        memset(pixnxt,0xA0A0,sizeof(PIXEL));
#endif
        pixnxt->nxt = ambnxt->pix;
        ambnxt->pix = pixnxt;
        pixnxt->dx = liste[2*i];         
        pixnxt->dy = liste[(2*i) + 1];     
	}    
   }
  fin: ;
}

void PUT_PIX_AMB(int *ipk_no,int *ipixx,int *ipixy,int *error)
/*
	put the given pixel (*ipixx,*ipixy) into the amoeba of the peak 
     ipk_no 
	*ipk_no,  peak number;
        tableau d'adresses des amibes *iamoeba (tableau statique Fortran)
        valeurs dx par rapport aux coord du pic des pixels de l'amibe 
     *pixelx (pointeur C)
        valeurs dy par rapport aux coord du pic des pixels de l'amibe
     *pixely (pointeur C)
*/

{	
   PIXEL *pixnxt;
   AMB *amoeba;
   int crdx,crdy,adr,ipkx,ipky;
/* get the adress of the initial pointeur of the pixel chained liste */
   amoeba = &deb_amb;
   while (amoeba->nxt != amoeba) {
     amoeba = amoeba->nxt;
/* look for the amoeba of the given peak */
     if (amoeba->indpk == *ipk_no) {
         ipkx = amoeba->pkx;
         ipky = amoeba->pky;
/* add the pixel at the beginnig of the chained list */
         if ((pixnxt = (PIXEL *)malloc(sizeof(PIXEL))) == NULL)
          {
          printf ("No more memory for pixel amoeba !");
          *error = 1;
          }
          else
          { 
#ifdef DEBUG
          memset(pixnxt,0xA0A0,sizeof(PIXEL));
#endif
          pixnxt->nxt = amoeba->pix;
          amoeba->pix = pixnxt;
          pixnxt->dx = (*ipixx) - ipkx;
          pixnxt->dy = (*ipixy) - ipky;
	  }
     break;
     }
   }
}

void CHECK_PIX_AMB(int *ipk_no,int *ipixx,int *ipixy,int *check)
/*
	cheak if the given pixel (*ipixx,*ipixy) belongs to a given peak 
     number *ipk_no 
        *check = 0; not belongs
        *check = 1; belongs
	*ipk_no,  peak number;
        tableau d'adresses des amibes *iamoeba (tableau statique Fortran)
        valeurs dx par rapport aux coord du pic des pixels de l'amibe 
     *pixelx (pointeur C)
        valeurs dy par rapport aux coord du pic des pixels de l'amibe
     *pixely (pointeur C)
*/

{	
   PIXEL *pixl;
   AMB *amoeba;
   int crdx,crdy,adr,ipkx,ipky;
/* get the adress of the initial pointeur of the pixel chained liste */
   amoeba = &deb_amb;
   while (amoeba->nxt != amoeba) {
     amoeba = amoeba->nxt;
/* look for the amoeba of the given peak */
     if (amoeba->indpk == *ipk_no) {
       ipkx = (*ipixx) - (amoeba->pkx);
       ipky = (*ipixy) - (amoeba->pky);
       pixl = amoeba->pix;
       if ((pixl->dx == -1) && (pixl->dy == -1)) {
/* empty amoeba, so it is finished */
        break;           
       } else {
/* the amoeba is not empty */
        while (pixl->nxt != pixl) {
/* check the current pixel */
         if ((pixl->dx == ipkx) && (pixl->dy == ipky)) {
          *check = 1;
          break; 
          }
         pixl = pixl->nxt;
        }
       break;
       }
     }
   }
}

void REM_PIX_AMB(int *ipk_no, int *ipixx, int *ipixy, int *error)
/*
	remove the pixel (*ipixx,*ipixy) from the amoeba of peak *ipk_no
	*ipk_no,  peak number;
        tableau d'adresses des amibes *iamoeba (tableau statique Fortran)
        valeurs dx par rapport aux coord du pic des pixels de l'amibe 
     *pixelx (pointeur C)
        valeurs dy par rapport aux coord du pic des pixels de l'amibe
     *pixely (pointeur C)
*/

{	
   PIXEL *pixl,*pixprv;
   AMB *amoeba;
   int crdx,crdy,adr,ipkx,ipky;
/* get the adress of the initial pointeur of the amoeba chained list */
   amoeba = &deb_amb;
   while (amoeba->nxt != amoeba) {
     amoeba = amoeba->nxt;
/* look for the amoeba of the given peak */
     if (amoeba->indpk == *ipk_no) {
        ipkx = (*ipixx) - (amoeba->pkx);
        ipky = (*ipixy) - (amoeba->pky);
        pixl = amoeba->pix;
        if ((pixl->dx == ipkx) && (pixl->dy == ipky)) {
/* if the first pixel should be removed */
          amoeba->pix = pixl->nxt;
          free(pixl);
#ifdef DEBUG
          memset(pixl,0xA0A0,sizeof(PIXEL));
#endif
          break;
	}
        pixprv = pixl;
        pixl = pixl->nxt;        
/* if a following pixel should be removed */
        while (pixl->nxt != pixl) {
/* check the current pixel */
          if ((pixl->dx == ipkx) && (pixl->dy == ipky)) {
/* if requested, remove the pixel and free the corresponding memory */
           pixprv->nxt = pixl->nxt;
           free(pixl);
#ifdef DEBUG
           memset(pixl,0xA0A0,sizeof(PIXEL));
#endif
           goto fin;
	  }
          pixprv = pixl;
          pixl = pixl->nxt;        
	}
/* the pixel was not found in the amoeba */
      printf ("This pixel does not belong to the amoeba !");
      *error = 1;        
     }
   }
  fin: ;
}



