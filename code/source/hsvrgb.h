
/*
     This file (hsvrgb.h) is a part of the GIFA program.

C
C     This software has been developed by the NMR Group at GIF/Yvette.
C
C     Authors :       A.Rouh
C                     NMR Laboratory,
C                     ICSN - CNRS
C                     91190 GIF/YVETTE   FRANCE
C
C     This software cannot be used unless the user has been fully 
C licensed to do so form the above laboratory.
C     This file cannot be copied, duplicated or used in any other program,
C without written permission from the above authors.
C

*******************************************************************************

                    hsvrgb

*******************************************************************************

h,s,v : hue, saturation and value repesentation of a color.
r,g,b : red, green and blue representation of the same color (I hope so ...).

hsvrg converts an hsv description of a color into its rgb definition.
*/


int hsvrgb(h,s,v,r,g,b)
float  h,s,v;
unsigned short *r,*g,*b;

{

int ok,i;
float rr,rg,rb,f,m,n,k;




ok = 1;

if ((v<0) || (v>1) || (s<0) || (s>1)) ok = 0;
if (ok==1)
{
if (s==0) 
   {
   rr = v;
   rg = v;
   rb = v;
   }
else
   {
   while (h>360) h = h-360;
   while (h<0) h = h+360;
   h = h/60;
   i = (int) h; 
   f = h-(float)i;
   m = v*(1-s);
   n = v*(1-s*f);
   k = v*(1-s*(1-f));
   switch (i)
      {
      case 0 :
         {
         rr = v;
         rg = k;
         rb = m;
         break;
         };
      case 1 :
         {
         rr = n;
         rg = v;
         rb = m;
         break;
         };
      case 2 :
         {
         rr = m;
         rg = v;
         rb = k;
         break;
         };
      case 3 :
         {
         rr = m;
         rg = n;
         rb = v;
         break;
         };
      case 4 :
         {
         rr = k;
         rg = m;
         rb = v;
         break;
         }
      case 5 :
         {
         rr = v;
         rg = m;
         rb = n;
         break;
         };
      default :
         {
         rr = 0;
         rg = 0;
         rb = 0;
         };
      };
   };

*r = (rr*255);
*r = (*r<<8);
*g = (rg*255);
*g = (*g<<8);
*b = (rb*255);
*b = (*b<<8);
}

return(ok);
}

