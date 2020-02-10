/* This small program will generate gifacolor files for GIFA
  Entries are :
start comment (usually *)
comments
end comments (ibid)
number_of_color-1
backgroud of cursor 1
foreground of cursor 1
backgroud of cursor 2
foreground of cursor 2
then number_of_color entries.

each color entry is hue (0-360) saturation (0.0-1.0) value (0.0-1.0)

*/

#include <stdio.h>
#include "/usr/local/gifa/fortran/hsvrgb.h"

#define TRUE 1
#define FALSE 0


#define nbcolor 65
#define nomfichier ".gifacolor"
#define marque '*'





main ()


{ 
   float  h,v,s,c;
   int ok,n,i;
   unsigned short r,g,b;
   FILE *fichier;



fichier = fopen(nomfichier,"w");
fprintf(fichier,"*\n");
fprintf(fichier,"Color table for GIFA\n");
fprintf(fichier,"*\n");   
fprintf(fichier,"%u\n",nbcolor-1);

/* this one will generate B&W color table */

/* bg1 */
h = 0;
s = 0;
v = 0;
fprintf(fichier,"%5.1f %6.4f %6.4f\n",h,s,v);
/* fg1 */
h = 0;
s = 0;
v = 0.8;
fprintf(fichier,"%5.1f %6.4f %6.4f\n",h,s,v);
/* bg2 */
h = 0;
s = 0;
v = 0;
fprintf(fichier,"%5.1f %6.4f %6.4f\n",h,s,v);
/* fg2 */
h = 0;
s = 0;
v = 0.8;
fprintf(fichier,"%5.1f %6.4f %6.4f\n",h,s,v);

 
/* for positive values */
s = 0;
h = 0;
v = 1;
for (i=0;i<((nbcolor-1)/2-1);i++)
   {
fprintf(fichier,"%5.1f %6.4f %6.4f\n",h,s,v);
     v = v - 2.0/(nbcolor-1);
   }

/* for 0 level (usually black = 0 0 0) */
s = 0;
h = 0;
v = 0;
fprintf(fichier,"%5.1f %6.4f %6.4f\n",h,s,v); 

/* then for negative values */
s = 0;
h = 0;
v = 0;
for (i=0;i<((nbcolor-1)/2-1);i++)
   {
     v = v + 2.0/(nbcolor-1);
fprintf(fichier,"%5.1f %6.4f %6.4f\n",h,s,v);
   }

fclose(fichier);
printf("gifacolor : done\n");

  
}

