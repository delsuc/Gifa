/*
This file is a part of the GIFA program
This software has been developped by the NMR Group in CBS/Montpellier

     Authors :       M.A.Delsuc, and J-L Pons
                     C.B.S. Fac de Pharmacie
                     34000 Montpellier

This software cannot be used unless the user have been fully
licensed to do so form the above laboratory.
This file cannot be copied, duplicated or used in any other program,
without written permission from the authors.

*/




/*
*******************************************************************************

                    include file             

*******************************************************************************
*/
#ifndef HP
#  include <stdio.h>
#  include <math.h>
#  include <Xm/Xm.h>
#  include <Xm/Separator.h>
#  include <Xm/PushB.h>
#  include <Xm/RowColumn.h>
#  include <X11/StringDefs.h>
#  include <Xm/Separator.h>
#  include <Xm/DrawingA.h>
#  include <X11/Intrinsic.h>
#  include <Xm/Protocols.h>
#  include <Xm/AtomMgr.h>
#  include <Xm/MwmUtil.h>
#  include <Xm/Form.h>
#else
#  include <Xm/XmAll.h>
#  include <Xm/AtomMgr.h>
#  include <Xm/MwmUtil.h>
#  include <math.h>
#endif
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>

/* 
hsvrgb is used to convert hue, saturation and value of colors into red green
and blue.
*/
#ifdef SPEC1
#  include <posix/stdlib.h>
#else
#  include <stdlib.h>
#endif
#include "hsvrgb.h"
#include "X_sizes.h"


/*
*******************************************************************************

                    define   

*******************************************************************************
*/


#define TRUE 1
#define FALSE 0


/* 
marque is the character that delimites the header of the colors file.
*/ 
#define marque '*'   
/*
userfile is a file where every user may define his own color map.
*/
#define userfile ".gifacolor"
/*
defaultfile is a file where a default color map for every user may be defined.
*/
#define defaultfile "/usr/local/gifa/gifacolor"
/*
basegray is the first level of gray out of 256, for a gray scaled map.
This feature has not been tested yet.
*/
#define basegray 56
/*
definition des couleurs par defaut des curseurs de fenetre 1d et 2d.
*/
#define bg1_hue 0 
#define bg1_saturation 0
#define bg1_value 0
#define fg1_hue 0
#define fg1_saturation 1
#define fg1_value 0.8
#define bg2_hue 0
#define bg2_saturation 0 
#define bg2_value 0
#define fg2_hue 0
#define fg2_saturation 1
#define fg2_value 0.8

/*
*******************************************************************************

                    function's type definitions

*******************************************************************************
*/

/* extern char *malloc(); 
void free(); */

void initcolor();
void initcolortable();
void initdefaultcolor();
void initgraytable();
void makebwtable();


/*
*******************************************************************************

                    initcolor

*******************************************************************************

display : current display
colortable : table of pixel values used by GIFA. The name of this color table
             in X_windows.c is colorno.
colormap : either the default colormap or the colormap created by initcolor.
visual : either the default visual or the visual used by initcolor.

initcolor tries to create a table of pixel values, with a colormap that matches
the description of userfile in the user's directory, or the description of
defaultfile, or the colors of initdefaultcolor. If the display is monochrome,
initcolor calls initgraytable.     
*/


void initcolor(flag,display,colortable,colormap,visual,cursor1,cursor2)
Display *display;
int flag,colortable[];
Colormap *colormap;
Visual **visual;
Cursor cursor1,cursor2;

{
Screen *screenpt;
XColor colorcell,bg1,fg1,bg2,fg2,*colorcell2;
Status stat;
Visual *visualpt;
Colormap *lcmap;
static Colormap colormap2 = NULL;
int i,ok,ncolorprive,screenid,iblack,iwhite,flagstatic,flagcolor,
    colormapsize,nrecopie,ncmap;
unsigned short r[NColor],g[NColor],b[NColor];
unsigned long black,white,pixelvalue;


if (flag == 1) 	/* flag == 1 -> ?? */ 
  { 

		/* recup parametres globaux */

screenpt = DefaultScreenOfDisplay(display);
screenid = DefaultScreen(display);

		/* recup valeur des pixels noir et blanc dans default map color */

black = BlackPixelOfScreen(screenpt);
white = WhitePixelOfScreen(screenpt);
*visual = DefaultVisual(display,screenid);
visualpt = *visual;

		/* nombre de cellules dans la default map color */

colormapsize = CellsOfScreen(screenpt);

		/* get a default couleur map */

*colormap = DefaultColormapOfScreen(screenpt);

                /* recherche du type d'ecran : tres souvent :PseudoColor*/
                /* flagcolor = 1        couleur */
                /* flagcolor = 0        pas de couleur */
                /* flagcolor =  -1      default -> ? */

                /* flagstatic -> ???? */

switch (visualpt->class) {
   case DirectColor:
        flagcolor = 1;
        flagstatic = 0;
        break;
   case TrueColor:
        flagcolor = 1;
        flagstatic = 1;
        break;
   case PseudoColor:
        flagcolor = 1;
        flagstatic = 0;
        break;
   case StaticColor:
        flagcolor = 1;
        flagstatic = 1;
        break;
   case GrayScale:
        flagcolor = 0;
        flagstatic = 0;
        break;
   case StaticGray:
        flagcolor = 0;
        flagstatic = 1;
        break;
   default:
        flagcolor = -1;
        flagstatic = -1;
      }


		/* cas du noir et blanc */

if (colormapsize == 2) flagcolor = -1; /* cas du noir et blanc */

		/* Initialisation de la table des couleurs */
        /* ne general appel de initcolortable car type PseudoColor */


switch (flagcolor) {
   case 1:
        initcolortable(r,g,b,&bg1,&fg1,&bg2,&fg2);
        break;
   case 0:
        initgraytable(r,g,b,&bg1,&fg1,&bg2,&fg2);
        break;
   case -1:
        makebwtable(display,colortable,&bg1,&fg1,&bg2,&fg2,black,white,cursor1,cursor2);
        break;   
}      



if ((flagstatic == 0) && (colormapsize >= NColor))
{


                /* Test si possibilite allocation en read-only seulement */
                /* et de partager les couleurs de la colormap deja allouee */


ncolorprive = 0;
i = 0;
while ((i<NColor)&&(ncolorprive==0))	
   {
   colorcell.red = r[i];
   colorcell.green = g[i];
   colorcell.blue = b[i];

	
                /* alloc des cellules consultables dans une colormap */
                /* une cell consultable est partagee entre plusieurs appli*/
                /* si la couleur est deja defini elle ne sera pas allouee */
                /* colorcell.pixel recoit l'indice de la cellule allouee*/
                /* si FALSE -> pas d'indice */


   stat = XAllocColor(display,*colormap,&colorcell);
   if (stat == FALSE) {
	ncolorprive++;
	}
   i++;
   }


		/* change the color of a cursor */

   stat = XRecolorCursor(display,cursor1,&fg1,&bg1);
   if (stat == FALSE) ncolorprive++;
   stat = XRecolorCursor(display,cursor2,&fg2,&bg2);
   if (stat == FALSE) ncolorprive++;

		/* stockage de l'ancienne colormap */

   colormap2 = *colormap;

                /* Creation d'une colormap avec cellules modifiables */
                /* l'id de la colormap par defaut est *colormap      */


if (ncolorprive != 0)
   {


		/* creation colormap */
                /*  AllocNone car allocation faite apres */


   visualpt = DefaultVisual(display,screenid);
   *colormap = XCreateColormap(display,RootWindow(display,screenid),
               visualpt,AllocNone);


                /* initialisation du tableau colorcell2[XColor] */
                /* contenant les couleurs de chaques cells      */
                /* Lecture des trois couleurs DoRed,DoGreen,DoBlue*/


   nrecopie = colormapsize-NColor-6;		/* ????? */
   colorcell2 = (XColor *) calloc(sizeof(XColor),nrecopie);
   for (i=0;i<nrecopie;i++)
      {
      colorcell2[i].pixel = i;
      colorcell2[i].flags = DoRed | DoGreen | DoBlue;
      XQueryColor(display,colormap2,&colorcell2[i]);
      }

                /* allocation cell consultable dans nouvelle colormap */
                /* colorcell2[i].pixel recoit l'indice de la cell allouee */
                /* si la couleur existe deja l'indice est redirige vers la */
                /* cell de la couleur ou celle de couleur approchante */

   for (i=0;i<nrecopie;i++)
      {
      stat = XAllocColor(display,*colormap,&colorcell2[i]);
      if (colorcell2[i].pixel != i)
		/*indice redirige sur indice de la couleur deja existante*/
         {
		/* alloue cellules modifiables */
         stat = XAllocColorCells(display,*colormap,0,0,0,
                &pixelvalue,1);
         colorcell2[i].pixel = pixelvalue;
         XStoreColor(display,*colormap,&colorcell2[i]);
         } 
      }
   free(colorcell2);
   colorcell.pixel = nrecopie;
   for (i=0;i<NColor;i++)
      {
      if (colorcell.pixel == (black - 1))
	 {
         colorcell.red = 0;
         colorcell.green = 0;
         colorcell.blue = 0;
         stat = XAllocColor(display,*colormap,&colorcell);
         } 
      if (colorcell.pixel == (white - 1))
	 {
         colorcell.red = -1;
         colorcell.green = -1;
         colorcell.blue = -1;
         stat = XAllocColor(display,*colormap,&colorcell);
         } 
      colorcell.red = r[i];
      colorcell.green = g[i];
      colorcell.blue = b[i];
      stat = XAllocColor(display,*colormap,&colorcell);
      if (stat != FALSE) colortable[i] = colorcell.pixel;
      else colortable[i] = black;
      }

	                  /* couleur curseur */

   stat = XRecolorCursor(display,cursor1,&fg1,&bg1);
   if (stat == FALSE)
      {
      bg1.red = 0;
      bg1.green = 0;
      bg1.blue = 0;
      fg1.red = -1;
      fg1.green = -1;
      fg1.blue = -1;
    stat = XRecolorCursor(display,cursor1,&fg1,&bg1);
      }
   stat = XRecolorCursor(display,cursor2,&fg2,&bg2);
   if (stat == FALSE)
      {
      bg2.red = 0;
      bg2.green = 0;
      bg2.blue = 0;
      fg2.red = -1;
      fg2.green = -1;
      fg2.blue = -1;
    stat = XRecolorCursor(display,cursor2,&fg2,&bg2);
     } 

                /* installation de la couleur map: non conseille */

   XInstallColormap(display,*colormap);
   }
else
   {
   for (i=0;i<NColor;i++)
      {
      colorcell.red = r[i];
      colorcell.green = g[i];
      colorcell.blue = b[i];
      stat = XAllocColor(display,*colormap,&colorcell);
      if (stat != FALSE) colortable[i] = colorcell.pixel;
      else colortable[i] = black;
      }
   stat = XRecolorCursor(display,cursor1,&fg1,&bg1);
   if (stat == FALSE)
      {
      bg1.red = 0;
      bg1.green = 0;
      bg1.blue = 0;
      fg1.red = -1;
      fg1.green = -1;
      fg1.blue = -1;
    stat = XRecolorCursor(display,cursor1,&fg1,&bg1);
      }
   stat = XRecolorCursor(display,cursor2,&fg2,&bg2);
   if (stat == FALSE)
      {
      bg2.red = 0;
      bg2.green = 0;
      bg2.blue = 0;
      fg2.red = -1;
      fg2.green = -1;
      fg2.blue = -1;
    stat = XRecolorCursor(display,cursor2,&fg2,&bg2);
      }
   }
}
else
{
if (flagcolor != -1)
{
for(i=0;i<NColor;i++)
   {      
   colorcell.red = r[i];
   colorcell.green = g[i];
   colorcell.blue = b[i];
   stat = XAllocColor(display,*colormap,&colorcell);
   if (stat != FALSE) colortable[i] = colorcell.pixel;
   else colortable[i] = black;
   }
   stat = XRecolorCursor(display,cursor1,&fg1,&bg1);
   if (stat == FALSE)
      {
      bg1.red = 0;
      bg1.green = 0;
      bg1.blue = 0;
      fg1.red = -1;
      fg1.green = -1;
      fg1.blue = -1;
    stat = XRecolorCursor(display,cursor1,&fg1,&bg1);
      }
   stat = XRecolorCursor(display,cursor2,&fg2,&bg2);
   if (stat == FALSE)
      {
      bg2.red = 0;
      bg2.green = 0;
      bg2.blue = 0;
      fg2.red = -1;
      fg2.green = -1;
      fg2.blue = -1;
    stat = XRecolorCursor(display,cursor2,&fg2,&bg2);
      }
}
}
}
else
{
if (*colormap != colormap2) 
   {
   stat = XFreeColormap(display,*colormap);
   if (colormap2 != NULL) XInstallColormap(display,colormap2);
   XFlush(display);
   }
}
}

/*
*******************************************************************************

                    initcolortable

*******************************************************************************

r,g,b : values of red, green and blue for each color in GIFA.

initcolortable reads the userfile, if it does not exist the defaultfile, and 
after that calls initdefaultcolor.

*/



void initcolortable(r,g,b,bg1,fg1,bg2,fg2)
unsigned short r[NColor],g[NColor],b[NColor];
XColor *bg1,*fg1,*bg2,*fg2;


{
unsigned short rr,gg,bb;
int i,ok,filencolor,iblack,iwhite;
FILE *filecolor;
char c,filename[80];
float h,s,v;

ok = TRUE;
strcpy(filename,getenv("HOME"));
strcat(filename,"/");
strcat(filename,userfile);
filecolor = fopen(filename,"r");
if (filecolor == FALSE)
   {
   filecolor = fopen(defaultfile,"r");
   if (filecolor == NULL) ok = FALSE;
   while (ok && (ok = fscanf(filecolor,"%c",&c)) && (c != marque));
   while (ok && (ok = fscanf(filecolor,"%c",&c)) && (c != marque));
   if (ok != FALSE) ok = fscanf(filecolor,"%u",&filencolor);
   if (ok != FALSE)
      if (filencolor == NColor-1) ok = TRUE;
      else ok = FALSE;
   }
else
   {
   while (ok && (ok = fscanf(filecolor,"%c",&c)) && (c != marque));
   while (ok && (ok = fscanf(filecolor,"%c",&c)) && (c != marque));
   if (ok != FALSE) ok = fscanf(filecolor,"%u",&filencolor);
   if (ok != FALSE) 
      if (filencolor == NColor-1) ok = TRUE;
      else ok = FALSE;
   if (ok == FALSE)
      {
      filecolor = fopen(defaultfile,"r");
   if (filecolor == NULL) ok = FALSE;
      while (ok && (ok = fscanf(filecolor,"%c",&c)) && (c != marque));
      while (ok && (ok = fscanf(filecolor,"%c",&c)) && (c != marque));
      if (ok != FALSE) ok = fscanf(filecolor,"%u",&filencolor);
      if (ok != FALSE) 
         if (filencolor == NColor-1) ok = TRUE;
         else ok = FALSE;
      }
   }


if (ok != FALSE)
   {
   ok = fscanf(filecolor,"%f",&h);
   ok = fscanf(filecolor,"%f",&s);
   ok = fscanf(filecolor,"%f",&v);
   ok = hsvrgb(h,s,v,&rr,&gg,&bb);
   if (ok != FALSE)
      {
      bg1->red = rr;
      bg1->green = gg;
      bg1->blue = bb;
      }
   else
      {
      bg1->red = 0;
      bg1->green = 0;
      bg1->blue = 0;
      }
   ok = fscanf(filecolor,"%f",&h);
   ok = fscanf(filecolor,"%f",&s);
   ok = fscanf(filecolor,"%f",&v);
   ok = hsvrgb(h,s,v,&rr,&gg,&bb);
   if (ok != FALSE)
      {
      fg1->red = rr;
      fg1->green = gg;
      fg1->blue = bb;
      }
   else
      {
      fg1->red = -1;
      fg1->green = -1;
      fg1->blue = -1;
      }
   ok = fscanf(filecolor,"%f",&h);
   ok = fscanf(filecolor,"%f",&s);
   ok = fscanf(filecolor,"%f",&v);
   ok = hsvrgb(h,s,v,&rr,&gg,&bb);
   if (ok != FALSE)
      {
      bg2->red = rr;
      bg2->green = gg;
      bg2->blue = bb;
      }
   else
      {
      bg2->red = 0;
      bg2->green = 0;
      bg2->blue = 0;
      }
   ok = fscanf(filecolor,"%f",&h);
   ok = fscanf(filecolor,"%f",&s);
   ok = fscanf(filecolor,"%f",&v);
   ok = hsvrgb(h,s,v,&rr,&gg,&bb);
   if (ok != FALSE)
      {
      fg2->red = rr;
      fg2->green = gg;
      fg2->blue = bb;
      }
   else
      {
      fg2->red = -1;
      fg2->green = -1;
      fg2->blue = -1;
      }
   for (i=0;((ok != FALSE) && (i<NColor-1));i++)
      {
      ok = fscanf(filecolor,"%f",&h);
      ok = fscanf(filecolor,"%f",&s);
      ok = fscanf(filecolor,"%f",&v);
      ok = hsvrgb(h,s,v,&rr,&gg,&bb);
      if (ok != FALSE)
	 {
         r[i] = rr;
         g[i] = gg;
         b[i] = bb;
         }
      else
         {
         r[i] = 0;
         g[i] = 0;
         b[i] = 0;
         } 
      }
   iblack = (NColor-1)/2 - 1;
   iwhite = NColor - 1;
   r[iblack] = 0;
   g[iblack] = 0;
   b[iblack] = 0;
   r[iwhite] = -1;
   g[iwhite] = -1;
   b[iwhite] = -1;
   }

if (filecolor != NULL) fclose(filecolor);

if (ok == FALSE) initdefaultcolor(r,g,b,bg1,fg1,bg2,fg2);


}


/*
*******************************************************************************

                    initdefaultcolor

*******************************************************************************

r,g,b : values of red, green and blue for each color in GIFA.

initdefaultcolor returns the values of the red, green and blue parts of each
color in GIFA, when neither userfile nor defaultfile exists.

*/



void initdefaultcolor(r,g,b,bg1,fg1,bg2,fg2)
unsigned short r[NColor],g[NColor],b[NColor];
XColor *bg1,*fg1,*bg2,*fg2;

{
int i,ok,iblack,iwhite;
float sat,hue,val;
unsigned short rr,gg,bb;


sat = 1;

hue = 240;
val = 1;

for (i=(NColor-1)/2;i<NColor-1;i++)
   {
   hue = hue - 120 / ((float) (NColor - 1) / 2);
   ok = hsvrgb(hue,sat,val,r+i,g+i,b+i);
   if (ok == FALSE) 
      {
      r[i] = 0;
      g[i] = 0;
      b[i] = 0;
      }
   }

hue = 60;
val = 1;
for (i=0;i<((NColor-1)/2-1);i++)
   {
   hue = hue -120 / ((float) (NColor - 1) / 2);
   if (hue<0) hue = hue + 360;
   ok = hsvrgb(hue,sat,val,r+i,g+i,b+i);
   if (ok == FALSE) 
      {
      r[i] = 0;
      g[i] = 0;
      b[i] = 0;
      }
   }
   iblack = (NColor-1)/2 - 1;
   iwhite = NColor - 1;
   r[iblack] = 0;
   g[iblack] = 0;
   b[iblack] = 0;
   r[iwhite] = -1;
   g[iwhite] = -1;
   b[iwhite] = -1;
   
   hue = bg1_hue;
   sat = bg1_saturation;
   val = bg1_value;
   ok = hsvrgb(hue,sat,val,&rr,&gg,&bb);
   if (ok != FALSE)
      {
      bg1->red = rr;
      bg1->green = gg;
      bg1->blue = bb;
      }
   else
      {
      bg1->red = 0;
      bg1->green = 0;
      bg1->blue = 0;
      }
   hue = fg1_hue;
   sat = fg1_saturation;
   val = fg1_value;
   ok = hsvrgb(hue,sat,val,&rr,&gg,&bb);
   if (ok != FALSE)
      {
      fg1->red = rr;
      fg1->green = gg;
      fg1->blue = bb;
      }
   else
      {
      fg1->red = 0;
      fg1->green = 0;
      fg1->blue = 0;
      }
   hue = bg2_hue;
   sat = bg2_saturation;
   val = bg2_value;
   ok = hsvrgb(hue,sat,val,&rr,&gg,&bb);
   if (ok != FALSE)
      {
      bg2->red = rr;
      bg2->green = gg;
      bg2->blue = bb;
      }
   else
      {
      bg2->red = 0;
      bg2->green = 0;
      bg2->blue = 0;
      }
   hue = fg2_hue;
   sat = fg2_saturation;
   val = fg2_value;
   ok = hsvrgb(hue,sat,val,&rr,&gg,&bb);
   if (ok != FALSE)
      {
      fg2->red = rr;
      fg2->green = gg;
      fg2->blue = bb;
      }
   else
      {
      fg2->red = 0;
      fg2->green = 0;
      fg2->blue = 0;
      }
   
}

/*
*******************************************************************************
 
                    initgraytable

*******************************************************************************

r,g,b : values of red, green and blue for each color in GIFA.

initgraytable returns the values of the red, green and blue parts of each
gray  in GIFA, when it runs on a monochrome display.

*/



void initgraytable(r,g,b,bg1,fg1,bg2,fg2)
unsigned short r[NColor],g[NColor],b[NColor];
XColor *bg1,*fg1,*bg2,*fg2;

{
int i,ok,iblack,iwhite;


for (i=0;i<NColor-1;i++)
   {
   r[i] = ((basegray + i * (255 - basegray) / (NColor-2))<<8);
   g[i] = 0;
   b[i] = 0;
   }      


   iblack = (NColor-1)/2 - 1;
   iwhite = NColor - 1;
   r[iblack] = 0;
   r[iwhite] = -1;
   bg1->red = 0;
   bg2->red = 0;
   fg1->red = -1;
   fg2->red = -1;

}


/*
*******************************************************************************
 
                    makebwtable

*******************************************************************************

r,g,b : values of red, green and blue for each color in GIFA.

makebwtable returns the values of the red, green and blue parts of each
gray  in GIFA, when it runs on a black and white display.

*/



void makebwtable(display,colortable,bg1,fg1,bg2,fg2,black,white,cursor1,cursor2)
int colortable[];
XColor *bg1,*fg1,*bg2,*fg2;
unsigned long black,white;
Cursor cursor1,cursor2;
Display *display;
{
int i,ok,iblack,iwhite;


iblack = (NColor-1)/2 - 1;
iwhite = NColor - 1;

for (i=0;i<=iblack;i++)
   {
   colortable[i] = black;
   }      
for (i=iblack+1;i<NColor-1;i++)
   {
   colortable[i] = white;
   }      

colortable[iblack] = black;
colortable[iwhite] = white;

   bg1->red = 0;
   bg1->green = 0;
   bg1->blue = 0;
   bg2->red = 0;
   bg2->green = 0;
   bg2->blue = 0;
   fg1->red = -1;
   fg1->green = -1;
   fg1->blue = -1;
   fg2->red = -1;
   fg2->green = -1;
   fg2->blue = -1;


   ok = XRecolorCursor(display,cursor1,fg1,bg1);
   ok = XRecolorCursor(display,cursor2,fg2,bg2);

}








