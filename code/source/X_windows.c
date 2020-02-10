/*
This file is a part of the GIFA program
This software has been developped by the NMR Group in CBS/Montpellier

     Authors :       M.A.Delsuc, and J-L Pons A.Rouh
                     C.B.S. Fac de Pharmacie
                     34000 Montpellier

This software cannot be used unless the user have been fully
licensed to do so form the above laboratory.
This file cannot be copied, duplicated or used in any other program,
without written permission from the authors.

*/


/*-------------------------------------------------------------
**    include
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
#  include <Xm/MwmUtil.h>
#else
#  include <Xm/XmAll.h>
#  include <Xm/AtomMgr.h>
#  include <Xm/MwmUtil.h>
#  include <math.h>
#  include <Xm/MwmUtil.h>
#endif

#include "sizebasec.h"
# include"logo.inc"


/*-------------------------------------------------------------
*/
#ifdef VMS
#include <decw$include/Xlib.h>
#include <decw$include/Xutil.h>
#include <decw$include/cursorfont.h>
#include <descrip.h>
#else
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>
#endif
#include "X_sizes.h"
#include "X_basic.h"
#include "X_windef.h"


/*-------------------------------------------------------------
**      forwarded functions
*/
void refresh_widg_1d();
void arrondir();
void calcule_tick();
void create_label();
void resize_pixmap();
void refresh_widg();
void del_wait_cursor();
void nothingCB();
void input_iconCB();
void add_callback_zoom();
void add_callback_point();
void input_picCB();
void QuitCB_disp1();
void del_copy();
void QuitCB_disp2();
void QuitCB_cdisp2();
void QuitCB_disp3();
void QuitCB_view();
void copy_pixmap();
void refresh_copy();
void resize_copy();
void freeze_comm();
void createfreeze();
void which_window();
void create_pixmap_icon();
void expose_icon();
void resize_icon();
void creer_image();
void TRACE_CADRE_ICON();
void init_cadre_icon();
void init_input_iconCB();
void end_input_iconCB();
void map_logo();
void SET_AXIS();
void draw_tick();
void makeGC();
void load_font();
void WIN_UPDATE();
void WIN_ERASE();


/*-------------------------------------------------------------
*
*/
/* static variables */

typedef struct {
        Widget icon_spectrum;
        int cadre_x, cadre_y, cadre_haut, cadre_larg;
	int xmouse,ymouse;
	int cadre_set;
	int mv_cadre;
        GC icon_GC;
	GC cadre_GC;
        Pixmap icon_pixmap;
        int haut, larg;
	int icon_set;
} Icone_data;

Icone_data *icone_data;
int w2d_open;


/* structure permetant la creation des pixmaps pour la 
commande Freeze */
typedef struct {
	Pixmap	pixmap;
	GC	gc;
	Dimension  largeur,hauteur;
} donnees_pixmap;

	Display *dpy;      /* display id */
        Screen *screen;      /* screen id */

/*  the arrays xx[NWindow] define a set of parameters used
    for each opend window.
    In an ideal world, there should be gathered in a structure holding
    all the parameters, and a structure should be mallocated and freed
    on demand. But we do not live in an ideal world (do we ?)
*/
        int win[NWindow];      /* array of free window ids. */
	Zoom_cadre cadre_zoom[NWindow];
	viseur_att viewfinder[NWindow];
        int pixmap[NWindow];      /* window pixmap */
        Window window[NWindow];      /* window id */
	Widget widget[NWindow];      /* Widget id */
        GC gc[NWindow];      /* gc id */
        int bg[NWindow];     /* back ground */
        int windowx[NWindow],windowy[NWindow];      /* window origin */
        int windoww[NWindow],windowh[NWindow];      /* window width */
	short windowd[NWindow];    /* code dim see win_open_2d */
	short windowt[NWindow];    /* code type (view / regular) */

	
	int attr_mask;      /* attributes mask */
        int depth;            /* number of planes */
        int func,status;      /* synchronous behavior */
        Colormap color_map;
        int colorno[NColor];
        int numcolors,black,white;
        Cursor cursor1,cursor2,cursor_wait;      /* Default cursor struct. */
        float     cmconvx;            /* conv cm. to pixels val */
        float     cmconvy;            /* conv cm. to pixels val */

        Visual *visual;   /* visual type */
        XSetWindowAttributes xswda;     /* window attributes */
        XGCValues xgcvl;   /* gc values */
        XSizeHints xszhn;   /* hints */
        XEvent event;    /* input event */
        XColor screen_color;
        XImage *image;             /* 2d image struct. */

        XFontStruct *font_info;    /* id of the font used */
        char *pldat; /* Image data,
                                     simax x simax array of pixel values. */
        short *pldat16; /* Image data, for 16 bit deep screens */
        char *pldat24; /* Image data, for 24 bit deep screens */
        int *pldat32; /* Image data, for 32 bit deep screens */

        char *font_name="fixed";    
/*
        char *font_name="-adobe-new century schoolbook-medium-i-normal--25-180-100-100-p-136-iso8859-1";
	char *font_name="nceni18";
*/


/* numerotation des fenetres freeze: 
cpt_freeze[0] -> compteur pour la numerotation des Fenetres 1D Display
cpt_freeze[1] -> compteur pour la numerotation des Fenetres 2D Display
cpt_freeze[2] -> compteur pour la numerotation des Fenetres 2D Contour Plot
cpt_freeze[3] -> compteur pour la numerotation des Fenetres 3D Display
*/

int cpt_freeze[4]; 


/*-------------------------------------------------------------
*     Variables externes
*/

extern int sortie_boucle_phase;
extern int versionx;
extern int bloquant;
extern Widget window_mere ;
extern int appel_phase;
extern Widget d3box_set;
extern Widget zoom_set;
extern int zoom_wind_id;

#define max(A,B)  ((A) > (B) ? (A) : (B) )
#define min(A,B)  ((A) > (B) ? (B) : (A) )


/*-------------------------------------------------------------
**   
*/

	/*  doit etre appelee a partir de motif_loop au tout debut */


int win_init()
/*
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Subroutine to initialize X Windows.                                C
C                                                                        C
C     int win_init()                                                     C
C                                                                        C
C     returns 0 if Ok, -1 if not succeed                                 C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
*/
{

	int i,screenid;           		/* Loop counter. */
	char *dpname;

	black = (NColor-1)/2 -1;            	/* Index of black color */
	white = NColor-1;                   	/* Index of white color */

      for (i=1;i<NWindow;i++){
      		  win[i] = 0;         		/* Reset the global window flag. */
      }


      if (versionx == 0){
                 printf("Gifa cannot open the display on X server\n");
      		return(-1);
      }

			/* set dpy,depth,cursor, screen */

	dpy = XtDisplay(window_mere);
	screen = DefaultScreenOfDisplay(dpy);
	screenid = DefaultScreen(dpy);
	sortie_boucle_phase = 0;
	w2d_open = 0;

        cmconvx = 10 * DisplayWidth(dpy,screenid) / DisplayWidthMM(dpy,screenid);
        cmconvy = 10 * DisplayHeight(dpy,screenid) / DisplayHeightMM(dpy,screenid);

                        /* Get the depth and default visual */

        depth = DefaultDepthOfScreen(screen);

        load_font();

        cursor1 = XCreateFontCursor(dpy,XC_left_ptr);
        cursor2 = XCreateFontCursor(dpy,XC_crosshair);
	cursor_wait =  XCreateFontCursor(dpy,XC_watch);

	init_color();
         

			/* initialisation a 0 des compteurs pour la 
			numerotation des fenetres Freeze */

	for(i=0;i<4;i++){
		cpt_freeze[i] = 1;
	}

return(0);
}

int init_color()
{
/*
initcolor tries to create a table of pixel values named colorno, with a color
map named color_map and a visual named visual. The colors may be discribed in
file .gifacolor where GIFA is started or in the directory /usr/local. If none
of these files exists initcolor uses a default definition, which is described
in uis_windows.for. If initcolor does not find all GIFA colors in the default
color map, it may try to create it's own colormap.
*/

      initcolor(1,dpy,colorno,&color_map,&visual,cursor1,cursor2);



		/*    This information is used by all of the routines for setting*/
		/*     window attributes.*/

      attr_mask = (CWEventMask | CWBackPixel);
      xswda.event_mask=ExposureMask;
      xswda.background_pixel = colorno[black];

      return(0);
}

/*-------------------------------------------------------------
**    
*/

void load_font()
/*
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C and  subroutine load_font() are used for internal use
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
*/
{
   if ((font_info = XLoadQueryFont(dpy,font_name)) == NULL)
   {
      printf( "Gifa cannot find the font %s on X server \n",font_name);
      return;
   }
}



/*-------------------------------------------------------------
**    
*/

int win_nextid(dimension)
int   dimension; /*   Should be 1 or 2, depends on routine. */
/*
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     function 2 win_nextid(dimension)                                    C
C     Function to get a unique window_id.                                 C
C                                                                         C
C     dimension = the dimension of window to be flagged.                  C
C                                                                         C
C CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
*/
{
      int      i;           /* Counter. */

      for (i=1;(win[i]!=0);i++) {  if (i >= NWindow)  return 0; }
      win[i] = dimension;
      return i;

}

/*-------------------------------------------------------------
**    
*/

void makeGC(vd_id,color,bgcol)
int *vd_id,color,bgcol;
/*
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                         C
C      subroutine makegc(vd_id)
C        makeGC builds and store the GC for window vd_id                  C
C                                                                         C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
*/
{
				/* create GC first */
      xgcvl.foreground = colorno[color];
      xgcvl.background = colorno[bgcol];

      gc[*vd_id] = XCreateGC(dpy,window[*vd_id],
         (GCForeground | GCBackground),&xgcvl);

       bg[*vd_id] = bgcol;
/* ECIR*/
/*
printf("makeGC : dpy=%d, vd_id=%d, gc=%d\n",dpy,vd_id,gc[*vd_id]);
*/

				/* find and load the font */

      XSetFont(dpy, gc[*vd_id], font_info->fid);
}


void WIN_ERASE(vd_id)         /* Window ID to erase. */
int *vd_id;
/*
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C   WIN_ERASE (vd_id)                                                    C
C                                                                        C
C     Clears the window.                                                 C
C                                                                        C
C       vd_id      = Address of virtual display ID                       C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
*/
{


     GC  tempgc;         /* Temporary Graphics Context to store */
                                /*  fore- and background colors to clear */
                                /*  the window with. */


/*    Create a graphics context of uniform background and */
/*    foreground color with which to write into the window...*/

      xgcvl.foreground = colorno[bg[*vd_id]];
      xgcvl.background = colorno[bg[*vd_id]];

      tempgc = XCreateGC(dpy,window[*vd_id],
                        (GCForeground | GCBackground),&xgcvl);



/*    Fill the window's Pixmap with this color*/

      XFillRectangle(dpy,pixmap[*vd_id],
                     tempgc,0,0,windoww[*vd_id],windowh[*vd_id]);



/*    Clear the physical window...*/

      XClearWindow(dpy,window[*vd_id]);

}

void WIN_REFRESH()
/*
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     WIN_REFRESH ()                                                     C
C                                                                       C
C     Redraws all of the mapped windows.                                 C
C                                                                        C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
*/
{
      int i;       /* Loop counter. */

      if (versionx != 0)
          for ( i=1; i < NWindow; i++)
            { if (win[i]!=0)

                  WIN_UPDATE(&i); }
         
}


void WIN_UPDATE(vd_id)
int *vd_id;   /* Window ID to update. */
/*
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     win_update (vd_id)                                                 C
C                                                                        C
C     vd_id = Window ID to update                                        C
C                                                                        C
C     Recopies Pixmap(vd_id) into Window(vd_id)                          C
C        and Flush the X requests      2eme                              C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
*/
{
      int  mx,my;    /* Width and height of window in pixels. */


	/* test d'existance du cadre de zoom */

	OFF_cadre_zoom(vd_id);

        /* reaffichage du point pivot si phasing */

        if(sortie_boucle_phase == -1 && appel_phase == 0){
	   if(windowd[*vd_id]==1){
                set_pivot_phase(2); 
	   }
        }


/*    Get the width and height of the window in pixels. */
          mx = windoww[*vd_id];
          my = windowh[*vd_id];

/*    Copy the pixmap to the window...*/

          XCopyArea(dpy,pixmap[*vd_id],window[*vd_id],
                             gc[*vd_id],0,0,mx,my,0,0);

          XFlush(dpy);
}


        /******************CALLBACK*******************/


/*-------------------------------------------------------------
**    	refresh_widg_1d 
*/
void refresh_widg_1d(w, client_data, call_data)
Widget          w;              /*  widget id           */
XtPointer       client_data;    /*  data from application   */
XtPointer 	call_data;	
{
	WIN_REFRESH();

}

/*-------------------------------------------------------------
**      refresh_widg
*/
void refresh_widg(w, client_data, call_data)
Widget          w;              /*  widget id           */
XtPointer       client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{

	WIN_REFRESH();


}

/*-------------------------------------------------------------
**      
*/
void nothingCB(w, client_data, event)
Widget          w;              /*  widget id           */
XtPointer       client_data;    /*  data from application   */
XtPointer      event ;      /*  data from widget class  */

{
}

/*-------------------------------------------------------------
**      input_picCB
*/
void input_picCB(w, client_data, event)
Widget          w;              /*  widget id           */
XtPointer       client_data;    /*  data from application   */
XtPointer      event ;      /*  data from widget class  */

{
/*
printf("input_pic\n");
*/
}



/*-------------------------------------------------------------
**	QuitCB_disp1		call back for closing 1D window
*/
void QuitCB_disp1(w, vd_id, call_data)
Widget          w;              /*  widget id           */
XtPointer       vd_id;    /*  data from application   */
XtPointer       call_data ;      

{

	if(sortie_boucle_phase == 0) /* impossible dans le mode phase */
		pre_execute("disp1d 0");

}

/*-------------------------------------------------------------
**	QuitCB_disp2		:call back for closing 2D window
*/
void QuitCB_disp2(w, vd_id, call_data)
Widget          w;              /*  widget id           */
XtPointer       vd_id;    /*  data from application   */
XtPointer       call_data ;      

{

	pre_execute("disp2d 0");

}

/*-------------------------------------------------------------
**      QuitCB_cdisp2              :call back for closing 2D contour window
*/
void QuitCB_cdisp2(w, vd_id, call_data)
Widget          w;              /*  widget id           */
XtPointer       vd_id;    /*  data from application   */
XtPointer       call_data ;

{

	pre_execute("cdisp2d 0");

}

/*-------------------------------------------------------------
**      QuitCB_disp3              :call back for closing 3D window
*/
void QuitCB_disp3(w, vd_id, call_data)
Widget          w;              /*  widget id           */
XtPointer       vd_id;    /*  data from application   */
XtPointer       call_data ;

{

	if(d3box_set != 0){
		XtDestroyWidget(d3box_set);
		d3box_set = 0;
	}
        pre_execute("disp3d 0");

}
/*-------------------------------------------------------------
**      QuitCB_view              call back for closing view
*/
void QuitCB_view(w, vd_id, call_data)
Widget          w;              /*  widget id           */
int             vd_id;    /*  data from application   */
XtPointer       call_data ;

{
        int err=0;
        int vl;
        char view_name[MAX_CHAR], cmd[MAX_CHAR]="close_view \0";

        WHICH_VIEW(&vd_id, view_name, &vl);

        strncat(cmd, view_name, vl);
        pre_execute(cmd);

}

/*-------------------------------------------------------------*/
   void WIN_CONFIG(st)
#ifdef VMS
  struct dsc$descriptor_s *st;
/*
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCc
C  subroutine win_config(st)
C  Identifies Graphic Manager
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCc
*/
  {
     strncpy(st->dsc$a_pointer,"X Windows 11",12);
     st->dsc$w_length = 12;
  }
#else
   char st[80];
  {  strcpy(st,"X Windows 11"); }
#endif

/*************************************************************************/

void WIN_OPEN_GENE(vd_id,cx,cy,vd_title,length, x,y, device)
int *vd_id,        /* Window ID to return. */
    *length,       /* length of title. */
    *device;       /* type of window */
float     *cx,*cy;       /* Width and Height of window in cm. */
float     *x,*y;         /* position of window in cm, unused if both 0.0 unless device == 4 */
char *vd_title;    /* Window title. */
/*
     This subroutine opens a window for spectral display with the                
      dimensions cx and cy (in cm.) and with the specified title. 

     Vd_id is  assigned appropriately and passed back to the C
      calling procedure.                                                

      Device determines what is to be opened :
    	1 : Density 2D display
    	2 : Contour 2D window
    	3 : 3D display window
    	4 : View window
    	10 : 1D window (strange coding due to historical reasons
      */
{
      Cardinal i;
      int a,time;
      char st[10];
      char title[MAX_CHAR];
      Widget wid_image_2d,shell_wind_2d;
      Arg args[10];
      static char         menu_string[MAX_CHAR];
      Arg arg;
      int n,dim_type;
      char *dpname;
      Atom wm_delete_window,wm_resize_window,mwm_messages;
      String      str = menu_string;



      if (versionx == 0){
                 printf("Gifa cannot open the display on X server\n");
                *vd_id = 0;
		return;
      }


      			/* get ID */
	if (*device == 10) {
		*vd_id = win_nextid(1);
	} else {
		*vd_id = win_nextid(2);
	}

 			   /*If too many windows, exit...*/

      if (*vd_id == 0) return;

			/* window size */
      if (*device == 1 )
       {  windoww[*vd_id] = simax;
          windowh[*vd_id] = simax; 
/*
printf("recup taille device=%d, w=%d, h=%d\n",*device,windoww[*vd_id],windowh[*vd_id]);
*/
             }
      else
       {  windoww[*vd_id] = (int)(cmconvx*(*cx));  
          windowh[*vd_id] = (int)(cmconvy*(*cy));  
/*
printf("recup taille device=%d, w=%d, h=%d\n",*device,windoww[*vd_id],windowh[*vd_id]);
*/
	}
/*ECIR*/

          		/* window position */
	if ((*device == 4) || (*x != 0.0) || (*y != 0.0)) {
		windowx[*vd_id] = (int)(cmconvx*(*x));  
	        windowy[*vd_id] = (int)(cmconvy*(*y));
	} else {        /* auto if x and y == 0 ; unless device = 4 => always*/
		windowx[*vd_id] = 50+30*(*vd_id-1);
		windowy[*vd_id] = 50+ 20*(*vd_id-1);
	}
	
                        /* 3d_box  associee a l'ancienne fenetre 3D */

        if(*device == 3 && d3box_set != 0){
                XtDestroyWidget(d3box_set);
                d3box_set = 0;
        }


			/*      creation du shell du widget  avec son titre */

      	convert_string(title,vd_title,*length);
      	n = 0;
        XtSetArg(args[n], XtNheight,windowh[*vd_id]); n++ ;
        XtSetArg(args[n], XtNwidth,windoww[*vd_id]); n++ ;
        XtSetArg(args[n], XtNx,windowx[*vd_id]); n++ ;
        XtSetArg(args[n], XtNy,windowy[*vd_id]); n++ ;
   	shell_wind_2d = XtAppCreateShell(title,"XMgifa",
        	applicationShellWidgetClass,XtDisplay(window_mere),args,n);


                        /* Detournement de la commande CLOSE generale */
                        /* vers Close de la Widget                    */

         wm_delete_window = XmInternAtom(XtDisplay(shell_wind_2d),
                                "WM_DELETE_WINDOW",FALSE);
         XmRemoveWMProtocols(shell_wind_2d,&wm_delete_window, 1);
         if ( *device == 1 ){
                XmAddWMProtocolCallback(shell_wind_2d, wm_delete_window, QuitCB_disp2, NULL);
		SetAppIcon(shell_wind_2d,4);
         }
	if ( *device == 2 ){
                XmAddWMProtocolCallback(shell_wind_2d, wm_delete_window, QuitCB_cdisp2, NULL);
		SetAppIcon(shell_wind_2d,4);
         }
	if ( *device == 3 ){
                XmAddWMProtocolCallback(shell_wind_2d, wm_delete_window, QuitCB_disp3, NULL);
		SetAppIcon(shell_wind_2d,5);
         }
	if ( *device == 4 ){
                XmAddWMProtocolCallback(shell_wind_2d, wm_delete_window, QuitCB_view, (XtPointer)(*vd_id));
		SetAppIcon(shell_wind_2d,5);
         }
	if ( *device == 10 ){
                XmAddWMProtocolCallback(shell_wind_2d, wm_delete_window, QuitCB_disp1, NULL);
		SetAppIcon(shell_wind_2d,3);
         }


			/* ajout de la commandes Freeze */


        mwm_messages = XmInternAtom(XtDisplay(shell_wind_2d),
                                _XA_MWM_MESSAGES,
                                FALSE);
        XmAddWMProtocols(shell_wind_2d, &mwm_messages, 1);
	i = 2; 
        *str = '\0';
        sprintf(str, " Freeze f.send_msg %d \n",i);
        str += strlen(str);
        *(str-1) = '\0';
	XtSetArg(arg, XmNmwmMenu,menu_string); 	
        XtSetValues(shell_wind_2d, &arg, 1);
	XmAddProtocolCallback(shell_wind_2d,mwm_messages,(Atom)i,
				freeze_comm,(XtPointer)vd_id);

/*
	 if(*device == 1){
A VOIR  3D 
         	XtSetArg(args[n], XmNresizeCallback,XmRESIZE_NONE); n++ ;
		XtSetArg (args[n], XmNnoResize, True);n++;
	 }
*/
        /**************************************************************/


			/* creation DrawingArea */

	 wid_image_2d = XmCreateDrawingArea(shell_wind_2d,"wid_image_2d",args,n);

	 widget[*vd_id] = wid_image_2d;
         XtManageChild (wid_image_2d);
         XtRealizeWidget(shell_wind_2d);
         window[*vd_id] = XtWindow(wid_image_2d);
	 windowt[*vd_id] = 1;  /* no view */
	 if (*device == 1)
	 	windowd[*vd_id] = 21;	/* 2D Display */
	 if (*device == 2)
	 	windowd[*vd_id] = 22; /* 2D Contour Plot */
	 if (*device == 3)
	 	windowd[*vd_id] = 3; /* 3D Display */
	 if (*device == 4) {
	 	windowd[*vd_id] = 22; /* View - will be reset by setup_view*/
		windowt[*vd_id] = 2;
	 }
	 if (*device == 10)
	 	windowd[*vd_id] = 1;
	 	
	 


			/*      create image pixmap     */


      pixmap[*vd_id] = XCreatePixmap(dpy,
		RootWindowOfScreen(XtScreen(wid_image_2d)),
                windoww[*vd_id],windowh[*vd_id],depth);

/*
printf("creation pixmap pixmap=%d, dpy=%d, w=%d, h=%d, depth=%d\n", pixmap[*vd_id],dpy,windoww[*vd_id],windowh[*vd_id],depth);
*/
/*ECIR*/


			/*    Create graphics context*/

      makeGC(vd_id,white,black);

			/*    Define the size, position, and cursor of the window.*/

      xszhn.x = windowx[*vd_id];
      xszhn.y = windowy[*vd_id];
      xszhn.width = windoww[*vd_id];
      xszhn.height = windowh[*vd_id];
      xszhn.flags = (PPosition | PSize);
    
/* a voir...... */
      status = XSetNormalHints(dpy,window[*vd_id],&xszhn);
      XDefineCursor(dpy,window[*vd_id],cursor1); 

                        /* add the callback of window for resize and expose*/

        XtAddCallback(wid_image_2d,XmNexposeCallback,refresh_widg,&xszhn);
	if( *device != 1 ){ 
        	XtAddCallback(wid_image_2d,XmNresizeCallback,resize_pixmap,vd_id);
	}
	else{	
			/* interdire le resize */
	}



                        /*add callback of window for zoom*/

        add_callback_zoom(vd_id);
	add_callback_point(vd_id);


                        /*add callback of window for pics*/

/*
        XtAddEventHandler(wid_image_2d,Button2MotionMask,FALSE,input_picCB,
                NULL);
*/

      WIN_ERASE(vd_id);

			/* flag specifiant l'existance d'une pixmap
			-> on peut afficher l' icone*/

	w2d_open++;
}



/**********************************************************************/

void WIN_CLOSE(vd_id)    /* Window ID to destroy. */
int *vd_id;
/*
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C   win_close (vd_id)                                                    C
C                                                                        C
C     Close a window.                                                    C
C                                                                        C
C     vd_id = Window ID                                                  C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
*/
{
	Widget id_parent;

			/*   destroy the parent shell of the window */

      id_parent = XtParent(widget[*vd_id]);
      XFreePixmap(XtDisplay(id_parent),pixmap[*vd_id]);
      XtDestroyWidget(widget[*vd_id]);
      XtUnrealizeWidget(id_parent);

			/* femeture si besoin de 3dbox associee a la 
				fenetre 3D	*/

	if(windowd[*vd_id] == 3 && d3box_set != 0){
		XtDestroyWidget(d3box_set);
		d3box_set = 0;
	}


			/* nb de win 2d open :si = 0 : pas d'icone*/

	if(windowd[*vd_id] == 21 || windowd[*vd_id] == 22){
		w2d_open--;
	}


			/*    Reset the window flag and set the passed id to zero so*/
			/*     GIFA will know the window has been cleared...*/

      pixmap[*vd_id] = 0;
      window[*vd_id] = 0;
      windowd[*vd_id] = 0;
      widget[*vd_id] = 0;
      win[*vd_id] = 0;
      *vd_id = 0;

}

/*****************************************************************************/

void SETUP_VIEW(vd_id,type,zz1,zz2,zz3,zz4)
int *vd_id, *type;
float *zz1, *zz2, *zz3, *zz4;
/* Defines zoom coordinates of the window
   used by OPEN_VIEW windows to compute viewfinder position.
   Value of the edges (zz1..zz4 are assumed to be in ppm
   zz1 = lower F1 zoom coordinate zz2 = lower F2 zoom coordinate,
   zz3 = upper F1 zoom coordinate zz4 = upper F2 zoom coordinate*/
{
	viewfinder[*vd_id].z1 = *zz1;
	viewfinder[*vd_id].z2 = *zz2;
	viewfinder[*vd_id].z3 = *zz3;
	viewfinder[*vd_id].z4 = *zz4;
 	if (*type == 1)
		windowd[*vd_id] = 1;
 	else
 		windowd[*vd_id] = 22;
}


/*****************************************************************************/

void WIN_SNAPSHOT(vd_id,filename) 
int *vd_id;             /* Window ID to take snapshot of. */
char filename[80];     /* Output file name and path. */
/*
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     This subroutine produces a vms file of the specified window and    C
C      puts the output into the file specified by filename.              C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
*/
{
/*    Tell user this featured not available yet.*/
      printf("Snapshot not supported in X Windows.\n");
}

/*************************************************************************/

void WIN_FGCOLOR( vd_id,color)
int *vd_id,            /* Window ID to plot into. */
    *color;            /* color index 1..65 */
    
/*
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C   win_fgcolor (vd_id,color)                                            C
C                                                                        C
C   sets the color for the next draw in the window                       C
C                                                                        C
C       vd_id = Virtual display ID                                       C
C       color = index in the color table                                 C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
*/
 
{  XSetForeground(dpy, gc[*vd_id], colorno[*color]); }


/***********************************************************************/


void WIN_BGCOLOR( vd_id,color)
int *vd_id,            /* Window ID to plot into. */
    *color;            /* color index 1..65 */
/*
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C   win_bgcolor (vd_id,color)                                            C
C                                                                        C
C   sets the color for the next draw in the window                       C
C                                                                        C
C       vd_id = Virtual display ID                                       C
C       color = index in the color table                                 C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
*/
{
  XSetBackground(dpy, gc[*vd_id], colorno[*color]);
    bg[*vd_id] = *color;
  }

/*************************************************************************/

void WIN_PLOT_1D( vd_id,x1,y1,x2,y2)
int *vd_id;            /* Window ID to plot into. */
float *x1,*y1,            /* Starting point of line in global coords. */
       *x2,*y2;            /* Ending point. */
/*
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C   win_plot_1d (vd_id,x1,y1,x2,y2)                                      C
C                                                                        C
C     Plot point or line(s)                                              C
C                                                                        C
C       vd_id = Virtual display ID                                       C
C       x1, y1 = x,y coordinates (floating) of start and end points.     C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
*/
{
      int a1,a2,            /* Starting point of line in pixel coords. */
          mx,my,            /* Window width and height */
          b1,b2;            /* Ending point. */

/*    Get the pixels in x and y direction for full window.*/
      mx = windoww[*vd_id]-1;
      my = windowh[*vd_id]-1;

/*    convert 0->1 to 0->mx or my values.  */
/*    translate y coordinates..., i.e. move origin from*/
/*    lower left to upper left of window*/
      a1 = (int)(mx * *x1);
      a2 = (int)(my - (my * *y1));
      b1 = (int)(mx * *x2);
      b2 = (int)(my - (my * *y2));

/*    Draw the line.*/
      XDrawLine(dpy,pixmap[*vd_id],gc[*vd_id],a1,a2,b1,b2);
}
 
/***********************************************************************/

void WIN_IMAGE_2D(vd_id,wd,ht,bitsperpixel,matrix)
int *vd_id,          /* Window ID for image. */
     *wd,*ht,          /* Image width and height in units. */
     *bitsperpixel;   /* Number of bits per pixel. */
char *matrix;     /* Natural matrix of dim wd x ht elements. */
/*
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     win_image_2d (vd_id,                                               C
C                wd,ht,bitsperpixel,                                     C
C                matrix)                                                 C
C                                                                        C
C     Write a natural image bitmap into the specified virtual            C
C     display rectangle.  If the size of the bitmap doesn't match        C
C     the rectangle, scaling is done.                                    C
C                                                                        C
C       vd_id = Virtual display ID                                       C
C       wd = Width of image bitmap in pixels                             C
C       ht = Height of image bitmap in pixels                            C
C       bitsperpixel = Number of image bits which make up 1 "pixel"      C
C       matrix = Address of natural image data                           C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
*/
{ 

	int mx,my;

			/*    Get the windows width and height in pixels...*/

      mx = windoww[*vd_id];
      my = windowh[*vd_id];

/*my = 128; mx = 128;*/

/*
printf("creer_image : wd=%d,ht=%d,mx=%d,my=%d,matrix=%d\n",*wd,*ht,mx,my,*matrix);
*/
      creer_image(wd,ht,mx,my,matrix);
/*ICI PB ECIR*/

/*my = 256; mx = 128;*/

			/*    Dump this image to the Pixmap for the image window.*/


/*ICI PB ECIR*/
/*
printf("vd_id=%d,mx=%d,my=%d,pixmap=%d,gc=%d,image=%d,dpy=%d \n",vd_id,mx,my,pixmap[*vd_id],gc[*vd_id],image,dpy);
printf("ICI PB ECIR\n");
printf("image_recup->width =%d\n",image->width);
printf("image_recup->height  =%d\n",image->height);
printf("image_recup->data  =%d\n",image->data);
printf("image_recup->depth  =%d\n",image->depth);
printf("image_recup->xoffset  =%d\n",image->xoffset);
printf("image_recup->format  =%d\n",image->format);
printf("image_recup->byte_order  =%d\n",image->byte_order);
printf("image_recup->bitmap_unit  =%d\n",image->bitmap_unit);
printf("image_recup->bitmap_bit_order  =%d\n",image->bitmap_bit_order);
printf("image_recup->bitmap_pad  =%d\n",image->bitmap_pad);
printf("image_recup->bytes_per_line  =%d\n",image->bytes_per_line);
*/

      status =  XPutImage(dpy,pixmap[*vd_id],gc[*vd_id],image,0,0,0,0,mx,my);


/*
printf("ICI PB ECIR 2 \n");
*/
			/*    Copy the Pixmap to the Window Drawable.*/

      WIN_UPDATE(vd_id);

}
/*************************************************************************/
void creer_image(wd,ht,mx,my,matrix) 
/*XImage *creer_image(wd,ht,mx,my,matrix)*/
int my,mx,
   *wd,*ht;          /* Image width and height in units. */
unsigned char *matrix;     /* Natural matrix of dim wd x ht elements. */
{


    int      k,ii,jj;        /* Data array index counters. */
    float pixelw,pixelh,  /* Real dimensions in pixels of each square. */
          i,j;            /* Loop counters to fill squares. */


/* ECIR */
    image = (XImage *) XtMalloc(sizeof (XImage));
/* ECIR */


                        /*    Calculate the number of increments per pixels ...*/



      pixelw = ((float) *wd) / ((float) mx);
      pixelh = ((float) *ht) / ((float) my);

      k = 0;

                        /*    Loop to convert natural data to pixel values which can be used*/
                        /*     by an X Windows image.  Lookup on colorno array...*/

    for (ii=0;ii<my;ii++) {
#ifdef VMS
        k++;               /* VMS is shifting the array (?) */
#endif
	switch (depth) {
	case (4) :
	case (8) :
           for (jj=0;jj<mx;jj++)
           { 
	     pldat[k] = colorno[matrix[(int) ( jj*pixelw )
                         +  *wd * ( (int) (ii*pixelh))  ]];
             k = k+1;
           }
	   image = XCreateImage(dpy,visual,
				depth,ZPixmap,0,pldat,
				mx,my,32,0);
	   break;
	case (15) :
        case (16) :
           for (jj=0;jj<mx;jj++)
           { pldat16[k] = colorno[matrix[(int) ( jj*pixelw )
                         +  *wd * ( (int) (ii*pixelh))  ]];
             k = k+1;
           }
           image = XCreateImage(dpy,visual,
                                depth,ZPixmap,0,(char *) pldat16,
                                mx,my,32,0);
           break;
	case (24) :
	case (32) :
           for (jj=0;jj<mx;jj++)
           { pldat32[k] = colorno[matrix[(int) ( jj*pixelw )
                         +  *wd * ( (int) (ii*pixelh))  ]];
             if (depth == 24) { pldat32[k] = pldat32[k] << 8; }
             k = k+1;
           }
	   image = XCreateImage(dpy,visual,
				depth,ZPixmap,0,(char *) pldat32,
				mx,my,32,0);
	   break;
	default :
	  printf("Gifa do not know how to handle depth of %d\n",depth);
	}
    }

/*ECIR parametres image 
printf("image->width =%d\n",image->width);
printf("image->height  =%d\n",image->height);
printf("image->data  =%d\n",image->data);
printf("image->depth  =%d\n",image->depth);
printf("image->xoffset  =%d\n",image->xoffset);
printf("image->format  =%d\n",image->format);
printf("image->byte_order  =%d\n",image->byte_order);
printf("image->bitmap_unit  =%d\n",image->bitmap_unit);
printf("image->bitmap_bit_order  =%d\n",image->bitmap_bit_order);
printf("image->bitmap_pad  =%d\n",image->bitmap_pad);
printf("image->bytes_per_line  =%d\n",image->bytes_per_line);
*/

}

/***********************************************************************/

void WIN_SET_WRITING_MODE(vd_id,mode)
int *vd_id,*mode;
/*
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C   win_set_writing_mode(vd_id,mode)                                     C
C                                                                        C
C     Sets the text and graphics writing mode.                           C
C                                                                       C
C       vd_id = Virtual display ID                                       C
C       mode = Number of the mode                                        C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
*/
{
/*    Not used by X Windows.*/
}

/************************************************************************/

void WIN_ENABLE_DISPLAY_LIST(vd_id)
int *vd_id;
/*
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C   win_enable_display_list(vd_id)                                       C
C                                                                        C
C     Enable keeping the display list (used after being disabled)        C
C                                                                        C
C       vd_id = Virtual display ID                                       C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
*/
{
/*    Not used X Windows.*/
}
/**********************************************************************/

void WIN_WRITE(vd_id,x,y,text,len)
int *vd_id,*len;
float *x,*y;
char  text[256];
/*

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C   win_write(vd_id,x,y,text,len)                                        C
C                                                                        C
C     Write text(lenght) at position x,y.                                C
C                                                                        C
C       vd_id = Virtual display ID                                       C
C       x,y  = coordinates where to write.                               C
C       text = text to write                                             C
C    note: the trailing blanks of the text are removed before writing.   C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
*/
{
      int a1,a2,            /* coord of text in pixel coords. */
          mx,my,            /* Window width and height */
          len1;
      char ltext[256];
      convert_string(ltext,text,*len);
      mx = windoww[*vd_id]-1;
      my = windowh[*vd_id]-1;
      a1 = (int)(mx * *x);
      a2 = (int)(my - (my * *y));
     XDrawString(dpy,pixmap[*vd_id],gc[*vd_id],a1,a2,ltext,*len);
}

/**********************************************************************/


void WIN_PLOT_ARRAY(vd_id,count,xv,yv)
      int *vd_id,              /* Window ID for plotting. */
          *count;              /* Number of points to plot. */
      float    *xv,*yv; /* Array Data points in Global Coords. 0.0->1.0 */
/*
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C   win_plot_array (vd_id,count,xv,yv)                                   C
C                                                                        C
C     Plot point or line(s)                                              C
C                                                                        C
C       vd_id = Virtual display ID                                       C
C       count = Number of points                                         C
C       xv = Address of array of x coordinates (F floating)              C
C       yv = Address of array of y coordinates (F floating)              C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
*/
{
      int    p, pmax,              /* Loop counter. */
          mx,my;              /* Window width and height in pixels. */
     	XPoint *pt;
	
/*  Array of X Windows point structures  */
                                    /*  to use for plotting. */

/*    Get the window width and height in pixels*/
      mx = windoww[*vd_id]-1;
      my = windowh[*vd_id]-1;

/*    Dump the vector data into the XPoint structure array.*/
	pmax = *count;
	pt = (XPoint *) malloc(pmax*sizeof(XPoint));

	
	for (p=0;p<pmax;p++)
/***************/
         { pt[p].x = min(8*mx,max(-8*mx,(int)(mx*xv[p])));
           pt[p].y = max(-8*my,min(8*my,(int)(my-my*yv[p]))); }
/**************/
/* was unlimited */
/***************
         { pt[p].x = (int)(mx*xv[p]);
           pt[p].y = (int)(my-my*yv[p]); }
***************/

/*    Send the point array to X Windows.*/
      XDrawLines(dpy,pixmap[*vd_id],gc[*vd_id],
                 pt,pmax,CoordModeOrigin);
   
   free(pt);
}


/************************************************************************/

void WIN_SET_POINTER_PATTERN( vd_id,pattern)
int *vd_id,          /* Window ID in which to set pointer pattern. */
    *pattern;       /* Pattern #, 0 to reset, 1 for crosshair. */
/*
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C   win_set_pointer_pattern (vd_id,pattern)                        C
C                                                                        C
C     Setup a cursor pattern.                                            C
C                                                                        C
C       vd_id = Virtual display ID                                       C
C       pattern_no = number of the pattern                               C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
*/
{

/*    pattern 1 is crosshair, pattern 0 to reset to arrow...*/

      if (*pattern == 1)
         XDefineCursor(dpy,window[*vd_id],cursor2);
      if (*pattern == 0)
         XDefineCursor(dpy,window[*vd_id],cursor1);
      if (*pattern == 2){ 
         XDefineCursor(dpy,window[*vd_id],cursor_wait);
      }

	XFlush(dpy);

}

/*************************************************************************/

int WIN_GET_POINTER_POSITION(vd_id,retx,rety)

int *vd_id;        /* Window ID to get pointer position in. */
float *retx,*rety;     /* Global return coords. */
/*
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C   status = win_get_pointer_position (vd_id, retx, rety)         C
C                                                                        C
C     Gets the current mouse position in world coordinates               C
C                                                                        C
C     Returns true/false if mouse is within the window                   C
C       vd_id = Virtual display ID                                       C
C       retx, rety = Address to return X,Y world coordinates             C
C                                                                        C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
*/
{
      Window rt,child;     /* Root and child window pointers. */
      int    retval,       /* Holds return value of function. */
          rtx,rty,      /* Global pointer coords. */
          rx,ry;        /* Local window coords to Window vd_id. */
      unsigned int  keys;  /* Status of mouse buttons. */
      Bool XQ;


      XQ = XQueryPointer(dpy,window[*vd_id],
        &rt,&child,
        &rtx,&rty,
        &rx,&ry,
        &keys);

/*    Check to see if pointer is within the window rectangle.*/
      if ( (rx>=0) && (ry>=0) && (rx <= windoww[*vd_id]) &&
          (ry <= windowh[*vd_id]) )
        {   retval = 1;
/*    Convert return values to 0->1 convention and test for out of bounds*/
/*     values.*/
            *retx = min(1.0,(float)(rx)/windoww[*vd_id]);
            *rety = max(0.0,(1.0-((float)(ry)/windowh[*vd_id]))); }
      else
        {   retval = 0;
            *retx = 0.0;
            *rety = 0.0; }
      return retval;
 }     

/************************************************************************/

int WIN_GET_BUTTONS(vd_id)
int *vd_id;       /* Window ID to get pointer position in. */
/*
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C   status = win_get_buttons (vd_id)                                     C
C                                                                        C
C     Gets the current mouse button state.                               C
C                                                                        C
C       status = Address to return button state in                       C
C                set to 0 if mouse buttons are up.                       C
C                set to 1 if mouse button 1 is down.                     C
C                set to 2 if mouse button 2 is down.                     C
C                set to 3 if mouse button 3 is down.                     C
C       vd_id =  window ID                                               C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
*/
{
      Window rt,child;     /* Root and child window pointers. */
      int rtx,rty,      /* Global pointer coords. */
          rx,ry;        /* Local window coords to Window vd_id. */
      unsigned int  keys;  /* Status of mouse buttons. */
      Bool XQ;

      XQ = XQueryPointer(dpy,window[*vd_id],
                              &rt,&child,
                              &rtx,&rty,
                              &rx,&ry,
                              &keys);

/*  (keys!=0)     Means a button is down...*/
	   switch (keys)
	     {
	       case ( 0 ) : return 0; break;
	       case ( Button1Mask ) : return 1; break;
	       case ( Button2Mask ) : return 2; break;
	       case ( Button3Mask ) : return 3; break;
	       default : return 1 ;
	       }

}

/************************************************************************/


void  del_wait_cursor(win)
Window win;
/*
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C   del_wait_cursor                                                      C
C                                                                        C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
*/
{

        XUndefineCursor(dpy,win);

}


void copy_pixmap(vd_id_orig)
int *vd_id_orig;
/*******************************************************************
*	duplique la fenetre contenant la pixmap
*	mode: 1 fenetre complete
*******************************************************************/

{

	Widget	shell_wincopy,wincopy;
	Arg	args[10];
	int 	n;
	char title[MAX_CHAR];
	Atom wm_delete_window;
	XGCValues	valeurs;
	donnees_pixmap   *donnees;
	Dimension largeur_w, hauteur_w ;
	XSizeHints hints;
	char *titre;


		/* test display et recup id windows */

	if (versionx == 0){
                 printf("Gifa cannot open the display on X server\n");
                 return;
	}


			/*recup de la dimension de la fenetre initiale*/

	donnees = (donnees_pixmap *) malloc(sizeof(donnees_pixmap));

        donnees->largeur = windoww[*vd_id_orig];
        donnees->hauteur = windowh[*vd_id_orig];



			/* Titre de la fenetre freeze */
			/* et numerotation */

	titre = (char *)malloc(40);
	if(windowd[*vd_id_orig] == 1){
		sprintf(titre,"1D Display Frozen Number %d",cpt_freeze[0]);
		cpt_freeze[0]++;
	}
	if(windowd[*vd_id_orig] == 21){
                sprintf(titre,"2D Display Frozen Number %d",cpt_freeze[1]);
                cpt_freeze[1]++;
        }
	if(windowd[*vd_id_orig] == 22){
                sprintf(titre,"Contour Plot Frozen Number %d",cpt_freeze[2]);
                cpt_freeze[2]++;
        }
	if(windowd[*vd_id_orig] == 3){
                sprintf(titre,"3D Display Frozen Number %d",cpt_freeze[3]);
                cpt_freeze[3]++;
        }



                        /*creation du shell du widget  avec le titre */

        n = 0;
        XtSetArg(args[n], XtNheight,donnees->hauteur); n++ ;
        XtSetArg(args[n], XtNwidth,donnees->largeur); n++ ;
        shell_wincopy = XtAppCreateShell(titre,"XMgifa",
                 applicationShellWidgetClass,XtDisplay(window_mere),args,n);


                        /* Detournement de la commande CLOSE generale */
                        /* vers Close de la Widget                    */


         wm_delete_window = XmInternAtom(XtDisplay(shell_wincopy),
                                "WM_DELETE_WINDOW", FALSE);
         XmRemoveWMProtocols(shell_wincopy,&wm_delete_window, 1);
         XmAddWMProtocolCallback(shell_wincopy,wm_delete_window, del_copy,(XtPointer)donnees);

                        /* Icone de presentation */

        if(windowd[*vd_id_orig] == 1){
		SetAppIcon(shell_wincopy,8);
        }
	else
		SetAppIcon(shell_wincopy,9);

			/* Creation du DrawingArea */

        n = 0;
        XtSetArg(args[n], XtNheight,donnees->hauteur); n++ ;
        XtSetArg(args[n], XtNwidth,donnees->largeur); n++ ;
        wincopy = XtCreateManagedWidget("copy",xmDrawingAreaWidgetClass,
					shell_wincopy,args,n);


                        /* creation pixmap */

        donnees->pixmap = XCreatePixmap(dpy,
                        RootWindowOfScreen(XtScreen(wincopy)),
                        donnees->largeur,donnees->hauteur,
                        DefaultDepthOfScreen(XtScreen(wincopy)));


                        /* creation GC */

	donnees->gc = XCreateGC(dpy,donnees->pixmap,0,&valeurs);
	XCopyGC(dpy,gc[*vd_id_orig],(GCForeground | GCBackground),donnees->gc);



                        /* copy de l'ancienne pixmap dans la nouvelle */

        XCopyArea(dpy,pixmap[*vd_id_orig],donnees->pixmap,donnees->gc,
                                0,0,donnees->largeur,donnees->hauteur,0,0);


			/* realisation */

        XtRealizeWidget(shell_wincopy);

                /* stockage dans la liste des shells affiches */

        add_shell_on_display(shell_wincopy);


                        /*add callback of window for expose et resize*/

        XtAddCallback(wincopy,XmNexposeCallback,refresh_copy,donnees);
        XtAddCallback(wincopy,XmNresizeCallback,resize_copy,donnees);
}

/*-------------------------------------------------------------
*/
void del_copy(w, donnees, call_data)
Widget          w;              /*  widget id           */
donnees_pixmap  *donnees;
XtPointer       call_data ;
{
#if defined SGI || defined AIX 
  del_shell_on_display(XtParent(w));
  XtDestroyWidget(XtParent(w));
#else
  del_shell_on_display(w);
  XtDestroyWidget(w);
#endif
XFreePixmap(dpy,donnees->pixmap);
}

/*-------------------------------------------------------------
*/
void refresh_copy(w, donnees, appel)
Widget          w;              /*  widget id           */
donnees_pixmap	*donnees;
XmDrawingAreaCallbackStruct	*appel;
{

        Arg args[2];
        int n;
        Dimension largeur,hauteur;


        n = 0;
        XtSetArg(args[n],XtNwidth,&largeur);n++;
        XtSetArg(args[n],XtNheight,&hauteur);n++;
        XtGetValues(w,args,n);
        XCopyArea(XtDisplay(w),donnees->pixmap,XtWindow(w),donnees->gc,
                        0,0,donnees->largeur,donnees->hauteur,
			(largeur - donnees->largeur) / 2,
			(hauteur - donnees->hauteur) / 2);
}



/*-------------------------------------------------------------
*/
void resize_copy(w, donnees, appel)
Widget          w;
caddr_t  donnees;
XmDrawingAreaCallbackStruct     *appel;
{
if(XtIsRealized(w))
	XClearArea(XtDisplay(w),XtWindow(w),0,0,0,0,TRUE);
}


/*-------------------------------------------------------------
*commande Freeze lancee a partir de XWindows
*/
void freeze_comm(w, vd_id, call_data)
    Widget      w;
    int   *vd_id, *call_data;
{

	copy_pixmap(vd_id);
}

/*-------------------------------------------------------------
*commande Freeze lancee a partir de la fonction execute:
*IN OUT : flag
*
*code de flag en entree:
*	1 -> 1D Display
*	2 -> 2D Display
*	3 -> 2D Contour Plot
*	4 -> 3D Display
*code de flag en sortie:
*	0 -> error
*	!= 0 -> ok
*/
void CREATEFREEZE(flag)
int *flag;
{
int i,ok;
int winflag;	/* pour stocker le code utilise dans windowd[vd_id]*/
	
	ok = 0;

	if(*flag == 1) winflag = 1;	/* 1D Display */
	if(*flag == 2) winflag = 21;	/* 2D Display */
	if(*flag == 3) winflag = 22;	/* 2D Contour Plot */
	if(*flag == 4) winflag = 3;	/* 3D Display */

	for(i=1;i<NWindow;i++){
		if( window[i] != 0 && windowd[i] == winflag){
			copy_pixmap(&i);
		ok = 1;
		}
	}
	if(ok == 0) *flag = 0;
}

/*-------------------------------------------------------------
* enleve la montre dans les fenetre spectres
*del_cursor_wait_on_display(flag);
*	flag = 1	on enleve la montre
*	flag = 0 	on met la montre
*/
void del_cursor_wait_on_display(flag)
int flag;
{
	int i,n;

    if(flag == 1){
      for(i=1; i<NWindow; i++){
            if( window[i] != 0){
		 n = 0;
		  WIN_SET_POINTER_PATTERN(&i,&n);

	    }		
      }
    }
    if(flag == 0){
      for(i=1; i<NWindow; i++){
            if( window[i] != 0){
		 n = 2;
                  WIN_SET_POINTER_PATTERN(&i,&n);
	    }
      }
    }
}

void which_window(win)
/* return the id of the current window */
int *win;
{

	int dim,i,flag;

	WHICH_DIM(&dim);

	flag=0;
	if(dim == 1 || dim == 3){ /* windowd[i]=1 pour 1d et windowd[i]=3 pour 3d */
		for(i=1; i<NWindow; i++){
			if(windowd[i] == dim) {*win = i;flag=1;return;}
		}
	}

	if(dim == 2 ){ /*windowd[i]=21 pour 2Ddisp et windowd[i]=22 pour contour Plot*/
		for(i=1; i<NWindow; i++){
			if(windowd[i] == 21) {*win = i;flag=1;return;}
			if(windowd[i] == 22) {*win = i;flag=1;return;}
		}
	}
	if(flag==0) *win = -1; /* n'a pas trouve de fenetre*/
}


/*-------------------------------------------------------------
**  create_pixmap_icon
ICON
*/
void create_pixmap_icon(mere,x,y)
Widget mere;
int x,y;
{

        XGCValues       valeurs;
        int             n;
        Arg             args[10];
	Widget		form;



	icone_data = (Icone_data *) malloc(sizeof(Icone_data));

                        /* Creation du DrawingArea */

        n = 0;
        XtSetArg(args[n], XtNheight,SizeIcon); n++ ;
        XtSetArg(args[n], XtNwidth,SizeIcon); n++ ;
        XtSetArg(args[n], XmNx,x);n++;
        XtSetArg(args[n], XmNy,y);n++;
        form = XmCreateForm(mere,"",args, n);
        XtManageChild (form);
	n = 0;
        XtSetArg(args[n], XtNheight,SizeIcon); n++ ;
        XtSetArg(args[n], XtNwidth,SizeIcon); n++ ;
        icone_data->icon_spectrum = XtCreateManagedWidget("copy",
					xmDrawingAreaWidgetClass,
                                        form,args, n);
	

                        /* creation pixmap */

        icone_data->icon_pixmap = XCreatePixmap(XtDisplay(window_mere),
                        RootWindowOfScreen(XtScreen(window_mere)),
                        SizeIcon,SizeIcon,
                        DefaultDepthOfScreen(XtScreen(window_mere)));

			/* set up parameters */

        icone_data->larg = SizeIcon;
        icone_data->haut = SizeIcon;
	icone_data->icon_set = 0; /* pas d'icone */

                        /* Create graphics context*/

        icone_data->icon_GC = XCreateGC(XtDisplay(window_mere),icone_data->icon_pixmap,
        		 0,&valeurs);

                        /*add callback of window for expose et resize*/

        XtAddCallback(icone_data->icon_spectrum,XmNexposeCallback,expose_icon,NULL);
      	XtAddCallback(icone_data->icon_spectrum,XmNresizeCallback,resize_icon,NULL);

	init_cadre_icon();

			/* Action souris sur icon */

        XtAddEventHandler(icone_data->icon_spectrum,ButtonPressMask,FALSE,init_input_iconCB, NULL);
	XtAddEventHandler(icone_data->icon_spectrum,Button1MotionMask,FALSE,input_iconCB, NULL);
	XtAddEventHandler(icone_data->icon_spectrum,ButtonReleaseMask,FALSE,end_input_iconCB, NULL);

}

void refresh_icon()
{

	int size,on;
        char             matrix[SizeIcon*SizeIcon];
        int             s1,s2;
	XGCValues       valeurs;
	GC  		tempgc;
        int             current_win,dim;


	
        WHICH_DIM(&dim);
	if(w2d_open == 0 || dim != 2){
		map_logo();
		return;
	}


		/* recherche de l'ID de la fenetre a iconifier*/
        which_window(&current_win);
        if(current_win==-1){
		map_logo();
                return; /* n'a pas trouve de fenetre */
                                   /* LOGO de GIFA */
       }

		/* recup des valeurs de son CG */
        XCopyGC(XtDisplay(window_mere),gc[current_win],
                        (GCForeground | GCBackground),icone_data->icon_GC);


		/* creation de l'image */
	size = icone_data->larg;  /* la pixmap est carree */
	DISPLAY_ICON(&size,&s1,&s2,matrix); 
	creer_image(&s2,&s1,icone_data->larg,icone_data->haut,matrix);

                        /*    Dump this image to the Pixmap for the image window.*/

        XPutImage(XtDisplay(icone_data->icon_spectrum),
			icone_data->icon_pixmap,
                        icone_data->icon_GC,image,
			0,0,0,0,
			icone_data->larg,icone_data->haut);

			/*    Copy the pixmap to the window...*/


         XCopyArea(XtDisplay(window_mere),icone_data->icon_pixmap,
			XtWindow(icone_data->icon_spectrum),
                        icone_data->icon_GC,0,0,
			icone_data->larg,icone_data->haut,0,0);
         XFlush(XtDisplay(icone_data->icon_spectrum));

	icone_data->icon_set = 1;

}

void map_logo()
{

	Pixmap bitmap;
	static char *data;
	int  larg_data,haut_data;
	GC tmpgc;
	int n;
        XGCValues valeurs;
        Arg  args[10];



	/* set valeurs data */

	data = logo_bits;
	larg_data =  logo_width;
	haut_data =  logo_height;
	icone_data->icon_set = 0;

	/* creation bitmap a partir des donnees*/
	bitmap = XCreateBitmapFromData(XtDisplay(window_mere),
			RootWindowOfScreen(XtScreen(window_mere)),
			data,icone_data->larg,icone_data->haut);

	/* contexte */
	n = 0;
	XtSetArg(args[n],XtNforeground,&valeurs.foreground); n++;
	XtSetArg(args[n],XtNbackground,&valeurs.background); n++;
	XtGetValues(icone_data->icon_spectrum,args,n);
	tmpgc = XtGetGC(icone_data->icon_spectrum,
			GCForeground|GCBackground,
			&valeurs);
			
        XCopyPlane(XtDisplay(window_mere),bitmap,icone_data->icon_pixmap,
                tmpgc,0,0,icone_data->larg,icone_data->haut,
                0,0,1);

        XCopyArea(XtDisplay(window_mere),icone_data->icon_pixmap,
                        XtWindow(icone_data->icon_spectrum),
                        tmpgc,0,0,
                        icone_data->larg,icone_data->haut,0,0);

        XFlush(XtDisplay(icone_data->icon_spectrum));
	XFreePixmap(XtDisplay(window_mere),bitmap);
			
}

/*-------------------------------------------------------------
*/
void resize_icon(w, client_data, call_data)
Widget          w;              /*  widget id           */
XtPointer       client_data;    /*  data from application   */
XtPointer       call_data;
{
if(XtIsRealized(w))
        XClearArea(XtDisplay(w),XtWindow(w),0,0,0,0,TRUE);

}


/*-------------------------------------------------------------
                init_input_iconCB
*/
void init_input_iconCB(w, client_data, evt)
Widget          w;              /*  widget id           */
XtPointer       client_data;    /*  data from application   */
XEvent          *evt;
{
        int dim;

        WHICH_DIM(&dim);
        if(dim != 2 || icone_data->icon_set == 0 || icone_data->cadre_set == 0) return;


                /* test souris dans cadre de zoom*/

        if(evt->xbutton.x < icone_data->cadre_x ||
        evt->xbutton.x>(icone_data->cadre_x+icone_data->cadre_larg))return;
        if(evt->xbutton.y < icone_data->cadre_y ||
        evt->xbutton.y>(icone_data->cadre_y+icone_data->cadre_haut))return;

		/* flag de mouvement du cadre */

	icone_data->mv_cadre = 1;

                /* recup coord souris */


        icone_data->xmouse  =  evt->xbutton.x;
        icone_data->ymouse  =  evt->xbutton.y;

}


/*-------------------------------------------------------------
                end_input_iconCB
*/
void end_input_iconCB(w, client_data, evt)
Widget          w;              /*  widget id           */
XtPointer       client_data;    /*  data from application   */
XEvent          *evt;
{
        float x1,x2,y1,y2;
        float urx,ury,llx,lly;
        int oux,ouy,olx,oly;
        int dim;

       if(icone_data->mv_cadre == 0) return;

        x1 = (float)(icone_data->cadre_x+icone_data->cadre_larg)/icone_data->larg;
	y1 = (1.0-((float)(icone_data->cadre_y+icone_data->cadre_haut)/icone_data->haut));

        x2 = (float)(icone_data->cadre_x)/icone_data->larg;
       	y2 = (1.0-((float)(icone_data->cadre_y)/icone_data->haut));

       	urx = max(x1,x2);   llx = min(x1,x2);
        ury = max(y1,y2);   lly = min(y1,y2);

                	/* appel de SET_ZOOM_PARAM */

       	SET_ZOOM_COORD(&lly, &llx, &ury, &urx);

       		         /* DOREFRESH*/

        DOREFRESH();

		/* fin du mouvement du cadre*/

	icone_data->mv_cadre = 0;

}


/*-------------------------------------------------------------
		input_iconCB
*/
void input_iconCB(w, client_data, evt)
Widget          w;              /*  widget id           */
XtPointer       client_data;    /*  data from application   */
XEvent       	*evt;
{
	int dim;

	if(icone_data->mv_cadre == 0) return;

		/* efface ancien cadre */

	XDrawRectangle(XtDisplay(window_mere),XtWindow(icone_data->icon_spectrum),
                       icone_data->cadre_GC,
                       icone_data->cadre_x,icone_data->cadre_y,
                       icone_data->cadre_larg,icone_data->cadre_haut);

		/* calcul nouvelles coord du cadre */

	icone_data->cadre_x = icone_data->cadre_x + (evt->xbutton.x-icone_data->xmouse);
	icone_data->cadre_y = icone_data->cadre_y + (evt->xbutton.y-icone_data->ymouse);


		/* calcul des bords */

	if(icone_data->cadre_x < 0) icone_data->cadre_x = 0;	
	if(icone_data->cadre_y < 0) icone_data->cadre_y = 0;
	if((icone_data->cadre_y + icone_data->cadre_haut) > icone_data->haut) 
			icone_data->cadre_y = (icone_data->haut-icone_data->cadre_haut);
   	if((icone_data->cadre_x + icone_data->cadre_larg) > icone_data->larg) 
			icone_data->cadre_x = (icone_data->larg-icone_data->cadre_larg);

		/*creer nouveau cadre */

        XDrawRectangle(XtDisplay(window_mere),XtWindow(icone_data->icon_spectrum),
                       icone_data->cadre_GC,
                       icone_data->cadre_x,icone_data->cadre_y,
                       icone_data->cadre_larg,icone_data->cadre_haut);

                /* recup coord souris */


        icone_data->xmouse  =  evt->xbutton.x;
        icone_data->ymouse  =  evt->xbutton.y;

}



/*-------------------------------------------------------------
**  expose_icon
*/

void expose_icon(w, client_data, call_data)
Widget          w;              /*  widget id           */
XtPointer       client_data;    /*  data from application   */
XtPointer       call_data;

{

        Arg args[2];
        int n,flag;
        Dimension largeur,hauteur;

        n = 0;
        XtSetArg(args[n],XtNwidth,&largeur);n++;
        XtSetArg(args[n],XtNheight,&hauteur);n++;
        XtGetValues(w,args,n);
	
        XCopyArea(XtDisplay(w),icone_data->icon_pixmap,XtWindow(w),
		icone_data->icon_GC,0,0,icone_data->larg,icone_data->haut,
                        (largeur - icone_data->larg) / 2,
                        (hauteur - icone_data->haut) / 2);

	/* redessine le cadre zoom dans l'icone */

	icone_data->cadre_set = 0;
	flag = 1;
	TRACE_CADRE_ICON(&flag);

}

/*-------------------------------------------------------------*/


void init_cadre_icon()
{

        XGCValues valeurs;
        Arg  args[10];
	int n;

                /* creer le GC */
                /* recuperer les couleurs de la widget */

        n = 0;
        XtSetArg(args[n],XtNforeground,&valeurs.foreground); n++;
        XtSetArg(args[n],XtNbackground,&valeurs.background); n++;
        XtGetValues(icone_data->icon_spectrum,args,n);

               /* OU exclusif des valeurs d'affichage et de fond */

        valeurs.foreground = valeurs.foreground ^ valeurs.background;

                /* fonction XOR pour le GC */

        valeurs.line_style = FillSolid;

        valeurs.function = GXxor;
        valeurs.line_width = 1 ;

        icone_data->cadre_GC = XtGetGC(icone_data->icon_spectrum,
				GCForeground | GCBackground
                                | GCFunction| GCLineWidth
                                | GCLineStyle ,&valeurs);

		/* init flag cadre */

	icone_data->cadre_set = 0;
	icone_data->mv_cadre = 0;

}
/*-------------------------------------------------------------*/


void TRACE_CADRE_ICON(aff)
int *aff;
{

	float llx,lly,urx,ury;
	int   sizey,sizex;
	float tmp;

		/* test de l'existance d'un cadre */
		/* si oui, on l'efface	*/

	if(zoom_set == 0)return;
	if(w2d_open == 0 )return;
	if(icone_data->icon_set == 0)return;

	if(icone_data->cadre_set == 1){
	        XDrawRectangle(XtDisplay(window_mere),XtWindow(icone_data->icon_spectrum),
                        icone_data->cadre_GC,
			icone_data->cadre_x,icone_data->cadre_y,
		 	icone_data->cadre_larg,icone_data->cadre_haut);

			/* cadre efface */
	
		icone_data->cadre_set = 0;
	}

		/* affichage du cadre */

	if(*aff == 1){ 	/* on veut afficher le cadre */

		GET_ZOOM_COORD(&llx,&lly,&urx,&ury,&sizey,&sizex);

		tmp = (llx/sizex)*SizeIcon;
		icone_data->cadre_x = (int)tmp;
		tmp = ((sizey - ury)/sizey)*SizeIcon;
		icone_data->cadre_y = (int)tmp;
		icone_data->cadre_larg = (int)(((urx - llx)/sizex)*SizeIcon);
		icone_data->cadre_haut = (int)(((ury - lly)/sizey)*SizeIcon);

       		 XDrawRectangle(XtDisplay(window_mere),XtWindow(icone_data->icon_spectrum),
		       icone_data->cadre_GC,
                       icone_data->cadre_x,icone_data->cadre_y,
                       icone_data->cadre_larg,icone_data->cadre_haut);

			/* flag : cadre affiche */

		icone_data->cadre_set = 1;
	}

}

/*-------------------------------------------------------------
**     resize_pixmap
*/


void resize_pixmap (w, id_pixmap, call_data)
Widget          w;              /*  widget id           */
int      *id_pixmap;      /*  data from application   */
XmDrawingAreaCallbackStruct      *call_data;      /*  data from widget class  */
{

        Arg args[5];
        int n;
        Dimension largeur, hauteur;
        int err=0;
        int *l_comm_f;
        char comm_f[MAX_CHAR];



                /* lire les nouvelles coordonnees de la widget */

        n = 0;
        XtSetArg(args[n], XtNheight,&hauteur); n++ ;
        XtSetArg(args[n], XtNwidth,&largeur); n++ ;
        XtGetValues(w,args,n);


        XClearArea(XtDisplay(w),XtWindow(w),0,0,0,0,TRUE);

                /* liberer la vieille pixmap et creer la nouvelle */


        XFreePixmap(XtDisplay(window_mere),pixmap[*id_pixmap]);

        pixmap[*id_pixmap] = XCreatePixmap(XtDisplay(window_mere),
                RootWindowOfScreen(XtScreen(window_mere)),
                largeur,hauteur,depth);


        windoww[*id_pixmap] = largeur;
        windowh[*id_pixmap] = hauteur;

        WIN_ERASE(id_pixmap);
        DOREFRESH();

}


#define TICKMAX 50

/*-------------------------------------------------------------*/

void SET_AXIS(axes,vd_id)
int *axes; /* 0=Off,1=f1, 2=f2, 3=f12*/
int *vd_id;
/*

	calcule la position des ticks et affiche les axes
	en f1, f2, f12, selon les choix
int *axes; 	:0=Off,1=f1, 2=f2, 3=f12
int *vd_id:	windows

*/
{

	int current_win,dim,taille,dd,axe;
	float  x1,x2,y1,y2;
	int xindL,yindL,xindR,yindU;
	float xindLf,yindLf,xindRf,yindUf;
	char label[80];
	float valtick[TICKMAX],postick[TICKMAX];
	float currval,posval;
	int nbtick,i,prec;

	WHICH_DIM(&dim);		/* recup dim courante */


		/* recuperation des valeurs extremes en index*/
		/* et passage en unite courantes  : */

		/* on obtient :              */
		/*                           */
		/*   y2 ----------------     */
		/*     |                |    */
		/*     |                |    */
		/*     |                |    */
 		/*   y1|                |    */
		/*      ----------------     */
		/*      x1             x2    */


        x1 = 0.0;
        y1 = 0.0;
        x2 = 1.0;
        y2 = 1.0;
	dd = 2; /* axe f2 */
	WIN2INDEXR(&xindLf, &x1, &dim, &dd);
	WIN2INDEXR(&xindRf, &x2, &dim, &dd);
        xindLf = xindLf-0.5;
        xindRf = xindRf+0.5;

	CURRUNITR(&x1,&xindLf,&dd);
        CURRUNITR(&x2,&xindRf,&dd);

	dd = 1; /* axe f1 */
	WIN2INDEXR(&yindLf, &y1, &dim, &dd);
	WIN2INDEXR(&yindUf, &y2, &dim, &dd);
        yindLf = yindLf-0.5;
        yindUf = yindUf+0.5;

	CURRUNITR(&y1,&yindLf,&dd);
        CURRUNITR(&y2,&yindUf,&dd);
/*
printf("axes = %d , unit curr: x1 = %f, x2 = %f, y1 = %f, y2 = %f,\n",*axes,x1,x2,y1,y2);
*/



	if(((*axes == 1) || (*axes == 3)) && dim != 1){	/*calcul pour f1*/

                	/* nb de tick et positionnement */

		nbtick = NbTick;    /* nbre de tick desire */
        	calcule_tick(y1,y2,&valtick,&nbtick,&prec,1);  
		if(nbtick == -1){return;}  /* erreur */
	
               		 /* affichage coord 0-1 */
		axe = 1;
		for(i=0;i<=nbtick;i++){

				/* passage de coord unit courante en unite 0-1 */
			UNIT_2_WINCOORD(&valtick[i],&postick[i],&axe);

				/* creation label selon unite courante */	
			create_label(valtick[i],&label,&taille,prec);

				/* dessin */
			draw_tick(vd_id,axe,postick[i],&label,taille);
		}
	}
	if((*axes == 2) || (*axes == 3)){	/*calcul pour f2*/

                        /* nb de tick et positionnement */

		nbtick = NbTick;    /* nbre de tick desire */
                calcule_tick(x1,x2,&valtick,&nbtick,&prec,2); 
		if(nbtick == -1){return;}  /* erreur */

                         /* affichage coord 0-1 */
		axe = 2;
                for(i=0;i<=nbtick;i++){

                                /* passage de coord unit courante en unite 0-1 */
                        UNIT_2_WINCOORD(&valtick[i],&postick[i],&axe);

                                /* creation label selon unite courante */       
                        create_label(valtick[i],&label,&taille,prec);

				/* dessin */
                        draw_tick(vd_id,axe,postick[i],&label,taille);
                }
        }

			/* affichage */
	WIN_UPDATE(vd_id);
}
/*-------------------------------------------------------------*/
void create_label(valtick,label,lglabel,prec)
float valtick;
char *label;            
int *lglabel;
int prec;
/*
void create_label(float valtick,char *label,int *lglabel,int prec)

	Cree une chaine de caracteres contenant la valeur flotante 
	(valtick), avec une precision de "prec" apres la virgule.
	Retourne la chaine (label) et sa longeur (lglabel).


float valtick; 		valeur en unite courante 
char *label; 		label de retour 
int *lglabel;		longueur du label 
int prec;		precision apres la virgule

*/
{
	char chaine[10];


		/* creation chaine avec precision */

	sprintf(chaine,"%%.%df",prec);
	sprintf(label,chaine,valtick);
	*lglabel = strlen(label);

}
/******************************************************/
void draw_tick(vd_id,axe,pos,label,taille)
int *vd_id;
int axe;
float pos;
int taille;
char *label;
/*
void draw_tick(int *vd_id,int axe,float pos,char *label,int taille)

	plot tick at position pos with label .


int *vd_id;      Virtual display ID         
int axe; 	 1=f1, 2=f2 
float pos;       position sur tick sur l'axe   
int taille;	 taille du label
char *label;	 text to write

*/

{

      float y1,x2,y2,x1;

      if (pos >= 0.0 && pos <= 1.0) {
 	if(axe == 2){	
		y1 = 0.0; 
		x2 = pos; 
		y2 = 0.01;
        	WIN_PLOT_1D(vd_id,&pos,&y1,&x2,&y2);
        	pos = pos - ((taille/2)*0.01);
        	y2 = y2 + 0.005;
        	WIN_WRITE(vd_id,&pos,&y2,label,&taille);
	}

	if(axe == 1){
                x1 = 0.0; 
                y2 = pos; 
                x2 = 0.01;
                WIN_PLOT_1D(vd_id,&x1,&pos,&x2,&y2);
                x1 = x2 + 0.005;
                WIN_WRITE(vd_id,&x1,&y2,label,&taille);
        }
      }
}

/*-------------------------------------------------------------*/

void calcule_tick(x1,x2,valtick,nbtick,prec,axe)
float x1,x2; 		/* coord d'encadrement en unite courante */
float valtick[]; 	/* tab avec valeurs de tick */
int *nbtick; 		/* nb de tick desire, retourne le nbre reel de tick*/
int *prec;
int axe;                /* 1 ou 2 */
/*

	 Segmente une longueur (x2-x1) en un nombre defini (nbtick) de
	batonnet. Les valeurs sont mises dans un tableau (valtick). 
	Retourne la precision (prec) significative apres la virgule

float x1,x2 ->coord d'encadrement en unite courante
float valtick[] -> tab avec valeurs de tick
int *nbtick; -> nb de tick desire, retourne le nbre reel de tick, -1 si erreur
int *prec -> precision	
	

*/
{

	float maxval, minval,arrminval,longeur, step, arstep, startval, arstart;
	int i,n,z,lg;
	char  unit[8];
	char unitf[8];

	maxval = max(x1,x2);
	minval = min(x1,x2);
	longeur = maxval-minval;
	step = longeur / (*nbtick);
	arrondir(step,&arstep,prec);
	if(arstep == 0){*nbtick = -1; return;} /* longeur trop courte */

 	GET_CURRUNIT(unitf,&lg,&axe);          /* index toujours en entier */

	convert_string(unit,unitf,lg);
	if (strncmp(unit,"INDEX",5) == 0 && arstep <1.0) {arstep = 1.0; *prec = 0;}

	if (abs(x1)>abs(x2)) {                 /* le pt de depart est la plus 'grosse' */
	  startval = x1;
	} else {
	  startval = x2;
	}
	arrondir(startval,&arstart,&z);
	if(startval < 0){arstart = -arstart;} /* valeur negative */

	while(arstart < minval){arstart = arstart+arstep;}
	while(arstart > minval){arstart = arstart-arstep;}
	arstart = arstart+arstep;
	valtick[0] = arstart;
	i = 1;
	while((valtick[i-1]+arstep) < maxval && i < TICKMAX){
		valtick[i] = valtick[i-1] + arstep;
		*nbtick = i;
		i++;
	}
}
/*-------------------------------------------------------------*/

void arrondir(val,retval,prec)
float val;
float *retval;
int *prec;
/*

	arrondi la valeur absolue de 'val' a 10^n, 2*10^n, 5*10^n, le plus proche
	retourne l'arrondi dans retval
	retourne la precision (n) : prec

float val	: valeur a arrondir
float *retval	: valeur arrondie
int *prec	: nbre de chiffre apres la virgule

*/
{

float m,a,b,c,d,lastval;
int out1,out2,i,n,pass,neg;

/*
printf("val  = %f\n",val);
*/

		/* test de negativite */
  neg = 0;
  if(val < 0) { val = val-(2*val);neg = 1;}

		/*  cas particulier */
  if(val == 0.0){ *retval = 0.0; }
  else{
	n = 0;
	out1 = 0;
	*prec = 0;
	while(out1 == 0){
				/* calcul puissance */
		m = 1;
		if(n < 0) for(i=-1;i>=n;i--){m = m / 10;} /* powf(10,-n) */
		if(n > 0) for(i=1;i<=n;i++){m = m * 10;} /* powf(10,n) */
		a = m; b = 2*m; c = 5*m; d = 10*m;

				/* recherche puissance de val */

		if((a > val) && (b > val) && (c > val) && (d > val)){ 
			n--;			/* shift /10 a gauche */ 
			*prec = *prec + 1; 	/* calcul de la precision */
		}	

		else if((a < val) && (b < val) && (c < val) && (d < val)){ 
			n++; 			/* shift *10 a droite */
		}

		else { 
		    if(neg != 1){	/* la valeur a arrondir est positive */
                        if(a <= val){*retval = a;}      /* a val b c d */
                        if(b <= val){*retval = b;}	/* a b val c d */
                        if(c <= val){*retval = c;}	/* a b c val d */
			if(d <= val){*retval = d;}	/* a b c d val */
		    }
		    else{		/* la valeur a arrondir est negative */
                        if(d >= val){*retval = d;}	/* a b c val d */
                        if(c >= val){*retval = c;}	/* a b val c d */
                        if(b >= val){*retval = b;}	/* a val b c d */
                        if(a >= val){*retval = a;}	/* val a b c d */
		    }
                out1 = 1;
		}
	}
	/*
	  printf(" in = %f, a = %f, b  = %f, c = %f ,retval = %f, prec = %d\n",val,a,b,c,*retval,*prec);
	  */
  }
}
/*-------------------------------------------------------------*/
int window_dim(int vd_id)
     /* returns the window dim from the coding in windowd */
{
	if ( windowd[vd_id] == 22 ||  windowd[vd_id] == 21) return(2);
	if ( windowd[vd_id] == 1) return(1);
	if ( windowd[vd_id] == 3) return(3);
	return(0);
}
