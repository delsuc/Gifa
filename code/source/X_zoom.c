/*

     Authors :       M.A.Delsuc, and J-L Pons
                     C.B.S. Fac de Pharmacy
                     34000 Montpellier

This software cannot be used unless the user have been fully
licensed to do so form the above laboratory.
This file cannot be copied, duplicated or used in any other program,
without written permission from the authors.

X_zoom.c contains all the entries for managing mouse zooming
and more generaly all mouse interaction, including
-creating zoom box
-managing the 'vignette' spectrum
-viewfinder (assignment cross)
-POINT and MONOPOINT commads

Most of this is through call back assigned to windows
*/

/*-------------------------------------------------------------
**    include
*/
#ifndef HP
#  include <Xm/Xm.h>
#  include <Xm/DialogS.h>
#  include <stdio.h>
#  include <Xm/Separator.h>
#  include <X11/Xlib.h>
#  include <X11/Xutil.h>
#  include <X11/keysymdef.h>
#  include <X11/keysym.h>
#  include <Xm/BulletinB.h>
#  include <Xm/ArrowB.h>
#  include <Xm/Label.h>
#  include <Xm/PushB.h>
#  include <X11/cursorfont.h>
#  include <X11/Intrinsic.h>
#  include <X11/StringDefs.h>
#  include <Xm/DrawingA.h>
#  include <Xm/Protocols.h>
#  include <Xm/Form.h>
#  include <Xm/RowColumn.h>
#  include <Xm/Label.h>
#  include <Xm/TextF.h>
#  include <Xm/Frame.h>
#  include <Xm/FileSB.h>
#  include <Xm/Text.h>
#  include <Xm/ScrolledW.h>
#include <Xm/MwmUtil.h>
#else
# include <Xm/XmAll.h>
# include <X11/cursorfont.h>
#  include <stdio.h>
#  include <X11/keysym.h>
#  include <X11/keysymdef.h>
#  include <Xm/AtomMgr.h>
#  include <Xm/MwmUtil.h>
#endif

#if defined DARWIN
#    include <stdlib.h>
#elif defined MACHTEN
#  include <sys/malloc.h>
#else
#    include <malloc.h>
#endif

#include "X_windef.h"
#include "X_basic.h"
#include <X11/StringDefs.h>

#include "sizebasec.h"


/*-------------------------------------------------------------
**      forwarded functions
*/

void zoom_arrow_rupCB();
void zoom_arrow_lupCB();
void zoom_arrow_rlowCB();
void zoom_arrow_llowCB();
void zoom_arrow_upCB();
void ZOOM_CATCHCB();
void clear_infoCB();
void zoom_arrow_downCB();
void zoom_arrow_leftCB();
void zoom_arrow_rightCB();
void zoom_scale_mul2CB();
void zoom_scale_div2CB();
void zoom_scale_plus20CB();
void zoom_scale_moins20CB();
void zoom_scale_resetCB();
void zoom_zoominCB();
void zoom_zoomoutCB();
void zoom_reset();
void zoom_refreshCB();
void zoom_closeCB();
void CREATE_ZOOMBOX();
void debut_cadre_zoom();
void trace_cadre_zoom();
void update_mouse_coord();
void UPDATE_LABEL_DIM();
void stock_mouse_coord();
void mouse_coordCB();
void fin_cadre_zoom();
void delete_last_cadre_zoom();
void OFF_cadre_zoom();
void Zoom_in();
void Zoom_out();
void monopoint();
void set_dim3CB();
void set_dim1CB();
void set_dim2CB();
void Enregistrer_bmp();
void gestion_bouton();
int window_dim(int );

/*-------------------------------------------------------------
**      Dessin button
*/

/*
static unsigned char arrow_ur[24] = {
   0x00, 0x00, 0x01, 0x00, 0x03, 0x00, 0x07, 0x00, 0x0f, 0x00, 0x1f, 0x00,
   0x3f, 0x00, 0x7f, 0x00, 0xff, 0x00, 0xff, 0x01, 0xff, 0x03, 0xff, 0x07};


static unsigned char arrow_ll[24] = {
   0xfe, 0x0f, 0xfc, 0x0f, 0xf8, 0x0f, 0xf0, 0x0f, 0xe0, 0x0f, 0xc0, 0x0f,
   0x80, 0x0f, 0x00, 0x0f, 0x00, 0x0e, 0x00, 0x0c, 0x00, 0x08, 0x00, 0x00};


static unsigned  char arrow_lr[24] = {
   0xff, 0x07, 0xff, 0x03, 0xff, 0x01, 0xff, 0x00, 0x7f, 0x00, 0x3f, 0x00,
   0x1f, 0x00, 0x0f, 0x00, 0x07, 0x00, 0x03, 0x00, 0x01, 0x00, 0x00, 0x00};


static unsigned char arrow_ul[24] = {
   0x00, 0x00, 0x00, 0x08, 0x00, 0x0c, 0x00, 0x0e, 0x00, 0x0f, 0x80, 0x0f,
   0xc0, 0x0f, 0xe0, 0x0f, 0xf0, 0x0f, 0xf8, 0x0f, 0xfc, 0x0f, 0xfe, 0x0f};
*/

/*
static unsigned char arrow_ur[24] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
*/

static unsigned char arrow_ur[24] = {
   0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
   0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff};



static unsigned char arrow_ll[24] = {
   0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
   0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff};



static unsigned  char arrow_lr[24] = {
   0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
   0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff};




static unsigned char arrow_ul[24] = {
   0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
   0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff};


static char *liste[]={
	"arrow-ur","arrow-ul", 
	"arrow-lr","arrow-ll",}; 



/*-------------------------------------------------------------
*		Variables Globales
*/

#define max(A,B)  ((A) > (B) ? (A) : (B) )
#define min(A,B)  ((A) > (B) ? (B) : (A) )


/*-------------------------------------------------------------
*     Variables externes
*/

extern int versionx;
extern int flag_bloquant;
extern XtAppContext app_context;
extern Widget window_mere;
extern Cursor cursor1,cursor2,cursor_wait;
extern Widget widget[NWindow];
extern int win[NWindow];
extern int windoww[NWindow];
extern int windowh[NWindow];
extern short windowt[NWindow];
extern Zoom_cadre cadre_zoom[NWindow];
extern viseur_att viewfinder[NWindow];
int zoom_wind_id;
Widget zoom_set;
int set_button1;
int set_button2;
int set_button3;
int zm_shift_key;
int zm_button1;
int zm_button2;
int zm_button3;
int zm_motion;
int flag_cadre_zoom;
KeyCode  shift_L,shift_R;
extern int sortie_boucle_phase;
/*-------------------------------------------------------------
*               Variables Globales
*/

typedef struct {
	Widget  label_intens;
	Widget	label_coord_x;
	Widget 	label_coord_y;
	Widget  label_dim;
	Widget	dim1;
	Widget	dim2;
	Widget  dim3;
} Label_zoombox;

Label_zoombox label_zoombox;

int point_flag;

        /* ------------- FONCTIONS GENERALES ---------------- */



/*-------------------------------------------------------------
**         create_zoombox
*/

void CREATE_ZOOMBOX()
/*
	Creation de la boite de zoom
	et des differents boutons:
		zoom
		shift
		scale...
*/
{
    Widget      toplevel_zoom,toplevel_box,arrow[8],scale_but[5],title[5],
		zoom_in,zoom_out,zoom_rst,close,refresh,frame[4],label[4],
		intens,coord_x,coord_y,labdim,form_zoom,form_root,button_dim1,
		button_dim2,button_dim3,dim_menu,form_dim,catch;

    char dimlab[50];
    XmString xms,strfont;
    XmString        label_string = NULL;
    Arg arg;
    Arg args[30];
    int n, dpy,position_x,dim;
    unsigned int  width_screen;
    int screen,flag;
    Display *display;
    char st[80];
    int argc;
    Atom wm_delete_window,wm_resize_window;
    char position[50];
    Pixmap pavage[4];
    Pixel encre, fond;
    XFontStruct *font;
    char *fontname="math.6x8";
    XmFontList fontlist = NULL;
    int font_ok;

    


                        /* test du display */

      if (versionx == 0){
                 printf("Gifa cannot open the display on X server\n");
                return;
      }


                       /* creation du shell du widget zoom */

   if(zoom_set == 0){

			/* recherche de la position de la boite */

        display = XtDisplay(window_mere);
        screen = DefaultScreen(display);
        width_screen = DisplayWidth(display,screen);
        position_x = (width_screen - 150);
	sprintf(position,"+%d+1",position_x);

			/* creation */

	n = 0;
	XtSetArg(args[n], XtNgeometry,position); n++ ;
        toplevel_zoom = XtCreatePopupShell ("Zoom Box", topLevelShellWidgetClass,
                        window_mere, args,n);

	add_shell_on_display(toplevel_zoom);

                        /* Elimination  de la commande Resize */


         wm_resize_window = XmInternAtom(XtDisplay(toplevel_zoom),
                                "WM_SIZE_WINDOW",
                                FALSE);
         XmRemoveWMProtocols(toplevel_zoom,&wm_resize_window, 1);

	
                        /* Icone de presentation */

        SetAppIcon(toplevel_zoom,2);


                        /* Create BulletinBoard box avec taille . */

	n = 0;
 	XtSetArg(args[n], XmNheight,655);n++;	
	XtSetArg(args[n], XmNwidth, 150);n++;
        XtSetArg (args[n], XmNnoResize, True);n++;
        toplevel_box = XmCreateBulletinBoard (toplevel_zoom, "work_area", args, n);

/*
    if (XmIsMotifWMRunning(toplevel_box)) {
        MwmHints     mwm_set_hints ;
        Atom         mwm_hints ;
        XWMHints     wm_set_hints;
        XUnmapEvent  Unmap_ev;
        mwm_hints = XmInternAtom(XtDisplay(toplevel_box), "_MOTIF_WM_HINTS", False);
        mwm_set_hints.flags = MWM_HINTS_FUNCTIONS ;
        mwm_set_hints.functions = MWM_FUNC_ALL | MWM_FUNC_RESIZE ;
        XChangeProperty(XtDisplay(toplevel_box),XtWindow(toplevel_box),
                        mwm_hints,mwm_hints, 32, PropModeReplace,
                        (unsigned char *)&mwm_set_hints, sizeof(MwmHints)/sizeof(int));
    }
*/

	XtManageChild(toplevel_box);




			/* Create Arrows */

        n = 0;
	XtSetArg(args[n], XmNx,60);n++;
        XtSetArg(args[n], XmNy,45);n++;

        title[0] = XmCreateLabel(toplevel_box, "Shift", args, n);
        XtManageChild (title[0]);

/*FONT ARROW*/

	font = XLoadQueryFont(display,fontname);
	if  (!font){
	   printf ("Unable to load font for arrows on zoombox: %s\n", fontname);
	   font_ok = -1;
	}
	else{
	   fontlist = XmFontListCreate (font, (XmStringCharSet) XmSTRING_DEFAULT_CHARSET);
	   font_ok = 1;
	}



        n = 0;
        XtSetArg(args[n], XmNx,65);n++;
        XtSetArg(args[n], XmNy,10);n++;
	if(font_ok == 1){
        	XtSetArg(args[n],XmNwidth,22); n++;
        	XtSetArg(args[n],XmNheight,22); n++;
		XtSetArg (args[n], XmNfontList, fontlist);  n++;
        	arrow[0] = XmCreatePushButton(toplevel_box,"0",args, n);
	}
	else{
	        XtSetArg(args[n], XmNarrowDirection,XmARROW_UP);n++;
        	arrow[0] = XmCreateArrowButton(toplevel_box,"up",args, n);
	}
        XtManageChild(arrow[0]);
        XtAddCallback (arrow[0], XmNactivateCallback, zoom_arrow_upCB,
                     NULL);



	n = 0;
        XtSetArg(args[n], XmNx,65);n++;
        XtSetArg(args[n], XmNy,70);n++;
	if(font_ok == 1){
        	XtSetArg(args[n],XmNwidth,22); n++;
        	XtSetArg(args[n],XmNheight,22); n++;
		XtSetArg (args[n], XmNfontList, fontlist);  n++;
		arrow[1] = XmCreatePushButton(toplevel_box,"4",args, n);
	}
	else{
	        XtSetArg(args[n], XmNarrowDirection,XmARROW_DOWN);n++;
        	arrow[1] = XmCreateArrowButton(toplevel_box,"Down",args, n);
	}
        XtManageChild(arrow[1]);
        XtAddCallback (arrow[1], XmNactivateCallback, zoom_arrow_downCB,
                     NULL);


        n = 0;
        XtSetArg(args[n], XmNx,20);n++;
        XtSetArg(args[n], XmNy,40);n++;
	if(font_ok == 1){
        	XtSetArg(args[n],XmNwidth,22); n++;
        	XtSetArg(args[n],XmNheight,22); n++;
		XtSetArg (args[n], XmNfontList, fontlist);  n++;
		arrow[2] = XmCreatePushButton(toplevel_box,"6",args, n);
	}
	else{
        XtSetArg(args[n], XmNarrowDirection,XmARROW_LEFT);n++;
        arrow[2] = XmCreateArrowButton(toplevel_box,"Left",args, n);
	}
        XtManageChild(arrow[2]);
        XtAddCallback (arrow[2], XmNactivateCallback, zoom_arrow_leftCB,
                     NULL);


        n = 0;
        XtSetArg(args[n], XmNx,110);n++;
        XtSetArg(args[n], XmNy,40);n++;
	if(font_ok == 1){
        	XtSetArg(args[n],XmNwidth,22); n++;
        	XtSetArg(args[n],XmNheight,22); n++;
		XtSetArg (args[n], XmNfontList, fontlist);  n++;
		arrow[3] = XmCreatePushButton(toplevel_box,"2",args, n);
	}
	else{
        	XtSetArg(args[n], XmNarrowDirection,XmARROW_RIGHT);n++;
        	arrow[3] = XmCreateArrowButton(toplevel_box,"Right",args, n);
	}
	XtManageChild(arrow[3]);
        XtAddCallback (arrow[3], XmNactivateCallback, zoom_arrow_rightCB,
                     NULL);


		/* Fleches obliques */

	
	n = 0;
	XtSetArg(args[n], XmNx,110);n++;
	XtSetArg(args[n], XmNy,10);n++;
        XtSetArg(args[n],XmNwidth,22); n++;
        XtSetArg(args[n],XmNheight,22); n++;
	if(font_ok == 1){
		XtSetArg (args[n], XmNfontList, fontlist);  n++;
		arrow[4] = XmCreatePushButton(toplevel_box,"1",args, n);
	}
	else
		arrow[4] = XmCreatePushButton(toplevel_box," ",args, n);
	XtManageChild(arrow[4]);
	XtAddCallback (arrow[4], XmNactivateCallback, zoom_arrow_rupCB,
                     NULL);


	n = 0;
	XtSetArg(args[n], XmNx,20);n++;
	XtSetArg(args[n], XmNy,10);n++;
        XtSetArg(args[n],XmNwidth,22); n++;
        XtSetArg(args[n],XmNheight,22); n++;
	if(font_ok == 1){
		XtSetArg (args[n], XmNfontList, fontlist);  n++;
		arrow[5] = XmCreatePushButton(toplevel_box,"7",args, n);
	}
	else
		arrow[5] = XmCreatePushButton(toplevel_box,"",args, n);
        XtManageChild(arrow[5]);
        XtAddCallback (arrow[5], XmNactivateCallback, zoom_arrow_lupCB,
                     NULL);


        n = 0;
        XtSetArg(args[n], XmNx,110);n++;
        XtSetArg(args[n], XmNy,70);n++;
        XtSetArg(args[n],XmNwidth,22); n++;
        XtSetArg(args[n],XmNheight,22); n++;
	if(font_ok == 1){
		XtSetArg (args[n], XmNfontList, fontlist);  n++;
        	arrow[6] = XmCreatePushButton(toplevel_box,"3",args, n);
	}
	else
		arrow[6] = XmCreatePushButton(toplevel_box," ",args, n);
        XtManageChild(arrow[6]);
        XtAddCallback (arrow[6], XmNactivateCallback, zoom_arrow_rlowCB,
                     NULL);



        n = 0;
        XtSetArg(args[n], XmNx,20);n++;
        XtSetArg(args[n], XmNy,70);n++;
        XtSetArg(args[n],XmNwidth,22); n++;
        XtSetArg(args[n],XmNheight,22); n++;
	if(font_ok == 1){
		XtSetArg (args[n], XmNfontList, fontlist);  n++;
        	arrow[7] = XmCreatePushButton(toplevel_box,"5",args, n);
	}
	else
		arrow[7] = XmCreatePushButton(toplevel_box," ",args, n);
        XtManageChild(arrow[7]);
        XtAddCallback (arrow[7], XmNactivateCallback, zoom_arrow_llowCB,
                     NULL);




			/* Realize Scale */

        n = 0;
        XtSetArg(args[n], XmNx,15);n++;
        XtSetArg(args[n], XmNy,105);n++;
        title[1] = XmCreateLabel(toplevel_box, "Scale", args, n);
        XtManageChild (title[1]);


        n = 0;
        XtSetArg(args[n],XmNwidth,65); n++;
        XtSetArg(args[n],XmNheight,30); n++;
        XtSetArg(args[n], XmNx,5);n++;
        XtSetArg(args[n], XmNy,125);n++;
	scale_but[0] = XmCreatePushButton(toplevel_box,"/2",args, n);
	XtManageChild(scale_but[0]);
        XtAddCallback (scale_but[0], XmNactivateCallback, zoom_scale_div2CB,
                     NULL);


        n = 0;
        XtSetArg(args[n],XmNwidth,65); n++;
        XtSetArg(args[n],XmNheight,30); n++;
        XtSetArg(args[n], XmNx,75);n++;
        XtSetArg(args[n], XmNy,125);n++;
        scale_but[1] = XmCreatePushButton(toplevel_box,"X2",args, n);
        XtManageChild(scale_but[1]);
        XtAddCallback (scale_but[1], XmNactivateCallback, zoom_scale_mul2CB,
                     NULL);


        n = 0;
        XtSetArg(args[n],XmNwidth,65);n++;
        XtSetArg(args[n],XmNheight,30); n++;
        XtSetArg(args[n], XmNx,5);n++;
        XtSetArg(args[n], XmNy,160);n++;
        scale_but[2] = XmCreatePushButton(toplevel_box,"-20%",args, n);
        XtManageChild(scale_but[2]);
        XtAddCallback (scale_but[2], XmNactivateCallback, zoom_scale_moins20CB,
                     NULL);


        n = 0;
        XtSetArg(args[n],XmNwidth,65); n++;
        XtSetArg(args[n],XmNheight,30); n++;
        XtSetArg(args[n], XmNx,75);n++;
        XtSetArg(args[n], XmNy,160);n++;
        scale_but[3] = XmCreatePushButton(toplevel_box,"+20%",args, n);
        XtManageChild(scale_but[3]);
        XtAddCallback (scale_but[3], XmNactivateCallback, zoom_scale_plus20CB,
                     NULL);

			/* create Graphic */

        n = 0;
        XtSetArg(args[n], XmNx,15);n++;
        XtSetArg(args[n], XmNy,200);n++;
        title[4] = XmCreateLabel(toplevel_box, "Graphic", args, n);
        XtManageChild (title[4]);

        n = 0;
        XtSetArg(args[n],XmNwidth,65); n++;
        XtSetArg(args[n],XmNheight,30); n++;
        XtSetArg(args[n], XmNx,5);n++;
        XtSetArg(args[n], XmNy,220);n++;
        XtSetArg(args[n], XmNrecomputeSize,False);n++;
        scale_but[4] = XmCreatePushButton(toplevel_box,"Reset",args, n);
        XtManageChild(scale_but[4]);
        XtAddCallback (scale_but[4], XmNactivateCallback, zoom_scale_resetCB,
                     NULL);


        n = 0;
        XtSetArg(args[n],XmNwidth,65); n++;
        XtSetArg(args[n],XmNheight,30); n++;
        XtSetArg(args[n], XmNx,75);n++;
        XtSetArg(args[n], XmNy,220);n++;
        XtSetArg(args[n], XmNrecomputeSize,False);n++;
        zoom_rst = XmCreatePushButton(toplevel_box,"Refresh",args, n);
        XtManageChild(zoom_rst);
        XtAddCallback (zoom_rst, XmNactivateCallback, clear_infoCB,
                     NULL);



			/* create button zoom */

        n = 0;
        XtSetArg(args[n], XmNx,15);n++;
        XtSetArg(args[n], XmNy,260);n++;
        title[3] = XmCreateLabel(toplevel_box, "Zoom", args, n);
        XtManageChild (title[3]);


        n = 0;
	XtSetArg(args[n],XmNwidth,65); n++;
        XtSetArg(args[n],XmNheight,30); n++;
        XtSetArg(args[n], XmNx,5);n++;
        XtSetArg(args[n], XmNy,280);n++;
        zoom_in = XmCreatePushButton(toplevel_box,"IN",args, n);
        XtManageChild(zoom_in);
        XtAddCallback (zoom_in, XmNactivateCallback, zoom_zoominCB,NULL);


        n = 0;
	XtSetArg(args[n],XmNwidth,65); n++;
        XtSetArg(args[n],XmNheight,30); n++;
        XtSetArg(args[n], XmNx,75);n++;
        XtSetArg(args[n], XmNy,280);n++;
        zoom_out = XmCreatePushButton(toplevel_box,"OUT",args, n);
        XtManageChild(zoom_out);
        XtAddCallback (zoom_out, XmNactivateCallback, zoom_zoomoutCB,
                     NULL);

        n = 0;
        XtSetArg(args[n],XmNwidth,130); n++;
        XtSetArg(args[n],XmNheight,25); n++;
        XtSetArg(args[n], XmNx,5);n++;
        XtSetArg(args[n], XmNy,315);n++;
        XtSetArg(args[n], XmNrecomputeSize,False);n++;
        zoom_rst = XmCreatePushButton(toplevel_box,"Full",args, n);
        XtManageChild(zoom_rst);
        XtAddCallback (zoom_rst, XmNactivateCallback, zoom_reset,
                     NULL);



	n = 0;
        XtSetArg(args[n],XmNwidth,110); n++;
        XtSetArg(args[n],XmNheight,30); n++;
        XtSetArg(args[n], XmNx,15);n++;
        XtSetArg(args[n], XmNy,350);n++;

	XtSetArg (args[n], XmNshadowType, XmSHADOW_ETCHED_IN );  n++;
        XtSetArg (args[n], XmNtopWidget,zoom_rst); n++;
        XtSetArg (args[n], XmNtopAttachment, XmATTACH_WIDGET);  n++;
        XtSetArg (args[n], XmNrightAttachment, XmATTACH_FORM);  n++;
        XtSetArg (args[n], XmNleftAttachment, XmATTACH_FORM);  n++;
	form_dim = XmCreateForm(toplevel_box,"",args, n);
	XtManageChild (form_dim);

	n = 0;
	labdim = XmCreatePulldownMenu(form_dim,"",args, n);

    	n = 0;
    	button_dim1 = XmCreatePushButton (labdim, "1D", args, n);
    	XtManageChild (button_dim1);
	label_zoombox.dim1 = button_dim1;
	XtAddCallback (button_dim1, XmNactivateCallback, set_dim1CB, NULL);
	button_dim2 = XmCreatePushButton (labdim, "2D", args, n);
        XtManageChild (button_dim2);
	label_zoombox.dim2 = button_dim2;
        XtAddCallback (button_dim2, XmNactivateCallback, set_dim2CB, NULL);
	button_dim3 = XmCreatePushButton (labdim, "3D", args, n);
        XtManageChild (button_dim3);
	label_zoombox.dim3 = button_dim3;
        XtAddCallback (button_dim3, XmNactivateCallback, set_dim3CB, NULL);
	label_string = XmStringCreateLtoR ("Dim",XmSTRING_DEFAULT_CHARSET);

        n = 0;
        XtSetArg (args[n], XmNlabelString, label_string);  n++;
        XtSetArg (args[n], XmNmenuHistory, button_dim1);  n++;
        XtSetArg (args[n], XmNsubMenuId, labdim);  n++;
        dim_menu = XmCreateOptionMenu (form_dim, "row_column1", args, n);
	XmStringFree(label_string);
	label_zoombox.label_dim = dim_menu;
        XtManageChild (dim_menu);


        n = 0;
        XtSetArg(args[n],XmNwidth,110); n++;
        XtSetArg(args[n],XmNheight,25); n++;
        XtSetArg(args[n], XmNx,5);n++;
        XtSetArg(args[n], XmNy,390);n++;
        XtSetArg(args[n], XmNrecomputeSize,False);n++;
        coord_y = XmCreateLabel(toplevel_box,"F1:                       .",args, n);
        XtManageChild(coord_y);
        label_zoombox.label_coord_y = coord_y;

        n = 0;
        XtSetArg(args[n],XmNwidth,110); n++;
        XtSetArg(args[n],XmNheight,25); n++;
        XtSetArg(args[n], XmNx,5);n++;
        XtSetArg(args[n], XmNy,420);n++;
        XtSetArg(args[n], XmNrecomputeSize,False);n++;
        coord_x = XmCreateLabel(toplevel_box,"F2:                      .",args, n);
        XtManageChild(coord_x);
        label_zoombox.label_coord_x = coord_x;

        n = 0;
        XtSetArg(args[n],XmNwidth,110); n++;
        XtSetArg(args[n],XmNheight,25); n++;
        XtSetArg(args[n], XmNx,5);n++;
        XtSetArg(args[n], XmNy,450);n++;
        XtSetArg(args[n], XmNrecomputeSize,False);n++;
        intens = XmCreateLabel(toplevel_box,"Int:                   .",args, n);
        XtManageChild(intens);
        label_zoombox.label_intens = intens;



                        /* icon du spectre */
        create_pixmap_icon(toplevel_box,0,480);


        n = 0;
        XtSetArg(args[n],XmNwidth,130); n++;
        XtSetArg(args[n],XmNheight,25); n++;
        XtSetArg(args[n], XmNx,5);n++;
        XtSetArg(args[n], XmNy,620);n++;
        XtSetArg(args[n], XmNrecomputeSize,False);n++;
        catch = XmCreatePushButton(toplevel_box,"Catch Spectrum",args, n);
        XtManageChild(catch);
        XtAddCallback (catch, XmNactivateCallback, ZOOM_CATCHCB,
                     NULL);


                        /* Detournement de la commande CLOSE generale */
                        /* vers Close de la Widget                    */


         wm_delete_window = XmInternAtom(XtDisplay(toplevel_zoom),
                                "WM_DELETE_WINDOW",
                                FALSE);
         XmRemoveWMProtocols(toplevel_zoom,&wm_delete_window, 1);
         XmAddWMProtocolCallback(toplevel_zoom, wm_delete_window, zoom_closeCB, 
				(XtPointer)toplevel_zoom);

			/* Realise Fenetre  generale */


        XtRealizeWidget(toplevel_zoom);
	XtPopup (toplevel_zoom, XtGrabNone);
	zoom_set = toplevel_zoom;


			/* set du label DIM */
	WHICH_DIM(&dim);
	UPDATE_LABEL_DIM(&dim);

			/* init icon display */

	map_logo();

    }
    else{
	 XtPopup(zoom_set,XtGrabNone);
    }

}



        /* ------------- CALLBACK WIDGET ZOOM ---------------- *

-------------------------------------------------------------
**zoom_arrow_llowCB
*/
void zoom_arrow_llowCB(w, client_data, call_data)
Widget          w;              /*  widget id           */
XtPointer       client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{

        int orient;
        int err,on;

        orient = 2;
        SHIFT_IMAGE(&zoom_wind_id,&orient);
        orient = 4;
        SHIFT_IMAGE(&zoom_wind_id,&orient);
        DOREFRESH();
        WIN_REFRESH();

        on = 1;
        TRACE_CADRE_ICON(&on);



}

/*-------------------------------------------------------------
**zoom_arrow_rlowCB
*/
void zoom_arrow_rlowCB(w, client_data, call_data)
Widget          w;              /*  widget id           */
XtPointer       client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{

        int orient;
        int err,on;

        orient = 2;
        SHIFT_IMAGE(&zoom_wind_id,&orient);
        orient = 3;
        SHIFT_IMAGE(&zoom_wind_id,&orient);
        DOREFRESH();
        WIN_REFRESH();

        on = 1;
        TRACE_CADRE_ICON(&on);



}
/*-------------------------------------------------------------
**zoom_arrow_rupCB
*/
void zoom_arrow_rupCB(w, client_data, call_data)
Widget          w;              /*  widget id           */
XtPointer       client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{

        int orient;
        int err,on;

        orient = 1;
        SHIFT_IMAGE(&zoom_wind_id,&orient);
        orient = 3;
        SHIFT_IMAGE(&zoom_wind_id,&orient);
        DOREFRESH();
        WIN_REFRESH();

        on = 1;
        TRACE_CADRE_ICON(&on);



}
/*-------------------------------------------------------------
**zoom_arrow_lupCB
*/
void zoom_arrow_lupCB(w, client_data, call_data)
Widget          w;              /*  widget id           */
XtPointer       client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{

        int orient;
        int err,on;

        orient = 1;
        SHIFT_IMAGE(&zoom_wind_id,&orient);
        orient = 4;
        SHIFT_IMAGE(&zoom_wind_id,&orient);
        DOREFRESH();
        WIN_REFRESH();

        on = 1;
        TRACE_CADRE_ICON(&on);



}




/*-------------------------------------------------------------
**	zoom_arrow_upCB      
*/
void zoom_arrow_upCB(w, client_data, call_data)
Widget          w;              /*  widget id           */
XtPointer       client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{
	int orient;
	char command;
        int err,on;
        int *l_comm_f;
        char comm_f[MAX_CHAR];

	orient = 1;
	SHIFT_IMAGE(&zoom_wind_id,&orient);
	DOREFRESH();
        WIN_REFRESH();

	on = 1;
	TRACE_CADRE_ICON(&on);

}

/*-------------------------------------------------------------
**      zoom_arrow_downCB
*/
void zoom_arrow_downCB(w, client_data, call_data)
Widget          w;              /*  widget id           */
XtPointer       client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{

        int orient;
        char command;
        int err,on;
        int *l_comm_f;
        char comm_f[MAX_CHAR];


        orient = 2;
	SHIFT_IMAGE(&zoom_wind_id,&orient);
	DOREFRESH();
        WIN_REFRESH();

        on = 1;
        TRACE_CADRE_ICON(&on);

}

/*-------------------------------------------------------------
**      zoom_arrow_leftCB
*/
void zoom_arrow_leftCB(w, client_data, call_data)
Widget          w;              /*  widget id           */
XtPointer       client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{

        int orient;
        char command;
        int err,on;
        int *l_comm_f;
        char comm_f[MAX_CHAR];

        orient = 4;
	SHIFT_IMAGE(&zoom_wind_id,&orient);
	DOREFRESH();
        WIN_REFRESH();
        on = 1;
        TRACE_CADRE_ICON(&on);

}

/*-------------------------------------------------------------
**      zoom_arrow_rightCB
*/
void zoom_arrow_rightCB(w, client_data, call_data)
Widget          w;              /*  widget id           */
XtPointer       client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */

{
        int orient;
        char command;
        int err,on;
        int *l_comm_f;
        char comm_f[MAX_CHAR];

        orient = 3;
	SHIFT_IMAGE(&zoom_wind_id,&orient);
	DOREFRESH();
        WIN_REFRESH();
        on = 1;
        TRACE_CADRE_ICON(&on);

}

/*-------------------------------------------------------------
**      zoom_scale_mul2CB
*/
void zoom_scale_mul2CB(w, client_data, call_data)
Widget          w;              /*  widget id           */
XtPointer       client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{
        char command;
        int err,scale;
        int *l_comm_f;
        char comm_f[MAX_CHAR];

	scale = 2;	/* scale*2 */
	SET_SCALE(&scale);
	DOREFRESH();

}

/*-------------------------------------------------------------
**     zoom_scale_div2CB 
*/
void zoom_scale_div2CB(w, client_data, call_data)
Widget          w;              /*  widget id           */
XtPointer       client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{
        char command;
        int err,scale;
        int *l_comm_f;
        char comm_f[MAX_CHAR];

	scale = 1;      /* scale/2 */
        SET_SCALE(&scale);
	DOREFRESH();
}

/*-------------------------------------------------------------
**      zoom_scale_plus20CB
*/
void zoom_scale_plus20CB(w, client_data, call_data)
Widget          w;              /*  widget id           */
XtPointer       client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{
        char command;
        int err,scale;
        int *l_comm_f;
        char comm_f[MAX_CHAR];

        scale = 4;      /* scale + 20% */
        SET_SCALE(&scale);
	DOREFRESH();
}

/*-------------------------------------------------------------
**     zoom_scale_moins20CB 
*/
void zoom_scale_moins20CB(w, client_data, call_data)
Widget          w;              /*  widget id           */
XtPointer       client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{
        char command;
        int err,scale;
        int *l_comm_f;
        char comm_f[MAX_CHAR];

        scale = 3;      /* scale -20 % */
        SET_SCALE(&scale);
	DOREFRESH();
}
/*-------------------------------------------------------------
**     zoom_scale_resetCB
*/
void zoom_scale_resetCB(w, client_data, call_data)
Widget          w;              /*  widget id           */
XtPointer       client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{
        char command;
        int err,off;
        int *l_comm_f;
        char comm_f[MAX_CHAR];

	RESET_ALL();
	DOREFRESH();
	off = 0;
	TRACE_CADRE_ICON(&off);

}


/*-------------------------------------------------------------
**      zoom_closeCB
*/
void zoom_closeCB(w, client_data, call_data)
Widget          w;              /*  widget id           */
XtPointer       client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{
	XtDestroyWidget(client_data);
	zoom_set = 0;
	del_shell_on_display(client_data);
}


/*-------------------------------------------------------------
**     zoom_zoominCB 
*/
void zoom_zoominCB(w,client_data, call_data)
Widget          w;              /*  widget id           */
XtPointer       client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{
Zoom_in(w);
}

/*-------------------------------------------------------------
**     Zoom_in
*/
void Zoom_in(w)
Widget          w;              /*  widget id           */
{
	float x1,x2,y1,y2;
	float urx,ury,llx,lly;
	int oux,ouy,olx,oly;

	int zoom,on;
        int err,i;
        int *l_comm_f;
        char comm_f[MAX_CHAR];

   if(zoom_wind_id != 0 && cadre_zoom[zoom_wind_id].cadre_ON != 0){

        x1 = (float)(cadre_zoom[zoom_wind_id].x_fin)/windoww[zoom_wind_id];
        y1 = (1.0-((float)(cadre_zoom[zoom_wind_id].y_fin)/windowh[zoom_wind_id]));

        x2 = (float)(cadre_zoom[zoom_wind_id].x_debut)/windoww[zoom_wind_id];
        y2 = (1.0-((float)(cadre_zoom[zoom_wind_id].y_debut)/windowh[zoom_wind_id]));

	urx = max(x1,x2);   llx = min(x1,x2);
	ury = max(y1,y2);   lly = min(y1,y2);

/*
printf(" zoomin urx = %f, ury  = %f, llx = %f, lly = %f\n",urx,ury,llx,lly);
*/

	zoom = 1;
	SET_ZOOM_PARAM(&zoom_wind_id,&zoom,&lly, &llx, &ury, &urx);
	DOREFRESH();
    }

                        /* efface cadre de zoom*/
                        /* dans chacune des fenetres */


        for ( i=1; i < NWindow; i++){
        	if(win[i] != 0)
            	cadre_zoom[i].cadre_ON = 0;
        }
	WIN_REFRESH();
        on = 1;
        TRACE_CADRE_ICON(&on);

}

/*-------------------------------------------------------------
**     set_area_var
*/
void set_area_var()
{
        float x1,x2,y1,y2;
        float urx,ury,llx,lly;
        int oux,ouy,olx,oly;

        int zoom,on;
        int err,i;
        int *l_comm_f;
        char comm_f[MAX_CHAR];

   if(zoom_wind_id != 0 && cadre_zoom[zoom_wind_id].cadre_ON != 0){
        x1 = (float)(cadre_zoom[zoom_wind_id].x_fin)/windoww[zoom_wind_id];
        y1 = (1.0-((float)(cadre_zoom[zoom_wind_id].y_fin)/windowh[zoom_wind_id]));

        x2 = (float)(cadre_zoom[zoom_wind_id].x_debut)/windoww[zoom_wind_id];
        y2 = (1.0-((float)(cadre_zoom[zoom_wind_id].y_debut)/windowh[zoom_wind_id]));

        urx = max(x1,x2);   llx = min(x1,x2);
        ury = max(y1,y2);   lly = min(y1,y2);

/*
printf("urx = %f, ury  = %f, llx = %f, lly = %f\n",urx,ury,llx,lly);
*/

        SET_SELECT_VAR(&lly, &llx, &ury, &urx);
    }

}



/*-------------------------------------------------------------
**      zoom_zoomoutCB
*/
void zoom_zoomoutCB(w, client_data, call_data)
Widget          w;              /*  widget id           */
XtPointer       client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{
Zoom_out(w);
}

/*-------------------------------------------------------------
**     Zoom_out
*/
void Zoom_out(w)
Widget          w;              /*  widget id           */
/*
	Zoom_out permet un recul dans le zoom
	Il double la fenetre
*/
{
        int err,i,on;
        int *l_comm_f;
        char comm_f[MAX_CHAR];

	ZOOM_PARAM_RETURN();
	DOREFRESH();
    	WIN_REFRESH();
        on = 1;
        TRACE_CADRE_ICON(&on);

}


/*-------------------------------------------------------------
**      zoom_reset
*/
void zoom_reset(w, client_data, call_data)
Widget          w;              /*  widget id           */
XtPointer       client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
/*
	remet zoom a zero
*/
{
        char command;
        int err,off;
        int *l_comm_f;
        char comm_f[MAX_CHAR];

	SET_ZOOM_ZERO();
	DOREFRESH();
	off = 0;
	TRACE_CADRE_ICON(&off);

}
/*-------------------------------------------------------------
**      ZOOM_CATCHCB
*/
void ZOOM_CATCHCB(w, client_data, call_data)
Widget          w;              /*  widget id           */
XtPointer       client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
/*
catch a spectrum
*/
{

	int flag;
	flag = 0;
	TRACE_CADRE_ICON(&flag); /*off*/
	refresh_icon();
	flag = 1;
	TRACE_CADRE_ICON(&flag); /* on*/
}

/*
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C   add_callback_zoom                                                    C
C                                                                        C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
*/

void  add_callback_zoom(vd_id)
int  *vd_id;
/*
        permet l'initialisation des parametres de zoom pour les
        nouvelles fenetres crees.
*/
{
        XGCValues valeurs;
        Arg  args[10];
        int n;


        XtAddEventHandler(widget[*vd_id],ButtonPressMask,FALSE,gestion_bouton,(XtPointer)(*vd_id));
        XtAddEventHandler(widget[*vd_id],ButtonReleaseMask,FALSE,gestion_bouton,(XtPointer)(*vd_id));
	XtAddEventHandler(widget[*vd_id],ButtonMotionMask,FALSE,gestion_bouton,(XtPointer)(*vd_id));
        XtAddEventHandler(widget[*vd_id],KeyPressMask,FALSE,gestion_bouton,(XtPointer)(*vd_id));
        XtAddEventHandler(widget[*vd_id],KeyReleaseMask,FALSE,gestion_bouton,(XtPointer)(*vd_id));

        XGrabButton(XtDisplay(widget[*vd_id]),Button1,ShiftMask,XtWindow(widget[*vd_id]),True,
                        ButtonPressMask|ButtonMotionMask|ButtonReleaseMask,
                        GrabModeAsync,GrabModeAsync,XtWindow(widget[*vd_id]),
                        XCreateFontCursor(XtDisplay(widget[*vd_id]),XC_crosshair));

        XGrabButton(XtDisplay(widget[*vd_id]),Button2,ShiftMask,XtWindow(widget[*vd_id]),True,
                        ButtonPressMask|ButtonMotionMask|ButtonReleaseMask,
                        GrabModeAsync,GrabModeAsync,XtWindow(widget[*vd_id]),
                        XCreateFontCursor(XtDisplay(widget[*vd_id]),XC_crosshair));

        XGrabButton(XtDisplay(widget[*vd_id]),Button3,ShiftMask,XtWindow(widget[*vd_id]),True,
                        ButtonPressMask|ButtonReleaseMask,
                        GrabModeAsync,GrabModeAsync,XtWindow(widget[*vd_id]),
                        XCreateFontCursor(XtDisplay(widget[*vd_id]),XC_crosshair));

        /* Pour les souris a 2 boutons: utilisation de la touche SHIFT */
	shift_L = XKeysymToKeycode(XtDisplay(widget[*vd_id]),XK_Shift_L);
	shift_R = XKeysymToKeycode(XtDisplay(widget[*vd_id]),XK_Shift_R);
        XGrabKey(XtDisplay(widget[*vd_id]),shift_L|shift_R,ShiftMask,
			XtWindow(widget[*vd_id]),True,GrabModeAsync,GrabModeAsync);


                /* creer le GC */
                /* recuperer les couleurs de la widget */


        n = 0;
        XtSetArg(args[n],XtNforeground,&valeurs.foreground); n++;
        XtSetArg(args[n],XtNbackground,&valeurs.background); n++;
        XtGetValues(widget[*vd_id],args,n);

               /* OU exclusif des valeurs d'affichage et de fond */

        valeurs.foreground = valeurs.foreground ^ valeurs.background;

                /* fonction XOR pour le GC */

        valeurs.line_style = FillSolid;

        valeurs.function = GXxor;
        valeurs.line_width = 2 ;
	
        cadre_zoom[*vd_id].gc = XtGetGC(widget[*vd_id],GCForeground | GCBackground
                                | GCFunction| GCLineWidth
                                | GCLineStyle ,&valeurs);

	valeurs.line_width = 1 ;
	viewfinder[*vd_id].gc = XtGetGC(widget[*vd_id],GCForeground | GCBackground
                                | GCFunction| GCLineWidth
                                | GCLineStyle ,&valeurs);


                /* flag d'effacement du cadre  de zoom*/
                /* cadre_ON = 1 => il existe un cadre  */
                /* cadre_ON = 0 => pas de cadre */


        cadre_zoom[*vd_id].cadre_ON = 0;
	viewfinder[*vd_id].lines_ON = 0;
        cadre_zoom[*vd_id].flag_passage_debut_zoom = 0;

                        /* initialisation parametres globaux */

        set_button1 = 0;
        set_button2 = 0;
        set_button3 = 0;
	zm_shift_key = 0;
        zm_button1 = 0;
        zm_button2 = 0;
        zm_button3 = 0;
	zm_motion = 0;
	flag_cadre_zoom = 0;
}

/*-------------------------------------------------------------
**     ptoview
*/
float ptoview(vd_id, axe, ppm)
int	vd_id;
int	axe;   /* 1 (F1) or 2 (F2) */
float   ppm;

/* computes window (0..1) coordinates from ppm value 
   for the view vd_id, using stored internal parameters*/
{
float x;

	if (axe == 1) {
		x = (ppm-viewfinder[vd_id].z1) / (viewfinder[vd_id].z3-viewfinder[vd_id].z1);
	} else {
		x = (ppm-viewfinder[vd_id].z2) / (viewfinder[vd_id].z4-viewfinder[vd_id].z2);
	}
	return(x);
}

/*-------------------------------------------------------------
**     viewtop
*/
float viewtop(vd_id, axe, win)
int	vd_id;
int	axe;   /* 1 (F1) or 2 (F2) */
float   win;

/* computes ppm value from window (0..1) coordinates  
   for the view vd_id, using stored internal parameters*/
{
float x;

	if (axe == 1) {
		x = viewfinder[vd_id].z1 + win*(viewfinder[vd_id].z3-viewfinder[vd_id].z1);
	} else {
		x = viewfinder[vd_id].z2 + win*(viewfinder[vd_id].z4-viewfinder[vd_id].z2);
	}
	return(x);
}

/*-------------------------------------------------------------
**     draw_h_vwf
*/
void draw_h_vwf(vd_id, y)
int             *vd_id;                 /*  id struct   */
float           *y;                     /* in 0..1 coord */
/*  draw the horinzontal viewfinder 
    added by MAD, july `99 */
{
	int     xmin, xmax, yy;
	Widget  w;

	w = widget[*vd_id];
	xmin = 1;
	xmax = windoww[*vd_id]-1;
	yy = (int) (windowh[*vd_id] - (*y * windowh[*vd_id]));
	XDrawLine(XtDisplay(w),XtWindow(w),viewfinder[*vd_id].gc,xmin,yy,xmax,yy);
}
/*-------------------------------------------------------------
**     draw_v_vwf
*/
void draw_v_vwf(vd_id, x)
int             *vd_id;                 /*  id struct   */
float  *x;                              /* in 0..1 coord */
/*  draw the vertical viewfinder 
    added by MAD, july `99 */
{
	int ymin, ymax, xx;
	Widget w;

	w = widget[*vd_id];
	ymin = 1;
	ymax = windowh[*vd_id]-1;
        xx = (int) (*x * windoww[*vd_id]);
	XDrawLine(XtDisplay(w),XtWindow(w),viewfinder[*vd_id].gc,xx,ymin,xx,ymax);
}
/*-------------------------------------------------------------
**     set_vwf
*/
void set_vwf(vd_id, x, y)
int             *vd_id;                 /*  id struct   */
float             x,y;
/*
  sets-up view finder for each window.
*/
{
/* stockage */
        viewfinder[*vd_id].y = y;
        viewfinder[*vd_id].x = x;

/* dessin*/
	if ( window_dim(*vd_id) != 1) {
	  draw_h_vwf(vd_id, &y);
	}
	draw_v_vwf(vd_id, &x);

        viewfinder[*vd_id].lines_ON = 1;

}

/*-------------------------------------------------------------
**     move_vwf
*/
void move_vwf(vd_id, x,y)
int             *vd_id;
float           x,y;
{

  if(viewfinder[*vd_id].lines_ON == 1){
                /* effacer le rectangle precedente */
                /* puis recherche nouvelles coord. */
	if ( window_dim(*vd_id) != 1) {
	  draw_h_vwf(vd_id, &(viewfinder[*vd_id].y));
	  draw_h_vwf(vd_id, &y);
	}
	draw_v_vwf(vd_id, &(viewfinder[*vd_id].x));
	draw_v_vwf(vd_id, &x);

        viewfinder[*vd_id].y = y;
        viewfinder[*vd_id].x = x;
  }
}
/*-------------------------------------------------------------
**     stop_vwf
*/
void stop_vwf(vd_id, x, y)
int             *vd_id;
float           x,y;
{

 if(viewfinder[*vd_id].lines_ON == 1){
	if ( window_dim(*vd_id) != 1) {
	  draw_h_vwf(vd_id, &y);
	}
	draw_v_vwf(vd_id, &x);

        viewfinder[*vd_id].lines_ON = 0;
 }
}

/*-------------------------------------------------------------
**     compxy_view
*/
void compxy_view(w, vd_id, evt,xppm,yppm)
Widget          w;              /*  widget id           */
int             *vd_id;                 /*  id struct   */
XEvent          *evt;
float	          *xppm,*yppm;

/*
computes the x and y values for view_finder
*/
{
        int i,dd,dim,windim,windimself,err;
	float xr,yr,xmouse,ymouse;

/* Check DIM */
	windimself = window_dim(*vd_id);
	if (windimself == 3) { return; }

	/* compute coord in window unit) */
        xr = (float)(evt->xbutton.x)/windoww[*vd_id];
        yr = (1.0-((float)(evt->xbutton.y)/windowh[*vd_id]));

	/* then transform to ppm */
        if (windowt[*vd_id] == 1) {       /* regular */
		err = 0;
		dd = 2;
		WIN2INDEXR(&xmouse, &xr, &windimself, &dd);
        	*xppm = D_ITOPR(&xmouse,&windimself,&dd,&err);

		if (windimself != 1) {
	  	  dd = 1;
	  	  WIN2INDEXR(&ymouse, &yr, &windimself, &dd);       /* in index */
	  	  *yppm = D_ITOPR(&ymouse,&windimself,&dd,&err);     /* and in pm */
		} else  {
	  	  *yppm = *xppm;
		}
		if (err != 0 ) { return; }
        } else {          /* view */
	     	*xppm = viewtop(*vd_id,2,xr);
	     	if (windimself != 1) {
	     		*yppm = viewtop(*vd_id,1,yr);
	     	} else {
	     		*yppm = *xppm;
	     	}
        }
}

/*-------------------------------------------------------------
**     set_viewfinder
*/
void set_viewfinder(w, vd_id, evt)
Widget          w;              /*  widget id           */
int             *vd_id;                 /*  id struct   */
XEvent          *evt;
/*
  sets evry thing up for the viewfinder thru all the active windows.
  Called the first time the user middle-clicks in a given window

*/
{
        int i,dd,dim,windim,windimself,err;
	float xr,yr,xmouse,ymouse,xppm,yppm;

/* Check DIM */
	windimself = window_dim(*vd_id);
	if (windimself == 3) { return; }

/* first my window */
        compxy_view(w, vd_id, evt, &xppm, &yppm);
        
/* then target window */
        for ( i=1; i < NWindow; i++) {
	  if (win[i] != 0) {
	     windim = window_dim(i);
	     if (windowt[i] == 1) {       /* regular */
		 dd = 1;
            	 ymouse = D_PTOIR(&yppm,&windim,&dd,&err);
             	INDEX2WINR(&yr,&ymouse,&windim,&dd);

	     	dd = 2;
             	xmouse = D_PTOIR(&xppm,&windim,&dd,&err);
             	INDEX2WINR(&xr,&xmouse,&windim,&dd);
	     } else {                     /* view */
	     	xr = ptoview(i,2,xppm);
	     	yr = ptoview(i,1,yppm);
	     }

	     if (windim == windimself) {    /* set_vwf(&i, xr, yr); draws at x,y (y not used if 1D */
	     	  set_vwf(&i, xr, yr);
	     } else if (windimself == 1) {  /* have to be carefull if 1D => 2D or 2D => 1D */
	        set_vwf(&i, xr, yr);
	     } else if (windim == 1) {
              if (xr < 0.0 || xr > 1.0) { xr = yr; }
	     	  set_vwf(&i, xr, yr);
	     }
	  }
        }
}
/*-------------------------------------------------------------
**     move_viewfinder
*/
void move_viewfinder(w, vd_id, evt)
     /* called when the mouse is moved with the viewfinder on */
Widget          w;              /*  widget id           */
int             *vd_id;
XEvent          *evt;
{

float xr,yr,xmouse,ymouse,xppm,yppm;
int   windim, windimself, dim, i, err, dd;

	windimself = window_dim(*vd_id);
	if (windimself == 3) { return; }

/* first my window */
        compxy_view(w, vd_id, evt, &xppm, &yppm);
        
/* then target window */

	for ( i=1; i < NWindow; i++) {
	  if (win[i] != 0) {
	     windim = window_dim(i);
	     if (windowt[i] == 1) {       /* regular */
		 dd = 1;
            	 ymouse = D_PTOIR(&yppm,&windim,&dd,&err);
             	INDEX2WINR(&yr,&ymouse,&windim,&dd);

	     	dd = 2;
             	xmouse = D_PTOIR(&xppm,&windim,&dd,&err);
             	INDEX2WINR(&xr,&xmouse,&windim,&dd);
	     } else {                     /* view */
	     	xr = ptoview(i,2,xppm);
	     	yr = ptoview(i,1,yppm);
	     }
	     if (windim == windimself) {    /* set_vwf(&i, xr, yr); draws at x,y (y not used if 1D */
	     	  move_vwf(&i, xr, yr);
	     } else if (windimself == 1) {  /* have to be carefull if 1D => 2D or 2D => 1D */
	        move_vwf(&i, xr, yr);
	     } else if (windim == 1) {
              if (xr < 0.0 || xr > 1.0) { xr = yr; }
	     	  move_vwf(&i, xr, yr);
	     }
	  }
	}
}

/*-------------------------------------------------------------
**     stop_viewfinder
*/
void stop_viewfinder(w, vd_id, evt)
     /* called when the viewfinder is stopped */
Widget          w;              /*  widget id           */
int             *vd_id;
XEvent          *evt;
{
float xr,yr,xmouse,ymouse,xppm,yppm;
int   windim,windimself, dim, i, err, dd;

	windimself = window_dim(*vd_id);
	if (windimself == 3) { return; }


/* first my window */
        compxy_view(w, vd_id, evt, &xppm, &yppm);
        
/* then target window */
 
	for ( i=1; i < NWindow; i++) {
	  if (win[i] != 0) {
	     windim = window_dim(i);
	     if (windowt[i] == 1) {       /* regular */
		 dd = 1;
            	 ymouse = D_PTOIR(&yppm,&windim,&dd,&err);
             	INDEX2WINR(&yr,&ymouse,&windim,&dd);

	     	dd = 2;
             	xmouse = D_PTOIR(&xppm,&windim,&dd,&err);
             	INDEX2WINR(&xr,&xmouse,&windim,&dd);
	     } else {                     /* view */
	     	xr = ptoview(i,2,xppm);
	     	yr = ptoview(i,1,yppm);
	     }
	     if (windim == windimself) {    /* set_vwf(&i, xr, yr); draws at x,y (y not used if 1D */
	     	  stop_vwf(&i, xr, yr);
	     } else if (windimself == 1) {  /* have to be carefull if 1D => 2D or 2D => 1D */
	        stop_vwf(&i, xr, yr);
	     } else if (windim == 1) {
              if (xr < 0.0 || xr > 1.0) { xr = yr; }
	     	  stop_vwf(&i, xr, yr);
	     }
	  }
	}
}

/*-------------------------------------------------------------
**     debut_cadre_zoom 
*/
void debut_cadre_zoom(w, vd_id, evt)
Widget          w;              /*  widget id           */
int      	*vd_id;    		/*  id struct   */
XEvent          *evt;
/*
	recupere les coordonnees au premier click
	de la souris
	initialisation de parametres
*/
{

	int i;
	int dim, windim;
	
	/* recuperation de la dimension courante $DIM et de la dimension de la fenetre */
 WHICH_DIM(&dim);
  windim = window_dim(*vd_id);
 if(dim != windim)return;	


			/* test de passage par debut_cadre_zoom
			utile selon la config utilisateur */

   if(cadre_zoom[*vd_id].flag_passage_debut_zoom == 0){

	cadre_zoom[*vd_id].flag_passage_debut_zoom = 1;

			/* test et efface cadre de zoom*/
			/* dans chacune des fenetres */


	
        for ( i=1; i < NWindow; i++){
	  if(win[i] != 0){
            if (cadre_zoom[i].cadre_ON == 1){
                delete_last_cadre_zoom(&i);
	    }
	  }
        }

			/* initialisation des parametres du cadre de zoom */

        cadre_zoom[*vd_id].largeur = 0;
        cadre_zoom[*vd_id].hauteur = 0;
	cadre_zoom[*vd_id].x_fin = 0;
	cadre_zoom[*vd_id].y_fin = 0;
	cadre_zoom[*vd_id].x_debut = 0;
	cadre_zoom[*vd_id].y_debut = 0;	
	cadre_zoom[*vd_id].x_orig = 0;
	cadre_zoom[*vd_id].y_orig = 0;

        cadre_zoom[*vd_id].x_fin = cadre_zoom[*vd_id].x_debut = evt->xbutton.x;
        cadre_zoom[*vd_id].y_fin = cadre_zoom[*vd_id].y_debut = evt->xbutton.y;
        cadre_zoom[*vd_id].x_orig = cadre_zoom[*vd_id].x_debut;
        cadre_zoom[*vd_id].y_orig = cadre_zoom[*vd_id].y_debut;


        XDrawRectangle(XtDisplay(w),XtWindow(w),cadre_zoom[*vd_id].gc,
                cadre_zoom[*vd_id].x_debut,cadre_zoom[*vd_id].y_debut,
                cadre_zoom[*vd_id].largeur,cadre_zoom[*vd_id].hauteur);

   }
}

/*-------------------------------------------------------------
**     trace_cadre_zoom 
*/
void trace_cadre_zoom(w, vd_id, evt)
Widget          w;              /*  widget id           */
int             *vd_id;   
XEvent          *evt;
/* 
	trace le cadre du zoom en fonction 
	des coordonnees initiales et courantes
	de la souris
*/
{


	int urx,ury,ulx,uly,llx,lly,lrx,lry;


		/* effacer le rectangle precedente */

   if(cadre_zoom[*vd_id].flag_passage_debut_zoom == 1){

        cadre_zoom[*vd_id].cadre_ON = 1;

	XDrawRectangle(XtDisplay(w),XtWindow(w),cadre_zoom[*vd_id].gc,
                	cadre_zoom[*vd_id].x_orig,cadre_zoom[*vd_id].y_orig,
                	cadre_zoom[*vd_id].largeur,cadre_zoom[*vd_id].hauteur);


		/* recherche nouvelles coord. */

	cadre_zoom[*vd_id].x_fin = evt->xbutton.x;
	cadre_zoom[*vd_id].y_fin =  evt->xbutton.y;

	ulx = min(cadre_zoom[*vd_id].x_fin,cadre_zoom[*vd_id].x_debut);
	uly = min(cadre_zoom[*vd_id].y_fin,cadre_zoom[*vd_id].y_debut);
	lrx = max(cadre_zoom[*vd_id].x_fin,cadre_zoom[*vd_id].x_debut);
	lry = max(cadre_zoom[*vd_id].y_fin,cadre_zoom[*vd_id].y_debut);

        cadre_zoom[*vd_id].largeur = lrx - ulx;
        cadre_zoom[*vd_id].hauteur = lry - uly;
        cadre_zoom[*vd_id].x_orig = ulx;
        cadre_zoom[*vd_id].y_orig = uly;

        XDrawRectangle(XtDisplay(w),XtWindow(w),cadre_zoom[*vd_id].gc,
                ulx,uly,lrx - ulx,lry - uly);

   }


}

/*-------------------------------------------------------------
**     fin_cadre_zoom
*/
void fin_cadre_zoom(vd_id,evt)
int       *vd_id;    
XEvent    *evt;

/*
	recupere les coordonnees finales de la souris et les
	sauvegarde afin de pouvoir effacer le cadre plus tard
*/
{




		/* recuperer les parametres finaux */


 if(cadre_zoom[*vd_id].flag_passage_debut_zoom == 1){

	cadre_zoom[*vd_id].x_fin = evt->xbutton.x;
	cadre_zoom[*vd_id].y_fin = evt->xbutton.y;

		/* id de la derniere fenetre selectionnee par le zoom*/

	zoom_wind_id = *vd_id;
	cadre_zoom[*vd_id].flag_passage_debut_zoom = 0;

        /*stockage des coord de la zone selectionnee */

        set_area_var();

 }


}


/*-------------------------------------------------------------
**     delete_last_cadre_zoom
*/
void delete_last_cadre_zoom(vd_id)
int       *vd_id;    

/* 
	efface le cadre de zoom associe a la fenetre
*/
{



		/* efface le dernier rectangle et reinitialisation 
		   des parametres */



        	XDrawRectangle(XtDisplay(widget[*vd_id]),XtWindow(widget[*vd_id]),
			cadre_zoom[*vd_id].gc,
                	cadre_zoom[*vd_id].x_orig,cadre_zoom[*vd_id].y_orig,
			cadre_zoom[*vd_id].largeur,cadre_zoom[*vd_id].hauteur);

 	        cadre_zoom[*vd_id].cadre_ON = 0;               

}


/*-------------------------------------------------------------
**     OFF_cadre_zoom
*/
void OFF_cadre_zoom(vd_id)
int       *vd_id;
/*
	efface le cadre du zoom de la fenetre
*/
{


	if(cadre_zoom[*vd_id].flag_passage_debut_zoom == 0){
                cadre_zoom[*vd_id].cadre_ON = 0;

	}
}

/*
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C   add_callback_point                                                   C
C                                                                        C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
*/

void  add_callback_point(vd_id)
int  *vd_id;
/*
	permet la mise en place des callback pour
	la commande point
*/
{
		/* mise a jour des coordonnees de la souris dans le label 
			coord_x et coord_y de la boite de zoom */
        XtAddEventHandler(widget[*vd_id],ButtonMotionMask,FALSE,mouse_coordCB,vd_id);

		/* le stockage des coord se fait pour 1 ButtonReleaseMask 
				du bouton 1*/
        XtAddEventHandler(widget[*vd_id],ButtonReleaseMask,FALSE,stock_mouse_coord,vd_id);
}


/*-------------------------------------------------------------
**     mouse_coordCB
*/
void mouse_coordCB(w, vd_id, evt)
Widget          w;              /*  widget id           */
int             *vd_id;
XEvent          *evt;      /*  data from widget class  */
{
	update_mouse_coord(vd_id);
}


/*-------------------------------------------------------------
**    update_mouse_coord
*/
void update_mouse_coord(vd_id)
int             *vd_id;
/*
        met a jour les coord de la souris dans les label
        de la boite de zoom
*/
{
	int dim, windim, dd,f1,f2,f3;
	float xmouse,ymouse,intensity,delta;
	char xcoord[50];
	char ycoord[50];
	char intcoord[50];
	float x,y,xx,yy;
	float xr,yr;
	Arg args;
	XmString xms;


	/* si la boite de zoom n'est pas affichee, on sort */

	if(zoom_set == 0){return;}


	/* determine si la dimension de la fenetre est en rapport avec
	la dimemsion de travail : si oui OK*/

 	WHICH_DIM(&dim);
 	windim = window_dim(*vd_id);
	if(dim != windim)return;


	/* recherche des coordonnees de la souris */

	WIN_GET_POINTER_POSITION(vd_id,&xmouse,&ymouse);

		/* passage en unite courante */
	if(dim == 2) {
	    dd = 2;
	    WIN2INDEXR(&xr, &xmouse, &dim, &dd);
            CURRUNITR(&xmouse,&xr,&dd);
	    dd = 1;
	    WIN2INDEXR(&yr, &ymouse, &dim, &dd);
            CURRUNITR(&ymouse,&yr,&dd);
	    if (cadre_zoom[*vd_id].flag_passage_debut_zoom != 0) {   /* if on cadre, display delta */
	      x = (float) (cadre_zoom[*vd_id].x_debut)/windoww[*vd_id];
	      y = 1.0 - (float) (cadre_zoom[*vd_id].y_debut)/windowh[*vd_id];
	      dd = 2;
	      WIN2INDEXR(&xx, &x, &dim, &dd);
	      CURRUNITR(&delta,&xx,&dd);
	      xmouse = xmouse-delta;
	      dd = 1;
	      WIN2INDEXR(&yy, &y, &dim, &dd);
	      CURRUNITR(&delta,&yy,&dd);
	      ymouse = ymouse-delta;
	    }
	}
	if(dim == 1) {
	    dd = 2;
	    WIN2INDEXR(&xr, &xmouse, &dim, &dd);
	    CURRUNITR(&xmouse,&xr,&dd);
	    if (cadre_zoom[*vd_id].flag_passage_debut_zoom != 0) {   /* if on cadre, display delta */
	      x = (float) (cadre_zoom[*vd_id].x_debut)/windoww[*vd_id];
	      WIN2INDEXR(&xx, &x, &dim, &dd);
	      CURRUNITR(&delta,&xx,&dd);
	      xmouse = xmouse-delta;
	    }
	}

	/* recherche de l'intensite */

	if(dim == 1) {
		f1 = xr+0.5; f2 = 0; f3 = 0;
		WHICH_INT(&intensity,&dim,&f1,&f2,&f3);
	}
	if(dim == 2) {
                f1 = yr+0.5; f2 = xr+0.5; f3 = 0;
                WHICH_INT(&intensity,&dim,&f1,&f2,&f3);
        }
/*	
	EN 3D:
        if(dim == 3) {
                f1 = 0; f2 = 0; f3 = 0;
                WHICH_INT(&intensity,&dim,&f1,&f2,&f3);
        }
*/

	/* stockage et mise a jour dans les labels de la boite de zoom */

	if(windim == 1){
		sprintf(ycoord,"F1:  %f",xmouse);
		xms = XmStringCreate(ycoord,XmSTRING_DEFAULT_CHARSET);
        	XtSetArg(args,XmNlabelString,xms);
        	XtSetValues(label_zoombox.label_coord_y,&args,1);
        	XmStringFree(xms);

                sprintf(intcoord,"Int:  %g",intensity);
                xms = XmStringCreate(intcoord,XmSTRING_DEFAULT_CHARSET);
                XtSetArg(args,XmNlabelString,xms);
                XtSetValues(label_zoombox.label_intens,&args,1);
                XmStringFree(xms);
	}
	else{
                sprintf(ycoord,"F1:  %f",ymouse);
                xms = XmStringCreate(ycoord,XmSTRING_DEFAULT_CHARSET);
                XtSetArg(args,XmNlabelString,xms);
                XtSetValues(label_zoombox.label_coord_y,&args,1);
                XmStringFree(xms);

                sprintf(xcoord,"F2:  %f",xmouse);
                xms = XmStringCreate(xcoord,XmSTRING_DEFAULT_CHARSET);
                XtSetArg(args,XmNlabelString,xms);
                XtSetValues(label_zoombox.label_coord_x,&args,1);
                XmStringFree(xms);

                sprintf(intcoord,"Int:  %g",intensity);
                xms = XmStringCreate(intcoord,XmSTRING_DEFAULT_CHARSET);
                XtSetArg(args,XmNlabelString,xms);
                XtSetValues(label_zoombox.label_intens,&args,1);
                XmStringFree(xms);
        }
}

/*-------------------------------------------------------------
**    stock_mouse_coord
*/
void stock_mouse_coord(w, vd_id, evt)
Widget          w;              /*  widget id           */
int             *vd_id;
XEvent          *evt;      /*  data from widget class  */

/*
	stocke les coordonnees x et y de la souris
*/
{
        int dim, windim;
        float xmouse,ymouse;
        float xr,yr;
        Arg args;
        XmString xms;
	int butt,dd;


                /* recherche du bouton utilise */

	butt = 0;
        if(evt->xbutton.button == Button1 && set_button2 == 0 && set_button3 == 0)
					{butt = 1;set_button1 = 0;}
        if(evt->xbutton.button == Button2 && set_button1 == 0 && set_button3 == 0)
					{butt = 2;set_button2 = 0;}
        if(evt->xbutton.button == Button3 && set_button2 == 0 && set_button1 == 0)
					{butt = 3;set_button3 = 0;}
					

	if(butt != 0){ /* que les boutons seuls et non associes*/
		SET_LAST_BUTT(&butt);
	}


       		/* verification du mode point */

    if(point_flag == 1){ 

        	/* determine si la dimension de la fenetre est en rapport avec
        	la dimemsion de travail : si oui OK*/

        WHICH_DIM(&dim);
 	windim = window_dim(*vd_id);
        if(dim != windim)return;


        	/* recherche des coordonnees de la souris */

	WIN_GET_POINTER_POSITION(vd_id,&xmouse,&ymouse);

		/* mise a jour des coord dans var internes */
	dd = 2;
	WIN2INDEXR(&xr, &xmouse, &dim, &dd);
	dd = 1;
	WIN2INDEXR(&yr, &ymouse, &dim, &dd);
	    
	SET_POINT_XY(&xr,&yr);

                /* sortir du mode point et de la boucle*/

	point_flag = 0;

    }


}

/*-------------------------------------------------------------
**    monopoint
*/
/*
	entre dans le mode point
	et se met en boucle d'attente
*/
void MONOPOINT()
{
        XEvent          event;
        int             bool;

	/* mode point */

	point_flag = 1;
        flag_bloquant = 1; /*flag general de mode bloquant: interdit l'appel a execute*/
        del_cursor_wait_on_display(1); /* enleve la montre */


       /* gestion des evenements */
       
         for (;;)
        {
         XtAppNextEvent(app_context,&event);
         bool = XtDispatchEvent(&event);
         if (point_flag == 0)
                {
                break;
                }
        }

        del_cursor_wait_on_display(0);  /* remet la montre */
        flag_bloquant  = 0 ; /*flag general de mode bloquant*/

}

/*-------------------------------------------------------------
**  update_label_dim
*/
void UPDATE_LABEL_DIM(dim)
int             *dim;
/*
        met a jour le label dimension dans la boite de zoom
*/
{

        char dimlab[50],xcoord[50],ycoord[50],intcoord[50];
        Arg args;
        XmString xms;

        /* si la boite de zoom n'est pas affichee, on sort */

        if(zoom_set == 0){return;}

        /* stockage et mise a jour de Dim dans le label de la boite de zoom */

	if(*dim == 1)
        	XtSetArg (args, XmNmenuHistory,label_zoombox.dim1);
        if(*dim == 2)
                XtSetArg (args, XmNmenuHistory, label_zoombox.dim2); 
        if(*dim == 3)
                XtSetArg (args, XmNmenuHistory, label_zoombox.dim3); 
	XtSetValues(label_zoombox.label_dim,&args,1);


		/* reset des label coordonnees */

        sprintf(xcoord,"F2:                      .");
        xms = XmStringCreate(xcoord,XmSTRING_DEFAULT_CHARSET);
        XtSetArg(args,XmNlabelString,xms);
        XtSetValues(label_zoombox.label_coord_x,&args,1);
        XmStringFree(xms);

        sprintf(ycoord,"F1:                      .");
        xms = XmStringCreate(ycoord,XmSTRING_DEFAULT_CHARSET);
        XtSetArg(args,XmNlabelString,xms);
        XtSetValues(label_zoombox.label_coord_y,&args,1);
        XmStringFree(xms);

        sprintf(intcoord,"Int:                     .");
        xms = XmStringCreate(intcoord,XmSTRING_DEFAULT_CHARSET);
        XtSetArg(args,XmNlabelString,xms);
        XtSetValues(label_zoombox.label_intens,&args,1);
        XmStringFree(xms);


}


/*-------------------------------------------------------------
**     Close la zoom box appel avec ZM 0 
*/
void ZOOMBOX_CLOSE()
{

	if(zoom_set != 0){
        	XtDestroyWidget(zoom_set);
		del_shell_on_display(zoom_set);
        	zoom_set = 0;
	}
}


/*-------------------------------------------------------------
**     set_dim1CB
*/
void set_dim1CB(w, client_data, call_data)
Widget          w;              /*  widget id           */
XtPointer       client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{
        pre_execute("dim 1");

}

/*-------------------------------------------------------------
**     set_dim2CB
*/
void set_dim2CB(w, client_data, call_data)
Widget          w;              /*  widget id           */
XtPointer       client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{
        pre_execute("dim 2");

}
/*-------------------------------------------------------------
**     set_dim3CB
*/
void set_dim3CB(w, client_data, call_data)
Widget          w;              /*  widget id           */
XtPointer       client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{
        pre_execute("dim 3");

}


/*************************************************************************
  Cette fonction introduit dans le serveur une image transmise sous
  forme de'un tableau d'octets, en creant au prealable la structure XImage
  adequate.
  Entree :
     w : widget associee,
     nom : nom de l'image,
     bits : tableau dans lequel se trouve l'image,
     largeur : largeur de l'image,
     hauteur :  hauteur de l'image.

	OBSOLETTE
**************************************************************************/
void Enregistrer_bmp (w, nom, bits, largeur, hauteur)
Widget w;
char *nom;
char *bits;
unsigned int largeur, hauteur;
{
  XImage *image;
  unsigned int z;

  z = 1;
  image = XCreateImage(XtDisplay(w), DefaultVisualOfScreen(XtScreen(w)),
                       z, XYBitmap,0, bits, largeur, hauteur, 8, 2);
  XmInstallImage(image, nom);
}

/*-------------------------------------------------------------
**     clear_infoCB
*/
void clear_infoCB(w,client_data, call_data)
Widget          w;              /*  widget id           */
XtPointer       client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{
DOREFRESH();
}


/*-------------------------------------------------------------
**     gestion_button
*/
void gestion_bouton(w, vd_id, evt)
Widget          w;              /*  widget id           */
int             vd_id;
XEvent          *evt;      /*  data from widget class  */
/*
	Gere l'ensemble des appels souris et clavier
	pour le zoom
*/
{

	int releas1,releas2,releas3;


                /* positionnement */

 zm_motion = 0;
 releas1 = 0;
 releas2 = 0;
 releas3 = 0;
 if(evt->xbutton.button == Button1 && evt->xbutton.type == ButtonPress){zm_button1 = 1;}
 if(evt->xbutton.button == Button1 && evt->xbutton.type == ButtonRelease){zm_button1 = 0;releas1 = 1;}
 if(evt->xbutton.button == Button2 && evt->xbutton.type == ButtonPress){zm_button2 = 1;}
 if(evt->xbutton.button == Button2 && evt->xbutton.type == ButtonRelease){zm_button2 = 0;releas2 = 1;}
 if(evt->xbutton.button == Button3 && evt->xbutton.type == ButtonPress){zm_button3 = 1;}
 if(evt->xbutton.button == Button3 && evt->xbutton.type == ButtonRelease){zm_button3 = 0;releas3 = 1;}
 if(evt->xbutton.type == MotionNotify){zm_motion = 1;}



 if((evt->xkey.keycode == shift_L || evt->xkey.keycode == shift_R) &&
	evt->xkey.type == KeyPress){zm_shift_key = 1;}
 if((evt->xkey.keycode == shift_L || evt->xkey.keycode == shift_R) &&
        evt->xkey.type == KeyRelease){zm_shift_key = 0;}

/*
printf("bouton = %d ,event : = %d\n" ,evt->xbutton.button,evt->xbutton.type);
printf("zm_button1=%d , zm_button2=%d , zm_button3=%d , zm_shift_key=%d, zm_motion=%d\n",zm_button1,zm_button2,zm_button3,zm_shift_key,zm_motion);
*/



	/* switch selon event:
        Bpress1 + Bpress2 => debut_cadre_zoom
        motion + Bpress1 + Bpress2 => trace_cadre_zoom
        Brelease2 => fin_cadre_zoom
        Bpress1 + Breleas3 => zoom_in
        Bpress2 + Breleas3 => zoom_out
        shift + Bpress1 => debut_cadre_zoom
        shift + motion + Bpress1 => trace_cadre_zoom
        Brelease1 => fin_cadre_zoom
	shift + Breleas3 => zoom_in
        shift + Breleas2 => zoom_out
	Bpress3 => set_viewfinder
        */

	

                /* test des positionnement pour le zoom in */

 if( ( zm_shift_key == 0 && zm_button1 == 1 && zm_button2 == 0 && releas3 == 1 && zm_motion == 0)
     ||
     ( zm_shift_key == 0 && releas1 == 1 && zm_button2 == 0 && zm_button3 == 1 && zm_motion == 0)
     ||
     ( zm_shift_key == 1 && zm_button1 == 0 && zm_button2 == 0 && releas3 == 1 && zm_motion == 0))
     {
          Zoom_in(w);
     }


                 /* test des positionnement pour le zoom out */

 else if((zm_shift_key == 0 && zm_button1 == 0 && zm_button2 == 1 && releas3 == 1 && zm_motion == 0)
	||
	( zm_shift_key == 0 && zm_button1 == 0 && releas2 == 1 && zm_button3 == 1 && zm_motion == 0)
	||
	( zm_shift_key == 1 && zm_button1 == 0 && releas2 == 1 && zm_button3 == 0 && zm_motion == 0))
	{
		if(viewfinder[vd_id].lines_ON == 1){stop_viewfinder(w, &vd_id, evt);}
          	Zoom_out(w);
 	}


                /* test de demarrage du cadre de zoom*/

 else if((zm_shift_key == 0 && zm_button1 == 1 && zm_button2 == 1 && zm_button3 == 0 && flag_cadre_zoom == 0 && zm_motion == 0)
	|| 
	( zm_shift_key == 1 && zm_button1 == 1 && zm_button2 == 0 && zm_button3 == 0 && flag_cadre_zoom == 0 && zm_motion == 0))
	{
	  debut_cadre_zoom(w, &vd_id, evt);
	  flag_cadre_zoom = 1;
 	}

                /* test de tracage du cadre de zoom*/

else if((zm_shift_key == 0 && zm_button1 == 1 && zm_button2 == 1 && zm_button3 == 0 && flag_cadre_zoom == 1 && zm_motion == 1)
	||
  	(zm_shift_key == 1 && zm_button1 == 1 && zm_button2 == 0 && zm_button3 == 0 && flag_cadre_zoom == 1 && zm_motion == 1))
	{
	  trace_cadre_zoom(w, &vd_id, evt);
 	}

                /* test de fin du cadre de zoom*/

 else if((zm_shift_key == 0 || zm_button1 == 0) && zm_button2 == 0 && zm_button3 == 0 && flag_cadre_zoom == 1)
	{
	  fin_cadre_zoom(&vd_id,evt);
	  flag_cadre_zoom = 0;
 	}

        	/* cas: mode phase, dim 1, mise en place du pivot */

 else if(zm_shift_key == 0 && zm_button1 == 0 && zm_button2 == 0 && zm_button3 == 1 && flag_cadre_zoom == 0)
	{
        if(sortie_boucle_phase == -1) set_pivot_phase(3); /* flag = 3 : recalcul de la position du pivot */
	}

		/* viseur attribution */

 else if(zm_shift_key == 0 && zm_button1 == 0 && zm_button2 == 1 && zm_button3 == 0 && flag_cadre_zoom == 0 && zm_motion == 0)
        {
        set_viewfinder(w, &vd_id, evt); /* positionnement du viseur d'attribution */
        }
 else if(zm_shift_key == 0 && zm_button1 == 0 && zm_button2 == 1 && zm_button3 == 0 && flag_cadre_zoom == 0 && zm_motion == 1)
        {
        move_viewfinder(w, &vd_id, evt); /* deplacement du viseur d'attribution */
        }
 else if(zm_shift_key == 0 && zm_button1 == 0 && zm_button2 == 0 && zm_button3 == 0 && flag_cadre_zoom == 0)
        {
        stop_viewfinder(w, &vd_id, evt); /* arret du viseur d'attribution */
        }
}
