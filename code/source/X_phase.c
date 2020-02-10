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


/*-------------------------------------------------------------
**    include
*/
#ifndef HP
#  include <Xm/Xm.h>
#  include <stdio.h>
#  include <Xm/Separator.h>
#  include <X11/Xlib.h>
#  include <X11/Xutil.h>
#  include <Xm/BulletinB.h>
#  include <Xm/ArrowB.h>
#  include <Xm/Label.h>
#  include <Xm/PushB.h>
#  include <X11/cursorfont.h>
#  include <X11/Intrinsic.h>
#  include <X11/StringDefs.h>
#  include <Xm/DrawingA.h>
#  include <Xm/Protocols.h>
#else
# include <Xm/XmAll.h>
# include <X11/cursorfont.h>
#  include <stdio.h>
#endif

#ifndef MACHTEN
#  include <malloc.h>
#else
#  include <sys/malloc.h>
#endif
#include "X_windef.h"


/*-------------------------------------------------------------
**      forwarded functions
*/

void phase_closeCB();
void phase_okCB();
void phase_cancelCB();
void CREATE_PHASEBOX();
void phase_o1_m90CB();
void phase_o1_m10CB();
void phase_o1_m1CB();
void phase_o1_p1CB();
void phase_o1_p10CB();
void phase_o1_p90CB();
void phase_o0_m90CB();
void phase_o0_m10CB();
void phase_o0_m1CB();
void phase_o0_p1CB();
void phase_o0_p10CB();
void phase_o0_p90CB();
void set_pivot_phase();


/*-------------------------------------------------------------
*     Variables externes et globales
*/

extern int versionx;
extern int flag_bloquant;
extern Widget window_mere;
extern XtAppContext app_context;
int sortie_boucle_phase;
int appel_phase;
float *ph1_loc,*ph0_loc;
typedef struct {
        float *spectrum; /* en FAIT pointeur sur tab de float */
        Widget mere;
        int *vd_id;
        float x0;
        float p0, p1;
}Data_phase;

Data_phase *donnees;



/*-------------------------------------------------------------
**         create_phasebox
*/

void CREATE_PHASEBOX(spectrum,vd_id,ph0,ph1,size)
/*
c IN    : vd_id,size
c INOUT : spectrum,
c OUT   : ph0,ph1
C    This subroutine allows an interactive phasage of a 1D spectrum.
C   The informations returned are: zero and first order phase
C   that have been applied, ph0 and ph1 respectively. These can then
C   be applied directly with 'phase'.
C   spectrum: 1D spectrum to phase
C   vd_id: window in which the new phased spectra is drawn
C   ph0: zero order phase that has been applied ;
C        it can then be used in the 'phase' program
C   ph1: first order phase that has been applied ;
C        it can then be used in the 'phase' program
*/
int *vd_id,size;
float *spectrum,*ph0,*ph1;
{
    Widget      toplevel_phase,toplevel_box,label[2],but_phase_0[7],but_phase_1[7],
		but_ok,but_cancel;

    Arg arg;
    Arg args[30];
    int n,position_x;
    unsigned int  width_screen;
    int screen;
    Display *display;
    Atom wm_delete_window,wm_resize_window;
    char position[50];
    int flag;
    XEvent  event;
    int bool,cursor;

                        /* test du display */

      if (versionx == 0){
                 printf("Gifa cannot open the display on X server\n");
            	 *ph0=0.0;
            	 *ph1=0.0;
                 return;
      }

	if(sortie_boucle_phase == -1)return; /* phase 1d deja lance*/


	donnees = malloc(sizeof(Data_phase));
	donnees->spectrum = spectrum;
	donnees->vd_id = vd_id;
	ph0_loc = ph0; 	/* stockage des pointeurs */
	ph1_loc = ph1;

                       /* creation du shell du widget phase */


			/* recherche de la position de la boite */

        display = XtDisplay(window_mere);
        screen = DefaultScreen(display);
        width_screen = DisplayWidth(display,screen);
        position_x = (width_screen - 435);
	sprintf(position,"+%d+1",position_x);

			/* init des variables */

	donnees->p0 = 0.0;
	donnees->p1 = 0.0;
	donnees->x0 = 0.5;
	cursor = 1;
	WIN_SET_POINTER_PATTERN(vd_id,&cursor);
	appel_phase = 0;


			/* creation */

	n = 0;
	XtSetArg(args[n], XtNgeometry,position); n++ ;
        toplevel_phase = XtCreatePopupShell ("Phase Box", topLevelShellWidgetClass,
                        window_mere, args,n);

	donnees->mere = toplevel_phase;


                        /* Elimination  de la commande Resize */


         wm_resize_window = XmInternAtom(XtDisplay(toplevel_phase),
                                "WM_SIZE_WINDOW",
                                FALSE);
         XmRemoveWMProtocols(toplevel_phase,&wm_resize_window, 1);

                        /* Icone de presentation */

         SetAppIcon(toplevel_phase,6);


                        /* Create BulletinBoard box avec taille . */

	n = 0;
 	XtSetArg(args[n], XmNheight,155);n++;	
	XtSetArg(args[n], XmNwidth, 280);n++;
        XtSetArg (args[n], XmNnoResize, True);n++;
        toplevel_box = XmCreateBulletinBoard (toplevel_phase, "work_area", args, n);
	XtManageChild(toplevel_box);


			/* Realize button */


						/* ORDRE 0 */

        n = 0;
        XtSetArg(args[n], XmNx,5);n++;
        XtSetArg(args[n], XmNy,3);n++;
        label[0] = XmCreateLabel(toplevel_box, "0th Order", args, n);
        XtManageChild (label[0]);

        n = 0;
        XtSetArg(args[n],XmNwidth,40); n++;
        XtSetArg(args[n],XmNheight,30); n++;
        XtSetArg(args[n], XmNx,5);n++;
        XtSetArg(args[n], XmNy,30);n++;
	but_phase_0[1] = XmCreatePushButton(toplevel_box,"-90",args, n);
	XtManageChild(but_phase_0[1]);
        XtAddCallback (but_phase_0[1], XmNactivateCallback, phase_o0_m90CB,
                                (XtPointer)donnees);

        n = 0;
        XtSetArg(args[n],XmNwidth,40); n++;
        XtSetArg(args[n],XmNheight,30); n++;
        XtSetArg(args[n], XmNx,50);n++;
        XtSetArg(args[n], XmNy,30);n++;
        but_phase_0[2] = XmCreatePushButton(toplevel_box,"-10",args, n);
        XtManageChild(but_phase_0[2]);
        XtAddCallback (but_phase_0[2], XmNactivateCallback, phase_o0_m10CB,
                                (XtPointer)donnees);

        n = 0;
        XtSetArg(args[n],XmNwidth,40); n++;
        XtSetArg(args[n],XmNheight,30); n++;
        XtSetArg(args[n], XmNx,95);n++;
        XtSetArg(args[n], XmNy,30);n++;
        but_phase_0[3] = XmCreatePushButton(toplevel_box,"-1",args, n);
        XtManageChild(but_phase_0[3]);
        XtAddCallback (but_phase_0[3], XmNactivateCallback, phase_o0_m1CB,
                                (XtPointer)donnees);


        n = 0;
        XtSetArg(args[n],XmNwidth,40); n++;
        XtSetArg(args[n],XmNheight,30); n++;
        XtSetArg(args[n], XmNx,145);n++;
        XtSetArg(args[n], XmNy,30);n++;
        but_phase_0[4] = XmCreatePushButton(toplevel_box,"+1",args, n);
        XtManageChild(but_phase_0[4]);
        XtAddCallback (but_phase_0[4], XmNactivateCallback, phase_o0_p1CB,
                                (XtPointer)donnees);

        n = 0;
        XtSetArg(args[n],XmNwidth,40); n++;
        XtSetArg(args[n],XmNheight,30); n++;
        XtSetArg(args[n], XmNx,190);n++;
        XtSetArg(args[n], XmNy,30);n++;
        but_phase_0[5] = XmCreatePushButton(toplevel_box,"+10",args, n);
        XtManageChild(but_phase_0[5]);
        XtAddCallback (but_phase_0[5], XmNactivateCallback, phase_o0_p10CB,
                                (XtPointer)donnees);

        n = 0;
        XtSetArg(args[n],XmNwidth,40); n++;
        XtSetArg(args[n],XmNheight,30); n++;
        XtSetArg(args[n], XmNx,235);n++;
        XtSetArg(args[n], XmNy,30);n++;
        but_phase_0[6] = XmCreatePushButton(toplevel_box,"+90",args, n);
        XtManageChild(but_phase_0[6]);
        XtAddCallback (but_phase_0[6], XmNactivateCallback, phase_o0_p90CB,
                                (XtPointer)donnees);



						/* ORDRE 1 */

        n = 0;
        XtSetArg(args[n], XmNx,5);n++;
        XtSetArg(args[n], XmNy,65);n++;
        label[1] = XmCreateLabel(toplevel_box, "1st Order", args, n);
        XtManageChild (label[1]);

        n = 0;
        XtSetArg(args[n],XmNwidth,40); n++;
        XtSetArg(args[n],XmNheight,30); n++;
        XtSetArg(args[n], XmNx,5);n++;
        XtSetArg(args[n], XmNy,85);n++;
        but_phase_1[1] = XmCreatePushButton(toplevel_box,"-90",args, n);
        XtManageChild(but_phase_1[1]);
        XtAddCallback (but_phase_1[1], XmNactivateCallback, phase_o1_m90CB,
                                (XtPointer)donnees);


        n = 0;
        XtSetArg(args[n],XmNwidth,40); n++;
        XtSetArg(args[n],XmNheight,30); n++;
        XtSetArg(args[n], XmNx,50);n++;
        XtSetArg(args[n], XmNy,85);n++;
        but_phase_1[2] = XmCreatePushButton(toplevel_box,"-10",args, n);
        XtManageChild(but_phase_1[2]);
        XtAddCallback (but_phase_1[2], XmNactivateCallback, phase_o1_m10CB,
                                (XtPointer)donnees);

        n = 0;
        XtSetArg(args[n],XmNwidth,40); n++;
        XtSetArg(args[n],XmNheight,30); n++;
        XtSetArg(args[n], XmNx,95);n++;
        XtSetArg(args[n], XmNy,85);n++;
        but_phase_1[3] = XmCreatePushButton(toplevel_box,"-1",args, n);
        XtManageChild(but_phase_1[3]);
        XtAddCallback (but_phase_1[3], XmNactivateCallback, phase_o1_m1CB,
                                (XtPointer)donnees);

        n = 0;
        XtSetArg(args[n],XmNwidth,40); n++;
        XtSetArg(args[n],XmNheight,30); n++;
        XtSetArg(args[n], XmNx,145);n++;
        XtSetArg(args[n], XmNy,85);n++;
        but_phase_1[4] = XmCreatePushButton(toplevel_box,"+1",args, n);
        XtManageChild(but_phase_1[4]);
        XtAddCallback (but_phase_1[4], XmNactivateCallback, phase_o1_p1CB,
                                (XtPointer)donnees);


        n = 0;
        XtSetArg(args[n],XmNwidth,40); n++;
        XtSetArg(args[n],XmNheight,30); n++;
        XtSetArg(args[n], XmNx,190);n++;
        XtSetArg(args[n], XmNy,85);n++;
        but_phase_1[5] = XmCreatePushButton(toplevel_box,"+10",args, n);
        XtManageChild(but_phase_1[5]);
        XtAddCallback (but_phase_1[5], XmNactivateCallback, phase_o1_p10CB,
                                (XtPointer)donnees);


        n = 0;
        XtSetArg(args[n],XmNwidth,40); n++;
        XtSetArg(args[n],XmNheight,30); n++;
        XtSetArg(args[n], XmNx,235);n++;
        XtSetArg(args[n], XmNy,85);n++;
        but_phase_1[6] = XmCreatePushButton(toplevel_box,"+90",args, n);
        XtManageChild(but_phase_1[6]);
        XtAddCallback (but_phase_1[6], XmNactivateCallback, phase_o1_p90CB,
                                (XtPointer)donnees);



			/* OK et CANCEL buttons */

        n = 0;
        XtSetArg(args[n],XmNwidth,60); n++;
        XtSetArg(args[n],XmNheight,30); n++;
        XtSetArg(args[n], XmNx,50);n++;
        XtSetArg(args[n], XmNy,120);n++;
        but_ok = XmCreatePushButton(toplevel_box,"OK",args, n);
        XtManageChild(but_ok);
        XtAddCallback (but_ok, XmNactivateCallback, phase_okCB,
                                (XtPointer)donnees);



        n = 0;
        XtSetArg(args[n],XmNwidth,60); n++;
        XtSetArg(args[n],XmNheight,30); n++;
        XtSetArg(args[n], XmNx,170);n++;
        XtSetArg(args[n], XmNy,120);n++;
        but_cancel = XmCreatePushButton(toplevel_box,"Cancel",args, n);
        XtManageChild(but_cancel);
        XtAddCallback (but_cancel, XmNactivateCallback, phase_cancelCB,
                                (XtPointer)donnees);



                        /* Detournement de la commande CLOSE generale */
                        /* vers Close de la Widget                    */


         wm_delete_window = XmInternAtom(XtDisplay(toplevel_phase),
                                "WM_DELETE_WINDOW",
                                FALSE);
         XmRemoveWMProtocols(toplevel_phase,&wm_delete_window, 1);
         XmAddWMProtocolCallback(toplevel_phase, wm_delete_window, phase_closeCB, 
				(XtPointer)toplevel_phase);


			/* Realise Fenetre  generale */

        XtRealizeWidget(toplevel_phase);
	XtPopup (toplevel_phase, XtGrabNone);

			/* display du point pivot sur disp1d */

	set_pivot_phase(1);	/* init du pivot*/

			/* init ( flag = 0 ) et sauvegarde de spectrum */

	flag = 0;
	SET_PHASE_VAR(donnees->spectrum,&donnees->x0,
                      donnees->vd_id,&donnees->p0,&donnees->p1,&flag);


			/* entree dans la boucle */

	sortie_boucle_phase = -1;
	flag_bloquant = 1; /*flag general de mode bloquant: interdit l'appel a execute*/
	

      			 /* gestion des evenements */

         for (;;)
        {
         XtAppNextEvent(app_context,&event);
         bool = XtDispatchEvent(&event);
         if (sortie_boucle_phase == 1)
                {
                break;
                }
        }
	sortie_boucle_phase = 0;
	flag_bloquant  = 0 ; /*flag general de mode bloquant*/
}

        /* ------------- CALLBACK WIDGET PHASE ---------------- *

		 ORDRE 1 */

/*-------------------------------------------------------------
**      phase_o1_m90CB
*/
void phase_o1_m90CB(w, client_data, call_data)
Widget          w;              /*  widget id           */
Data_phase      *client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{
	int flag ;

	flag = 2;	/* phasing test callback */
	donnees->p1 = donnees->p1 - 90;
	SET_PHASE_VAR(client_data->spectrum,&client_data->x0,
		      client_data->vd_id,&donnees->p0,&donnees->p1,&flag);
}


/*-------------------------------------------------------------
**      phase_o1_m10CB
*/
void phase_o1_m10CB(w, client_data, call_data)
Widget          w;              /*  widget id           */
Data_phase      *client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{
        int flag ;

        flag = 2;/* phasing test callback */
	donnees->p1 = donnees->p1 - 10;
        SET_PHASE_VAR(client_data->spectrum,&client_data->x0,
                      client_data->vd_id,&donnees->p0,&donnees->p1,&flag);

}


/*-------------------------------------------------------------
**      phase_o1_m1CB
*/
void phase_o1_m1CB(w, client_data, call_data)
Widget          w;              /*  widget id           */
Data_phase      *client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{

        int flag ;

        flag = 2;/* phasing test callback */
	donnees->p1 = donnees->p1 - 1;
        SET_PHASE_VAR(client_data->spectrum,&client_data->x0,
                      client_data->vd_id,&donnees->p0,&donnees->p1,&flag);

}


/*-------------------------------------------------------------
**      phase_o1_p1CB
*/
void phase_o1_p1CB(w, client_data, call_data)
Widget          w;              /*  widget id           */
Data_phase      *client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{

        int flag ;

        flag = 2;/* phasing test callback */
	donnees->p1 = donnees->p1 + 1;
        SET_PHASE_VAR(client_data->spectrum,&client_data->x0,
                      client_data->vd_id,&donnees->p0,&donnees->p1,&flag);

}


/*-------------------------------------------------------------
**      phase_o1_p10CB
*/
void phase_o1_p10CB(w, client_data, call_data)
Widget          w;              /*  widget id           */
Data_phase      *client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{
        int flag ;

        flag = 2;/* phasing test callback */
	donnees->p1 = donnees->p1 + 10;
        SET_PHASE_VAR(client_data->spectrum,&client_data->x0,
                      client_data->vd_id,&donnees->p0,&donnees->p1,&flag);

}



/*-------------------------------------------------------------
**      phase_o1_p90CB
*/
void phase_o1_p90CB(w, client_data, call_data)
Widget          w;              /*  widget id           */
Data_phase      *client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{

        int flag ;

        flag = 2;/* phasing test callback */
	donnees->p1 = donnees->p1 + 90;
        SET_PHASE_VAR(client_data->spectrum,&client_data->x0,
                      client_data->vd_id,&donnees->p0,&donnees->p1,&flag);

}


/*-------------------------------------------------------------
**      phase_closeCB
*/
void phase_closeCB(w, client_data, call_data)
Widget          w;              /*  widget id           */
Data_phase      *client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{
	int cursor;

	XtDestroyWidget(client_data->mere);/* BUG LINUX: plante au close du form*/
	cursor = 0;
	WIN_SET_POINTER_PATTERN(client_data->vd_id,&cursor);
	sortie_boucle_phase = 1;
}

/*-------------------------------------------------------------
**      phase_okCB
*/
void phase_okCB(w, client_data, call_data)
Widget          w;              /*  widget id           */
Data_phase      *client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{

	int cursor,flag;

        XtDestroyWidget(client_data->mere);
	*ph0_loc = donnees->p0;
	*ph1_loc = donnees->p1;
	flag = 3; /* affichge des valeurs sous le prompt */
        SET_PHASE_VAR(client_data->spectrum,&client_data->x0,
                      client_data->vd_id,&donnees->p0,&donnees->p1,&flag);
	sortie_boucle_phase = 1;
	cursor = 0;
	WIN_SET_POINTER_PATTERN(client_data->vd_id,&cursor);

}

/*-------------------------------------------------------------
**      phase_cancelCB
*/
void phase_cancelCB(w, client_data, call_data)
Widget          w;              /*  widget id           */
Data_phase      *client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{

        int cursor;
	int flag ;

        flag = 1;/* cancel callback */


        XtDestroyWidget(client_data->mere);
	printf(" no phase correction \n");
	donnees->p0 = 0.0 ;
	donnees->p1 = 0.0 ;
	*ph0_loc = 0.0;
        *ph1_loc = 0.0;
        SET_PHASE_VAR(client_data->spectrum,&client_data->x0,
                      client_data->vd_id,&donnees->p0,&donnees->p1,&flag);

	sortie_boucle_phase = 1;
	cursor = 0;
	WIN_SET_POINTER_PATTERN(client_data->vd_id,&cursor);
}

		/* ORDRE 0 */

/*-------------------------------------------------------------
**      phase_o0_m90CB
*/
void phase_o0_m90CB(w, client_data, call_data)
Widget          w;              /*  widget id           */
Data_phase      *client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{
        int flag ;

        flag = 2;/* phasing test callback */
	donnees->p0 = donnees->p0 - 90 ;
        SET_PHASE_VAR(client_data->spectrum,&client_data->x0,
                      client_data->vd_id,&donnees->p0,&donnees->p1,&flag);

}


/*-------------------------------------------------------------
**      phase_o0_m10CB
*/
void phase_o0_m10CB(w, client_data, call_data)
Widget          w;              /*  widget id           */
Data_phase      *client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{

        int flag ;

        flag = 2;/* phasing test callback */
	donnees->p0 = donnees->p0 - 10 ;
        SET_PHASE_VAR(client_data->spectrum,&client_data->x0,
                      client_data->vd_id,&donnees->p0,&donnees->p1,&flag);

}

/*-------------------------------------------------------------
**      phase_o0_m1CB
*/
void phase_o0_m1CB(w, client_data, call_data)
Widget          w;              /*  widget id           */
Data_phase      *client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{
        int flag ;

        flag = 2;/* phasing test callback */
	donnees->p0 = donnees->p0 - 1 ;
        SET_PHASE_VAR(client_data->spectrum,&client_data->x0,
                      client_data->vd_id,&donnees->p0,&donnees->p1,&flag);

}


/*-------------------------------------------------------------
**      phase_o0_p1CB
*/
void phase_o0_p1CB(w, client_data, call_data)
Widget          w;              /*  widget id           */
Data_phase      *client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{

        int flag ;

        flag = 2;/* phasing test callback */
	donnees->p0 = donnees->p0 + 1 ;
        SET_PHASE_VAR(client_data->spectrum,&client_data->x0,
                      client_data->vd_id,&donnees->p0,&donnees->p1,&flag);

}


/*-------------------------------------------------------------
**      phase_o0_p10CB
*/
void phase_o0_p10CB(w, client_data, call_data)
Widget          w;              /*  widget id           */
Data_phase      *client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{
        int flag ;

        flag = 2;/* phasing test callback */
	donnees->p0 = donnees->p0 + 10 ;
        SET_PHASE_VAR(client_data->spectrum,&client_data->x0,
                      client_data->vd_id,&donnees->p0,&donnees->p1,&flag);
}



/*-------------------------------------------------------------
**      phase_o0_p90CB
*/
void phase_o0_p90CB(w, client_data, call_data)
Widget          w;              /*  widget id           */
Data_phase      *client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{
        int flag ;

        flag = 2;/* phasing test callback */
	donnees->p0 = donnees->p0 + 90 ;
        SET_PHASE_VAR(client_data->spectrum,&client_data->x0,
                      client_data->vd_id,&donnees->p0,&donnees->p1,&flag);



}

/*-------------------------------------------------------------
**      set_pivot_phase
*/
void set_pivot_phase(flag)
int flag;
{

	float coord1,coord2;
	int ok;

	coord1 = 0.0;
	coord2 = 0.0;


	appel_phase = 1;
	if(flag != 2)
		DOREFRESH();
	if(flag == 1){	/*initialisation*/
	DISPLAY_PIVOT_POINT(&coord1,&donnees->p0,
			&donnees->p1,&donnees->x0,donnees->vd_id,&flag);
	WIN_UPDATE(donnees->vd_id);
	}

	if(flag == 2){  /* reaffichage */
        DISPLAY_PIVOT_POINT(&coord1,&donnees->p0,
                        &donnees->p1,&donnees->x0,donnees->vd_id,&flag);
	}

	/* flag = 3 : recalcul due la position du pivot */
	if(flag == 3){
	ok = WIN_GET_POINTER_POSITION(donnees->vd_id,&coord1,&coord2);
	if(ok != 0) 
        DISPLAY_PIVOT_POINT(&coord1,&donnees->p0,
                        &donnees->p1,&donnees->x0,donnees->vd_id,&flag);
	WIN_UPDATE(donnees->vd_id);
	}
	appel_phase = 0;
	
}

