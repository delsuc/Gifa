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
#  include <Xm/Scale.h>
#  include <Xm/Form.h>
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

void CREATE_3DBOX();
void d3_arrow_upCB();
void d3_refreshCB();
void update_znotCB();
void d3_arrow_downCB();
void d3_arrow_leftCB();
void d3_arrow_rightCB();
void d3_scale_p10CB();
void d3_scale_m10CB();
void d3_scale_resetCB();
void d3_closeCB();
void rotation_F12CB();
void rotation_F13CB();
void rotation_F23CB();
void rotation_F123CB();
void alpha_p10CB();
void alpha_p1CB();
void alpha_m10CB();
void alpha_m1CB();
void beta_m1CB();
void beta_m10CB();
void beta_p1CB();
void beta_p10CB();
void gama_p10CB();
void gama_p1CB();
void gama_m10CB();
void gama_m1CB();




/*-------------------------------------------------------------
*		Variables Globales
*/

typedef struct {
	int *vd_id;
	float  alpha,beta,gama,sc3d,z0;
        float  ox,oy,oz,dx3d,dy3d,dz3d;
	float sc3dinit;
}Data_3d;

Data_3d *donnees_3d;



/*-------------------------------------------------------------
*     Variables externes
*/

extern int versionx;
extern int flag_bloquant;
extern XtAppContext app_context;
extern Widget window_mere;
extern Cursor cursor1,cursor2,cursor_wait;
extern Widget widget[10];
extern int win[10];
extern int windoww[10];
extern int windowh[10];
extern int windowd[10];
extern Widget zoom_set;
Widget d3box_set;
extern int sortie_boucle_phase;


        /* ------------- FONCTIONS GENERALES ---------------- */


/*-------------------------------------------------------------
**         create_d3box
*/

void CREATE_3DBOX(vd_id)
int *vd_id;
/*
	Creation de la boite 3D
	et des differents boutons:
		shift
		scale...
*/
{
    Widget      toplevel_d3,toplevel_box,arrow[4],scale_but[5],title[6],
		close,refresh,frame[4],label[10],alpha[4],beta[4],gama[4],
		rotation[4],form_znot,label_znot,scale_znot;

    char dimlab[50];
    XmString xms;
    Arg arg;
    Arg args[30];
    int n, dpy,position_x,dim;
    unsigned int  width_screen;
    int screen;
    Display *display;
    char st[80];
    int argc;
    Atom wm_delete_window,wm_resize_window;
    char position[50];
    int znot_int;



                        /* test du display */

      if (versionx == 0){
                 printf("Gifa cannot open the display on X server\n");
                return;
      }


                       /* creation du shell du widget d3 */

   if(d3box_set == 0){


			/* malloc data struc */

	donnees_3d = malloc(sizeof(Data_3d));
        GET_VAR_3D(&donnees_3d->alpha,&donnees_3d->beta,&donnees_3d->gama,
                        &donnees_3d->sc3d,&donnees_3d->z0, &donnees_3d->ox,
                        &donnees_3d->oy,&donnees_3d->oz,&donnees_3d->dx3d,
                        &donnees_3d->dy3d,&donnees_3d->dz3d);
	donnees_3d->sc3dinit = donnees_3d->sc3d; /* pour reset */
        donnees_3d->vd_id = vd_id;



			/* recherche de la position de la boite */

        display = XtDisplay(window_mere);
        screen = DefaultScreen(display);
        width_screen = DisplayWidth(display,screen);
	if(zoom_set != 0){
        	position_x = (width_screen - 310);
	}
	else{
		position_x = (width_screen - 155);
	}
	sprintf(position,"+%d+1",position_x);

			/* creation */

	n = 0;
	XtSetArg(args[n], XtNgeometry,position); n++ ;
        toplevel_d3 = XtCreatePopupShell ("3D Box", topLevelShellWidgetClass,
                        window_mere, args,n);


                        /* Elimination  de la commande Resize */


         wm_resize_window = XmInternAtom(XtDisplay(toplevel_d3),
                                "WM_SIZE_WINDOW",
                                FALSE);
         XmRemoveWMProtocols(toplevel_d3,&wm_resize_window, 1);

                        /* Icone de presentation */

         SetAppIcon(toplevel_d3,7);

			/* stockage des widgets affiches */

	add_shell_on_display(toplevel_d3);


                        /* Create BulletinBoard box avec taille . */

	n = 0;
 	XtSetArg(args[n], XmNheight,540);n++;	
	XtSetArg(args[n], XmNwidth, 155);n++;
/*
ICI
        XtSetArg (args[n], XmNnoResize, True);n++;
*/
        toplevel_box = XmCreateBulletinBoard (toplevel_d3, "work_area", args, n);
	XtManageChild(toplevel_box);


			/* Create Arrows */

        n = 0;
        XtSetArg(args[n], XmNx,10);n++;
        XtSetArg(args[n], XmNy,3);n++;
        title[0] = XmCreateLabel(toplevel_box, "Shift", args, n);
        XtManageChild (title[0]);



	n = 0;
	XtSetArg(args[n], XmNx,65);n++;
	XtSetArg(args[n], XmNy,30);n++;
	XtSetArg(args[n], XmNarrowDirection,XmARROW_UP);n++;
	arrow[0] = XmCreateArrowButton(toplevel_box,"Up", args, n);
	XtManageChild(arrow[0]);
        XtAddCallback (arrow[0], XmNactivateCallback, d3_arrow_upCB,
                     NULL);


	n = 0;
        XtSetArg(args[n], XmNx,65);n++;
        XtSetArg(args[n], XmNy,70);n++;
        XtSetArg(args[n], XmNarrowDirection,XmARROW_DOWN);n++;
	arrow[1] = XmCreateArrowButton(toplevel_box,"Down",args, n);
        XtManageChild(arrow[1]);
        XtAddCallback (arrow[1], XmNactivateCallback, d3_arrow_downCB,
                     NULL);


        n = 0;
        XtSetArg(args[n], XmNx,30);n++;
        XtSetArg(args[n], XmNy,50);n++;
        XtSetArg(args[n], XmNarrowDirection,XmARROW_LEFT);n++;
	arrow[2] = XmCreateArrowButton(toplevel_box,"Left",args, n);
        XtManageChild(arrow[2]);
        XtAddCallback (arrow[2], XmNactivateCallback, d3_arrow_leftCB,
                     NULL);


        n = 0;
        XtSetArg(args[n], XmNx,100);n++;
        XtSetArg(args[n], XmNy,50);n++;
        XtSetArg(args[n], XmNarrowDirection,XmARROW_RIGHT);n++;
	arrow[3] = XmCreateArrowButton(toplevel_box,"Right",args, n);
	XtManageChild(arrow[3]);
        XtAddCallback (arrow[3], XmNactivateCallback, d3_arrow_rightCB,
                     NULL);





			/* Realize Scale */

        n = 0;
        XtSetArg(args[n], XmNx,10);n++;
        XtSetArg(args[n], XmNy,105);n++;
        title[1] = XmCreateLabel(toplevel_box, "Size", args, n);
        XtManageChild (title[1]);


        n = 0;
        XtSetArg(args[n],XmNwidth,55); n++;
        XtSetArg(args[n],XmNheight,30); n++;
        XtSetArg(args[n], XmNx,20);n++;
        XtSetArg(args[n], XmNy,125);n++;
	scale_but[0] = XmCreatePushButton(toplevel_box,"-10%",args, n);
	XtManageChild(scale_but[0]);
        XtAddCallback (scale_but[0], XmNactivateCallback, d3_scale_m10CB,
                     NULL);


        n = 0;
        XtSetArg(args[n],XmNwidth,55); n++;
        XtSetArg(args[n],XmNheight,30); n++;
        XtSetArg(args[n], XmNx,80);n++;
        XtSetArg(args[n], XmNy,125);n++;
        scale_but[1] = XmCreatePushButton(toplevel_box,"+10%",args, n);
        XtManageChild(scale_but[1]);
        XtAddCallback (scale_but[1], XmNactivateCallback, d3_scale_p10CB,
                     NULL);


        n = 0;
        XtSetArg(args[n],XmNwidth,115); n++;
        XtSetArg(args[n],XmNheight,25); n++;
        XtSetArg(args[n], XmNx,20);n++;
        XtSetArg(args[n], XmNy,160);n++;
        XtSetArg(args[n], XmNrecomputeSize,False);n++;
        scale_but[4] = XmCreatePushButton(toplevel_box,"Reset",args, n);
        XtManageChild(scale_but[4]);
        XtAddCallback (scale_but[4], XmNactivateCallback, d3_scale_resetCB,
                     NULL);



			/* create button rotation */

        n = 0;
        XtSetArg(args[n], XmNx,10);n++;
        XtSetArg(args[n], XmNy,200);n++;
        title[3] = XmCreateLabel(toplevel_box, "Prefixed Positions", args, n);
        XtManageChild (title[3]);


        n = 0;
        XtSetArg(args[n],XmNwidth,55); n++;
        XtSetArg(args[n],XmNheight,30); n++;
        XtSetArg(args[n], XmNx,20);n++;
        XtSetArg(args[n], XmNy,220);n++;
        rotation[0] = XmCreatePushButton(toplevel_box,"F1+F2",args, n);
        XtManageChild(rotation[0]);
        XtAddCallback (rotation[0], XmNactivateCallback, rotation_F12CB,NULL);


        n = 0;
        XtSetArg(args[n],XmNwidth,55); n++;
        XtSetArg(args[n],XmNheight,30); n++;
        XtSetArg(args[n], XmNx,80);n++;
        XtSetArg(args[n], XmNy,220);n++;
        rotation[1] = XmCreatePushButton(toplevel_box,"F1+F3",args, n);
        XtManageChild(rotation[1]);
        XtAddCallback (rotation[1], XmNactivateCallback, rotation_F13CB,NULL);


        n = 0;
        XtSetArg(args[n],XmNwidth,55); n++;
        XtSetArg(args[n],XmNheight,30); n++;
        XtSetArg(args[n], XmNx,20);n++;
        XtSetArg(args[n], XmNy,255);n++;
        rotation[2] = XmCreatePushButton(toplevel_box,"F2+F3",args, n);
        XtManageChild(rotation[2]);
        XtAddCallback (rotation[2], XmNactivateCallback, rotation_F23CB,NULL);


        n = 0;
        XtSetArg(args[n],XmNwidth,55); n++;
        XtSetArg(args[n],XmNheight,30); n++;
        XtSetArg(args[n], XmNx,80);n++;
        XtSetArg(args[n], XmNy,255);n++;
        rotation[3] = XmCreatePushButton(toplevel_box,"F1+2+3",args, n);
        XtManageChild(rotation[3]);
        XtAddCallback (rotation[3], XmNactivateCallback, rotation_F123CB,NULL);


                        /* create button Alpha,Beta,Gama */ 

		/* ALPHA */

        n = 0;
        XtSetArg(args[n], XmNx,10);n++;
        XtSetArg(args[n], XmNy,295);n++;
        title[4] = XmCreateLabel(toplevel_box, "Alpha", args, n);
        XtManageChild (title[4]);

        n = 0;
        XtSetArg(args[n],XmNwidth,30); n++;
        XtSetArg(args[n],XmNheight,25); n++;
        XtSetArg(args[n], XmNx,10);n++;
        XtSetArg(args[n], XmNy,315);n++;
        alpha[0] = XmCreatePushButton(toplevel_box,"-10",args, n);
        XtManageChild(alpha[0]);
        XtAddCallback (alpha[0], XmNactivateCallback, alpha_m10CB,NULL);

        n = 0;
        XtSetArg(args[n],XmNwidth,30); n++;
        XtSetArg(args[n],XmNheight,25); n++;
        XtSetArg(args[n], XmNx,45);n++;
        XtSetArg(args[n], XmNy,315);n++;
        alpha[1] = XmCreatePushButton(toplevel_box,"-1",args, n);
        XtManageChild(alpha[1]);
        XtAddCallback (alpha[1], XmNactivateCallback, alpha_m1CB,NULL);

        n = 0;
        XtSetArg(args[n],XmNwidth,30); n++;
        XtSetArg(args[n],XmNheight,25); n++;
        XtSetArg(args[n], XmNx,80);n++;
        XtSetArg(args[n], XmNy,315);n++;
        alpha[2] = XmCreatePushButton(toplevel_box,"+1",args, n);
        XtManageChild(alpha[2]);
        XtAddCallback (alpha[2], XmNactivateCallback, alpha_p1CB,NULL);

        n = 0;
        XtSetArg(args[n],XmNwidth,30); n++;
        XtSetArg(args[n],XmNheight,25); n++;
        XtSetArg(args[n], XmNx,115);n++;
        XtSetArg(args[n], XmNy,315);n++;
        alpha[3] = XmCreatePushButton(toplevel_box,"+10",args, n);
        XtManageChild(alpha[3]);
        XtAddCallback (alpha[3], XmNactivateCallback, alpha_p10CB,NULL);

		/* BETA */

        n = 0;
        XtSetArg(args[n], XmNx,10);n++;
        XtSetArg(args[n], XmNy,340);n++;
        title[4] = XmCreateLabel(toplevel_box, "Beta", args, n);
        XtManageChild (title[4]);

        n = 0;
        XtSetArg(args[n],XmNwidth,30); n++;
        XtSetArg(args[n],XmNheight,25); n++;
        XtSetArg(args[n], XmNx,10);n++;
        XtSetArg(args[n], XmNy,360);n++;
        beta[0] = XmCreatePushButton(toplevel_box,"-10",args, n);
        XtManageChild(beta[0]);
        XtAddCallback (beta[0], XmNactivateCallback, beta_m10CB,NULL);

        n = 0;
        XtSetArg(args[n],XmNwidth,30); n++;
        XtSetArg(args[n],XmNheight,25); n++;
        XtSetArg(args[n], XmNx,45);n++;
        XtSetArg(args[n], XmNy,360);n++;
        beta[1] = XmCreatePushButton(toplevel_box,"-1",args, n);
        XtManageChild(beta[1]);
        XtAddCallback (beta[1], XmNactivateCallback, beta_m1CB,NULL);

        n = 0;
        XtSetArg(args[n],XmNwidth,30); n++;
        XtSetArg(args[n],XmNheight,25); n++;
        XtSetArg(args[n], XmNx,80);n++;
        XtSetArg(args[n], XmNy,360);n++;
        beta[2] = XmCreatePushButton(toplevel_box,"+1",args, n);
        XtManageChild(beta[2]);
        XtAddCallback (beta[2], XmNactivateCallback, beta_p1CB,NULL);

        n = 0;
        XtSetArg(args[n],XmNwidth,30); n++;
        XtSetArg(args[n],XmNheight,25); n++;
        XtSetArg(args[n], XmNx,115);n++;
        XtSetArg(args[n], XmNy,360);n++;
        beta[3] = XmCreatePushButton(toplevel_box,"+10",args, n);
        XtManageChild(beta[3]);
        XtAddCallback (beta[3], XmNactivateCallback, beta_p10CB,NULL);


		/* GAMA */
        n = 0;
        XtSetArg(args[n], XmNx,10);n++;
        XtSetArg(args[n], XmNy,385);n++;
        title[4] = XmCreateLabel(toplevel_box, "Gama", args, n);
        XtManageChild (title[4]);

        n = 0;
        XtSetArg(args[n],XmNwidth,30); n++;
        XtSetArg(args[n],XmNheight,25); n++;
        XtSetArg(args[n], XmNx,10);n++;
        XtSetArg(args[n], XmNy,405);n++;
        gama[0] = XmCreatePushButton(toplevel_box,"-10",args, n);
        XtManageChild(gama[0]);
        XtAddCallback (gama[0], XmNactivateCallback, gama_m10CB,NULL);

        n = 0;
        XtSetArg(args[n],XmNwidth,30); n++;
        XtSetArg(args[n],XmNheight,25); n++;
        XtSetArg(args[n], XmNx,45);n++;
        XtSetArg(args[n], XmNy,405);n++;
        gama[1] = XmCreatePushButton(toplevel_box,"-1",args, n);
        XtManageChild(gama[1]);
        XtAddCallback (gama[1], XmNactivateCallback, gama_m1CB,NULL);

        n = 0;
        XtSetArg(args[n],XmNwidth,30); n++;
        XtSetArg(args[n],XmNheight,25); n++;
        XtSetArg(args[n], XmNx,80);n++;
        XtSetArg(args[n], XmNy,405);n++;
        gama[2] = XmCreatePushButton(toplevel_box,"+1",args, n);
        XtManageChild(gama[2]);
        XtAddCallback (gama[2], XmNactivateCallback, gama_p1CB,NULL);

        n = 0;
        XtSetArg(args[n],XmNwidth,30); n++;
        XtSetArg(args[n],XmNheight,25); n++;
        XtSetArg(args[n], XmNx,115);n++;
        XtSetArg(args[n], XmNy,405);n++;
        gama[3] = XmCreatePushButton(toplevel_box,"+10",args, n);
        XtManageChild(gama[3]);
        XtAddCallback (gama[3], XmNactivateCallback, gama_p10CB,NULL);

			/* Scale de Znot */

	n = 0;
        XtSetArg(args[n],XmNwidth,135); n++;
        XtSetArg(args[n],XmNheight,65); n++;
        XtSetArg(args[n], XmNx,5);n++;
        XtSetArg(args[n], XmNy,435);n++;
        XtSetArg (args[n], XmNshadowType, XmSHADOW_ETCHED_IN );  n++;
        form_znot = XmCreateForm(toplevel_box,"",args, n);
        XtManageChild (form_znot);

	znot_int = (int)donnees_3d->z0;
/* ASTUCE A REFAIRE en echelle log */
	if(znot_int > 100) znot_int= 100;
	n = 0;
	XtSetArg (args[n], XmNorientation, XmHORIZONTAL);  n++;
    	XtSetArg (args[n], XmNshowValue, True);  n++;
    	XtSetArg (args[n], XmNminimum, 1);  n++;
	XtSetArg (args[n], XmNmaximum, 100); n++;
	XtSetArg (args[n], XmNleftAttachment, XmATTACH_FORM);  n++;
	XtSetArg (args[n], XmNrightAttachment, XmATTACH_FORM);  n++;
    	XtSetArg (args[n], XmNvalue, znot_int);  n++;
    	scale_znot = XmCreateScale(form_znot, "znot", args, n);
    	XtAddCallback (scale_znot, XmNvalueChangedCallback, update_znotCB, NULL);
    	XtManageChild (scale_znot);

    	n = 0;
    	label_znot = XmCreateLabel(scale_znot, "Znot : ", args, n);
    	XtManageChild (label_znot);

        n = 0;
        XtSetArg(args[n],XmNwidth,115); n++;
        XtSetArg(args[n],XmNheight,25); n++;
        XtSetArg(args[n], XmNx,20);n++;
        XtSetArg(args[n], XmNy,500);n++;
        XtSetArg(args[n], XmNrecomputeSize,False);n++;
        refresh = XmCreatePushButton(toplevel_box,"Refresh",args, n);
        XtManageChild(refresh);
        XtAddCallback (refresh, XmNactivateCallback, d3_refreshCB,
                     NULL);




                        /* Detournement de la commande CLOSE generale */
                        /* vers Close de la Widget                    */


         wm_delete_window = XmInternAtom(XtDisplay(toplevel_d3),
                                "WM_DELETE_WINDOW",
                                FALSE);
         XmRemoveWMProtocols(toplevel_d3,&wm_delete_window, 1);
         XmAddWMProtocolCallback(toplevel_d3, wm_delete_window,d3_closeCB, 
				(XtPointer)toplevel_d3);

			/* Realise Fenetre  generale */


        XtRealizeWidget(toplevel_d3);
	XtPopup (toplevel_d3, XtGrabNone);

	d3box_set = toplevel_d3;


	SET_VAR_3D(donnees_3d->vd_id,&donnees_3d->alpha,&donnees_3d->beta,
                    &donnees_3d->gama,
                    &donnees_3d->sc3d,&donnees_3d->z0, &donnees_3d->ox,
                    &donnees_3d->oy,&donnees_3d->oz,&donnees_3d->dx3d,
                    &donnees_3d->dy3d,&donnees_3d->dz3d);


    }
    else{
	 XtPopup(d3box_set,XtGrabNone);
    }

}



        /* ------------- CALLBACK WIDGET d3 ---------------- *


-------------------------------------------------------------
**	d3_arrow_upCB      
*/
void d3_arrow_upCB(w, client_data, call_data)
Widget          w;              /*  widget id           */
XtPointer       client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{

        GET_VAR_3D(&donnees_3d->alpha,&donnees_3d->beta,&donnees_3d->gama,
                        &donnees_3d->sc3d,&donnees_3d->z0, &donnees_3d->ox,
                        &donnees_3d->oy,&donnees_3d->oz,&donnees_3d->dx3d,
                        &donnees_3d->dy3d,&donnees_3d->dz3d);


	donnees_3d->oy = donnees_3d->oy + 0.1;

        SET_VAR_3D(donnees_3d->vd_id,&donnees_3d->alpha,&donnees_3d->beta,
                        &donnees_3d->gama,
                        &donnees_3d->sc3d,&donnees_3d->z0, &donnees_3d->ox,
                        &donnees_3d->oy,&donnees_3d->oz,&donnees_3d->dx3d,
                        &donnees_3d->dy3d,&donnees_3d->dz3d);


}

/*-------------------------------------------------------------
**      d3_arrow_downCB
*/
void d3_arrow_downCB(w, client_data, call_data)
Widget          w;              /*  widget id           */
XtPointer       client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{

        GET_VAR_3D(&donnees_3d->alpha,&donnees_3d->beta,&donnees_3d->gama,
                        &donnees_3d->sc3d,&donnees_3d->z0, &donnees_3d->ox,
                        &donnees_3d->oy,&donnees_3d->oz,&donnees_3d->dx3d,
                        &donnees_3d->dy3d,&donnees_3d->dz3d);



	donnees_3d->oy = donnees_3d->oy - 0.1;

        SET_VAR_3D(donnees_3d->vd_id,&donnees_3d->alpha,&donnees_3d->beta,
                        &donnees_3d->gama,
                        &donnees_3d->sc3d,&donnees_3d->z0, &donnees_3d->ox,
                        &donnees_3d->oy,&donnees_3d->oz,&donnees_3d->dx3d,
                        &donnees_3d->dy3d,&donnees_3d->dz3d);



}

/*-------------------------------------------------------------
**      d3_arrow_leftCB
*/
void d3_arrow_leftCB(w, client_data, call_data)
Widget          w;              /*  widget id           */
XtPointer       client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{


        GET_VAR_3D(&donnees_3d->alpha,&donnees_3d->beta,&donnees_3d->gama,
                        &donnees_3d->sc3d,&donnees_3d->z0, &donnees_3d->ox,
                        &donnees_3d->oy,&donnees_3d->oz,&donnees_3d->dx3d,
                        &donnees_3d->dy3d,&donnees_3d->dz3d);



	donnees_3d->ox = donnees_3d->ox - 0.1;

        SET_VAR_3D(donnees_3d->vd_id,&donnees_3d->alpha,&donnees_3d->beta,
                        &donnees_3d->gama,
                        &donnees_3d->sc3d,&donnees_3d->z0, &donnees_3d->ox,
                        &donnees_3d->oy,&donnees_3d->oz,&donnees_3d->dx3d,
                        &donnees_3d->dy3d,&donnees_3d->dz3d);


}

/*-------------------------------------------------------------
**      d3_arrow_rightCB
*/
void d3_arrow_rightCB(w, client_data, call_data)
Widget          w;              /*  widget id           */
XtPointer       client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */

{

        GET_VAR_3D(&donnees_3d->alpha,&donnees_3d->beta,&donnees_3d->gama,
                        &donnees_3d->sc3d,&donnees_3d->z0, &donnees_3d->ox,
                        &donnees_3d->oy,&donnees_3d->oz,&donnees_3d->dx3d,
                        &donnees_3d->dy3d,&donnees_3d->dz3d);


	donnees_3d->ox = donnees_3d->ox + 0.1;

        SET_VAR_3D(donnees_3d->vd_id,&donnees_3d->alpha,&donnees_3d->beta,
                        &donnees_3d->gama,
                        &donnees_3d->sc3d,&donnees_3d->z0, &donnees_3d->ox,
                        &donnees_3d->oy,&donnees_3d->oz,&donnees_3d->dx3d,
                        &donnees_3d->dy3d,&donnees_3d->dz3d);


}

/*-------------------------------------------------------------
**      d3_scale_p10CB
*/
void d3_scale_p10CB(w, client_data, call_data)
Widget          w;              /*  widget id           */
XtPointer       client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{


        GET_VAR_3D(&donnees_3d->alpha,&donnees_3d->beta,&donnees_3d->gama,
                        &donnees_3d->sc3d,&donnees_3d->z0, &donnees_3d->ox,
                        &donnees_3d->oy,&donnees_3d->oz,&donnees_3d->dx3d,
                        &donnees_3d->dy3d,&donnees_3d->dz3d);


	donnees_3d->sc3d = donnees_3d->sc3d * 1.1;


        SET_VAR_3D(donnees_3d->vd_id,&donnees_3d->alpha,&donnees_3d->beta,
                        &donnees_3d->gama,
                        &donnees_3d->sc3d,&donnees_3d->z0, &donnees_3d->ox,
                        &donnees_3d->oy,&donnees_3d->oz,&donnees_3d->dx3d,
                        &donnees_3d->dy3d,&donnees_3d->dz3d);


}

/*-------------------------------------------------------------
**      d3_scale_m10CB
*/
void d3_scale_m10CB(w, client_data, call_data)
Widget          w;              /*  widget id           */
XtPointer       client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{


        GET_VAR_3D(&donnees_3d->alpha,&donnees_3d->beta,&donnees_3d->gama,
                        &donnees_3d->sc3d,&donnees_3d->z0, &donnees_3d->ox,
                        &donnees_3d->oy,&donnees_3d->oz,&donnees_3d->dx3d,
                        &donnees_3d->dy3d,&donnees_3d->dz3d);



	donnees_3d->sc3d = donnees_3d->sc3d / 1.1;



        SET_VAR_3D(donnees_3d->vd_id,&donnees_3d->alpha,&donnees_3d->beta,
                        &donnees_3d->gama,
                        &donnees_3d->sc3d,&donnees_3d->z0, &donnees_3d->ox,
                        &donnees_3d->oy,&donnees_3d->oz,&donnees_3d->dx3d,
                        &donnees_3d->dy3d,&donnees_3d->dz3d);


}

/*-------------------------------------------------------------
**     d3_scale_resetCB
*/
void d3_scale_resetCB(w, client_data, call_data)
Widget          w;              /*  widget id           */
XtPointer       client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{

	donnees_3d->sc3d = donnees_3d->sc3dinit; 

        SET_VAR_3D(donnees_3d->vd_id,&donnees_3d->alpha,&donnees_3d->beta,
                        &donnees_3d->gama,
                        &donnees_3d->sc3d,&donnees_3d->z0, &donnees_3d->ox,
                        &donnees_3d->oy,&donnees_3d->oz,&donnees_3d->dx3d,
                        &donnees_3d->dy3d,&donnees_3d->dz3d);


}


/*-------------------------------------------------------------
**      d3_closeCB
*/
void d3_closeCB(w, client_data, call_data)
Widget          w;              /*  widget id           */
XtPointer       client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{
	XtDestroyWidget(client_data);
	del_shell_on_display(client_data);
	d3box_set = 0;
}

/*-------------------------------------------------------------
**     rotation_F12CB
*/
void rotation_F12CB(w, client_data, call_data)
Widget          w;              /*  widget id           */
XtPointer       client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{


	
	donnees_3d->ox = 0.2;
	donnees_3d->oy = 0.2;
	donnees_3d->alpha = 0;
	donnees_3d->beta = -90;
	donnees_3d->gama = -90;

        SET_VAR_3D(donnees_3d->vd_id,&donnees_3d->alpha,&donnees_3d->beta,
                        &donnees_3d->gama,
                        &donnees_3d->sc3d,&donnees_3d->z0, &donnees_3d->ox,
                        &donnees_3d->oy,&donnees_3d->oz,&donnees_3d->dx3d,
                        &donnees_3d->dy3d,&donnees_3d->dz3d);


}

/*-------------------------------------------------------------
**     rotation_F13CB
*/
void rotation_F13CB(w, client_data, call_data)
Widget          w;              /*  widget id           */
XtPointer       client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{


        donnees_3d->ox = 0.2;
        donnees_3d->oy = 0.2;
        donnees_3d->alpha = 0;
        donnees_3d->beta = 0;
        donnees_3d->gama = -90;


        SET_VAR_3D(donnees_3d->vd_id,&donnees_3d->alpha,&donnees_3d->beta,
                        &donnees_3d->gama,
                        &donnees_3d->sc3d,&donnees_3d->z0, &donnees_3d->ox,
                        &donnees_3d->oy,&donnees_3d->oz,&donnees_3d->dx3d,
                        &donnees_3d->dy3d,&donnees_3d->dz3d);


}
/*-------------------------------------------------------------
**     rotation_F23CB
*/
void rotation_F23CB(w, client_data, call_data)
Widget          w;              /*  widget id           */
XtPointer       client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{



        donnees_3d->ox = 0.2;
        donnees_3d->oy = 0.2;
        donnees_3d->alpha = 0;
        donnees_3d->beta = 0;
        donnees_3d->gama = 0;


        SET_VAR_3D(donnees_3d->vd_id,&donnees_3d->alpha,&donnees_3d->beta,
                        &donnees_3d->gama,
                        &donnees_3d->sc3d,&donnees_3d->z0, &donnees_3d->ox,
                        &donnees_3d->oy,&donnees_3d->oz,&donnees_3d->dx3d,
                        &donnees_3d->dy3d,&donnees_3d->dz3d);


}
/*-------------------------------------------------------------
**     rotation_F123CB
*/
void rotation_F123CB(w, client_data, call_data)
Widget          w;              /*  widget id           */
XtPointer       client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{




        donnees_3d->ox = 0.5;
        donnees_3d->oy = 0.5;
        donnees_3d->alpha = 150;
        donnees_3d->beta = 145;
        donnees_3d->gama = 45;

        SET_VAR_3D(donnees_3d->vd_id,&donnees_3d->alpha,&donnees_3d->beta,
                        &donnees_3d->gama,
                        &donnees_3d->sc3d,&donnees_3d->z0, &donnees_3d->ox,
                        &donnees_3d->oy,&donnees_3d->oz,&donnees_3d->dx3d,
                        &donnees_3d->dy3d,&donnees_3d->dz3d);


}

/*-------------------------------------------------------------
**    alpha_p1CB
*/
void alpha_p1CB(w, client_data, call_data)
Widget          w;              /*  widget id           */
XtPointer       client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{

        GET_VAR_3D(&donnees_3d->alpha,&donnees_3d->beta,&donnees_3d->gama,
                        &donnees_3d->sc3d,&donnees_3d->z0, &donnees_3d->ox,
                        &donnees_3d->oy,&donnees_3d->oz,&donnees_3d->dx3d,
                        &donnees_3d->dy3d,&donnees_3d->dz3d);

        donnees_3d->alpha = donnees_3d->alpha + 1;

        SET_VAR_3D(donnees_3d->vd_id,&donnees_3d->alpha,&donnees_3d->beta,
                        &donnees_3d->gama,
                        &donnees_3d->sc3d,&donnees_3d->z0, &donnees_3d->ox,
                        &donnees_3d->oy,&donnees_3d->oz,&donnees_3d->dx3d,
                        &donnees_3d->dy3d,&donnees_3d->dz3d);


}



/*-------------------------------------------------------------
**    alpha_p10CB 
*/
void alpha_p10CB(w, client_data, call_data)
Widget          w;              /*  widget id           */
XtPointer       client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{

        GET_VAR_3D(&donnees_3d->alpha,&donnees_3d->beta,&donnees_3d->gama,
                        &donnees_3d->sc3d,&donnees_3d->z0, &donnees_3d->ox,
                        &donnees_3d->oy,&donnees_3d->oz,&donnees_3d->dx3d,
                        &donnees_3d->dy3d,&donnees_3d->dz3d);


	donnees_3d->alpha = donnees_3d->alpha + 10;

        SET_VAR_3D(donnees_3d->vd_id,&donnees_3d->alpha,&donnees_3d->beta,
                        &donnees_3d->gama,
                        &donnees_3d->sc3d,&donnees_3d->z0, &donnees_3d->ox,
                        &donnees_3d->oy,&donnees_3d->oz,&donnees_3d->dx3d,
                        &donnees_3d->dy3d,&donnees_3d->dz3d);


}
/*-------------------------------------------------------------
**    alpha_m10CB 
*/
void alpha_m10CB(w, client_data, call_data)
Widget          w;              /*  widget id           */
XtPointer       client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{

        GET_VAR_3D(&donnees_3d->alpha,&donnees_3d->beta,&donnees_3d->gama,
                        &donnees_3d->sc3d,&donnees_3d->z0, &donnees_3d->ox,
                        &donnees_3d->oy,&donnees_3d->oz,&donnees_3d->dx3d,
                        &donnees_3d->dy3d,&donnees_3d->dz3d);


	donnees_3d->alpha = donnees_3d->alpha - 10;

        SET_VAR_3D(donnees_3d->vd_id,&donnees_3d->alpha,&donnees_3d->beta,
                        &donnees_3d->gama,
                        &donnees_3d->sc3d,&donnees_3d->z0, &donnees_3d->ox,
                        &donnees_3d->oy,&donnees_3d->oz,&donnees_3d->dx3d,
                        &donnees_3d->dy3d,&donnees_3d->dz3d);


}
/*-------------------------------------------------------------
**    alpha_m1CB 
*/
void alpha_m1CB(w, client_data, call_data)
Widget          w;              /*  widget id           */
XtPointer       client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{


        GET_VAR_3D(&donnees_3d->alpha,&donnees_3d->beta,&donnees_3d->gama,
                        &donnees_3d->sc3d,&donnees_3d->z0, &donnees_3d->ox,
                        &donnees_3d->oy,&donnees_3d->oz,&donnees_3d->dx3d,
                        &donnees_3d->dy3d,&donnees_3d->dz3d);


	donnees_3d->alpha = donnees_3d->alpha - 1;

        SET_VAR_3D(donnees_3d->vd_id,&donnees_3d->alpha,&donnees_3d->beta,
                        &donnees_3d->gama,
                        &donnees_3d->sc3d,&donnees_3d->z0, &donnees_3d->ox,
                        &donnees_3d->oy,&donnees_3d->oz,&donnees_3d->dx3d,
                        &donnees_3d->dy3d,&donnees_3d->dz3d);


}

/*-------------------------------------------------------------
**    beta_m1CB
*/
void beta_m1CB(w, client_data, call_data)
Widget          w;              /*  widget id           */
XtPointer       client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{

        GET_VAR_3D(&donnees_3d->alpha,&donnees_3d->beta,&donnees_3d->gama,
                        &donnees_3d->sc3d,&donnees_3d->z0, &donnees_3d->ox,
                        &donnees_3d->oy,&donnees_3d->oz,&donnees_3d->dx3d,
                        &donnees_3d->dy3d,&donnees_3d->dz3d);



	donnees_3d->beta = donnees_3d->beta -1;

        SET_VAR_3D(donnees_3d->vd_id,&donnees_3d->alpha,&donnees_3d->beta,
                        &donnees_3d->gama,
                        &donnees_3d->sc3d,&donnees_3d->z0, &donnees_3d->ox,
                        &donnees_3d->oy,&donnees_3d->oz,&donnees_3d->dx3d,
                        &donnees_3d->dy3d,&donnees_3d->dz3d);


}

/*-------------------------------------------------------------
**    beta_m10CB
*/
void beta_m10CB(w, client_data, call_data)
Widget          w;              /*  widget id           */
XtPointer       client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{

        GET_VAR_3D(&donnees_3d->alpha,&donnees_3d->beta,&donnees_3d->gama,
                        &donnees_3d->sc3d,&donnees_3d->z0, &donnees_3d->ox,
                        &donnees_3d->oy,&donnees_3d->oz,&donnees_3d->dx3d,
                        &donnees_3d->dy3d,&donnees_3d->dz3d);



	donnees_3d->beta = donnees_3d->beta -10;

        SET_VAR_3D(donnees_3d->vd_id,&donnees_3d->alpha,&donnees_3d->beta,
                        &donnees_3d->gama,
                        &donnees_3d->sc3d,&donnees_3d->z0, &donnees_3d->ox,
                        &donnees_3d->oy,&donnees_3d->oz,&donnees_3d->dx3d,
                        &donnees_3d->dy3d,&donnees_3d->dz3d);


}
/*-------------------------------------------------------------
**    beta_p1CB
*/
void beta_p1CB(w, client_data, call_data)
Widget          w;              /*  widget id           */
XtPointer       client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{

        GET_VAR_3D(&donnees_3d->alpha,&donnees_3d->beta,&donnees_3d->gama,
                        &donnees_3d->sc3d,&donnees_3d->z0, &donnees_3d->ox,
                        &donnees_3d->oy,&donnees_3d->oz,&donnees_3d->dx3d,
                        &donnees_3d->dy3d,&donnees_3d->dz3d);


	donnees_3d->beta = donnees_3d->beta +1;

        SET_VAR_3D(donnees_3d->vd_id,&donnees_3d->alpha,&donnees_3d->beta,
                        &donnees_3d->gama,
                        &donnees_3d->sc3d,&donnees_3d->z0, &donnees_3d->ox,
                        &donnees_3d->oy,&donnees_3d->oz,&donnees_3d->dx3d,
                        &donnees_3d->dy3d,&donnees_3d->dz3d);


}
/*-------------------------------------------------------------
**    beta_p10CB
*/
void beta_p10CB(w, client_data, call_data)
Widget          w;              /*  widget id           */
XtPointer       client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{


        GET_VAR_3D(&donnees_3d->alpha,&donnees_3d->beta,&donnees_3d->gama,
                        &donnees_3d->sc3d,&donnees_3d->z0, &donnees_3d->ox,
                        &donnees_3d->oy,&donnees_3d->oz,&donnees_3d->dx3d,
                        &donnees_3d->dy3d,&donnees_3d->dz3d);


	donnees_3d->beta = donnees_3d->beta +10;

        SET_VAR_3D(donnees_3d->vd_id,&donnees_3d->alpha,&donnees_3d->beta,
                        &donnees_3d->gama,
                        &donnees_3d->sc3d,&donnees_3d->z0, &donnees_3d->ox,
                        &donnees_3d->oy,&donnees_3d->oz,&donnees_3d->dx3d,
                        &donnees_3d->dy3d,&donnees_3d->dz3d);


}

/*-------------------------------------------------------------
**    gama_p10CB
*/
void gama_p10CB(w, client_data, call_data)
Widget          w;              /*  widget id           */
XtPointer       client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{


        GET_VAR_3D(&donnees_3d->alpha,&donnees_3d->beta,&donnees_3d->gama,
                        &donnees_3d->sc3d,&donnees_3d->z0, &donnees_3d->ox,
                        &donnees_3d->oy,&donnees_3d->oz,&donnees_3d->dx3d,
                        &donnees_3d->dy3d,&donnees_3d->dz3d);


	donnees_3d->gama = donnees_3d->gama + 10;

        SET_VAR_3D(donnees_3d->vd_id,&donnees_3d->alpha,&donnees_3d->beta,
                        &donnees_3d->gama,
                        &donnees_3d->sc3d,&donnees_3d->z0, &donnees_3d->ox,
                        &donnees_3d->oy,&donnees_3d->oz,&donnees_3d->dx3d,
                        &donnees_3d->dy3d,&donnees_3d->dz3d);


}

/*-------------------------------------------------------------
**    gama_p1CB
*/
void gama_p1CB(w, client_data, call_data)
Widget          w;              /*  widget id           */
XtPointer       client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{


        GET_VAR_3D(&donnees_3d->alpha,&donnees_3d->beta,&donnees_3d->gama,
                        &donnees_3d->sc3d,&donnees_3d->z0, &donnees_3d->ox,
                        &donnees_3d->oy,&donnees_3d->oz,&donnees_3d->dx3d,
                        &donnees_3d->dy3d,&donnees_3d->dz3d);


	donnees_3d->gama = donnees_3d->gama + 1;

        SET_VAR_3D(donnees_3d->vd_id,&donnees_3d->alpha,&donnees_3d->beta,
                        &donnees_3d->gama,
                        &donnees_3d->sc3d,&donnees_3d->z0, &donnees_3d->ox,
                        &donnees_3d->oy,&donnees_3d->oz,&donnees_3d->dx3d,
                        &donnees_3d->dy3d,&donnees_3d->dz3d);


}
/*-------------------------------------------------------------
**    gama_m10CB
*/
void gama_m10CB(w, client_data, call_data)
Widget          w;              /*  widget id           */
XtPointer       client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{


        GET_VAR_3D(&donnees_3d->alpha,&donnees_3d->beta,&donnees_3d->gama,
                        &donnees_3d->sc3d,&donnees_3d->z0, &donnees_3d->ox,
                        &donnees_3d->oy,&donnees_3d->oz,&donnees_3d->dx3d,
                        &donnees_3d->dy3d,&donnees_3d->dz3d);


	donnees_3d->gama = donnees_3d->gama - 10;

        SET_VAR_3D(donnees_3d->vd_id,&donnees_3d->alpha,&donnees_3d->beta,
                        &donnees_3d->gama,
                        &donnees_3d->sc3d,&donnees_3d->z0, &donnees_3d->ox,
                        &donnees_3d->oy,&donnees_3d->oz,&donnees_3d->dx3d,
                        &donnees_3d->dy3d,&donnees_3d->dz3d);


}
/*-------------------------------------------------------------
**    gama_m1CB
*/
void gama_m1CB(w, client_data, call_data)
Widget          w;              /*  widget id           */
XtPointer       client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{

        GET_VAR_3D(&donnees_3d->alpha,&donnees_3d->beta,&donnees_3d->gama,
                        &donnees_3d->sc3d,&donnees_3d->z0, &donnees_3d->ox,
                        &donnees_3d->oy,&donnees_3d->oz,&donnees_3d->dx3d,
                        &donnees_3d->dy3d,&donnees_3d->dz3d);


	donnees_3d->gama = donnees_3d->gama - 1;

	SET_VAR_3D(donnees_3d->vd_id,&donnees_3d->alpha,&donnees_3d->beta,
			&donnees_3d->gama,
                        &donnees_3d->sc3d,&donnees_3d->z0, &donnees_3d->ox,
                        &donnees_3d->oy,&donnees_3d->oz,&donnees_3d->dx3d,
                        &donnees_3d->dy3d,&donnees_3d->dz3d);



}

/*-------------------------------------------------------------
**      close_3dbox appel a partir de check3d 0
*/
void CLOSE_3DBOX()
{

	if(d3box_set != 0){
        	XtDestroyWidget(d3box_set);
		del_shell_on_display(d3box_set);
        	d3box_set = 0;
	}
}


/*-------------------------------------------------------------
**      update_znotCB
*/
void update_znotCB (w, client_data, call_data)
Widget          w;              /*  widget id           */
XtPointer       client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{
    Arg args[1] ;
    XmScaleCallbackStruct * scb = (XmScaleCallbackStruct *) call_data ;

        donnees_3d->z0 =(float)scb->value;
        SET_VAR_3D(donnees_3d->vd_id,&donnees_3d->alpha,&donnees_3d->beta,
                        &donnees_3d->gama,
                        &donnees_3d->sc3d,&donnees_3d->z0, &donnees_3d->ox,
                        &donnees_3d->oy,&donnees_3d->oz,&donnees_3d->dx3d,
                        &donnees_3d->dy3d,&donnees_3d->dz3d);

}

/*-------------------------------------------------------------
**      d3_refreshCB
*/
void d3_refreshCB (w, client_data, call_data)
Widget          w;              /*  widget id           */
XtPointer       client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{

        pre_execute("ref3d");

}


