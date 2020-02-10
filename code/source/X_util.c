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
**	Include Files
*/

#ifndef HP
#  include <Xm/Xm.h>
#  include <stdio.h>
#  include <Xm/Separator.h>
#  include <X11/Xlib.h>
#  include <X11/Xutil.h>
#  include <Xm/BulletinB.h>
#  include <Xm/Label.h>
#  include <X11/cursorfont.h>
#  include <X11/Intrinsic.h>
#  include <X11/StringDefs.h>
#  include <Xm/DrawingA.h>
#  include <Xm/DialogS.h>
#  include <Xm/Form.h>
#  include <Xm/Frame.h>
#  include <Xm/Label.h>
#  include <Xm/TextF.h>
#  include <Xm/Protocols.h>
#else
# include <Xm/XmAll.h>
# include <X11/cursorfont.h>
#endif

#ifndef MACHTEN
#  include <malloc.h>
#else
#  include <sys/malloc.h>
#endif
#include "X_windef.h"

#if defined(SOLARIS) || defined(SPEC1)
#include <string.h>
#else
#include <strings.h>
#endif


/*-------------------------------------------------------------
**      Xt callbacks
*/

void update_progress();
void Real_close();
void stop_processCB();

/*-------------------------------------------------------------
*     Variables externes
*/

extern Widget window_mere;
extern XtAppContext app_context;
extern int IndexDrawPile;
int stop_in_progress;
int draw_size;

typedef struct {
	GC progress_gc;
	int real_size;
        Widget draw_scale;
}Draw_scaleStruct;

Draw_scaleStruct *DrawPile[10];

/*-------------------------------------------------------------
**      forwarded functions
*/

void x_initprogress();
void x_progress();

/*-------------------------------------------------------------
**      Create_Scale progress
*/
void x_initprogress(size)
int *size;
{
        Widget          box,pbut;            /*  Form                */
        Widget          dial_shell,row_column;
        Widget          frame0, frame1, frame2, frame3; /*  Frames      */
        Arg             al[10];         /*  arg list            */
        int    		ac;             /*  arg count           */
        Atom            wm_delete_window;
        Widget          label_string;
	Display 	*display;
	unsigned int    width_screen,height_screen;
	int 		position_y,position_x,screen,bool,nb,i;
	XGCValues valeurs;
	Draw_scaleStruct *DSstruct;
	XEvent evt;
	int ok;


	stop_in_progress = 0;

                        /* test du display */


        /*      Create DialogShell.
        */

	ac = 0;
        XtSetArg(al[ac], XmNallowShellResize, True); ac++ ;
        dial_shell=XmCreateDialogShell(window_mere,"In Progress ...",al,ac);


        /* Detournement de la commande CLOSE generale vers Close de la Widget */

/*
        wm_delete_window = XmInternAtom(XtDisplay(dial_shell),
                                "WM_DELETE_WINDOW", FALSE);
        XmRemoveWMProtocols(dial_shell,&wm_delete_window, 1);
        XmAddWMProtocolCallback(dial_shell, wm_delete_window,stop_processCB,NULL);
*/
				


                         

	/* recuperation taille de l'ecran pour place des Widgets */

	DSstruct = (Draw_scaleStruct *)XtMalloc(sizeof(Draw_scaleStruct));
	DrawPile[IndexDrawPile] = DSstruct;
	IndexDrawPile++;
        display = XtDisplay(window_mere);
        screen = DefaultScreen(display);
        width_screen = DisplayWidth(display,screen);
        position_x = ((width_screen/2) - 100 + (IndexDrawPile * 50 ));
	height_screen = DisplayHeight(display,screen);
	position_y = ((height_screen/2) - 100 + (IndexDrawPile * 50 ));
	DSstruct->real_size = *size;
	draw_size = 300;


       /*      Create outer Form box.
        */
        ac = 0;
	XtSetArg (al[ac], XmNdialogStyle,XmDIALOG_FULL_APPLICATION_MODAL);ac++;
	XtSetArg (al[ac], XmNnoResize, True);ac++;
	XtSetArg (al[ac], XmNdefaultPosition, False);ac++;
	XtSetArg (al[ac], XmNx ,position_x);ac++; 
        XtSetArg (al[ac], XmNy ,position_y);ac++; 
	XtSetArg (al[ac], XmNdialogTitle,NULL);ac++;
	box = XmCreateForm (dial_shell, "Message", al, ac);
        


	/* creation fenetre de remplissage */
        /* Creation du DrawingArea */



        ac = 0;
        XtSetArg (al[ac], XmNleftAttachment, XmATTACH_FORM);  ac++;
        XtSetArg (al[ac], XmNrightAttachment, XmATTACH_FORM);  ac++;
	XtSetArg (al[ac], XmNtopAttachment, XmATTACH_FORM);  ac++;
/* a enlever si bouton stop*/
XtSetArg (al[ac], XmNbottomAttachment, XmATTACH_FORM);  ac++;

        XtSetArg (al[ac], XmNtopOffset, 10);  ac++;
        XtSetArg (al[ac], XmNrightOffset, 10);  ac++;
        XtSetArg (al[ac], XmNleftOffset, 10);  ac++;
	XtSetArg (al[ac], XmNbottomOffset, 10);  ac++;
        frame2 = XmCreateFrame (box, "frame2", al, ac);
        XtManageChild (frame2);

        ac = 0;
        XtSetArg(al[ac], XtNheight,10); ac++ ;
        XtSetArg(al[ac], XtNwidth,draw_size); ac++ ;
        DSstruct->draw_scale = XtCreateManagedWidget("copy", xmDrawingAreaWidgetClass,
                                        frame2,al, ac);

/*
	BUTTON STOP 
		vider la pile d'evenement ne marche pas
		la solution:
		fork d'une autre applie et communication par atomes
		
*/
/*
        ac = 0;
        XtSetArg (al[ac], XmNleftAttachment, XmATTACH_FORM);  ac++;
        XtSetArg (al[ac], XmNrightAttachment, XmATTACH_FORM);  ac++;
        XtSetArg (al[ac], XmNtopAttachment, XmATTACH_WIDGET);  ac++;
	XtSetArg (al[ac], XmNtopWidget, frame2);  ac++;
        XtSetArg (al[ac], XmNbottomAttachment, XmATTACH_FORM);  ac++;
        XtSetArg (al[ac], XmNtopOffset, 10);  ac++;
        XtSetArg (al[ac], XmNrightOffset, 100);  ac++;
        XtSetArg (al[ac], XmNleftOffset, 100);  ac++;
        XtSetArg (al[ac], XmNbottomOffset, 10);  ac++;
	XtSetArg (al[ac], XmNshadowThickness,FALSE);  ac++;
        frame1 = XmCreateFrame (box, "frame1", al, ac);
        XtManageChild (frame1);


	ac = 0;
        pbut = XmCreatePushButton(frame1,"Stop Process",al, ac);
	XtManageChild (pbut);
	XtAddCallback (pbut, XmNactivateCallback, stop_processCB,
                     NULL);
*/



        /* Creation du Shell */
        XtManageChild(box);
        XtRealizeWidget(dial_shell);


                /* creer le GC */

        valeurs.background = 0;
        valeurs.foreground = 1;
        DSstruct->progress_gc = XCreateGC(XtDisplay(DSstruct->draw_scale),
                        XtWindow(DSstruct->draw_scale),
                        (GCForeground | GCBackground),&valeurs);

	/* gestion de la pile d'evenements pour le bouton stop*/
	
/*
        ok=0;
        while(ok == 0){
           if(XtPending()){
                XtNextEvent(&evt);
                XtDispatchEvent(&evt);
                printf("gestion evenements 2\n");
           }else{ok=1;}
        }
*/

}

/*-------------------------------------------------------------
*              x_progress: progression du Draw Scale
*			count : compteur jusqu'a real_size 
*/


void x_progress(count)
int *count;
{
	float step ;
	Widget ActualDraw;
	XEvent evt;
	int ok; 

	if (IndexDrawPile == 0) return; /* test de passage par initinprogress*/

        /* gestion de la pile d'evenements pour le bouton stop*/

/*
	ok=0;
	while(ok == 0){
           if(XtPending()){
                XtNextEvent(&evt);
                XtDispatchEvent(&evt);
                printf("gestion evenements 2\n");
           }else{ok=1;}
	}

*/

	ActualDraw = DrawPile[IndexDrawPile-1]->draw_scale;
	
	step = (float)(((float)*count/DrawPile[IndexDrawPile-1]->real_size)*draw_size);
	XFillRectangle(XtDisplay(ActualDraw),XtWindow(ActualDraw),
			DrawPile[IndexDrawPile-1]->progress_gc,0,0,(int)step,10);
	XFlush(XtDisplay(ActualDraw));
	if(step >= draw_size) {
		Real_close(XtParent(XtParent(XtParent(ActualDraw))));
	}
}


/*-------------------------------------------------------------
*                close des widgets en fin d'iterations
*/
void Real_close(w)
Widget          w;
{
        XtUnrealizeWidget(w);
        XtDestroyWidget(w);
	XFreeGC(XtDisplay(w),DrawPile[IndexDrawPile-1]->progress_gc);
	XtFree((char *)DrawPile[IndexDrawPile-1]);
	IndexDrawPile--;

}

/*-------------------------------------------------------------
**    stop_processCB 
*/
void stop_processCB(w, client_data, call_data)
Widget          w;              /*  widget id           */
XtPointer       client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{
printf("stop_processCB\n");
stop_in_progress = 1;
}

