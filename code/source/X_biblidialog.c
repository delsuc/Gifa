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
#  include <Xm/ArrowB.h>
#  include <Xm/Label.h>
#  include <Xm/PushB.h>
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
#ifndef NOREADLINE
#  include <readline/readline.h>
#endif


/*-------------------------------------------------------------
**      Xt callbacks
*/
void Close_CB();
void Action_OK();
void Action_CANCEL();


/*-------------------------------------------------------------
*     Variables externes
*/

extern int versionx;
extern Widget window_mere;
extern XtAppContext app_context;
extern XtInputId input_id;

int reponse;


/*-------------------------------------------------------------
**      forwarded functions
*/


/*
void CREAT_DIALOG_BOX();
*/
int Create_Message_Ok();
int Create_Message_Ok_Cancel();
int Create_Saisie();
void F_MSG_OK();
void F_SAISIE();


/*-------------------------------------------------------------
**      Create_Message_Ok
*/
int Create_Message_Ok (titre,message)
char *titre;
char *message;
{
	Widget		dial_shell;
        Widget          box;            /*  Form                */
        Widget          ok_button;
        Widget          frame0, frame1, frame2, frame3; /*  Frames      */
        Arg             al[10];         /*  arg list            */
        int    		ac;             /*  arg count           */
	int		passage;
        Atom            wm_delete_window;
        Widget          label_string;
	XEvent 		event;
	Display 	*display;
	unsigned int    width_screen,height_screen;
	int 		position_y,position_x,screen,bool;


			/* init des parametres */

	reponse = 0;


                        /* test du display */

         if (versionx == 0){
                 printf("Gifa cannot open the display on X server\n");
                return(1);
         }

        /*      Create DialogShell.
        */

	ac = 0;
        XtSetArg(al[ac], XmNallowShellResize, True); ac++ ;
        dial_shell=XmCreateDialogShell(window_mere,titre,al,ac);
                         

	/* recuperation taille de l'ecran pour place des Widgets */

        display = XtDisplay(window_mere);
        screen = DefaultScreen(display);
        width_screen = DisplayWidth(display,screen);
        position_x = ((width_screen/2) - 100);
	height_screen = DisplayHeight(display,screen);
	position_y = ((height_screen/2) - 100);


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
        



        /* Detournement de la commande CLOSE generale */

        wm_delete_window = XmInternAtom(XtDisplay(dial_shell),
                                "WM_DELETE_WINDOW",
                                FALSE);
        XmRemoveWMProtocols(dial_shell,&wm_delete_window, 1);
        XmAddWMProtocolCallback(dial_shell, wm_delete_window, Close_CB, NULL);




        /*      Create Label String
        */
        ac = 0;
        XtSetArg (al[ac], XmNmarginWidth, 25);ac++;
        XtSetArg (al[ac], XmNmarginHeight,10);ac++;
	XtSetArg (al[ac], XmNshadowThickness, 0);  ac++;
/*
bug lesstif
        XtSetArg (al[ac], XmNleftAttachment, XmATTACH_FORM);  ac++;
        XtSetArg (al[ac], XmNrightAttachment, XmATTACH_FORM);  ac++;
        XtSetArg (al[ac], XmNtopAttachment, XmATTACH_WIDGET);  ac++;
*/
        XtSetArg (al[ac], XmNrightOffset, 10);  ac++;
        XtSetArg (al[ac], XmNleftOffset, 10);  ac++;
        XtSetArg (al[ac], XmNtopOffset, 10);  ac++;
        frame0 = XmCreateFrame (box, "frame", al, ac);
        XtManageChild (frame0);

        label_string = XmCreateLabel(frame0, message, NULL, 0);
        XtManageChild (label_string);


        /*      Create buttons.
        */
        ac = 0;
        XtSetArg (al[ac], XmNtopAttachment, XmATTACH_WIDGET);  ac++;
        XtSetArg (al[ac], XmNtopWidget, frame0);  ac++;
        XtSetArg (al[ac], XmNtopOffset, 10);  ac++;
        XtSetArg (al[ac], XmNbottomOffset, 10);  ac++;
        XtSetArg (al[ac], XmNbottomAttachment, XmATTACH_FORM);  ac++;
        XtSetArg (al[ac], XmNx,10);ac++;
        ok_button =
                XmCreatePushButton (box, "ok", al, ac);
	XtAddCallback (ok_button, XmNactivateCallback, Action_OK, NULL);
        XtManageChild (ok_button);



	/* Creation du Shell */
	XtManageChild(box);
	XtRealizeWidget(dial_shell);
	


	/* gestion des evenements */

         for (;;)
        {
         XtAppNextEvent(app_context,&event);
         bool = XtDispatchEvent(&event);

            if (reponse != 0)
            {
                break;
            }
        }
        XtUnrealizeWidget(dial_shell);
        XtDestroyWidget(dial_shell);
	return(0);


}



/*-------------------------------------------------------------
**      Create_Message_Ok_Cancel
*/
int Create_Message_Ok_Cancel (titre,message)
char *titre;
char *message;
{
        Widget          dial_shell;
        Widget          box;            /*  Form                */
        Widget          ok_button;
	Widget		cancel_button;
        Widget          frame0, frame1, frame2, frame3; /*  Frames      */

        Arg             al[10];         /*  arg list            */
        int		ac;             /*  arg count           */
        Atom            wm_delete_window;
	Widget		label_string;
	int 		passage;
        XEvent          event;
        Display         *display;
        unsigned int    width_screen,height_screen;
        int             position_y,position_x,screen,bool;



			/* init parametres */

	reponse = 0;

                        /* test du display */

         if (versionx == 0){
                 printf("Gifa cannot open the display on X server\n");
                return(1);
         }

        /*      Create DialogShell.
        */

        ac = 0;
        XtSetArg(al[ac], XmNallowShellResize, True); ac++ ;
        dial_shell = XmCreateDialogShell (window_mere, titre,al, ac);

        /* recuperation taille de l'ecran pour place des Widgets */

        display = XtDisplay(window_mere);
        screen = DefaultScreen(display);
        width_screen = DisplayWidth(display,screen);
        position_x = ((width_screen/2) - 100);
        height_screen = DisplayHeight(display,screen);
        position_y = ((height_screen/2) - 100);


       /*      Create outer Form box.
        */
        ac = 0;
        XtSetArg (al[ac], XmNdialogStyle,XmDIALOG_FULL_APPLICATION_MODAL);ac++;
        XtSetArg (al[ac], XmNnoResize, True);ac++;
        XtSetArg (al[ac], XmNdefaultPosition, False);ac++;
        XtSetArg (al[ac], XmNx ,position_x);ac++;
        XtSetArg (al[ac], XmNy ,position_y);ac++;
        box = XmCreateForm (dial_shell, "Message", al, ac);


        /* Detournement de la commande CLOSE generale */

        wm_delete_window = XmInternAtom(XtDisplay(dial_shell),
                                "WM_DELETE_WINDOW",
                                FALSE);
        XmRemoveWMProtocols(dial_shell,&wm_delete_window, 1);
        XmAddWMProtocolCallback(dial_shell, wm_delete_window, Close_CB, NULL);


        /*      Create Label String
        */
        ac = 0;
        XtSetArg (al[ac], XmNmarginWidth, 25);ac++;
        XtSetArg (al[ac], XmNmarginHeight,10);ac++;
	XtSetArg (al[ac], XmNshadowThickness, 0);  ac++;
/*
bug lesstif

        XtSetArg (al[ac], XmNleftAttachment, XmATTACH_FORM);  ac++;
        XtSetArg (al[ac], XmNrightAttachment, XmATTACH_FORM);  ac++;
        XtSetArg (al[ac], XmNtopAttachment, XmATTACH_WIDGET);  ac++;
*/
        XtSetArg (al[ac], XmNrightOffset, 10);  ac++;
        XtSetArg (al[ac], XmNleftOffset, 10);  ac++;
        XtSetArg (al[ac], XmNtopOffset, 10);  ac++;
        frame0 = XmCreateFrame (box, "frame", al, ac);
        XtManageChild (frame0);

        label_string = XmCreateLabel(frame0, message, NULL, 0);
        XtManageChild (label_string);


        /*      Create  buttons.
        */

        ac = 0;
        XtSetArg (al[ac], XmNtopAttachment, XmATTACH_WIDGET);  ac++;
        XtSetArg (al[ac], XmNtopWidget, frame0);  ac++;
        XtSetArg (al[ac], XmNtopOffset, 10);  ac++;
        XtSetArg (al[ac], XmNbottomOffset, 10);  ac++;
        XtSetArg (al[ac], XmNbottomAttachment, XmATTACH_FORM);  ac++;
        XtSetArg (al[ac], XmNx,10);ac++;
        ok_button =
                XmCreatePushButton (box, "ok", al, ac);
	XtAddCallback (ok_button, XmNactivateCallback, Action_OK, NULL);
        XtManageChild (ok_button);



        ac = 0;
        XtSetArg (al[ac], XmNtopAttachment, XmATTACH_WIDGET);  ac++;
        XtSetArg (al[ac], XmNtopWidget, frame0);  ac++;
        XtSetArg (al[ac], XmNleftAttachment, XmATTACH_WIDGET);  ac++;
        XtSetArg (al[ac], XmNleftWidget, ok_button);  ac++;
        XtSetArg (al[ac], XmNtopOffset, 10);  ac++;
        XtSetArg (al[ac], XmNleftOffset, 10);  ac++;
        XtSetArg (al[ac], XmNbottomOffset, 10);  ac++;
        XtSetArg (al[ac], XmNbottomAttachment, XmATTACH_FORM);  ac++;
        cancel_button =
                XmCreatePushButton (box, "cancel", al, ac);
	XtAddCallback (cancel_button, XmNactivateCallback, Action_CANCEL, NULL);
        XtManageChild (cancel_button);



        /* Creation du Shell */
        XtManageChild (box);
        XtRealizeWidget(dial_shell);


        /* gestion des evenements */


 	 for (;;)
  	{
   	 XtAppNextEvent(app_context,&event);
    	 bool = XtDispatchEvent(&event);

 	    if (reponse != 0)
 	    {
 	       	break;
    	    }
  	}
        XtUnrealizeWidget(dial_shell);
        XtDestroyWidget(dial_shell);
        if(reponse == 1)return(0);
        if(reponse == 2)return(1);


}




/*-------------------------------------------------------------
**	Create_Saisie
*/
int  Create_Saisie (titre,message,string)
char *string;
char *titre;
char *message;
{
        Widget          dial_shell;
	Widget		box;		/*  Form		*/
	Widget 		cancel_button;
	Widget		ok_button;
	Widget		frame0, frame1, frame2, frame3;	/*  Frames	*/
	Widget		textfield;

	Arg		al[10];		/*  arg list		*/
	int     	ac;		/*  arg count		*/
        Atom            wm_delete_window;

	Widget label_string;
	int 		passage,index;
        XEvent          event;
        Display         *display;
        unsigned int    width_screen,height_screen;
        int             position_y,position_x,screen,bool;


			/* init des parametres */
	reponse = 0;
#ifndef toto
#ifndef NOREADLINE
/* it seems that it's better to remove Input before going into this strange loop */
	XtRemoveInput(input_id);
#endif
#endif

                        /* test du display */

         if (versionx == 0){
                 printf("Gifa cannot open the display on X server\n");
                return(1);
         }


        /*      Create DialogShell.
        */

        ac = 0;
        XtSetArg(al[ac], XmNallowShellResize, True); ac++ ;
        dial_shell = XmCreateDialogShell (window_mere, titre,al, ac);


        /* recuperation taille de l'ecran pour place des Widgets */

        display = XtDisplay(window_mere);
        screen = DefaultScreen(display);
        width_screen = DisplayWidth(display,screen);
        position_x = ((width_screen/2) - 100);
        height_screen = DisplayHeight(display,screen);
        position_y = ((height_screen/2) - 100);



	/*	Create outer Form box.
	*/
	ac = 0;
        XtSetArg (al[ac], XmNdialogStyle,XmDIALOG_FULL_APPLICATION_MODAL);ac++;
        XtSetArg (al[ac], XmNnoResize, True);ac++;
        XtSetArg (al[ac], XmNdefaultPosition, False);ac++;
        XtSetArg (al[ac], XmNx ,position_x);ac++;
        XtSetArg (al[ac], XmNy ,position_y);ac++;
	box = XmCreateForm (dial_shell, "Message", al, ac);


        /* Detournement de la commande CLOSE generale */

        wm_delete_window = XmInternAtom(XtDisplay(dial_shell),
                                "WM_DELETE_WINDOW",
                                FALSE);
        XmRemoveWMProtocols(dial_shell,&wm_delete_window, 1);
        XmAddWMProtocolCallback(dial_shell, wm_delete_window, Close_CB, NULL);



	/*	Create Label String 
	*/
	ac = 0;
	XtSetArg (al[ac], XmNmarginWidth, 25);ac++;
	XtSetArg (al[ac], XmNmarginHeight,10);ac++;
	XtSetArg (al[ac], XmNshadowThickness, 0);  ac++;
/*
bug lesstif
*/
#ifndef toto
    XtSetArg (al[ac], XmNleftAttachment, XmATTACH_FORM);  ac++;
	XtSetArg (al[ac], XmNrightAttachment, XmATTACH_FORM);  ac++;
	XtSetArg (al[ac], XmNtopAttachment, XmATTACH_WIDGET);  ac++;
#endif

	XtSetArg (al[ac], XmNrightOffset, 10);  ac++;
        XtSetArg (al[ac], XmNleftOffset, 10);  ac++;
	XtSetArg (al[ac], XmNtopOffset, 10);  ac++;
	frame0 = XmCreateFrame (box, "frame", al, ac);
	XtManageChild (frame0);

	label_string = XmCreateLabel(frame0, message, NULL, 0);
	XtManageChild (label_string);


	/* Create Text Field */


        ac = 0;
        XtSetArg (al[ac], XmNshadowType, XmSHADOW_ETCHED_IN );  ac++;
#ifndef toto
        XtSetArg (al[ac], XmNleftAttachment, XmATTACH_FORM);  ac++;
        XtSetArg (al[ac], XmNrightAttachment, XmATTACH_FORM);  ac++;
        XtSetArg (al[ac], XmNtopAttachment, XmATTACH_WIDGET);  ac++;
#endif

        XtSetArg (al[ac], XmNtopWidget, frame0);  ac++;
        XtSetArg (al[ac], XmNtopOffset, 10);  ac++;
	XtSetArg (al[ac], XmNrightOffset, 10);  ac++;
	XtSetArg (al[ac], XmNleftOffset, 10);  ac++;
        frame2 = XmCreateFrame (box, "frame2", al, ac);
        XtManageChild (frame2);

	ac = 0;
	XtSetArg (al[ac], XmNvalue, string); ac++;
	textfield = XmCreateTextField(frame2, "text", al, ac);
	XtAddCallback (textfield, XmNactivateCallback, Action_OK,NULL);
        XtManageChild (textfield);



	/*	Create  buttons.
	*/
        ac = 0;
        XtSetArg (al[ac], XmNtopAttachment, XmATTACH_WIDGET);  ac++;
        XtSetArg (al[ac], XmNtopWidget, frame2);  ac++;
        XtSetArg (al[ac], XmNtopOffset, 10);  ac++;
        XtSetArg (al[ac], XmNbottomOffset, 10);  ac++;
        XtSetArg (al[ac], XmNbottomAttachment, XmATTACH_FORM);  ac++;
	XtSetArg (al[ac], XmNx,10);ac++;
	ok_button =
                XmCreatePushButton (box, "ok", al, ac);
	XtAddCallback (ok_button, XmNactivateCallback, Action_OK, NULL);
        XtManageChild (ok_button);



        ac = 0;
	XtSetArg (al[ac], XmNtopAttachment, XmATTACH_WIDGET);  ac++;
        XtSetArg (al[ac], XmNtopWidget, frame2);  ac++;
	XtSetArg (al[ac], XmNleftAttachment, XmATTACH_WIDGET);  ac++;
        XtSetArg (al[ac], XmNleftWidget, ok_button);  ac++;
        XtSetArg (al[ac], XmNtopOffset, 10);  ac++;
        XtSetArg (al[ac], XmNleftOffset, 10);  ac++;
        XtSetArg (al[ac], XmNbottomOffset, 10);  ac++;
        XtSetArg (al[ac], XmNbottomAttachment, XmATTACH_FORM);  ac++;
        cancel_button =
                XmCreatePushButton (box, "cancel", al, ac);
	XtAddCallback (cancel_button, XmNactivateCallback, Action_CANCEL, NULL);
        XtManageChild (cancel_button);



        /* Creation du Shell */
	XtManageChild (box);
        XtRealizeWidget(dial_shell);



       /* gestion des evenements */


         for (;;)
        {
         XtAppNextEvent(app_context,&event);
         bool = XtDispatchEvent(&event);

            if (reponse != 0)
            {
                break;
            }
        }
        XtUnrealizeWidget(dial_shell);
        XtDestroyWidget(dial_shell);
#ifndef toto
#ifndef NOREADLINE
    input_id = XtAppAddInput(app_context,fileno(stdin),(XtPointer)XtInputReadMask,
			rl_callback_read_char,NULL);
#endif
#endif

        if(reponse == 1){
		strcpy(string,XmTextFieldGetString(textfield)) ;
		return(0);
	}
        else if(reponse == 2){
		return(1);
	}

}




/*-------------------------------------------------------------
*		Permet d'eviter le close des widgets
*/
void Close_CB(w, client_data, call_data)
Widget          w;              /*  widget id           */
XtPointer       client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{}

/*-------------------------------------------------------------
*		To call boxes from fortran..
*/

void F_MSG_OK(label,llabel,text,ltext,err)
char *label,*text;
int  *llabel,*ltext,*err;
{
char ll[256],lt[256];

  convert_string(ll,label,*llabel);
  convert_string(lt,text,*ltext);
  *err = Create_Message_Ok(ll,lt);
}

void F_SAISIE(label,llabel,text,ltext,st,lst,err)
char *label,*text,*st;
int  *llabel,*ltext,*lst,*err;
{
char ll[256],lt[256],local[256];

  convert_string(ll,label,*llabel);
  convert_string(lt,text,*ltext);
  convert_string(local,st,*lst);
  *err = Create_Saisie(ll,lt,local);
  if (*err == 0)
    {
      cconvert_string(st,local,lst);
	}
}
  

/********************* CALL BACK ****************************/


void Action_OK (w, client_data, call_data)
  Widget   w;
XtPointer       client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{ 
  reponse = 1;
}

void Action_CANCEL (w, client_data, call_data)
  Widget   w;
XtPointer       client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{
  reponse = 2;
}


