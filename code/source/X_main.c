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
#  include <stdio.h>
#  include <Xm/Xm.h>
#  include <Xm/Separator.h>
#  include <Xm/PushB.h>
#  include <Xm/RowColumn.h>
#  include <X11/Intrinsic.h>
#  include <X11/Xlib.h>
#  include <X11/Xutil.h>
#  include <X11/Xos.h>
#  include <Xm/Protocols.h>
#  include <Xm/CascadeB.h>
#  include <Xm/MwmUtil.h>
#else
#  include <Xm/XmAll.h>
#endif


#include "X_windef.h"
#include "util.h"
#include "xmgifa.xbm"

#if defined DARWIN
#    include <stdlib.h>
#elif defined MACHTEN
#  include <sys/malloc.h>
#else
#    include <malloc.h>
#endif

#ifndef NOREADLINE
#  include <readline/readline.h>
#endif

#include "sizebasec.h"


/*-------------------------------------------------------------
**	forwarded functions
*/
void AFF_ALL_TITRE ();	
void SET_PDMENU();
Widget Butt_titre();
void BUTT_COMM();
Widget Creat_shell_comm();
void CREAT_TITRE();
void MOTIF_LOOP();
void MAKE_SEPARATEUR();
void QUIT_ALL();
void WAIT_CURSOR_IN_ALL_SHELL();
void ARROW_CURSOR_IN_ALL_SHELL();
void pre_execute();
void SetAppIcon();
void add_shell_on_display();
void del_shell_on_display();
void rlg_execute();

/*-------------------------------------------------------------
**      Xt callbacks 
*/
void QuitCB_titre ();			
void QuitCB_comm ();
void ButtCB_comm ();
void Aff_shell_comm();
void Read_comm();

/*-------------------------------------------------------------
*     Variables externes
*/

int menu_deroulant;
int nb_rows_title;
int versionx;
int flag_bloquant;
Widget window_mere;
XtAppContext app_context;
XtInputId input_id;

extern  char *pldat;
extern short *pldat16;
extern char *pldat24;
extern int *pldat32;
/* global PROMPT variable*/
extern char cprompt[MAX_CHAR];

typedef struct {
    Widget root_w,root_comm,mere_titre,mere_comm;
    XtAppContext ap_cont;
} RC_Data ;

static RC_Data rc_data ;

/* this one used by Fortran parser to tell if user interface has to be in text
   mode or in graphic mode */
extern struct {
  int on_graph;
  int on_x_win;
  } WIN_COMMON;

extern Cursor cursor1,cursor2,cursor_wait;
extern Widget zoom_set;
extern int point_flag;
int IndexDrawPile;

/*-------------------------------------------------------------
*     Variables glogales
*/


/*
unsigned int screen_width,screen_height;
*/
int first_pass,incre_pos;
int nb_shell,nb_form,nb_shell_aff;
Widget id_shell[100],id_shell_aff[100];


	/* ------------- FONCTIONS GENERALES ---------------- */

/*-------------------------------------------------------------
**	    Big Loop body
*/
int MOTIF_INIT() 
/*
	Initialisation de la partie Motif
	test de display
	callback sur stdin
	Toplevel,dimension ecran........
*/
	
{
/*
    XtAppContext app_context;
*/
    Widget      toplevel_titre;
    int screen;
    Display *dpy;
    Arg args[10];
    int depth, n;

    char st[80];
    int argc;
    char *argv[5];
    Atom  wm_delete_window;

			/* creation Toplevel resizable */

    argc=1;
    strcpy(st,"gifa\0");
    argv[0] = st;
    argv[1] = NULL;




			/* test de display  et mise de versionx = 1 si correct*/

    dpy = XOpenDisplay(NULL);
    if(dpy == NULL){
         printf("Gifa cannot open the display on X server %s\n",
         XDisplayName(NULL));
	 WIN_COMMON.on_graph = 0;
	 versionx = 0;
	 return(0);    /* means no X connection */
    } else  {
        depth = DefaultDepth(dpy,DefaultScreen(dpy));

#ifdef DEBUG
	printf ("Screen depth is : %d\n", depth);
#endif
	switch (depth) {
	case (4) :
          printf("%s\n","WARNING - Colors will probably be wrong in bitmap mode, because of screen depth mode (4bits)");
	case (8) :
	  pldat = malloc(simax*simax);
	  break;
	case (15) :
          printf("%s\n","WARNING - Colors will probably be wrong in bitmap mode, because of screen depth mode (15bits)");
	case (16) :
	  pldat16 = malloc(2*simax*simax);
	  break;
	case (24) :
          printf("%s\n","WARNING - Colors will probably be wrong in bitmap mode, because of screen depth mode (24bits)");
	case (32) :
	  pldat32 = malloc(4*simax*simax);
	  break;
	default :
	  printf("Gifa cannot handle screen depth of %d\n",depth);
	  versionx = 0;
	  return(0);
	}
	versionx = 1;
    }
    WIN_COMMON.on_graph = 1;
    XCloseDisplay(dpy);


			/* creation shell */

    n = 0;
    XtSetArg(args[n], XmNallowShellResize, True); n++ ;
    XtSetArg(args[n], XtNgeometry, "+1+1"); n++ ;
    toplevel_titre = XtAppInitialize(&app_context, "XMgifa", NULL, 0,
			       &argc, argv, NULL,args, n);


		        /* Detournement de la commande CLOSE generale */
			/* vers Close de la Widget		      */


    wm_delete_window = XmInternAtom(XtDisplay(toplevel_titre), "WM_DELETE_WINDOW", FALSE);
    XmRemoveWMProtocols(toplevel_titre,&wm_delete_window, 1);
    XmAddWMProtocolCallback(toplevel_titre, wm_delete_window, QUIT_ALL, NULL);


			/* Icone de presentation */

    SetAppIcon(toplevel_titre,1);

			/* Stockage des Widgets en variable globales */


    rc_data.root_w = toplevel_titre;
    rc_data.ap_cont = app_context;
    window_mere = toplevel_titre;


			/* flag du premier passage */

    first_pass = 0;
    nb_shell = 0;
    nb_shell_aff = 0;
    nb_form = 0;
    incre_pos = 0;
    flag_bloquant = 0;
    zoom_set = 0;
    point_flag = 0;
    IndexDrawPile = 0;

    /* set-up kbd input callback,  default is to use readline */
    /* change have to be made in execute.for if you are not using readline*/
#ifdef NOREADLINE
			/* callback sur stdin */

    XtAppAddInput(app_context,fileno(stdin),(XtPointer)XtInputReadMask,
			Read_comm,NULL);
#else
    /* using readline library */
    rl_bind_key('\t',rl_insert);   /* inhibit completion */
    rl_callback_handler_install(cprompt, rlg_execute);
    input_id = XtAppAddInput(app_context,fileno(stdin),(XtPointer)XtInputReadMask,
			rl_callback_read_char,NULL);
#endif
			/* initialisation des windows 1d et 2d */

    win_init();
    return(1);

}


void MOTIF_LOOP()
/*
	lance la XtAppMainLoop
*/
{
   XtAppMainLoop(rc_data.ap_cont);
}

/*-------------------------------------------------------------
*/
void AFF_ALL_TITRE (error) 
int *error;
/*-------------------------------------------------------------
**      Affiche la fenetre de menu titre
*/
{


    if(first_pass == 0){

           XtRealizeWidget(rc_data.root_w);
	   add_shell_on_display(rc_data.root_w);		
           first_pass = 1;

   }

}

		/*   ----------  MENU TITRE ----------------   */ 


/*-------------------------------------------------------------
*/

Widget Butt_titre(label_titre,context)
XContext context;
String label_titre;
/*
**      Butt_titre     :ajoute un bouton dans le titre
*/
{
    	Widget butt_tit,cascade;
	int n;
	Arg args[10];

		/* creation des buttons titre avec leur label (label_titre)
			les callbacks seront mis em place durant la creation
			des shells asscies. */

	if(menu_deroulant == 1){
		butt_tit = XmCreatePulldownMenu (rc_data.mere_titre,label_titre,NULL,0);
		n = 0;
    		XtSetArg (args[n], XmNsubMenuId, butt_tit);  n++;
    		cascade = XmCreateCascadeButton(rc_data.mere_titre, label_titre, args, n);
    		XtManageChild (cascade);

	}
	else{
    		butt_tit = XmCreatePushButton(rc_data.mere_titre,label_titre,NULL,0);
    		XtManageChild (butt_tit);
	}
    	return (butt_tit);
}

	/******************CALLBACK*******************/


/*-------------------------------------------------------------
**	QuitCB_titre			- callback for quit button titre
*/
void QuitCB_titre (w, client_data, call_data) 
Widget		w;		/*  widget id		*/
XtPointer	client_data;	/*  data from application   */
XtPointer	call_data;	/*  data from widget class  */
/*
- callback for quit button titre
*/
{


	XtDestroyWidget(rc_data.mere_titre);
        XtUnrealizeWidget(rc_data.root_w);
	first_pass = 0;
	del_shell_on_display(rc_data.root_w);
	
}

/*-------------------------------------------------------------
**      QUIT_ALL                    - callback for quit button titre
*/
void QUIT_ALL (error)
int *error;
/*
	appel a XtDestroyWidget(mere)
*/
{

	int i;

if(first_pass != 0){

	for(i=0;i<nb_shell;i++){
		XtDestroyWidget(id_shell[i]);
		del_shell_on_display(id_shell[i]);
	}

	nb_shell = 0;
	del_shell_on_display(rc_data.root_w);
        XtDestroyWidget(rc_data.mere_titre);
        XtUnrealizeWidget(rc_data.root_w);
        first_pass = 0;
	incre_pos = 0;
}

}



	/*    -------------  MENU COMMANDES  ------------------   */


/*-------------------------------------------------------------
**      BUTT_COMM     :ajoute un bouton dans les commandes
*/

void BUTT_COMM(w_parent,name_f,lg_name,command_f,lg_comm,error)
Widget *w_parent;
int *lg_name,*lg_comm,*error;
char name_f[];
char command_f[];
/*
	ajoute un bouton dans les commandes
*/
{
    XContext cont_comm;
    Widget Wbutt_comm;
    char name[MAX_CHAR];	 
    char *command;
    Arg args[5];


	/* Creation des boutons commandes avec contexte associe contenant 
		   la commande (MAX_CHAR char) a lancer */

    convert_string(name,name_f,*lg_name);

    if (strcmp ("", name) != 0) {
	command = (char *)malloc(*lg_comm + 1);  
	convert_string(command,command_f,*lg_comm);
    	cont_comm = XUniqueContext();
    	Wbutt_comm = XmCreatePushButton (*w_parent, name, NULL, 0);
    	XSaveContext(XtDisplay(Wbutt_comm),(XID)Wbutt_comm,cont_comm,command);
    	XtManageChild (Wbutt_comm);
    	XtAddCallback (Wbutt_comm, XmNactivateCallback, ButtCB_comm,
				(XtPointer)cont_comm);

    } else *error = 1; 


}


        /******************CALLBACK*******************/


/*-------------------------------------------------------------
**      QuitCB_comm                    - callback for button commandes
*/
void QuitCB_comm (w, client_data, call_data)
Widget          w;              /*  widget id           */
XtPointer       client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{

	int i;

#ifdef SGI

       XtUnrealizeWidget(XtParent(w));

		/* destockage des shells affiches */
       del_shell_on_display(XtParent(w));

#else
#ifdef AIX

       XtUnrealizeWidget(XtParent(w));

       		 /* destockage des shells affiches */
      del_shell_on_display(XtParent(w)); 

#else

       XtUnrealizeWidget(w);
	
	        /* destockage des shells affiches */
       del_shell_on_display(w);

#endif
#endif

        /* position du widget */
        incre_pos--;

}
/*-------------------------------------------------------------
**      ButtCB_comm                    - callback for button commandes 
*/
void ButtCB_comm (w, client_data, call_data)
Widget          w;              /*  widget id           */
XContext       client_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{

	String command;
	
	XFindContext(XtDisplay(w),(XID)w,client_data,&command);
	pre_execute(command);

}

/*-------------------------------------------------------------
**      Creat_shell_comm                  - Creation du Row commandes
*/
Widget Creat_shell_comm(label_titre,b_titre)
String label_titre;
Widget b_titre;
/*
	 Creation du Row commandes shell
*/
{

	char	*position;
	char	*posi_loc;
	int	n,position_x,position_y,i;
	Atom    wm_delete_window;
	Widget  toplevel_comm,menu_bar_comm;
	Arg 	args[5];



		/* position et creation de la widget */

    position = (char *)malloc(10);
    position_x = (1+(incre_pos*50));
    position_y = 81;
    sprintf(position,"+%d+%d",position_x,position_y);
    incre_pos++;


                /* creation du shell de commande et de la Row Column
                        contenant les boutons commandes */
    
    n = 0;
    XtSetArg(args[n], XtNgeometry,position); n++;
    toplevel_comm = XtAppCreateShell(label_titre,"XMgifa",
        applicationShellWidgetClass,XtDisplay(rc_data.root_w),args,n);
    rc_data.mere_comm = toplevel_comm;

    n = 0;
    menu_bar_comm = XmCreateRowColumn(toplevel_comm, "menu_bar_comm", args, n);
    XtManageChild(menu_bar_comm);

    		/* stockage des shells crees pour le close all */

    id_shell[nb_shell] = toplevel_comm;
    nb_shell++; 

		/* Detournement de la commande CLOSE generale vers Close de la Widget */

    wm_delete_window = XmInternAtom(XtDisplay(toplevel_comm),
                                "WM_DELETE_WINDOW",
                                FALSE);
    XmRemoveWMProtocols(toplevel_comm,&wm_delete_window, 1);
    XmAddWMProtocolCallback(toplevel_comm, wm_delete_window, QuitCB_comm, NULL);


		/*mise en place du callback associe au boutton titre
		  cree dans la fonction precedente et permettant 
		  l'affichage du shell de commande */

    XtAddCallback (b_titre,XmNactivateCallback,Aff_shell_comm,
                                toplevel_comm);

    return(menu_bar_comm);
}


/*-------------------------------------------------------------
**      Aff_shell_comm                    - callback for button commandes
*/
void Aff_shell_comm (w,shell_fils_comm,call_data)
Widget          w;                   /*  widget id           */
Widget          shell_fils_comm;         /*  data from application   */
XtPointer       call_data;           /*  data from widget class  */
/*
	affiche le Row comm shell
*/
{
	int i, ok;
	caddr_t *data_return;


        XtRealizeWidget(shell_fils_comm);

                /* pop the windows lors d'un second click */

	XRaiseWindow(XtDisplay(shell_fils_comm), XtWindow(shell_fils_comm));

       		 /* stockage des id des shells crees (cursor_watch)*/

	ok = 0;
		/* recherche si deja affiche */
	for(i=0;i<nb_shell_aff;i++){
		if(id_shell_aff[i] == shell_fils_comm) ok = 1;
	}
 
		/* si non affiche */
	if(ok == 0){
		add_shell_on_display(shell_fils_comm);
	}

}



        /* ------------- INTERFACE E/S ----------------- */

/*-------------------------------------------------------------
*/
/*      recuperation commandes STDIN et envoi a gifa   */
extern struct {
                int journal;
                int haswritten;
                } JOURCOMMON;

void Read_comm (client_data,source,id)
caddr_t       client_data;
int          *source;
XtInputId      *id ;
/*
	recuperation commandes STDIN et envoi a gifa
*/
{
        int n_octets,err=0,l,i;
        Widget menu_bar_comm;
        char  tampon[MAX_CHAR],tt[MAX_CHAR];

                                /* Lecture sur STDIN */
        n_octets = 0;
        for(i=0;i<MAX_CHAR;i++) tampon[i] = 0;
        n_octets = read(0,tampon,MAX_CHAR);
        if(n_octets != -1){
                err = 0;
                JOURCOMMON.haswritten = 1;   /* window has changed */
                WIN_COMMON.on_graph = 0;     /* user i/o with text */
		pre_execute(tampon);
		WIN_COMMON.on_graph = 1;
        }
}


/*-------------------------------------------------------------*/
#ifndef NOREADLINE
void rlg_execute(char *tt)
{
     if( tt && *tt){
                JOURCOMMON.haswritten = 1;   /* window has changed */
                WIN_COMMON.on_graph = 0;     /* user i/o with text */
		add_history(tt);
		pre_execute(tt);
		free(tt);
                WIN_COMMON.on_graph = 1;
        }

}
#endif
/*-------------------------------------------------------------*/

void CREAT_TITRE(label_titre_f,lg,id)
int *lg;
Widget  *id;
char label_titre_f[];
/*      Creation et organisation des buttons    */
{
        XContext cont_titre;
        Widget menu_bar_comm,new_but_titre,menu_bar_titre;
        char label_titre[MAX_CHAR];
        Arg args[5];
        int n;
        n = 0;


                                /* creation de la barre de menu titre */


        if(first_pass == 0){

           if(menu_deroulant == 1) { 
			XtSetArg(args[n],XmNrowColumnType,XmMENU_BAR);n++;
			XtSetArg(args[n],XmNtearOffModel,XmTEAR_OFF_ENABLED);n++;
	   }
           XtSetArg(args[n],XmNorientation,XmHORIZONTAL);n++;
/*
ICI nb de ligne dans le titre DEBRANCHE A FINIR
	   if(nb_rows_title <= 0)nb_rows_title = 1;
	   XtSetArg(args[n],XmNpacking,XmPACK_COLUMN);n++;
	   XtSetArg(args[n],XmNnumColumns,nb_rows_title);n++;
*/
           menu_bar_titre = XmCreateRowColumn(rc_data.root_w,"menu_bar_titre",args,n);
           rc_data.mere_titre = menu_bar_titre;
           XtManageChild (rc_data.mere_titre);
           AFF_ALL_TITRE(0);

        }



                               /* Creation de context associe aux button titre */

         cont_titre = XUniqueContext();

                                /* Changement string fortrant en C*/

         convert_string(label_titre,label_titre_f,*lg);

                                /* Creation des buttons titre avec passage du
                                   label (label_titre_f) et de l'id du contexte associe
                                   (cont_titre) */

         new_but_titre = Butt_titre(label_titre,cont_titre);

                                /* Creation de shell de commande associe
                                   a chacun des boutons titre */
	 if(menu_deroulant != 1){
         	menu_bar_comm = Creat_shell_comm(label_titre,new_but_titre);
	 }
	 else {menu_bar_comm = new_but_titre;}

                                /* Stockage de l'id du shell de commande dans le
                                   contexte associe au bouton titre d'appel */
         XSaveContext(XtDisplay(new_but_titre),(XID)new_but_titre,cont_titre,
                                (caddr_t)rc_data.mere_comm);

                                /* Retourne l'id du shell de comm */

        *id = menu_bar_comm;

        }


        /******************SEPARATEUR************************/

/*-------------------------------------------------------------
*/


void MAKE_SEPARATEUR(w_parent,error)
Widget *w_parent;
int *error;
/*      Creation d'un separateur     */
{
    Widget wid_sep1,wid_sep2,wid_sep3,wid_sep4,wid_sep5,wid_sep6;
    Arg args[10];
    int n;

                        /* mise en place d'un separateur */


    n = 0;
    XtSetArg (args[n], XmNleftAttachment, XmATTACH_FORM);  n++;
    XtSetArg (args[n], XmNrightAttachment, XmATTACH_FORM);  n++;
    XtSetArg(args[n],XmNseparatorType,XmNO_LINE);n++;
    wid_sep1 = XmCreateSeparator(*w_parent, "separator", args, n);
    XtManageChild (wid_sep1);

    n = 0;
    XtSetArg (args[n], XmNleftAttachment, XmATTACH_FORM);  n++;
    XtSetArg (args[n], XmNrightAttachment, XmATTACH_FORM);  n++;
    XtSetArg(args[n],XmNseparatorType,XmNO_LINE);n++;
    wid_sep2 = XmCreateSeparator(*w_parent, "separator", args, n);
    XtManageChild (wid_sep2);


    n = 0;
    XtSetArg (args[n], XmNleftAttachment, XmATTACH_FORM);  n++;
    XtSetArg (args[n], XmNrightAttachment, XmATTACH_FORM);  n++;
    XtSetArg(args[n],XmNseparatorType,XmSHADOW_ETCHED_IN);n++;
    wid_sep3 = XmCreateSeparator(*w_parent, "separator", args, n);
    XtManageChild (wid_sep3);


    n = 0;
    XtSetArg (args[n], XmNleftAttachment, XmATTACH_FORM);  n++;
    XtSetArg (args[n], XmNrightAttachment, XmATTACH_FORM);  n++;
    XtSetArg(args[n],XmNseparatorType,XmNO_LINE);n++;
    wid_sep5 = XmCreateSeparator(*w_parent, "separator", args, n);
    XtManageChild (wid_sep5);

    n = 0;
    XtSetArg (args[n], XmNleftAttachment, XmATTACH_FORM);  n++;
    XtSetArg (args[n], XmNrightAttachment, XmATTACH_FORM);  n++;
    XtSetArg(args[n],XmNseparatorType,XmNO_LINE);n++;
    wid_sep6 = XmCreateSeparator(*w_parent, "separator", args, n);
    XtManageChild (wid_sep6);

}


/*-------------------------------------------------------------
*/


void WAIT_CURSOR_IN_ALL_SHELL()
/*
	cursor montre (attente) dans
	tout les shell de commandes
	dans boite de zoom , window_mere,
	et formulaire....
*/
{

	int i,j;


	/* parcour de la liste des shells affiches sur le display */
	j = -1;
	for(i=0;i<nb_shell_aff;i++){
		XDefineCursor(XtDisplay(id_shell_aff[i]),
			XtWindow(id_shell_aff[i]),cursor_wait);
		j = i;
	}
		/* test d'existance d'au moins 1 shell*/
	if(j != -1) XFlush(XtDisplay(id_shell_aff[j]));

}

/*-------------------------------------------------------------
*/


void ARROW_CURSOR_IN_ALL_SHELL()
/*
        delete cursor montre (attente) dans
        tout les shell de commandes
        dans boite de zoom , window_mere,
        et formulaire....
*/
{

        int i,j;

       /* parcour de la liste des shells affiches sur le display */

	j = -1;
        for(i=0;i<nb_shell_aff;i++){
                XUndefineCursor(XtDisplay(id_shell_aff[i]),
                        XtWindow(id_shell_aff[i]));
                j = i; 
        }
		/* test d'existance d'au moins 1 shell*/
        if(j != -1) XFlush(XtDisplay(id_shell_aff[j]));



}

/*-------------------------------------------------------------
* permet de tester si le formulaire courant n'est pas bloquant
*	si oui : on ne lance pas la commande a execute
*/
void pre_execute(command)
String command;
{
        int err=0;
        int l_comm_f,i;
        char comm_f[MAX_CHAR];

	if(flag_bloquant == 1){
		XBell(XtDisplay(window_mere), 100);
	}
	else{
       		cconvert_string(comm_f,command,&l_comm_f);
    		EXECUTE(comm_f,&l_comm_f,&err);
        	if(err == -1) {
			i=0;
#ifndef NOREADLINE
			rl_callback_handler_remove();
#endif
			GIFA_EXIT(&i);
		}
	}
}

	/* ICI nbre de lignes dans le titre */

void SET_NB_ROWS(nbrowtitle)
int *nbrowtitle;
{

  if(first_pass == 0 ){
	if(*nbrowtitle > 0) nb_rows_title = *nbrowtitle;
 }
 else {
        *nbrowtitle = -1; /* error*/
 }
}

	/* choix entre menus deroulants et statiques */

void SET_PDMENU(pdmenu)
int *pdmenu;
{
 
 if(first_pass == 0 ){
	menu_deroulant = *pdmenu;
 } 
 else {
	*pdmenu = 2; /* error*/
 }
}

/*-------------------------------------------------------------*
 |                          SetAppIcon()                       |
 *-------------------------------------------------------------*/
void SetAppIcon(shell,orig)
Widget shell;
int orig;
/*		orig = 1: logo general (else)
		orig = 2: boite de zoom
		orig = 3: disp 1d
		orig = 4: disp 2d
		orig = 5: disp 3d
		orig = 6: phbox
		orig = 7: 3dbox
		orig = 8: freeze1d
		orig = 9: freeze2d
*/
{
  Pixmap  iconPixmap;

  if(orig == 3)
  iconPixmap = XCreateBitmapFromData(XtDisplay(shell), XtScreen(shell)->root,
                                     oned_bits, oned_width, oned_height);
  else if(orig == 2 || orig == 7)
  iconPixmap = XCreateBitmapFromData(XtDisplay(shell), XtScreen(shell)->root,
                                     zoom_bits, zoom_width, zoom_height);
  else if(orig == 4 || orig == 5)
  iconPixmap = XCreateBitmapFromData(XtDisplay(shell), XtScreen(shell)->root,
                                     twod_bits, twod_width, twod_height);
  else if(orig == 6)
  iconPixmap = XCreateBitmapFromData(XtDisplay(shell), XtScreen(shell)->root,
                                     ph_bits, ph_width, ph_height);
  else if(orig == 8)
  iconPixmap = XCreateBitmapFromData(XtDisplay(shell), XtScreen(shell)->root,
                                     frezone_bits, frezone_width, frezone_height);
  else if(orig == 9)
  iconPixmap = XCreateBitmapFromData(XtDisplay(shell), XtScreen(shell)->root,
                                     freztwo_bits, freztwo_width, freztwo_height);
  else
  iconPixmap = XCreateBitmapFromData(XtDisplay(shell), XtScreen(shell)->root,
                                     gifa_bits, gifa_width, gifa_height);

  XtVaSetValues(shell, XmNiconPixmap, iconPixmap, NULL);
}


/*-------------------------------------------------------------
*	maintient a jour la liste des widgets 
*	affiches a l'ecran
*/
void add_shell_on_display(id_shell)
Widget id_shell;
{
	id_shell_aff[nb_shell_aff] = id_shell;
        nb_shell_aff ++;

}

/*-------------------------------------------------------------
*	enleve id_shell de la liste des widgets
*	affiches a l'ecran
*/
void del_shell_on_display(id_shell)
Widget id_shell;
{

	int i;

        for(i=0;i<nb_shell_aff;i++){
                if(id_shell_aff[i] == id_shell){
                        id_shell_aff[i] = 0;
                        for(;i<nb_shell_aff-1;i++){
                                id_shell_aff[i] = id_shell_aff[i+1];
                        }
                nb_shell_aff --;
                break;
                }
        }

}

