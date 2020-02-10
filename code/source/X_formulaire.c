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


/**************************************************************************
**    include
*/
#ifndef HP
#  include <Xm/Xm.h>
#  include <Xm/DialogS.h>
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
#  include <Xm/Form.h>
#  include <Xm/RowColumn.h>
#  include <Xm/Label.h>
#  include <Xm/TextF.h>
#  include <Xm/Frame.h>
#  include <Xm/FileSB.h>
#  include <Xm/Text.h>
#  include <Xm/ScrolledW.h>
#  include <Xm/Protocols.h>
#  include <Xm/Scale.h>
#  include <Xm/List.h>
#  include <Xm/MwmUtil.h>
#else
# include <Xm/XmAll.h>
# include <X11/cursorfont.h>
# include <Xm/SelectioB.h>
# include <Xm/Text.h>
# include <Xm/ScrolledW.h>
# include <Xm/List.h>
# include <Xm/MwmUtil.h>
#endif
#ifndef MACHTEN
#  include <malloc.h>
#else
#  include <sys/malloc.h>
#endif

#include "X_windef.h"
#include "util.h"
#if defined(SOLARIS) || defined(SPEC1)
#include <string.h>
#else
#include <strings.h>
#endif

#include <stdlib.h>

/*-------------------------------------------------------------
**      forwarded functions
*/


void CREATE_CHAMPS();
void CREAT_FORMULAIRE();
void MAKE_SEPARATEUR_FORM();
void AFF_FORMULAIRE();
void creat_var_champ();
int  creat_text();
void creat_action();
void do_action();
void creat_PulldownMenu();
void creat_Multi_select();
void recherche_var();
void alloue_variable();
void creat_message_text();
void creat_file();
void creat_scale();
void default_butt_form();
void free_struct();
void arrow_scale_lCB();
void arrow_scale_rCB();
void alloc_scale_var();


/*-------------------------------------------------------------
**      Xt callbacks 
*/
void OK_QuitCB();			
void ApplyCB();
void CancelCB();
void CLOSE_BOX();
void CloseCB_widg();
void TextFieldCB();
void PDvarCB();
void arrow_rightCB();
void DialogAcceptCB();
void DialogCancelCB();
void update_scaleCB();
void change_scaleCB();
void set_label_scale();
void boucle_event();
void MultiSelectCB();

/*-------------------------------------------------------------
*     Variables externes
*/

extern int versionx;
extern Widget window_mere;
extern XtAppContext app_context;
extern int nb_form;
Widget id_form[100]; /* 100 : nbre de formulaires */
int id_struct[100];
extern int flag_bloquant;

/*-------------------------------------------------------------
*     definitions globales
*/

#define Max_Text 100             /* max number of test field in a form */
#define Max_PullDown 1000       /* max number of pulldown entries in a form */
#define Max_Scale 100		/* max number of scale entries in a form */
#define Max_MultiEnum 100 	/* max number of MultiEnum entries in a form */

typedef struct {
        String name_but;
        char variable_but[32];
        int context;
        int nor_var;
}PulldownStruct;

typedef struct {
	char variable[32];
	int context;
	int nor_var;
}MultiEnumStruct;

typedef struct {
        char variable[32];
        int context;
        int nor_var;
        Widget tfield;
}TextFieldStruct;

typedef struct {
	char variable[32];
	int context;
	int nor_var;
	int decpoint;
	Widget scale;
	int cur_val,min,max;
	Widget label;
}ScaleStruct;


typedef struct{
    Widget mere_form;
    Widget menu_bar_form;
    Widget menu_bar_row;
    Widget default_button;
    Widget scroll_window;
    Widget prev_field;
    int flag_prev_field;
    int bloquant;
    int nor_var;
    int orig_c;
    int ok_button;
    int cancel_button;
    int apply_button;
    int help_button;	/* non branche */
    char command[256];
    XtAppContext ap_cont_form;
    TextFieldStruct *pt_tf_struct[Max_Text];
    PulldownStruct *pt_pd_struct[Max_PullDown];
    ScaleStruct	*pt_sc_struct[Max_Scale];
    MultiEnumStruct *pt_me_struct[Max_MultiEnum];
    int cpt_nb_tf;
    int cpt_nb_pd;
    int cpt_nb_sc;
    int cpt_nb_me;
} FORM_data ;

typedef struct{
	Widget mere_form;
	char but_act[256];
} STR_ACT_FORM;

/* flag de sortie de boucle_event :
		si = 1 -> ok
		si = -1 -> cancel ou close
*/
int sortie_boucle_ok; 
Dimension height_of_display,width_of_display;

	/* ------------- FONCTIONS GENERALES ---------------- */


void CREAT_FORMULAIRE(label_titre_f,lg,call_back_f,lg_CB,pt_struct,mask)
int *lg,*lg_CB;
int *pt_struct;
char label_titre_f[];
char call_back_f[];
unsigned *mask;
/*-------------------------------------------------------------
*       IN: label_titre_f,call_back_f,lg_CB,mask
*       OUT:pt_struct
*
*       Cette fonction cree le cadre global du formulaire
*       var:
*               int *lg,*lg_CB;         longueur des chaines de char
*                                       label_titre_f et call_back_f.
*                                       (non utilises si appel du C)
*               Widget  *id;            id du widget formulaire
*               pt_struct;              pt sur struct Form_data
*               char label_titre_f[];   titre du formulaire
*               char call_back_f[];     callback general associe au
*                                       formulaire.
*                                       (non utilise si le formulaire
*                                        est bloquant)
*               unsigned *mask;
*                       APPLY_MASK 01   : bouton Apply
*                       CANCEL_MASK 02  : bouton cancel
*                       OK_MASK 04      : bouton ok
*                       HELP_MASK 08    : bouton help (pas de callback associe)
*                       BLOQ_MASK 16    : form bloquant
*                       NORMVAR_MASK 32 : variables non gifa
*                       ORIGC_MASK 64   : appel du C
*
*       remarque:
*        Pour un formulaire bloquant et utilisant les variables gifa generales,
*        le contexte est automatiquement le contexte courant
*
*/
{

        int     n;
        Widget  toplevel_form,menu_bar_form,menu_bar_row,scroll;
        Arg     args[5];
	char label_titre[256];
	char commande[256];
   	Atom  wm_delete_window;
	Atom wm_size_window;
	int  position_x,position_y;
    	int screen;
    	Display *display;
	char *varval;
	FORM_data *form_data ;

                 /* Changement string fortrant en C si necessaire*/

	if((*mask & (ORIGC_MASK)) == 0){
         	convert_string(label_titre,label_titre_f,*lg);
	 	convert_string(commande,call_back_f,*lg_CB);
	}
	else{
		strcpy(label_titre,label_titre_f);
		strcpy(commande,call_back_f);
	}
		



                /* recherche de la position de la boite sur l'ecran*/

	display = XtDisplay(window_mere);
        screen = DefaultScreen(display);
        width_of_display = DisplayWidth(display,screen);
	height_of_display = DisplayHeight(display,screen);
        position_x = ((width_of_display/2) - 100);
	position_y = 100;

                /* creation du shell de commande et de la Row Column
                        contenant les champs de variables */


	n = 0;
        XtSetArg(args[n], XmNallowShellResize, True); n++ ; /* resize en fct du contenu */
	/* XtSetArg(args[n], XmNiconWindow,True); n++ ; */
        toplevel_form=XmCreateDialogShell(window_mere,label_titre,args,n);
	
		/* stockage des formulaires */

	add_shell_on_display(toplevel_form);
	id_form[nb_form] = toplevel_form;
	nb_form++;


		/* Creation Form ou dialog */

	n = 0;
        XtSetArg (args[n], XmNnoResize, True);n++;
        XtSetArg (args[n], XmNdefaultPosition, False);n++;
        XtSetArg (args[n], XmNx ,position_x);n++;
        XtSetArg (args[n], XmNy ,position_y);n++;
        menu_bar_form = XmCreateForm(toplevel_form, "Message", args,n);

	n = 0;
	XtSetArg (args[n], XmNscrollingPolicy, XmAUTOMATIC);  n++;
	XtSetArg (args[n], XmNresizable, True);n++;
	XtSetArg (args[n], XmNleftAttachment, XmATTACH_FORM);  n++;
    	XtSetArg (args[n], XmNrightAttachment, XmATTACH_FORM);  n++;
	XtSetArg (args[n], XmNtopAttachment,XmATTACH_FORM);  n++;
       	scroll = XmCreateScrolledWindow(menu_bar_form, "scroll", args, n);
	XtManageChild(scroll);

       	n = 0;
       	menu_bar_row = XmCreateRowColumn(scroll, "menu_bar_form", args, n);
       	XtManageChild(menu_bar_row);



		/* init variables globales */

	form_data = (FORM_data *)XtMalloc(sizeof(FORM_data));
	form_data->scroll_window = scroll;
	form_data->mere_form = toplevel_form;
	form_data->menu_bar_row = menu_bar_row;
	form_data->menu_bar_form = menu_bar_form;
	form_data->flag_prev_field = 0;
	if((*mask & (APPLY_MASK)) == 1)  form_data->apply_button = 1; 	
	else form_data->apply_button = 0;	

        if((*mask & (CANCEL_MASK)) == 2) form_data->cancel_button = 1; 
        else form_data->cancel_button = 0;   

        if((*mask & (OK_MASK)) == 4) form_data->ok_button = 1;     
        else form_data->ok_button = 0;    

        if((*mask & (BLOQ_MASK)) == 16) form_data->bloquant = 1;  
        else form_data->bloquant = 0;    

        if((*mask & (NORMVAR_MASK)) == 32) form_data->nor_var = 1;    
        else form_data->nor_var = 0;   

        if((*mask & (ORIGC_MASK)) == 64) form_data->orig_c = 1;  
        else form_data->orig_c = 0;   

	sortie_boucle_ok = 0;

		/* compteurs sur le nbre de struct allouees (pour le free)*/
	form_data->cpt_nb_tf = 0;
	form_data->cpt_nb_pd = 0;
	form_data->cpt_nb_sc = 0;
	form_data->cpt_nb_me = 0;

	strcpy(form_data->command,commande);	/* commande globale du formulaire */


       		 /* Detournement de la commande CLOSE generale vers Close de la Widget */

    	wm_delete_window = XmInternAtom(XtDisplay(toplevel_form),
                                "WM_DELETE_WINDOW",
                                FALSE);
    	XmRemoveWMProtocols(toplevel_form,&wm_delete_window, 1);
    	XmAddWMProtocolCallback(toplevel_form, wm_delete_window,CloseCB_widg,(XtPointer)form_data);


		/* enleve la fonction resize */

 
        if (XmIsMotifWMRunning(toplevel_form)) {
           MwmHints     mwm_set_hints ;
           Atom         mwm_hints ;
           XWMHints     wm_set_hints;
           XUnmapEvent  Unmap_ev;
           mwm_hints = XmInternAtom(XtDisplay(toplevel_form), "_MOTIF_WM_HINTS", False);
           mwm_set_hints.flags = MWM_HINTS_FUNCTIONS ;
           mwm_set_hints.functions = MWM_FUNC_ALL | MWM_FUNC_RESIZE ; 
           XChangeProperty(XtDisplay(toplevel_form),XtWindow(toplevel_form),
                        mwm_hints,mwm_hints, 32, PropModeReplace,
                        (unsigned char *)&mwm_set_hints,
                        sizeof(MwmHints)/sizeof(int));
        }



	*pt_struct = (int)form_data;

		/* mise a jour d'une variable "form" contenant le pointeur sur la
		structure du formulaire.C'est une pseudo variable. Elle
		permettra la fermeture des formulaires */

	
	id_struct[nb_form-1] = *pt_struct;
	varval = (char *)malloc(20);
	sprintf(varval,"%d",*pt_struct);
	alloue_variable("$widget",varval,form_data->mere_form);

}



void AFF_FORMULAIRE(str_pt,error)
int *str_pt;
int *error;
/*-------------------------------------------------------------
*       void AFF_FORMULAIRE(str_form_data,error)
*       IN : str_form_data
*       INOUT: error
*
*       Affiche le formulaire apres son elaboration
*       var:
*               FORM_data *str_form_data;pt struct generale
*               int *error; error si != 0;
*                         ( si formulaire est bloquant
*                           error = 1 si cancel )
*/
{
	Arg args[10] ;
	int n,i;
	FORM_data *str_form_data;
	Dimension form_width,form_height;

		/* *str_form_data;pt struct generale:*/

	str_form_data = (FORM_data *)*str_pt;

		/* les derniers buttons */
	 default_butt_form(str_form_data);

		/* creation de la widget form */
         XtManageChild(str_form_data->menu_bar_form);

		/* calcul de la taille du formulaire*/
	
        form_height = 0;
        form_width = 0;
	
	n = 0 ;
        XtSetArg(args[n], XmNwidth, &form_width); n++ ;
        XtSetArg(args[n], XmNheight, &form_height); n++ ;
        XtGetValues(str_form_data->menu_bar_row, args, n);
	
        form_height = form_height + 15;
        form_width = form_width + 15;

	if(form_height > (height_of_display-200)) {
		form_height = (height_of_display-200);
		form_width = form_width + 20;
	}
	if(form_width > (width_of_display-200)) {
		form_width = (width_of_display-200); 
		form_height = form_height + 20;
	}

       	n = 0 ;
       	XtSetArg(args[n], XmNwidth, form_width); n++ ;
       	XtSetArg(args[n], XmNheight, form_height); n++ ;
/*
XtSetArg(args[n], XmNbottomWidget,str_form_data->default_button); n++;
XtSetArg(args[n], XmNbottomAttachment,XmATTACH_WIDGET);n++;
*/
/*here*/

       	XtSetValues(str_form_data->scroll_window, args, n);

		/* creation totale */
	
	 XtManageChild(str_form_data->mere_form);
	 XtRealizeWidget(str_form_data->mere_form);


		/* appel de la boucle locale si bloquant */

	if(str_form_data->bloquant == 1){
		flag_bloquant = 1; /*flag general de mode bloquant: interdit l'appel a execute*/
		del_cursor_wait_on_display(1); /* enleve la montre */
		boucle_event();
		del_cursor_wait_on_display(0);	/* remet la montre */
		flag_bloquant  = 0 ; /*flag general de mode bloquant*/
		/* pour savoir si on sort avec cancel ou ok */
		if(sortie_boucle_ok == -1) *error = 1;
		sortie_boucle_ok = 0;
	}
}

void CREATE_CHAMPS(str_pt,label_f,lg_label,var_f,lg_var,type_var,lg_type_var,list_var,lg_list_var,default_value_f,lg_default_value,max_var_f,lg_max_var,dec_pt_f,lg_dec_pt,error)
int *lg_label,*lg_type_var,*lg_var,*lg_list_var,*error,*lg_default_value,*lg_max_var,*lg_dec_pt;
char var_f[];
char label_f[];
char type_var[];
char list_var[];
char default_value_f[];
char max_var_f[];
char dec_pt_f[];
int *str_pt;
/*-------------------------------------------------------------
*       void CREATE_CHAMPS(str_w_parent,label_f,lg_label,var_f,lg_var,
*                       type_var,lg_type_var,list_var,lg_list_var,
*                       default_value_f,lg_default_val,error)
*
*       IN: str_w_parent,label_f,lg_label,var_f,lg_var,type_var,lg_type_var,
*               list_var,lg_list_var,default_value_f,lg_default_val,
*		max_var,lg_max_var
*       INOUT: error
*
*       Cette fonction est appelee pour chacun des champs crees dans
*       le formulaire. Les champs crees dependent de type_var.
*       var:
*               int *lg_label,*lg_type_var,*lg_var,*lg_list_var,*lg_default_value;
*                       longueur des chaine de char
*                       ( non utilises si appel du C)
*               int *error;     si pb error != 0
*               char var_f[];   nom de la variable ou command (Type Action)
*               char label_f[]; label du champ
*               char default_value_f[]; valeur par defaut
*               char type_var[];type de champ:
*                               file:   TextField avec SelectionBox
*                               string: TextField de saisie de string
*                               int:    TextField de saisie de int
*                               real:   TextField de saisie de real
*                               enum:   PullDown menu
*				multienum: PullDown menu multi select
*				text:	text view
*                               message:affichage de texte
*
*               char list_var[];liste des valeurs d'un PullDown menu
*               Form_data *str_w_parent;pt sur struct du  cadre du formulaire
*		max_var[],lg_max_var: max pour le cursor (le min est passe dans
*							list_var)
*
*
*/
{
        char label[256];
	char label_tmp[256];
	char default_val[256];
	char type[256];
	char variable[256];
	char list[256];
        char maxv[256];
	char decpt[256];
	char *list_states[32];
	char *list_def[32];
	char *pdvar,*tmp_var;
	char *defvar,*tmp_def;
	int  pass,nb_def,nb_but,i,j;
	FORM_data *str_w_parent;
	char *ct;


		/* recuperation des chaines en C si necessaire*/
	
	str_w_parent = (FORM_data *)*str_pt;

	if(str_w_parent->orig_c == 1){
		strcpy(label,label_f);
		strcpy(variable,var_f);
		strcpy(type,type_var);
		strcpy(list,list_var);
		strcpy(default_val,default_value_f);
		strcpy(maxv,max_var_f);
		strcpy(decpt,dec_pt_f);
	}

	else{
        	convert_string(label,label_f,*lg_label);
        	convert_string(type,type_var,*lg_type_var);
        	convert_string(variable,var_f,*lg_var);
        	convert_string(list,list_var,*lg_list_var);
		convert_string(default_val,default_value_f,*lg_default_value);
		convert_string(maxv,max_var_f,*lg_max_var);
		convert_string(decpt,dec_pt_f,*lg_dec_pt);
	}


		/* switch en fction des types de champs*/

                	/* type  == enum:  PulldownMenu */

	pass = 0;
	if (strncmp(type,"ENUM",4) == 0){


         	ct = ",";
        	nb_but = 0;

			/* recuperation des differentes variables */

		tmp_var  = (char *)XtMalloc(256);
		strcpy(tmp_var,list);

		pdvar = strtok(tmp_var,ct);
		list_states[nb_but] = pdvar; nb_but++;

		while(pdvar != NULL){
			tmp_var = NULL;		
			pdvar = strtok(tmp_var,ct);
			list_states[nb_but] = pdvar; nb_but++;
		}
		nb_but --;

		creat_PulldownMenu(str_w_parent,label,variable,list_states,nb_but,default_val);
		pass = 1;
	}

                        /* type  == multienum:  PulldownMenu multi select*/

        if (strncmp(type,"MULTIENUM",9) == 0){


                ct = ",";
                nb_but = 0;
		nb_def = 0;

                        /* recuperation des differentes variables */

                tmp_var  = (char *)XtMalloc(256);
                strcpy(tmp_var,list);

                pdvar = strtok(tmp_var,ct);
                list_states[nb_but] = pdvar; nb_but++;

                while(pdvar != NULL){
                        tmp_var = NULL;
                        pdvar = strtok(tmp_var,ct);
                        list_states[nb_but] = pdvar; nb_but++;
                }
                nb_but --;

                creat_Multi_select(str_w_parent,label,variable,list_states,nb_but,default_val);
                pass = 1;
        }

	
		/*  type  == string seule  variable avec champ editable */

	if ((strncmp(type,"STRING",6) == 0) || (strncmp(type,"INT",3) == 0) || (strncmp(type,"REAL",4) == 0)){
                creat_var_champ(str_w_parent,label,variable,type,default_val);
		pass = 1;
	}

		/*  type  ==  file  champ editable  avec widget de recherche*/

	if (strncmp(type,"FILE",4) == 0){
		pass = 1;
		creat_file(str_w_parent,label,variable,default_val);
	}

		/*  type  ==  message : boite de dialogue */

	if (strncmp(type,"MESSAGE",7) == 0){
		pass = 1;
		creat_message_text(str_w_parent,label);
	}

		/* type == text: viewver de texte */

	
        if (strncmp(type,"TEXT",4) == 0){
                pass = creat_text(str_w_parent,label);
        }


		/* type == action : button with action */

	if (strncmp(type,"ACTION",6) == 0){
		creat_action(str_w_parent,label,default_value_f,lg_default_value);
		pass =1;
	}


                /* type == scale : scale cursor */

        if (strncmp(type,"CURSOR",6) == 0){
                creat_scale(str_w_parent,label,variable,list,maxv,default_val,decpt,error);
                pass =1;
        }


	
		/* test de bon passage */

	if(pass == 0){
		*error = 1;
	}
	

}

void creat_file(str_form,label_var,variable,val_def)
FORM_data *str_form;
char label_var[256];
char variable[32];
char val_def[256];
/*-------------------------------------------------------------
*       void creat_file(str_form,label_var,variable,val_def)
*
*       IN: parent,label_var,variable,val_def;
*
*       creation d'un champ de formulaire de type file
*               -> TextField avec SelectionFile
*       var:
*
*               FORM_data *str_form; pt struct generale
*               char *label_var; Label du champ
*               char *variable;  nom de la variable
*               char *val_def;   valeur par defaut
*
*/
{
        Arg     args[10];
        int n,pos_cursor;
        int err;
        Widget tf_box,select_file_box,parent_form,frame,frame0,frame1,label_string,
				row_column,textfield,label,arrow;
        char valeur_variable[256];
        char valeur_variable_f[256]; /* int, char, float... a faire*/
        char var_f[256];
        int *l_var_f;
        int *l_valeur_var;
	int *cont_wid;
	TextFieldStruct *TFstruct;
	int scontext;
	XmString string;

	/* mise a jour de la valeur par defaut */

	strcpy(valeur_variable,val_def);
	

        if (str_form->flag_prev_field == 0){

	

	        /*      Create RadioBox and dialog style toggles.
        	*/

 	       	n = 0;
		frame = XmCreateForm (str_form->menu_bar_row, "frame", args, n);
	        XtManageChild (frame);


                n = 0;
                XtSetArg(args[n],XmNorientation,XmHORIZONTAL);n++;
                XtSetArg(args[n],XmNmarginWidth,10);n++;
                frame0 = XmCreateRowColumn(frame, " frame_form",args, n);
                XtManageChild(frame0);

                        /* stockage pour eventuel alignement*/
                str_form->prev_field = frame0;

        }
        else{
                /* alignement dans le meme champ */
                frame0 = str_form->prev_field;
                str_form->flag_prev_field = 0;
        }




                /* set de la variable et recuperation de sa
                        valeur */


	if(str_form->nor_var == 0){
		if(str_form->bloquant == 1){
                 	GETVCONTEXT(&scontext);
			recherche_var(variable,valeur_variable,scontext);
		}
		else{
        		recherche_var(variable,valeur_variable,str_form->mere_form);
		}
	}


                /* champ editable de la variable */

        n = 0;
        XtSetArg (args[n], XmNleftAttachment, XmATTACH_FORM);  n++;
        XtSetArg (args[n], XmNtopAttachment, XmATTACH_FORM);  n++;
        XtSetArg (args[n], XmNbottomAttachment, XmATTACH_FORM);  n++;
        label = XmCreateLabel(frame0, label_var, args, n);
        XtManageChild (label);

		/* struct avec variable et contexte */

	TFstruct = (TextFieldStruct *)XtMalloc(sizeof(TextFieldStruct));
        str_form->pt_tf_struct[str_form->cpt_nb_tf] = TFstruct;/*pour le free*/ 
	str_form->cpt_nb_tf++;/*pour le free*/
	strcpy(TFstruct->variable,variable);
	if((str_form->nor_var == 0) && (str_form->bloquant == 1)){
		 GETVCONTEXT(&(TFstruct->context));
		 			 /* cas ou on utilise les var gifa */
	}				 /* et c'est bloquant */
	else{
		TFstruct->context = (int)str_form->mere_form;
	}
	if(str_form->nor_var == 1) {	/* cas ou on utilise pas les var gifa */
		TFstruct->nor_var = 1;
	}
	else{
		TFstruct->nor_var = 0;
	}

        n = 0;
        XtSetArg (args[n], XmNtopAttachment, XmATTACH_FORM);  n++;
	XtSetArg (args[n], XmNbottomAttachment, XmATTACH_FORM);  n++;
        XtSetArg (args[n], XmNleftWidget, label);  n++;
	XtSetArg (args[n], XmNleftAttachment, XmATTACH_WIDGET);  n++;
        XtSetArg (args[n], XmNcolumns, 20);  n++;
	if(str_form->nor_var == 0){
        	XtSetArg (args[n], XmNvalue, valeur_variable);  n++;
	}
        textfield = XmCreateTextField(frame0, "text", args, n);
	TFstruct->tfield = textfield;
        XtAddCallback (textfield, XmNactivateCallback, TextFieldCB,
                   TFstruct);
	XtAddCallback (textfield, XmNlosingFocusCallback, TextFieldCB,
                   TFstruct);
	XtAddCallback (textfield, XmNvalueChangedCallback, TextFieldCB,
                   TFstruct);

	

        XtManageChild (textfield);


                /* position curseur dans TextField */

        pos_cursor = XmTextGetMaxLength(textfield);
        XmTextSetInsertionPosition(textfield,pos_cursor);


        n = 0;
        XtSetArg (args[n], XmNrightAttachment, XmATTACH_FORM);  n++;
	XtSetArg (args[n], XmNleftWidget, textfield);  n++;
        XtSetArg (args[n], XmNleftAttachment, XmATTACH_WIDGET);  n++;
        XtSetArg(args[n], XmNarrowDirection,XmARROW_RIGHT);n++;
        arrow = XmCreateArrowButton(frame0,"Right",args, n);

                /* creation du selection file */

/*
par defaut, le lieu dans gifa

	string = XmStringCreateLtoR ("*",XmSTRING_DEFAULT_CHARSET);
	n=0;
	XtSetArg(args[n],XmNdirMask,string);n++;
	XtSetArg(args[n],XmNdirectory,NULL);n++;
*/
n = 0;
/* BUG LINUX */
        select_file_box = XmCreateFileSelectionDialog(arrow,
                "file selection", args, n);
/*
	XmStringFree(string);
*/

		/* callback*/

        XtAddCallback(select_file_box, XmNokCallback,
                 DialogAcceptCB,textfield);
        XtAddCallback(select_file_box, XmNcancelCallback,
                DialogCancelCB, select_file_box);


        XtAddCallback (arrow, XmNactivateCallback, arrow_rightCB,select_file_box);
        XtManageChild(arrow);

}

void creat_message_text(str_form,message)
FORM_data *str_form;
char message[256];
/*-------------------------------------------------------------
*       void creat_message_text(str_form,message)
*
*       IN: message
*
*       Creation d'un champ de type message
*       -> text sur une ligne
*       var:
*               char *message; texte
*/
{

        Widget          frame,frame0;
        Arg             al[10];         /*  arg list            */
        int             ac;             /*  arg count           */
	Widget          label_string;
	Dimension	n;



        if (str_form->flag_prev_field == 0){

     	  	 /*      Create  message map.
       		 */

		n = 0;
       		ac = 0;
		XtSetArg (al[ac], XmNshadowThickness,n);  ac++;
       	 	frame = XmCreateFrame (str_form->menu_bar_row, "frame", al, ac);
        	XtManageChild (frame);

#ifdef SGI
		ac = 0;
		XtSetArg (al[ac], XmNshadowThickness, n);  ac++;
		XtSetValues(frame,al,ac);
#endif

                ac = 0;
                XtSetArg(al[ac],XmNorientation,XmHORIZONTAL);ac++;
                XtSetArg(al[ac],XmNmarginWidth,10);ac++;
                frame0 = XmCreateRowColumn(frame, " frame_form",al, ac);
                XtManageChild(frame0);

                        /* stockage pour eventuel alignement*/
                str_form->prev_field = frame0;

        }
        else{
                /* alignement dans le meme champ */
                frame0 = str_form->prev_field;
                str_form->flag_prev_field = 0;
        }


        label_string = XmCreateLabel(frame0, message, NULL, 0);
        XtManageChild (label_string);
}


void creat_action(str_form,label,command_f,lg_comm)
FORM_data *str_form;
char label[256];
char command_f[];
int *lg_comm;
/*-------------------------------------------------------------
*       void creat_action(str_form,label,command)
*
*       IN: str_form,label,command;
*
*       creation d'un champ de formulaire de type action
*	button with action
*       var:
*
*               FORM_data *str_form; pt struct generale
*               char *label; Label du button
*               char *command;  command
*
*/
{


        Widget          frame,frame_row;
        Arg             al[10];         /*  arg list            */
        int             ac;             /*  arg count           */
        Widget          button;
        Dimension       n;
	XContext cont_comm;
	char *comm;
	STR_ACT_FORM *str_act;


	str_act = (STR_ACT_FORM *)XtMalloc(sizeof(STR_ACT_FORM));

	if (str_form->flag_prev_field == 0){
	        n = 0;
        	ac = 0;
 	        XtSetArg (al[ac], XmNshadowThickness,n);  ac++;
       		frame = XmCreateFrame (str_form->menu_bar_row, "frame", al, ac);
       		XtManageChild (frame);

#ifdef SGI
	        ac = 0;
       		XtSetArg (al[ac], XmNshadowThickness, n);  ac++;
	        XtSetValues(frame,al,ac);
#endif

		ac = 0;
      		XtSetArg(al[ac],XmNorientation,XmHORIZONTAL);ac++;
		XtSetArg(al[ac],XmNmarginWidth,10);ac++;
       		frame_row = XmCreateRowColumn(frame, " frame_form", al, ac);
  	        XtManageChild(frame_row);

			/* stockage pour eventuel alignement*/
		str_form->prev_field = frame_row;
	}
	else{
		/* alignement dans le meme champ */
		frame_row = str_form->prev_field;
		str_form->flag_prev_field = 0;
	}

	comm = (char *)malloc(*lg_comm + 1);
        convert_string(comm,command_f,*lg_comm);
	strcpy(str_act->but_act,comm);
	str_act->mere_form = str_form->mere_form;
        button = XmCreatePushButton(frame_row,label, NULL, 0);
        XtManageChild (button);
	XtAddCallback (button, XmNactivateCallback, do_action,
                               str_act );

}

int creat_text(str_form,file_name)
FORM_data *str_form;
char file_name[256];
/*-------------------------------------------------------------
*       void creat_text(str_form,file_name)
*
*       IN: file_name
*
*       Creation d'un champ de type texte
*/
{
        Widget          frame,frame0;
        Arg             al[10];         /*  arg list            */
        int             ac;             /*  arg count           */
        Widget          textfile;
        Dimension       n;
	int		err;
        int             nb_rows;


        /*      Create  message map.
        */

        if (str_form->flag_prev_field == 0){
    	   	n = 0;
       		ac = 0;
 	       	XtSetArg (al[ac], XmNshadowThickness,n);  ac++;
 	        frame0 = XmCreateFrame (str_form->menu_bar_row, "frame", al, ac);
        	XtManageChild (frame0);

#ifdef SGI
        	ac = 0;
        	XtSetArg (al[ac], XmNshadowThickness, n);  ac++;
        	XtSetValues(frame0,al,ac);
#endif
                ac = 0;
                XtSetArg(al[ac],XmNorientation,XmHORIZONTAL);ac++;
                XtSetArg(al[ac],XmNmarginWidth,10);ac++;
                frame = XmCreateRowColumn(frame0, " frame_form",al, ac);
                XtManageChild(frame);

                        /* stockage pour eventuel alignement*/
                str_form->prev_field = frame;

        }
        else{
                /* alignement dans le meme champ */
                frame = str_form->prev_field;
                str_form->flag_prev_field = 0;
        }


	n = 0;
        ac = 0;
        XtSetArg (al[ac], XmNeditMode,XmMULTI_LINE_EDIT);  ac++;
	XtSetArg (al[ac], XmNeditable, False);ac++;
        textfile = XmCreateScrolledText(frame, file_name , al, ac);

	err = 0;
	err = Load_file(file_name,textfile);
	if (err == -1){
		 printf("Unable to open file: %s\n", file_name);
		 return(0);	/* return(0)=> erreur */
	}
	else{ 
	
		nb_rows = err+1;
		if (nb_rows > 20) nb_rows = 20;
		n = 0;ac = 0;
        	XtSetArg (al[ac], XmNrows,nb_rows);ac++;
        	XtSetArg (al[ac], XmNcolumns,55);ac++;
		XtSetValues(textfile, al, ac);
		XtManageChild (textfile);
		return(1);  		/* return(1)=> ok */
	}
}


void creat_Multi_select(str_form,name,variable,list_states,nb_but,val_def)
FORM_data *str_form;
char name[256];
char variable[32];
int nb_but;
char *list_states[32];
char val_def[256];
/*-------------------------------------------------------------
*       void creat_Multi_select(str_form,name,variable,list_states,nb_but,list_def,nb_def)
*
*       IN: str_form,name,variable,list_states,nb_but,list_def,nb_def
*
*       Creation d'un champ de type multienum (multiple select)
*       var:
*               Widget parent; id widget du cadre general formulaire
*               FORM_data *str_form; pt struct generale;
*               char *name; label du champ
*               char *variable; nom de la variable
*               int nb_but; nbre de champs dans la liste de selection
*               char *list_states[]; liste pt des differents champs de la liste de selection
*               char *list_def[]; liste pt des differents champs preselectionnes 
*		int nb_def; nbre de champs preselectionnes
*
*/
{

        Arg args[15];
        int n,i,nb_def;
        Widget frame,frame0,menu_pane,row_column,select,label;
        XmString label_string;
	XmString list_item[500];
	XmString list_sel[500];
        int scontext, nb_field;
	char valeur_variable[500];
	char *valtmp,*tmp_def,*defvar,*ct;
	char *list_def[32];
	MultiEnumStruct *MEStruct;

        if (str_form->flag_prev_field == 0){

                /*      Create RadioBox and dialog style toggles.
                */
                n = 0;
                XtSetArg (args[n], XmNshadowType, XmSHADOW_ETCHED_IN );  n++;
                frame = XmCreateForm (str_form->menu_bar_row, "frame", args, n);
                XtManageChild (frame);

                n = 0;
                XtSetArg(args[n],XmNorientation,XmHORIZONTAL);n++;
                XtSetArg(args[n],XmNmarginWidth,10);n++;
                frame0 = XmCreateRowColumn(frame, " frame_form",args, n);
                XtManageChild(frame0);

                        /* stockage pour eventuel alignement*/
                str_form->prev_field = frame0;

        }
	else{
                /* alignement dans le meme champ */
                frame0 = str_form->prev_field;
                str_form->flag_prev_field = 0;
        }

                        /* set de la variable et recuperation de sa
                        valeur */


	strcpy(valeur_variable,val_def);
        if(str_form->nor_var == 0){
                if(str_form->bloquant == 1){
                        GETVCONTEXT(&scontext);
                        recherche_var(variable,valeur_variable,scontext);
                }
                else{
                        recherche_var(variable,valeur_variable,str_form->mere_form);
                }
        }


                /* struct avec variable et contexte */

        MEStruct = (MultiEnumStruct *)XtMalloc(sizeof(MultiEnumStruct));
        str_form->pt_me_struct[str_form->cpt_nb_me] = MEStruct; /*pour le free*/
        str_form->cpt_nb_me++;
        strcpy(MEStruct->variable,variable);
        if((str_form->nor_var == 0) && (str_form->bloquant == 1)){
                 GETVCONTEXT(&(MEStruct->context));

                                         /* cas ou on utilise les var gifa */
                                        /* et c'est bloquant */
        }
        else{
                MEStruct->context = (int)str_form->mere_form;
        }

        if(str_form->nor_var == 1) {
                                 /* cas ou on utilise pas les var gifa */
                MEStruct->nor_var = 1;
        }
        else{
                MEStruct->nor_var = 0;
        }


                /* decomposition de la  variable dans un tableau*/

        ct = ",";
        nb_def = 0;
        tmp_def  = (char *)XtMalloc(256);
        strcpy(tmp_def,valeur_variable);
        defvar = strtok(tmp_def,ct);
        list_def[nb_def] = defvar; nb_def++;

        while(defvar != NULL){
               tmp_def = NULL;
               defvar = strtok(tmp_def,ct);
               list_def[nb_def] = defvar; nb_def++;
        }
        nb_def --;


        /*      Set up items for List. */

        for ( i = 0;  i < nb_but;  i++ ){
		if(nb_but >= 500 ) printf (" error : To many fields in Multienum Box ! \n");
                list_item[i] = XmStringCreateLtoR (list_states[i],XmSTRING_DEFAULT_CHARSET);
	}
        for ( i = 0;  i < nb_def;  i++ ){
                if(nb_def >= 500 ) printf (" error : To many Selected Fields in Multienum Box ! \n");
                list_sel[i] = XmStringCreateLtoR (list_def[i],XmSTRING_DEFAULT_CHARSET);
        }


                /* creation label */
        n = 0;
        label = XmCreateLabel(frame0,name, args, n);
        XtManageChild (label);



		/* selection box */

        n = 0;
/*
	XtSetArg(args[n],XmNwidth,22); n++;
*/
	XtSetArg(args[n],XmNlistMarginWidth,10);n++;
        XtSetArg(args[n], XmNitems, (XtArgVal) list_item); n++;
        XtSetArg(args[n], XmNitemCount, (XtArgVal) nb_but); n++;
	if(nb_but < 5) nb_field = nb_but;
	else nb_field = 5;
        XtSetArg(args[n], XmNvisibleItemCount,nb_field); n++;
        XtSetArg(args[n], XmNselectedItems, (XtArgVal) list_sel); n++;
        XtSetArg(args[n], XmNselectedItemCount, (XtArgVal) nb_def); n++;
	XtSetArg(args[n], XmNselectionPolicy,XmMULTIPLE_SELECT); n++;
    	select = XmCreateScrolledList(frame0, "ListItem", args, n);
	XtManageChild (select);

	XtAddCallback(select, XmNmultipleSelectionCallback, MultiSelectCB, MEStruct);

}




void creat_PulldownMenu(str_form,name,variable,list_states,nb_but,def_val)
FORM_data *str_form;
char name[256];
char variable[32];
int nb_but;
char *list_states[32];
char def_val[256];
/*-------------------------------------------------------------
*       void creat_PulldownMenu(str_form,name,variable,list_states,nb_but,def_val)
*
*       IN: str_form,name,variable,list_states,nb_but,def_val;
*
*       Creation d'un champ de type enum
*       -> text sur une ligne
*       var:
*               Widget parent; id widget du cadre general formulaire
*               FORM_data *str_form; pt struct generale;
*               char *name; label du champ
*               char *variable; nom de la variable
*               int nb_but; nbre de boutons dans le pulldown
*               char *list_states[]; liste pt des differentes valeurs predefinies
*               char *def_val;valeur par defaut
*
*/
{
        Arg     args[5];
        int n,i,set_val;
        Widget frame,frame0,menu_pane,button,default_button,row_column,cascade;
	XmString label_string;
	char valeur_variable[256];
	PulldownStruct *PDstruct;
	int scontext;

	set_val = 0;
	strcpy(valeur_variable,def_val);


	if (str_form->flag_prev_field == 0){

        	/*      Create RadioBox and dialog style toggles.
        	*/
     	        n = 0;
	        XtSetArg (args[n], XmNshadowType, XmSHADOW_ETCHED_IN );  n++;
       		frame = XmCreateForm (str_form->menu_bar_row, "frame", args, n);
	        XtManageChild (frame);

       		n = 0;
	        XtSetArg(args[n],XmNorientation,XmHORIZONTAL);n++;
       		XtSetArg(args[n],XmNmarginWidth,10);n++;
	        frame0 = XmCreateRowColumn(frame, " frame_form",args, n);
	        XtManageChild(frame0);

                        /* stockage pour eventuel alignement*/
                str_form->prev_field = frame0;

	}
	else{
                /* alignement dans le meme champ */
                frame0 = str_form->prev_field;
                str_form->flag_prev_field = 0;
        }

        n = 0;
        menu_pane = XmCreatePulldownMenu (frame0, "menu_pane", args, n);


	                /* set de la variable et recuperation de sa
                        valeur */

	if(str_form->nor_var == 0){
                if(str_form->bloquant == 1){
			GETVCONTEXT(&scontext);
                        recherche_var(variable,valeur_variable,scontext);
                }
                else{
                        recherche_var(variable,valeur_variable,str_form->mere_form);
                }
        }

	for(i=0;i<nb_but;i++){
		PDstruct = (PulldownStruct *)XtMalloc(sizeof(PulldownStruct));
		str_form->pt_pd_struct[str_form->cpt_nb_pd] = PDstruct;/*pour le free*/
		str_form->cpt_nb_pd++;/*pour le free*/
		strcpy(PDstruct->variable_but,variable);
		PDstruct->name_but = list_states[i];

		if((str_form->nor_var == 0) && (str_form->bloquant == 1)){
			GETVCONTEXT(&(PDstruct->context));
                 			 /* cas ou on utilise les var gifa */
        	}                                /* et c'est bloquant */
        	else{
                	PDstruct->context = (int)str_form->mere_form;
        	}

		if(str_form->nor_var == 1) {   /* cas ou on utilise pas les var gifa */
                	PDstruct->nor_var = 1;
        	}
        	else{
                	PDstruct->nor_var = 0;
        	}

        	n = 0;
        	button = XmCreatePushButton (menu_pane, list_states[i], NULL, 0);
		XtAddCallback (button, XmNactivateCallback, PDvarCB,PDstruct);
			/* il faut passer la variable */
        	XtManageChild (button);
		if(i == 0) {default_button = button;scontext = PDstruct->context;}
		if(strcmp(list_states[i],valeur_variable) == 0)
				{default_button = button;set_val = 1;}
	}
	
		/* allocation du premier etat a la variable */


	if(strcmp(valeur_variable,def_val) == 0) alloue_variable(variable,def_val,scontext);
	if(set_val == 0) alloue_variable(variable,list_states[0],scontext);


        label_string = XmStringCreateLtoR (name,XmSTRING_DEFAULT_CHARSET);

        n = 0;
        XtSetArg (args[n], XmNlabelString, label_string);  n++;
        XtSetArg (args[n], XmNmenuHistory, default_button);  n++;
        XtSetArg (args[n], XmNsubMenuId, menu_pane);  n++;
/*
	
        XtSetArg (args[n], XmNrightAttachment, XmATTACH_FORM);  n++;
	XtSetArg (args[n], XmNleftAttachment, XmATTACH_FORM);  n++;
*/
        row_column = XmCreateOptionMenu (frame0, "row_column1", args, n);
        XtManageChild (row_column);

        if (label_string)
                XmStringFree(label_string);


}

void creat_var_champ(str_form,label_var,variable,type,val_def)
FORM_data *str_form;
char label_var[256];
char variable[32];
char type[256];
char val_def[256];
/*-------------------------------------------------------------
*       void creat_var_champ(str_form,label_var,variable,type,val_def)
*
*       IN: parent,label_var,variable,type,val_def
*
*       Creation d'un champ de type string
*       -> TextField pour saisir une chaine de char
*       var:
*               FORM_data *str_form; pt sur struct general
*               char *label_var; Label du champ
*               char *variable; nom de la variable
*               char *type; type de la variable; int,char,real...
*               char *val_def; valeur par defaut
*	
*		str_form->nor_var == 0 : variable gifa
*               str_form->nor_var == 1 : variable non gifa
*/
{


        Arg     args[10];
        int n,pos_cursor;
	int err;
        Widget frame,frame0,label_string,row_column,textfield,label;
	char valeur_variable[256];
	char valeur_variable_f[256]; /* int, char, float... a faire*/
	char var_f[256];
	int *l_var_f;
	int *l_valeur_var;
	TextFieldStruct *TFstruct;
	int scontext;


	strcpy(valeur_variable,val_def);

        /*      Create RadioBox and dialog style toggles.
        */
	
       if (str_form->flag_prev_field == 0){
        	n = 0;
 		XtSetArg (args[n], XmNshadowType, XmSHADOW_ETCHED_IN );  n++;
        	frame = XmCreateForm (str_form->menu_bar_row, "frame", args, n);
   		XtManageChild (frame);

                n = 0;
                XtSetArg(args[n],XmNorientation,XmHORIZONTAL);n++;
                XtSetArg(args[n],XmNmarginWidth,10);n++;
                frame0 = XmCreateRowColumn(frame, " frame_form",args, n);
                XtManageChild(frame0);

                        /* stockage pour eventuel alignement*/
                str_form->prev_field = frame0;
        }
        else{
                /* alignement dans le meme champ */
                frame0 = str_form->prev_field;
                str_form->flag_prev_field = 0;
        }


		/* set de la variable et recuperation de sa
			valeur si variable gifa*/

        if(str_form->nor_var == 0){
                if(str_form->bloquant == 1){
			GETVCONTEXT(&scontext);
                        recherche_var(variable,valeur_variable,scontext);
                }
                else{
                        recherche_var(variable,valeur_variable,str_form->mere_form);
                }
        }


                /* struct avec variable et contexte */

        TFstruct = (TextFieldStruct *)XtMalloc(sizeof(TextFieldStruct));
        str_form->pt_tf_struct[str_form->cpt_nb_tf] = TFstruct;/*pour le free*/
        str_form->cpt_nb_tf++;/*pour le free*/
        strcpy(TFstruct->variable,variable);
	if((str_form->nor_var == 0) && (str_form->bloquant == 1)){
		 GETVCONTEXT(&(TFstruct->context));
                 			 /* cas ou on utilise les var gifa */
        }                                /* et c'est bloquant */
        else{
                TFstruct->context = (int)str_form->mere_form;
        }

	if(str_form->nor_var == 1) { 	/* cas ou on utilise pas les var gifa */
                TFstruct->nor_var = 1;
        }
        else{
                TFstruct->nor_var = 0;
        }



		/* champ editable de la variable */

	n = 0;
        label = XmCreateLabel(frame0, label_var, args, n);
        XtManageChild (label);


        n = 0;
	if(strncmp(type,"STRING",6) == 0)
		XtSetArg (args[n], XmNcolumns, 20);
	else
		XtSetArg (args[n], XmNcolumns, 6); /* Type real et int */
	n++;
	XtSetArg (args[n], XmNtopPosition, 0);  n++;


	if(str_form->nor_var == 0){ 
        	XtSetArg (args[n], XmNvalue, valeur_variable);  n++;
	}
        textfield = XmCreateTextField(frame0, "text", args, n);
    	XtAddCallback (textfield, XmNactivateCallback, TextFieldCB,
                   TFstruct);
        XtAddCallback (textfield, XmNlosingFocusCallback, TextFieldCB,
                   TFstruct);


        XtManageChild (textfield);

		/* position curseur dans TextField */

	/* pos_cursor = XmTextGetMaxLength(textfield); */
	XmTextSetInsertionPosition(textfield,0); /* Cursor en debut de champ*/


}



void creat_scale(str_form,label_var,variable,min,max,val_def,dec_pt,error)
FORM_data *str_form;
char label_var[256];
char variable[32];
char min[256];
char max[256];
char val_def[256];
char dec_pt[256];
int *error;
/*-------------------------------------------------------------
*	void creat_scale(str_form,label_var,variable,min,max,val_def)
*
*       IN: str_form,label_var,variable,list_states,val_def,type
*
*       Creation d'un champ de type scale
*
*               FORM_data *str_form; pt sur struct general
*               char *label_var; Label du champ
*               char *variable; nom de la variable
*		char *min,*max; min et max du cursor scale
*               char *val_def; valeur par defaut
*		char *dec_pt; nombre de chiffre apres virgule
*
*               str_form->nor_var == 0 : variable gifa
*               str_form->nor_var == 1 : variable non gifa
*/
{


        Arg     args[10];
        int n;
        int err;
        Widget arrow_r, arrow_l,frame,frame0,scale,row_column,label,label2;
        char valeur_variable[256];
        char var_f[256];
        int *l_var_f;
        int *l_valeur_var;
        ScaleStruct *SCstruct;
        int scontext;
	int i, min_i, max_i, valdef, val_w, decpt;
	float min_f, max_f, valdef_f;


        strcpy(valeur_variable,val_def);

        /*      Create RadioBox and dialog style toggles.
        */


       if (str_form->flag_prev_field == 0){
                n = 0;
                XtSetArg (args[n], XmNshadowType, XmSHADOW_ETCHED_IN );  n++;
                frame = XmCreateForm (str_form->menu_bar_row, "frame", args, n);
                XtManageChild (frame);

                n = 0;
                XtSetArg(args[n],XmNorientation,XmHORIZONTAL);n++;
                XtSetArg(args[n],XmNmarginWidth,10);n++;
                frame0 = XmCreateRowColumn(frame, " frame_form",args, n);
                XtManageChild(frame0);

                        /* stockage pour eventuel alignement*/
                str_form->prev_field = frame0;
        }
        else{
                /* alignement dans le meme champ */
                frame0 = str_form->prev_field;
                str_form->flag_prev_field = 0;
        }



                /* set de la variable et recuperation de sa
                        valeur si variable gifa*/

        if(str_form->nor_var == 0){
                if(str_form->bloquant == 1){
                        GETVCONTEXT(&scontext);
                        recherche_var(variable,valeur_variable,scontext);
                }
                else{
                        recherche_var(variable,valeur_variable,str_form->mere_form);
                }
        }

                /* struct avec variable et contexte */

        SCstruct = (ScaleStruct *)XtMalloc(sizeof(ScaleStruct));
        str_form->pt_sc_struct[str_form->cpt_nb_sc] = SCstruct;
        str_form->cpt_nb_sc++;
        strcpy(SCstruct->variable,variable);
        if((str_form->nor_var == 0) && (str_form->bloquant == 1)){
                 GETVCONTEXT(&(SCstruct->context));
                                         /* cas ou on utilise les var gifa */
                                        /* et c'est bloquant */
	}
        else{
                SCstruct->context = (int)str_form->mere_form;
        }

        if(str_form->nor_var == 1) {   
				 /* cas ou on utilise pas les var gifa */
                SCstruct->nor_var = 1;
        }
        else{
                SCstruct->nor_var = 0;
        }

                /* champ scale */
        n = 0;
        label = XmCreateLabel(frame0, label_var, args, n);
        XtManageChild (label);

		/* fleche de gauche */

        n = 0;
        XtSetArg(args[n],XmNwidth,22); n++;
        XtSetArg(args[n],XmNheight,22); n++;
        XtSetArg (args[n], XmNleftWidget, label);  n++;
        XtSetArg (args[n], XmNleftAttachment, XmATTACH_WIDGET);  n++;
        XtSetArg (args[n], XmNrightAttachment, XmATTACH_FORM);  n++;
        XtSetArg(args[n], XmNarrowDirection,XmARROW_LEFT);n++;
        arrow_l = XmCreateArrowButton(frame0,"Right",args, n);
        XtAddCallback (arrow_l, XmNactivateCallback, arrow_scale_lCB,SCstruct);
        XtManageChild(arrow_l);


                        /* stockage decimal point */
	decpt = atoi(dec_pt);
        SCstruct->decpoint = decpt;

			/* recup min max, def dans float */

	min_f = (float)atof(min) ;
        max_f = (float)atof(max) ;
	valdef_f = (float)atof(valeur_variable);
	if(decpt == 0){
	        valdef = (int)valdef_f;
	        sprintf(valeur_variable,"%d      ",valdef);
	}else{
		if(decpt == 1)
			sprintf(valeur_variable,"%.1f     ",valdef_f);
 	        if(decpt == 2)
       		        sprintf(valeur_variable,"%.2f     ",valdef_f);
	        if(decpt == 3)
        	        sprintf(valeur_variable,"%.3f     ",valdef_f);
      		if(decpt == 4)
                	sprintf(valeur_variable,"%.4f     ",valdef_f);
     		if(decpt == 5)
                	sprintf(valeur_variable,"%.5f     ",valdef_f);
        	if(decpt == 6)
               		sprintf(valeur_variable,"%.6f     ",valdef_f);

	}

			/* calcul en entier selon dec point 
				necessaire pour le scale */
	for(i=0;i<decpt;i++){
		min_f = min_f*10;
		max_f = max_f*10;
		valdef_f = valdef_f*10;
	}

			/* repositionnement dans var int */
	min_i = (int)min_f;
	max_i = (int)max_f;
	valdef = (int)valdef_f;

                	/* test de validite des valeurs */
        if(valdef < min_i) valdef = min_i;
        if(valdef > max_i) valdef = max_i;
	if(min_i >= max_i) {*error = 1;return;}


	n = 0;
        XtSetArg (args[n], XmNorientation, XmHORIZONTAL);  n++;
	SCstruct->min = min_i;
	SCstruct->max = max_i;
        XtSetArg (args[n], XmNminimum, min_i);  n++;
        XtSetArg (args[n], XmNmaximum, max_i); n++;

			/* calcul de la largeur du scale*/
	val_w = (max_i - min_i)+40; /* largeur curseur = 40 pixels*/
	if(val_w < 90) val_w = 90;
	if(val_w > 240) val_w = 240;

	XtSetArg (args[n], XmNscaleWidth, val_w); n++;
	XtSetArg (args[n], XmNdecimalPoints, decpt); n++;
        if(str_form->nor_var == 0){
         	XtSetArg (args[n], XmNvalue, valdef);  n++;
		SCstruct->cur_val = valdef;
        }


        scale = XmCreateScale(frame0, "cursor", args, n);
	SCstruct->scale = scale;
        XtAddCallback (scale, XmNvalueChangedCallback, update_scaleCB, SCstruct);
	XtAddCallback (scale, XmNdragCallback,change_scaleCB,SCstruct);
        XtManageChild (scale);

	

		/* fleche de droite */

        n = 0;
        XtSetArg (args[n], XmNrightAttachment, XmATTACH_FORM);  n++;
        XtSetArg (args[n], XmNleftWidget, scale);  n++;
        XtSetArg (args[n], XmNleftAttachment, XmATTACH_WIDGET);  n++;
        XtSetArg(args[n], XmNarrowDirection,XmARROW_RIGHT);n++;
        arrow_r = XmCreateArrowButton(frame0,"Right",args, n);
        XtAddCallback (arrow_r, XmNactivateCallback, arrow_scale_rCB,SCstruct);
        XtManageChild(arrow_r);

                        /* affichage de la valeur courante */

        n = 0;
        XtSetArg (args[n], XmNleftWidget, arrow_r);  n++;
        XtSetArg (args[n], XmNleftAttachment, XmATTACH_WIDGET);  n++;
	XtSetArg (args[n], XmNrecomputeSize, FALSE);n++;
        label2 = XmCreateLabel(frame0,valeur_variable, args, n);
        XtManageChild (label2);
        SCstruct->label = label2;


}



	/******************CALLBACK*******************/

/*-------------------------------------------------------------
**	MultiSelectCB	
*/
void  MultiSelectCB(w,str_var,call_data)
    Widget w;
    MultiEnumStruct *str_var;
    XmListCallbackStruct *call_data;
{

	int i;
	char *val;
	char variable[500];

	strcpy(variable,"");
        for (i = 0; i < call_data->selected_item_count; i++)
        {
            XmStringGetLtoR(call_data->selected_items[i],XmSTRING_DEFAULT_CHARSET,&val);
/*
            if((strlen(valeur_variable) + strlen(list_def[i])) >= 500)
                        {printf(" To many items in Multiemun variable\n");break;}
*/
            strcat(variable,val);
	    if(i != call_data->selected_item_count-1) strcat(variable,",");
        }

        if(str_var->nor_var == 0)
                alloue_variable(str_var->variable,variable,str_var->context);
        else    /* cas ou on utilise pas les var gifa */
                strcpy(str_var->variable,variable);
}




/*-------------------------------------------------------------
**      CloseCB_widg                    - callback for close wiget
*/
void CloseCB_widg (w, str_data, call_data)
Widget          w;              /*  widget id           */
FORM_data       *str_data;
XtPointer       call_data;      /*  data from widget class  */
{

	int i,n;

	XtDestroyWidget(str_data->mere_form);
	if(str_data->bloquant == 0){
		CONTCLEAR(str_data->mere_form);
	}
	else{
	
                /* permet de sortir de la boucle locale */
        	str_data->bloquant = 0 ;
                /* pour savoir si on sort par ok ou cancel */
        	sortie_boucle_ok = -1;
	}
	                /* destockage du formulaire */

        for(i=0;i<nb_form;i++){
                if(id_form[i] == str_data->mere_form){
                        id_form[i] = 0;
			id_struct[i] = 0;
                        for(;i<nb_form-1;i++){
                                id_form[i] = id_form[i+1];
				id_struct[i] = id_struct[i+1];
                        }
                nb_form --;
		del_shell_on_display(str_data->mere_form);
                break;
                }       
        }

		/* desallocation des struct */
	free_struct(str_data);
}

	/*****************************************************/

void ALIGN_FIELD_FORM(str_pt,error)
int *str_pt;
int *error;
/*-------------------------------------------------------------
*       void ALIGN_FIELD_FORM(str_w_parent,error)
*
*       IN: str_w_parent
*       INOUT: error;
*
*	permet d'aligner n objets dans un meme champs
*
*/
{

    FORM_data *str_w_parent;
    int n;

    str_w_parent = (FORM_data *)*str_pt;
    str_w_parent->flag_prev_field = 1;

}



        /******************SEPARATEUR************************/


void MAKE_SEPARATEUR_FORM(str_pt,error)
/*
Widget *w_parent;
*/
int *str_pt;
int *error;
/*-------------------------------------------------------------
*       void MAKE_SEPARATEUR_FORM(str_w_parent,error)
*
*       IN: str_w_parent
*       INOUT: error;
*
*       Creation d'un separateur entre les champ du formulaire
*       var:
*               Widget *w_parent; pt sur id du cadre general du formulaire
*               Form_data *str_w_parent; pt sur struct form_data general
*               int *error; si pb -> error != 0
*
*/
{
    Widget wid_sep1,wid_sep2,wid_sep3,wid_sep4,wid_sep5,wid_sep6;
    Arg args[10];
    FORM_data *str_w_parent;
    int n;

                        /* mise en place d'un separateur */

    str_w_parent = (FORM_data *)*str_pt;

    n = 0;
    XtSetArg (args[n], XmNleftAttachment, XmATTACH_FORM);  n++;
    XtSetArg (args[n], XmNrightAttachment, XmATTACH_FORM);  n++;
    XtSetArg(args[n],XmNseparatorType,XmNO_LINE);n++;
    wid_sep1 = XmCreateSeparator(str_w_parent->menu_bar_row, "separator", args, n);
    XtManageChild (wid_sep1);

    n = 0;
    XtSetArg (args[n], XmNleftAttachment, XmATTACH_FORM);  n++;
    XtSetArg (args[n], XmNrightAttachment, XmATTACH_FORM);  n++;
    XtSetArg(args[n],XmNseparatorType,XmNO_LINE);n++;
    wid_sep2 = XmCreateSeparator(str_w_parent->menu_bar_row, "separator", args, n);
    XtManageChild (wid_sep2);


    n = 0;
    XtSetArg (args[n], XmNleftAttachment, XmATTACH_FORM);  n++;
    XtSetArg (args[n], XmNrightAttachment, XmATTACH_FORM);  n++;
    XtSetArg(args[n],XmNseparatorType,XmSHADOW_ETCHED_IN);n++;
    wid_sep3 = XmCreateSeparator(str_w_parent->menu_bar_row, "separator", args, n);
    XtManageChild (wid_sep3);


    n = 0;
    XtSetArg (args[n], XmNleftAttachment, XmATTACH_FORM);  n++;
    XtSetArg (args[n], XmNrightAttachment, XmATTACH_FORM);  n++;
    XtSetArg(args[n],XmNseparatorType,XmNO_LINE);n++;
    wid_sep5 = XmCreateSeparator(str_w_parent->menu_bar_row, "separator", args, n);
    XtManageChild (wid_sep5);

    n = 0;
    XtSetArg (args[n], XmNleftAttachment, XmATTACH_FORM);  n++;
    XtSetArg (args[n], XmNrightAttachment, XmATTACH_FORM);  n++;
    XtSetArg(args[n],XmNseparatorType,XmNO_LINE);n++;
    wid_sep6 = XmCreateSeparator(str_w_parent->menu_bar_row, "separator", args, n);
    XtManageChild (wid_sep6);

}

        /******************CALLBACK*******************/

void PDvarCB(w,str_var,call_data)
Widget w;
PulldownStruct *str_var;
XtPointer call_data;
/*-------------------------------------------------------------
*       PDvarCB
*               mise a jour des variables a partir des valeurs
*               choisies dans le Pulldown
*/
{
    int l_comm_f, err, lnam, i;
    char comm[256], comm_f[256], namloc[32];

        strcpy(comm,str_var->name_but) ;

        if(str_var->nor_var == 0)
      		alloue_variable(str_var->variable_but,str_var->name_but,str_var->context);
        else    /* cas ou on utilise pas les var gifa */
                strcpy(str_var->variable_but,str_var->name_but);
}


void arrow_rightCB(w,select_box,call_data)
Widget w;
Widget select_box;
XtPointer call_data;
/*-------------------------------------------------------------
*      arrow_rightCB
*               affiche la widget de selection de fichier
*/
{
	 XtManageChild(select_box);
}

void DialogAcceptCB(w,orig_tf,call_data)
Widget w;
Widget orig_tf;
XtPointer call_data;
/*-------------------------------------------------------------
*     DialogAcceptCB
*               copie le nom du fichier choisi dans le textfield
*               appeleur et efface la widget de selection de fichier
*/
{
    	char *string ;
	char *filename;
	Widget text,select_box;
	XmTextPosition position;
	XClientMessageEvent cm;


    text = XmFileSelectionBoxGetChild(w,XmDIALOG_TEXT);
    string = XmTextGetString(text);

    if (string) {

	XmTextSetString(orig_tf,string); /* copie de la string dans le TextField d'appel */
	position = XmTextGetCursorPosition(text);/*recherche position du curseur */
	XmTextSetInsertionPosition(orig_tf,position);

    }
 
    XtUnmanageChild(w);
}


void DialogCancelCB(w,select_box,call_data)
Widget w;
Widget select_box;
XtPointer call_data;
/*-------------------------------------------------------------
*     DialogCancelCB
*               efface la widget de selection de fichier
*/
{
	XtUnmanageChild(select_box);
}


void TextFieldCB (w, str_tf, call_data)
Widget          w;              /*  widget id           */
TextFieldStruct *str_tf;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
/*-------------------------------------------------------------
**      TextFieldCB
*               mise a jour des variables a partir des valeurs
*               inserees dans le textfield
*/
{
    Arg args[10] ;
    String string ;

		/* recuperation de la chaine de char inseree dans le TextField */

    string = XmTextGetString(w);

    if (string) {
	if(str_tf->nor_var == 0)
      		alloue_variable(str_tf->variable,string,str_tf->context);
	else 	/* cas ou on utilise pas les var gifa */	
		strcpy(str_tf->variable,string);
    }
}


void change_scaleCB(w, str_sc, call_data)
Widget          w;              /*  widget id           */
ScaleStruct *str_sc;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
/*-------------------------------------------------------------
**     change_scaleCB
*/
{

XmScaleCallbackStruct * scb = (XmScaleCallbackStruct *) call_data ;
set_label_scale(str_sc,scb->value);

}


void set_label_scale(str_sc,val)
ScaleStruct *str_sc;
int val;
/*-------------------------------------------------------------
**     set_label_scale: mise a jour du label (valeur courante)
**	du scale (CURSOR)
*/
{

	Arg args;
	int n;
	XmString xms;
	char valeur[50];
        float recupf;
	int i;

        if(str_sc->decpoint == 0){
		            /* cas des entiers */
                 sprintf(valeur,"%d",val);
        }else{

        	recupf = (float)val;

                        /* recalage selon decimal point */
        	for(i=0;i<str_sc->decpoint;i++){
                	recupf = recupf/10;
        	}
                if(str_sc->decpoint == 1)
                        sprintf(valeur,"%.1f",recupf);
                if(str_sc->decpoint == 2)
                        sprintf(valeur,"%.2f",recupf);
                if(str_sc->decpoint == 3)
                        sprintf(valeur,"%.3f",recupf);
                if(str_sc->decpoint == 4)
                        sprintf(valeur,"%.4f",recupf);
                if(str_sc->decpoint == 5)
                        sprintf(valeur,"%.5f",recupf);
                if(str_sc->decpoint == 6)
                        sprintf(valeur,"%.6f",recupf);

        }

	n = 0;
        xms = XmStringCreate(valeur,XmSTRING_DEFAULT_CHARSET);
        XtSetArg(args,XmNlabelString,xms); n++;
        XtSetValues(str_sc->label,&args,1);
        XmStringFree(xms);

}



void update_scaleCB (w, str_sc, call_data)
Widget          w;              /*  widget id           */
ScaleStruct *str_sc;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
/*-------------------------------------------------------------
**      set_scaleCB
*               mise a jour des variables a partir de la valeur 
*               du curseur scale
*/
{
    int i,recup;
    float recupf;

                /* recuperation de la valeur du curseur */


    XmScaleCallbackStruct * scb = (XmScaleCallbackStruct *) call_data ;

        /*stockage valeur courante en entier*/

    str_sc->cur_val = scb->value;
    set_label_scale(str_sc,scb->value);
    alloc_scale_var(str_sc);
}




void alloc_scale_var(str_sc)
ScaleStruct *str_sc;    /*  data from application   */
/*-------------------------------------------------------------
**   alloc_scale_var
*		alloc de la variable scale (cursor)
*/
{

    String string ;
    char *chaine;
    float recupf;
    int i,recup;
    


    chaine = (char *)malloc(20);

    if(str_sc->decpoint != 0){ 
    	recupf = (float)str_sc->cur_val;

			/* recalage selon decimal point */
    	for(i=0;i<str_sc->decpoint;i++){
		recupf = recupf/10;
    	}
    	sprintf(chaine,"%f",recupf);
    }
    else{		/* cas des entiers */
	recup = str_sc->cur_val;
	sprintf(chaine,"%d",recup);
    }

    if (chaine) {
        if(str_sc->nor_var == 0)
                alloue_variable(str_sc->variable,chaine,str_sc->context);
        else   
                strcpy(str_sc->variable,chaine);
    }
}




void alloue_variable(variable,string,context)
char *variable; 
char *string;
int context;
/*-------------------------------------------------------------
*       void alloue_variable(variable,string,context)
*
*       IN: variable,string,context;
*
*       mise a jour de la variable
*       var:
*               char *variable; pt sur variable
*               char *string; valeur de la variable
*               int context; contexte de recherche
*/
{

    int l_comm_f, err, lnam, i;
    char namloc[32];
    char comm[256], comm_f[256];

 
      strcpy(comm,string) ;
      cconvert_string(namloc,variable,&lnam);
      for (i=lnam; i<32; i++) {
                namloc[i] = ' ';
        }
      cconvert_string(comm_f,comm,&l_comm_f);
      for (i=l_comm_f; i<256; i++) {
                comm_f[i] = ' ';
        }

      err = 0;
      ASSIGN(namloc,&context,comm_f,&err);
}


void recherche_var(varname,value,context)
char *varname, *value;
int context;
/*-------------------------------------------------------------
*       void recherche_var(varname,value,context)
*
*       IN:varname,context
*       INOUT: value
*
*       Recherche de la variable dans le contexte
*               si elle existe: recuperation de sa valeur
*               si elle n'existe pas: creation de la variable dans le contexte
*       var:
*               char *varname; pt sur la variable
*               char *value; pt sur la valeur de la variable
*               int context; id widget = context of variable
*/
{
  char valloc[256];
  char namloc[32];
  int lnam,lval;
  int err,i;

	cconvert_string(namloc,varname,&lnam);
	for (i=lnam ; i<32; i++) {
		namloc[i] = ' ';
	}
		/* test de l'existance du contexte !!!! a faire */

	err = 0;
        CHECKVAR(namloc,&context,&err);
        if (err == 0) {		/* on trouve, on recupere dans value */
                GETVAR(namloc,&context,valloc,&err);

                if (err == 0) {
                        lval = 256;
                        convert_string(value,valloc,lval);
			adjust_cstring(value,lval);
                }
        }
        else { 		/* on trouve pas, on alloue avec value */
	  cconvert_string(valloc,value,&lval);
	  for (i=lval; i<256;i++) {
		valloc[i] = ' ';
	  }
	  err = 0;
          ASSIGN(namloc,&context,valloc,&err);
	}
}


void default_butt_form(str_form_data)
FORM_data *str_form_data;
/***************************************************************************
*       void default_butt_form(str_form_data)
*
*       IN:str_form_data
*       INOUT:
*
*       Creation des buttons ok, cancel, apply
*
*       var:
*               FORM_data str_form_data; Structure associee au
*                                               formulaire
*/
{
	Widget          button_1;
        Widget          button_2;
	Widget          button_3;
	Widget		frame0,frame_row;
        Arg             al[10];         /*  arg list            */
        int             ac;             /*  arg count           */
	int 		error;
	char 		commande[256];


        /*      Create  default buttons of formulaire.
        */

	ac = 0;
	XtSetArg (al[ac], XmNleftAttachment, XmATTACH_FORM);  ac++;
        XtSetArg (al[ac], XmNrightAttachment, XmATTACH_FORM);  ac++;
	XtSetArg (al[ac], XmNtopWidget, str_form_data->scroll_window);  ac++;
	XtSetArg (al[ac], XmNtopAttachment, XmATTACH_WIDGET);  ac++;
	XtSetArg (al[ac], XmNbottomAttachment, XmATTACH_FORM);  ac++;
	XtSetArg (al[ac], XmNresizable, False);ac++;
	frame0 = XmCreateFrame (str_form_data->menu_bar_form, "frame", al, ac);
	str_form_data->default_button = frame0;
	XtManageChild (frame0);

	ac = 0;
 	XtSetArg(al[ac],XmNorientation,XmHORIZONTAL);ac++;
        frame_row = XmCreateRowColumn(frame0, " frame_form", al, ac);
        XtManageChild(frame_row);

        if(str_form_data->ok_button == 1){
                ac = 0;
                button_3 = XmCreatePushButton (frame_row, "   Ok   ", al, ac);
                XtAddCallback (button_3, XmNactivateCallback, OK_QuitCB, str_form_data);
                XtManageChild (button_3);
        }


	if(str_form_data->apply_button == 1){
        	ac = 0;
        	button_1 = XmCreatePushButton (frame_row, "Apply", al, ac);
        	XtAddCallback (button_1, XmNactivateCallback, ApplyCB,str_form_data);
        	XtManageChild (button_1);
	}

	if(str_form_data->cancel_button == 1){
        	ac = 0;
		if(str_form_data->bloquant == 1){
		  button_2 = XmCreatePushButton (frame_row, "Cancel", al, ac);
		} else {
		  button_2 = XmCreatePushButton (frame_row, "Close", al, ac);
		}
        	XtAddCallback (button_2, XmNactivateCallback, CancelCB, str_form_data);
        	XtManageChild (button_2);
	}

}


void boucle_event()
/*-------------------------------------------------------------
*       void boucle_event(str_form_data)
*
*       IN:str_form_data
*       INOUT:
*
*       Mise en place d'une boucle locale d'evenements
*       dans le cadre de formulaires bloquants
*
*       var:
*               FORM_data str_form_data; Structure associee au
*                                               formulaire
*/
{

        XEvent          event;
	int 		bool;


       /* gestion des evenements */

	
         for (;;)
        {
         XtAppNextEvent(app_context,&event);
         bool = XtDispatchEvent(&event);
         if (sortie_boucle_ok != 0) 
	 	{
		break;
	 	}
	}

}

void free_struct(str_form_data)
FORM_data *str_form_data;
/*-------------------------------------------------------------
*       void free_struct(str_form_data)
*
*       IN:str_form_data
*       INOUT:
*
*       desalloue les struct TextField,PullDown et form_data.
*
*       var:
*               FORM_data str_form_data; Structure associee au
*                                               formulaire
*/
{

	int i;

for(i=0;i<str_form_data->cpt_nb_tf;i++){
	XtFree((char *)str_form_data->pt_tf_struct[i]);
}

for(i=0;i<str_form_data->cpt_nb_pd;i++){
	XtFree((char *)str_form_data->pt_pd_struct[i]);
}

for(i=0;i<str_form_data->cpt_nb_sc;i++){
        XtFree((char *)str_form_data->pt_sc_struct[i]);
}

for(i=0;i<str_form_data->cpt_nb_me;i++){
        XtFree((char *)str_form_data->pt_me_struct[i]);
}

	XtFree((char *)str_form_data);

}

void OK_QuitCB (w, struct_data, call_data)
Widget          w;              /*  widget id           */
FORM_data       *struct_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
/*-------------------------------------------------------------
**      OK_QuitCB               - callback for quit end save the configuration
*/
{

        char command;
        int err,vcont,i;
        int *l_comm_f;
        char comm_f[256];
        char comm[256];

                /* on ne veut pas detruire le formulaire si on est en
                mode bloquant, sauf pour le formulaire bloquant lui meme */

if(struct_data->bloquant == 1 || flag_bloquant == 0){

        err = 0;
        if(struct_data->bloquant == 0){
                strcpy(comm,struct_data->command) ;
                SETVCONTEXT(&(struct_data->mere_form));
                pre_execute(comm);
                vcont = 20;
                SETVCONTEXT(&vcont);

        }
        /* destruction du formulaire */

        XtDestroyWidget(struct_data->mere_form);
        CONTCLEAR(struct_data->mere_form);

                /* permet de sortir de la boucle locale si elle
                        est utilisee                            */
        struct_data->bloquant = 0;
                /* pour savoir si on sort par ok ou cancel */
        sortie_boucle_ok = 1;


                /* destockage du formulaire */

        for(i=0;i<nb_form;i++){
                if(id_form[i] == struct_data->mere_form){
                        id_form[i] = 0;
			id_struct[i] = 0;
                        for(;i<nb_form-1;i++){
                                id_form[i] = id_form[i+1];
				id_struct[i] = id_struct[i+1];
                        }
                nb_form --;
		del_shell_on_display(struct_data->mere_form);
                break;
                }
        }


                /* desallocation des structures */
        free_struct(struct_data);
}
else{XBell(XtDisplay(window_mere), 100);}/* form courent en mode bloquant:
                                        on ne fait rien pour les autres*/

}

/*-------------------------------------------------------------
**      ApplyCB                    - callback for apply
*/
void ApplyCB  (w, struct_command, call_data)
Widget          w;              /*  widget id           */
FORM_data       *struct_command;
XtPointer       call_data;      /*  data from widget class  */
{

        char command;
        int err,vcont;
        int *l_comm_f,i;
        char comm_f[256];
        char comm[256];

        err = 0;
        if(struct_command->bloquant == 0){

                strcpy(comm,struct_command->command) ;
                SETVCONTEXT(&(struct_command->mere_form));
                pre_execute(comm);
                vcont = 20;
                SETVCONTEXT(&vcont);

        }

}

/*-------------------------------------------------------------
**      CancelCB
*
*/
void CancelCB (w, struct_data, call_data)
Widget          w;              /*  widget id           */
FORM_data       *struct_data;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{


        int i;

                /* on ne veut pas detruire le formulaire si on est en
                mode bloquant, sauf pour le formulaire bloquant lui meme */

if(struct_data->bloquant == 1 || flag_bloquant == 0){

        XtDestroyWidget(struct_data->mere_form);

                /*destruction du contexte associe et des variables */

        CONTCLEAR(struct_data->mere_form);

                /* permet de sortir de la boucle locale si elle
                        est utilisee                            */
                struct_data->bloquant = 0 ;
                /* pour savoir si on sort par ok ou cancel */
        sortie_boucle_ok = -1;

                /* destockage du formulaire */

        for(i=0;i<nb_form;i++){
                if(id_form[i] == struct_data->mere_form){
                        id_form[i] = 0;
			id_struct[i] = 0;
                        for(;i<nb_form-1;i++){
                                id_form[i] = id_form[i+1];
				id_struct[i] = id_struct[i+1];
                        }
                nb_form --;
		del_shell_on_display(struct_data->mere_form);
                break;
                }
        }

                /* desallocation des struct */
        free_struct(struct_data);
}
else{XBell(XtDisplay(window_mere), 100);}/* le formulaire courant est bloquant
                                        on ne fait rien pour les autres */
}


int Load_file(fname,txtf)
char *fname;
Widget txtf;
/***************************************
*	Load_file
*	Load a file fname on a text field txtf
*
*/
{
	FILE 	*infile;
	long 	fsize;
	char	*lclptr;
	int	i,nb;	

	infile = fopen(fname,"r");
	if (infile == NULL){
		return(-1);
	}

	else{
		fseek(infile,0,2);
		fsize = ftell( infile);
		rewind (infile);

		lclptr = (char *)XtMalloc (fsize + 1);
		fread( lclptr, sizeof(char), fsize, infile);
		lclptr[fsize] = '\0';

		nb = 0;
		for(i=0;i<fsize;i++){
			if(lclptr[i] == '\n') nb++;
		}

		XmTextSetString(txtf,lclptr);
		XtFree(lclptr);
		fclose(infile);
		return(nb);
	}

}

/*-------------------------------------------------------------
**      do_action                    - callback for button commandes
*/
void do_action(w,struct_command, call_data)
Widget          w;              /*  widget id           */
STR_ACT_FORM       *struct_command;
XtPointer       call_data;      /*  data from widget class  */
{

        char command;
        int err,vcont;
        int *l_comm_f,i;
        char comm_f[256];
        char comm[256];

        strcpy(comm,struct_command->but_act) ;
        SETVCONTEXT(&(struct_command->mere_form));
        pre_execute(comm);
        vcont = 20;
        SETVCONTEXT(&vcont);

}

void CLOSE_BOX(str_pt,str_lg,error)
char str_pt[256];
int *str_lg;
int *error;
{


    int i,str_int;
    FORM_data *struct_data;
    int n,present;
    char new_pt[256];


		/* recuperation de la variable et conversion en entier */

	convert_string(new_pt,str_pt,*str_lg);
	str_int = atoi(new_pt);

		/* test de l'existance du formulaire */

	present = 0;
        for(i=0;i<nb_form;i++){
                if(id_struct[i] == str_int){
		present = 1;
                break;
                }
        }
	if(present == 0){
		*error = 1;
		return;
	}


        struct_data = (FORM_data *)str_int;

                /* on ne veut pas detruire le formulaire si on est en
                mode bloquant, sauf pour le formulaire bloquant lui meme */

	if(struct_data->bloquant == 1 || flag_bloquant == 0){

       		 XtDestroyWidget(struct_data->mere_form);

                /*destruction du contexte associe et des variables */

        	CONTCLEAR(struct_data->mere_form);
                /* permet de sortir de la boucle locale si elle
                        est utilisee                            */
                struct_data->bloquant = 0 ;
                /* pour savoir si on sort par ok ou cancel */
        	sortie_boucle_ok = -1;

                /* destockage du formulaire */

        	for(i=0;i<nb_form;i++){
               	 if(id_form[i] == struct_data->mere_form){
                        id_form[i] = 0;
			id_struct[i] = 0;
                        for(;i<nb_form-1;i++){
                                id_form[i] = id_form[i+1];
				id_struct[i] = id_struct[i+1];
                        }
                 nb_form --;
		 del_shell_on_display(struct_data->mere_form);	
                 break;
                 }
        	}

                /* desallocation des struct */
        	free_struct(struct_data);
	}
	else{XBell(XtDisplay(window_mere), 100);}/* le formulaire courant est bloquant
                                        on ne fait rien pour les autres */

}


/*-------------------------------------------------------------
*	arrow_scale_lCB
*/
void arrow_scale_lCB(w,str_sc, call_data)
Widget          w;              /*  widget id           */
ScaleStruct *str_sc;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{
    Arg args[10] ;
    int n;

		/* recup valeur courante, decrement et 
		positionnement du scale */
		
    if((str_sc->cur_val-1)>=str_sc->min){
    	str_sc->cur_val--;
    	n = 0;
    	XtSetArg (args[n], XmNvalue, str_sc->cur_val);  n++;
    	XtSetValues(str_sc->scale,args,n);

	/* alloc variable */

    	set_label_scale(str_sc,str_sc->cur_val);
    	alloc_scale_var(str_sc);
    }


}

/*-------------------------------------------------------------
*       arrow_scale_rCB
*/
void arrow_scale_rCB(w,str_sc, call_data)
Widget          w;              /*  widget id           */
ScaleStruct *str_sc;    /*  data from application   */
XtPointer       call_data;      /*  data from widget class  */
{
    Arg args[10] ;
    int n;


               /* recup valeur courante, increment,
                positionnement du scale et alloc de
		la variable*/

    if((str_sc->cur_val+1)<=str_sc->max){
    	str_sc->cur_val++;
    	n = 0;
    	XtSetArg (args[n], XmNvalue, str_sc->cur_val);  n++;
    	XtSetValues(str_sc->scale,args,n);

		/* alloc variable */

 	set_label_scale(str_sc,str_sc->cur_val);
    	alloc_scale_var(str_sc);
    }

}
