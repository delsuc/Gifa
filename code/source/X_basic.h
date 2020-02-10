/*
Basic structure definitions, used in X_window stuff.
Mostly by X_window and X_zoom
*/
typedef struct {
        unsigned largeur, hauteur;
        int x_debut, y_debut,x_fin, y_fin;
	GC gc;
	int cadre_ON;
	int x_orig, y_orig;
	int flag_passage_debut_zoom;
} Zoom_cadre;


typedef struct {
        float x,y;
        GC gc;
        int lines_ON;
        float z1,z2,z3,z4;   /* these are the zoom coord used by OPEN_VIEW windows */
} viseur_att;

