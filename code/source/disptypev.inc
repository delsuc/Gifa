C file disptypev.inc
C holds the variables for the display
C
	real     stdy, stshift, stskew,loga,scale
	integer  disp1d,disp2d,cdisp2d,stskip,level,sign,freeze_cpt,
     *           color,clear,
     *           scolor,ccolor,bcol,
     *           pdmenu,nbrowtitle,axes
C this is sloppy !, the folowwing 32 should be extracted from "variable.inc"
        character*32 view_name

	common /display/ view_name,disp1d, disp2d, cdisp2d,stdy,
     *           stshift, stskew, loga,scale,
     *           stskip,level,sign,freeze_cpt,color,clear,bcol,
     *           scolor,ccolor,pdmenu,nbrowtitle,axes

C color definitions from within GIFA
#include "gcolor.inc"


