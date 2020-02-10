;Set-up the minimum GUI
; 
; see also : button.g startup.g BUTTONBOX


if ($LICENCE s= "NON-VALID") then
  buttonbox 'NON-VALID Licence' \
	'There is no valid Licence' gm/licence.g \
	'For this version of Gifa' gm/licence.g \
	'Please Register!' gm/licence.g *
endif

buttonbox About \
   'About GIFA' gm/config \
   separator \
   'Help' 'help' \
   'Full on-line Documentation...' 'print ("starting";$viewer//"...") sh ($viewer;"/usr/local/gifa/doc/gifa.html &") ' \
   'Gifa home page on the WEB...' 'print ("starting";$viewer//"...") sh ($viewer;"http://www.cbs.univ-montp1.fr/GIFA/base.html &")' \
   'History file' 'formbox History DO_NOTHING /usr/local/gifa/History text *' \
   separator \
   'Switch to Simplified mode' 'closebutton env_basic.g' \
   separator \
   'Quit GIFA' 'formbox Ok? "exit $ok" " " message "-  Ok to Quit Gifa ?  -" message " " message "Keep log file?" enum "no,yes" ok "no" *' \
   *

buttonbox File \
   Read... "dialogbox Read 'Enter file name' message ' '  file _f $name * read $_f unset _f zm 1" \ 
   Write... "dialogbox Write 'Enter file name' message ' '  file _f $name * Write $_f unset _f" \ 
   separator \
   'Read all format' gm/readf \
   'Write all format' gm/writef \
   'Bring from UXNMR' getuxnmr \
   'Read from VNMR' varian_read \
   separator \
   'Add file to current' gm/addf \
   separator \
   'File name' 'print ("current file is : "//$name)' \
   'Size of data' size \
   'Data param' param \
   'More details' list \
   *

buttonbox Mode \
   'Switch to Varian default' Varian \
   separator \
   'Proc 1D' 'closebutton disp2d 0 cdisp2d 0 disp1d 1 dim 1 env_base.g env_proc1d.g' \
   'Proc 2D' 'closebutton disp2d 1 cdisp2d 0 disp1d 1 dim 2 env_base.g env_proc1d.g env_proc2d.g' \
   'Proc 3D' 'closebutton disp2d 1 cdisp2d 0 disp1d 1 dim 3 env_base.g env_proc1d.g env_proc2d.g env_proc3d.g' \
   Assignment env_att.g \
   separator \
   '... Advanced proc' env_advp.g \
   '... MaxEnt' env_maxent.g \
   '... Linear Prediction' env_lin_pred.g \
   '... Dosy processing' env_dosy.g \
   separator \
   '... Plot' env_plot.g \
   '... Peak' env_peak.g \
   '... Unix' env_unix.g \
   separator \
   'Pulldown menus' 'closebutton pulldownmenu 1 button.g' \
   'Static Box menus' 'closebutton pulldownmenu 0 button.g' \
   *

buttonbox Display \
   'Disp1D on' 'disp1d 1' \
   'Disp2D on' 'disp2d 1' \
   'CDisp2D on' 'cdisp2d 1' \
   separator \
   'Zm (Zoom Control)' 'zm 1' \
   'Display Control' dispcont \
   separator \
   Freeze 'freeze' \
   Rzoom rzoom \
   'Store zoom' gm/store_zoom \
   'Back to stored zoom' gm/back_store_zoom \
   multi_zoom multi_zoom \
   separator \
   Super1d super1d \
   Super2D super2d \
   *
buttonbox 'Move' \
   'Dim 1' 'dim 1' \ 
   'Dim 2' 'dim 2' \ 
   'Dim 3' 'dim 3' \
   separator \
   'Put Data' 'Put Data' \ 
   'Get Data' 'get data' \
   'AddData' 'adddata' \
   separator \
   'Point' point \
   'Unit...' choose_unit \
   'Calib...' calib \
   separator \
   'Select Row' rowint \
   'Select Col' colint \
   'Row... ' row \
   'Col... ' col \
   *



