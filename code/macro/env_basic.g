;Set-up the simplified GUI
; 
; see also : button.g startup.g BUTTONBOX


if ($LICENCE s= "NON-VALID") then
  buttonbox 'NON-VALID Licence' \
	'There is no valid Licence' gm/licence.g \
	'For this version of Gifa' gm/licence.g \
	'Please Register!' gm/licence.g *
endif

buttonbox About \
   'About GIFA' config \
   separator \
   'Help' 'help' \
   'Full on-line Documentation...' 'print ("starting";$viewer//"...") sh ($viewer;"/usr/local/gifa/doc/gifa.html &") ' \
   'Gifa home page on the WEB...' 'print ("starting";$viewer//"...") sh ($viewer;"http://www.cbs.univ-montp1.fr/GIFA/base.html &")' \
   separator \
   'Switch to Complete Mode' 'closebutton disp2d 1 cdisp2d 0 disp1d 1 dim 2 env_base.g env_proc1d.g env_proc2d.g' \
   separator \
   'Quit GIFA' 'formbox Ok? "exit $ok" " " message "-  Ok to Quit Gifa ?  -" message " " message "Keep log file?" enum "no,yes" ok "no" *' \
   *

buttonbox File \
   Read... "dialogbox Read 'Enter file name' message ' '  file _f $name * read $_f unset _f zm 1" \ 
   Write... "dialogbox Write 'Enter file name' message ' '  file _f $name * Write $_f unset _f" \ 
   separator \
   'Read all format' gm/readf \
   'Write all format' gm/writef \
   'Import from XWinNMR/TopSpin' import_bruker.g \
   'Read from VNMR' varian_read \
    separator \
    'Bring from Bruker Spectrometer' getuxnmr \
    separator \
   'File name' 'print ("current file is : "//$name)' \
   'Acq param' param \
   *

buttonbox Display \
   'Disp1D on' 'disp1d 1' \
   'Disp2D on' 'disp2d 1' \
   'CDisp2D on' 'cdisp2d 1' \
   separator \
   'Display Control' dispcont \
   separator \
   Super1d super1d \
   Super2D super2d \
   separator \
   'Plot' easyplot \
   *

buttonbox 'Move' \
   'Unit...' choose_unit \
   'Calib...' calib \
   separator \
   'Select Row' rowint \
   'Select Col' colint \
   *
   
buttonbox 'Process' \
   'Easy 1D' 'dim 1 easy1d $name no expbroad 1 sim on ($si1_1d+0) % 0 0  %' \
   'Integrate' integrate \
   separator \
   'Easy 2D' 'dim 2 easy2d $name yes no sin 0 sin 0 0 0 sh ($si1_2d+0) no sim on ($si2_2d+0) % 0 0 0 0 %%' \
   'Proj F1' 'proj f1 $_' \
   'Proj F2' 'proj f2 $_' \
   *



