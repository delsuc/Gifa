; set-up the assignment module
; 
; bound to the 'Assignment' button in the Mode menu
;

set att_version := 1.4
; first check if sh is available
set tmp = ('gifa' // int(100000*$random))
onerrorgoto notwritable
open $tmp       ; create the file
fprint $tmp 'coucou'
close $tmp

sh ('pwd >'; $tmp)     ; change its content
open $tmp
set l = <$tmp
close $tmp
onerrorgoto " "

if ($l s= 'coucou') then    ; if it did not change -> sh does not workf
   dialogbox 'Error' \
      'The sh command does not work !' message \
      'The assignment module cannot work without it' message \
      separator \
      'This is probably due to a shortage in central memory' message \
      'Try - using a smaller version of Gifa' message \
      '    - quitting some unneeded programs running on this station' message \
      '    - buying some more MegaBytes' message \
      *
      error 'SH does not work'
endif
sh ('/bin/rm'; $tmp)

if (index($gifapath,"att")==0) setpath ("/usr/local/gifa/macro/att" ; $gifapath)

disp2d 0 
cdisp2d 1
disp1d 0
unit p

closebutton
env_base.g
env_peak.g

buttonbox project \
   "Help (Short)" help_assign \
   separator \
   "New project" "new_proj" \
   "Select project" select_proj \
   "Change param." ch_param \
   "Save data-bases" save_db \
   "Make backup" make_backup \
   separator \
   "File selector" file_selector \
   "Add spectra" add_spec \
   "Add PDB file" add_pdb \
   'Stat on Primary seq' stat_primary \
   separator \
   'Copy pk to peak db' pk->att \
   'Rem unassigned peaks' rem_unass_att \
   'Copy the db from another data-set ' copy_att \
   'Merge the peak table into the db' merge_pkatt \
   separator \
   'Import db from BMRB file' bmrb2spin \
   'Import db from ascii file' ascii2spin \
   'Create peak db from current spin system definitions' build_att \
   separator \
   '... Integration module' env_integ.g \
   '... Relaxation module' env_dyna.g \
   'Quit assignment'  quit_assign \
   *
buttonbox data-bases \
   'Show database' show_curr \
   'Show all peaks' show_all_att \
   'Show unassigned peaks' show_unass \
   'Show selected peaks' grep_peak \
   separator \
   'Find a peak' find_att \
   'Edit a peak' edit_att \
   'Create new peak' add_att \
   separator \
   'Find a spin' find_spin \
   'Edit a spin' edit_spin \
   'Create new spin' add_spin \
   'List all spins' list_all_spin \
   separator \
   'Find a syst' find_sys \
   'Edit a syst' edit_sys \
   'List all systs' list_all_sys \
   'Stat on systs' stat_sys \
   separator \
   'Search spins' search_spin \
   separator \
   'Draw noesy walk' draw_noesy_walk \
   'List Primary seq' show_prim_seq \
   *
buttonbox 'graph-tools' \
	'Choose base color' choose_scolor \
	Point point \
	Marker 'point marker' \
        separator \
	"Reset build-list" "set build_list := ' '" \
	"Print build-list" "print $build_list" \
	"Show build-list" 'show_build_list $build_list'\
	separator \
	"Add peak to list" "find_att if ($returned != 0) set build_list := ($returned; $build_list)" \
	"Edit build list" edit_build_list \
	*

buttonbox utilities \
   'Check data bases (kind of slow)' dbck \
   'Check topology of spin systs' topock \
   'Low level Editor' low_level \
   'Find distances' find_dist \
   separator \
   'Peak Listing' list_att \
   'Spin Listing' list_spin \
   'System Listing' list_sys \
   'Assignment Listing' list_assign \
   'Noesy Stat. and CSI (Real Slow)' stat_att_noe \
   'Write to NMRStar File' write_nmrstar \
   separator \
   'Plot database' 'dialogbox "plot database" "Plot file name" file f Gifa_Temp.Plot * plot_curr $f' \
   'Plot one label' 'dialogbox "plot one label" "Plot file name" file f Gifa_Temp.Plot * plot_label $f' \
   separator \
   'Recalibrate current Experiment' recalibrate \
   'Build strip file from 3D HSQC' build_strip \
   *

; build a list of peaks
set build_list := ' '

; last clicked datum
set last_click := ' '

print ("Assignment module Version :"; $att_version)

dialogbox "Choose action" \
   "Choose one action:" enum 'New project,Select project' act 'Select project' \
   *
dim 2 zero   ; erase before entering
if ($act s= 'Select project') then
  select_proj
else
  new_proj
endif

exit

=notwritable
print "It seems that you cannot write in this directory"
print "Assignment cannot start"
error "Assignment cannot start"

