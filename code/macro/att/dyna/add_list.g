; add_list
;
; This formbox permits to manage a data-set list 
;
; This dataset list is a file which contains the name of all the spectra to
; be handled by the relaxation module.
; It should be located in the 'processing' subdirectory of the project
;
; the syntax of the file is the following
; one line per relaxation delay, first the delay value, then the experiment name.
; more than one experiment for a relaxation delay is possible
; # comments are possible
;
; e.g. :
; #dummy T1 list
; 0.1  T1_1
; 0.2  T1_2
; 0.3  T1_3_1 T1_3_2 T1_3_2
; 0.5  T1_5
;
; this one describes 6 experiments, for 4 delays.
;
; having several experiments for one delay pemits to estimate
; experiemnt to experiment noise.
;
; NOE lists have no delays of course, 
;  they are supposed to have only 2 lines,
;  one with the i'with' keyword, 
;  another one with the 'without' keyword signifying with or without presaturation

formbox 'List of data-sets' \
  DO_NOTHING \
  'File : ' string listfile 'R1_list' \
  'Edit (vi)'  action 'vi ("processing/"//$listfile)' noreturn \
  'Show (more)'  action 'more ("processing/"//$listfile)' noreturn \
  'Fill (ls spectra/*)'  action 'sh ("ls spectra > processing/" //$listfile)' noreturn \
  'Remove (rm)'  action 'sh ("/bin/rm processing/" //$listfile)' noreturn \
  'Help' action 'help add_list.g' \
  *

