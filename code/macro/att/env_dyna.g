; set-up the assignment module
; 
; bound to the 'Assignment' button in the Mode menu
;

buttonbox Dynamic \
   "Data-set list" add_list.g \
   separator \
   "Integ/process one peak" integ_att.g \
   "Show last processing" 'showfit1pk.g $data_typ' \
   separator \
   "Integrate in zoom region" quantalllst.g \
   "Process from file" procalllst.g \
   "Do Monte-Carlo from file" procMC.g \
   *
setpath ('/usr/local/gifa/macro/att/dyna';$gifapath)

set listfile := 'dataset_list'


