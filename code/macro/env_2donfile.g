;set up a menu for 2D processing on file
; 
; see also : button.g startup.g BUTTONBOX


buttonbox '2D on file' \
   'Join data-set...' "dialogbox Join 'Enter file name' message ' '  file _f $c_name * join $_f unset _f dataset" \ 
   'Dataset parameters' dataset \
   'list all files' listfilec \
   disjoin disjoin \
   separator \
   'get 1D'  get_1d \
   'get region' 'dim 2 getc $_ $_ $_ $_ ' \
   separator \
   '2D proc...' proc2d_form \
   separator \
   'Proj F1 ...' 'proj2d f1' \
   'Proj F2 ...' 'proj2d f2' \
   *




