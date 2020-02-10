;set up a menu for 3D processing
; 
; see also : button.g startup.g BUTTONBOX


if (index($gifapath,"3d")==0) setpath ("/usr/local/gifa/macro/3d" ; $gifapath)

buttonbox 'Proc 3D' \
   'Largest Set'   largest_3d \
   separator \
   'ZeroFill F1'   'gm/test3d chsize (power2(%)*2) % %' \
   'ZeroFill F2'   'gm/test3d chsize % (power2(%)*2) %' \
   'ZeroFill F3'   'gm/test3d chsize % % (power2(%)*2)' \
   Burg3d burg3d \
   separator \
   'ft_Seq (F3)' 'ft_seq' \
   'ft_Sim (F3)' 'ft_sim' \
   'ft_tppi (F2)' 'ft_tppi f2' \
   'ft_sh (F2)' 'ft_sh f2' \
   'ft_sh_tppi (F2)' 'ft_sh_tppi f2' \
   'ft_tppi (F1)' 'ft_tppi f1' \
   'ft_sh (F1)' 'ft_sh f1' \
   'ft_sh_tppi (F1)' 'ft_sh_tppi f1' \
   separator \
   'Phase' phase \
   'Real' 'gm/test3d real $_' \
   'Real F123' 'gm/test3d real F123' \
   'Modulus' 'gm/test3d modulus' \
   *

buttonbox 'Display 3D' \
  'disp3d on' 'disp3d 1' \
  'parameters...' disp3d_form \
  'control box' 'check3d 1' \
  refresh ref3d \
  separator \
  'choose zoom...' zoom3di \
  separator \
  plane... 'gm/test3d plane' \
  diag... 'gm/test3d diag' \
  separator \
  vertint vertint \
  planeint planeint \
  separator \
  'Proj F1 ...' 'proj f1 $_' \
  'Proj F2 ...' 'proj f2 $_' \
  'Proj F3 ...' 'proj f3 $_' \
  *
  
buttonbox '3D on file' \
   Easy3d 'easy3d $name "F1 (plane F2xF3)" plane_F1.g "F2 (plane F1xF3)" plane_F2.g ($name // ".out")' \
   separator \
   'Join data-set...' "dialogbox Join 'Enter file name' message ' '  file _f $c_name * join $_f unset _f dataset" \ 
   'Dataset parameters' dataset \
   'list all files' listfilec \
   disjoin disjoin \
   separator \
   'get plane'  get_plane \
   'get region' 'dim 3 getc $_ $_ $_ $_ $_ $_' \
   'get vertical' vertintf \
   separator \
   '3D proc...' proc3d_form \
   separator \
   'Proj F1 ...' 'proj3d f1' \
   'Proj F2 ...' 'proj3d f2' \
   'Proj F3 ...' 'proj3d f3' \
   'Compute all Proj...' proj3d_all \
   separator \
   'strip_file...' strip_file \
   'strip_plot...' strip_plot \
   *

;   'Easy 3D' 'easy3d $c_name yes sin 0 sin 0 sin 0 $c_sizef1 $c_sizef1 $c_sizef1 % % % 0 0 0 0 %%' \



