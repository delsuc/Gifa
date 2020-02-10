;set up a menu for 2D processing
; 
; see also : button.g startup.g BUTTONBOX


buttonbox 'Proc 2D' \
   'Easy 2D' 'easy2d $name sin 0 yes sin 0 no 0 0 sh (2*power2($si1_2d-1)) no sim (2*power2($si2_2d-1)) % 0 0 0 0 %%' \
   separator \
   'ZeroFill F1'   'gm/test2d chsize (power2(%)*2) %' \
   'ZeroFill F2'   'gm/test2d chsize % (power2(%)*2)' \
   Burg2d burg2d \
   Svd2d svd2d \
   separator \
   'Ft_Seq (F2)' 'ft_seq' \
   'Ft_Sim (F2)' 'ft_sim' \
   'Ft_phase_modu (F1)' 'ft_phase_modu' \
   'Ft_tppi (F1)' 'ft_tppi' \
   'Ft_sh (F1)' 'ft_sh' \
   'Ft_sh_tppi (F1)' ft_sh_tppi \
   'Ft_n+p (F1)' ft_n+p \
   separator \
   'Ph in f1 (ph2dc)' ph2dc \
   'Ph in f2 (ph2dr)' ph2dr \
   'Redo Phase F1' 'gm/test2d Phase % % f1' \
   'Redo Phase F2' 'gm/test2d Phase % % f2' \
   'Real F12' 'gm/test2d real F12' \
   'Modulus' 'gm/test2d modulus' \
   separator \
   'Proj F1' 'proj f1 $_' \
   'Proj F2' 'proj f2 $_' \
   'Proj_loc' proj_loc \
   separator \
   '...2D on file' env_2donfile.g \
   *


