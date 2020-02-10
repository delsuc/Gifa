;set up a menu for 1D processing
; 
; see also : button.g startup.g BUTTONBOX


buttonbox 'Apod.' \
   'Em...' 'em' \
   'Gm...' 'gm' \
   'Sin...' 'sin' \
   'SqSin...' 'sqsin' \
   'Tm...' tm \
   separator \
   'Gaussian Interactive' gm_inter \
   'User defined...' user_apod \
   separator \
   'Correct 1st point' correct1st \
   separator \
   'ChSize...' 'chsize' \
   *
buttonbox 'Proc 1D' \
   'Easy 1D' 'dim 1 easy1d $name expbroad 1 sim (2*power2($si1_1d-1)) % 0 0  %' \
   separator \
   'ZeroFill'   'gm/test1d chsize (power2(%)*2)' \
   'Ft_Seq' 'ft_seq' \
   'Ft_Sim' 'ft_sim' \
   separator \
   'Ph' ph \
   'Auto. Phasing' 'gm/test1d apsl' \
   'Redo Phase' 'gm/test1d Phase %%' \
   'Real' 'gm/test1d real' \
   'Modulus' 'gm/test1d modulus' \
   'Integrate' 'integrate' \
   *



