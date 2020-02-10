; set up a menu for Linear Prediction
;
; 
; see also : button.g startup.g BUTTONBOX


buttonbox 'Linear Pred.' \
   'Order...' 'order' \
   separator \
   'Make FID Cplx' 'iftbis ft' \
   separator \
   'Burg extension' 'dt->ar   ar->dt $_ 1'  \
   'SVD extension' 'dt->svd %   svd->ar 1   ar->dt $_ 1' \
   'Stable_SVD extension' 'dt->svd %   svd->ar 1   ar->rt 1   rtreflect 1   rt->ar 1   ar->dt $_ 1' \
   separator \
   'Burg 1st points' burg_rev \
   'SVD 1st points' svd_rev \
   separator \
   'Burg spectrum (mem1)' 'dt->ar   chsize $_ ar->sp' \
   'SVD analysis' 'dt->svd %   svd->ar 2   ar->rt 2   rtclean 2   rtinv 2   rt->pk $si1_1d 1 0 pklist %% ' \
   'FAB-SVD analysis' 'dt->svd %   svd->ar 3   ar->rt 3   rtmatch $_   rt->pk $si1_1d 2 0  pklist %%' \
    separator \
   'Cadzow proc.' cadzow \
    separator \
   PKlist 'pklist %%' \
   *
