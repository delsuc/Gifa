; Test for peak extraction using backward linear prediction coeff 
; calculated by LPSVD
dim 1
unit h 
order 10
simu 1000 512 2 1.0 200 1.0 0. \
1.0 600 1.0 0. 0.0
dt->svd 512
svd->ar 2
ar->rt 2
rtclean 2
rtlist 2 %%
rtinv 2
rtlist 1 %%
rt->pk 512 1 0.
pklist %%
quit
;
;Number of Root          2
;backward roots of PE polynome
;       -.809826
;        .588373
;        .309326
;       -.952008
;
;Number of Root          2
;forward roots of PE polynome
;       -.808208
;       -.587198
;        .308708
;        .950106
;
;Peaks computed with linear prediction
;Number of 1D peaks :          2
;Peak coordinates in HZ       widthes in Hz
;   Index   Coord          Ampli         Volume       Width     Phase
;     1   601.174    6.13590        1.00000          .318      .000
;     2   200.391    6.13593       1.000000          .318      .000








