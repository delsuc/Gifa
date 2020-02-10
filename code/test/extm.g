; Test for peak extraction by matching bacward and forward
; roots
dim 1
unit h 
order 10
simu 1000 512 2 1.0 200 1.0 0. \
1.0 600 1.0 0. 0.0
dt->svd 512
svd->ar 1
ar->rt 1
svd->ar 2
ar->rt 2
rtmatch 3
rtlist 3 %%
rt->pk 512 2 0.
pklist %%
quit
;
;Number of Root          3
;matched roots of PE polynome
;        .308708
;        .950106
;       -.808208
;       -.587198
;        .990458
;       -.161216
;
;Peaks computed with linear prediction
;Number of 1D peaks :          3
;Peak coordinates in HZ       widthes in Hz
;   Index   Coord          Ampli         Volume       Width     Phase
;     1   200.391    6.13592       1.000000          .318      .000
;     2   601.174    6.13592        1.00000          .318      .000
;     3   976.226      .000
;       Linked to peak #     0









