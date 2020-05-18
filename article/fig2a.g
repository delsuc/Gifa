; this text reproduces Figure 2a from the original paper

; This macro creates a form box which permits to modify the
; parameters of the contour-mode display window.
; The user can - open / close the contour-mode display window
;              - choose to display positive / negative / both sides
;              - change the number and spacing of contour levels.
;Preset parameters:
if ($CDISP2D==1) then     ;$cdisp2d describes graphic states.
    set cdef = on
else
    set cdef = off
endif
if ($SIGN==1) then        ;Describes which levels are displayed
    set pndef = positive  ; (positive, negative, both)
elsif ($SIGN == 0) then
    set pndef = both
else
    set pndef = negative
endif
;Build the form
formbox "Display control" \
    "dispcont_doit $cont $posneg $lev $log" \   ;Callback
    "Contour Display (cdisp2d)" \
        enum "on,off" cont $cdef \             ;Define fields
    "Display mode (sign)" \
        enum "positive,negative,both" posneg $pndef \
    "Number of levels (level)" int lev $LEVEL \
    "Level Algo : (loga)" real log $LOGA \
    "(x=1 : equidistant; x>l : by power of x)" message \
    *            ; * is end of list
exit

