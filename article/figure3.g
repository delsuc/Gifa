; first close all windows
dialogbox figure_3 "please close the 'gifa' bar, then close this dialog" message *

; set-up the old fashion user environment
pulldownmenu 0
button.g

; read and process the data
; this part was created from 'Easy2d' results
refmacro 0            ; no display during macro
read Gifa/data/ARTE_ref_20.gf2    ; load the data
correct1st                        ; compensate first FID
sin 0 F2                          ; apodize in F2
chsize % 8k                       ; zerofill in F2
ft_sim                            ; and FT
bruker_corr 10 0 32               ; correct for DSP offset
sin 0 F1                          ; apodize in F1
chsize 1024 %                     ; zerofill in F1
ft_sh_tppi                        ; FT in F1 - States-Haberkorn - TPPI scheme
phase 0 180 F1                    ; and phase corrections in F1 and F2
phase 290.399810791 700 F2

; now display
refmacro 1     ; with this command, the result of each command line is displayed

; the 2D spectrum - tune levels, unit in ppm, and zoom window
cdisp2d 1 scale 15 loga 1.4 unit p zoom 1 3.5p 3.5p 0.8p 0.8p  

; the 1D spectrum extracted at 1.0 ppm 
disp1d 1 row 1p dim 1 zoom 1 1.3p 0.9p   ; the 1D spectrum

print " " print " " print " "
print ('current file is :';$name)

size

