; macro executing the 3D processing presented in Table 1
dim 3   ; prepare memory
itype 0
chsize 256 64 512   ; set-up sizes
sin 0 F123          ; apodisation in F3 (acquisition dimension)
chsize % % (%*2)    ; zerofilling
rft f3              ; real FT
phase 90 90 F3      ; phase correction
real F3             ; throw away imaginary part
chsize % (%*2) %    ; redo for F2
rft f2
phase 90 90 F2
real F2
chsize (%*2) % %    ; redo for F1
rft f1
phase 90 90 F1
real F1
bcorr 1 2 F3 5 500 0  ; linear baseline correction using pivot points 5 and 500


