; listout.g $dbno $out_file
;
; List the integration of assignment entry $dbno to the file $out_file
;

set dbno = $_
set out_file = $_

set l = $att[$dbno]
set tmp1 = (tail(tail(tail(tail(tail($l))))))
set t = (tail(tail(tail($l))))
compute_label $dbno
set label = $returned
fprint $out_file ("Peak" ; $dbno ; $label ; (head($l))//","//(head(tail($l)))//" ppm" ; $tmp1)

for p = 1 to $nbexp
  fprint $out_file ("     "; $paramx[$p] ; "    " ; $integ[$p] ; "+/-" ; $error[$p])
endfor

