; used as a default startup macro
;
; this macro is called whenever Gifa is started.
; However if another startup.g file is available earlier in the GIFAPATH
; (typically in . or in $HOME/macro) then this other one will be used instead
;
; load the default GUI and preset some default values
;
; see also : button.g
;
;
; you can adapt this file to your local set-up, either by :
;      copying it over to your $HOME/macro directory, and modifying it
; or   modifying it directly to adapt to your general set-up
;
; however, if you choose the second solution, remember to save it before
; upgrading Gifa to a new version, because this file :
; /usr/local/gifa/macro/startup.g 
; will be overwritten


; first check licence
if ($LICENCE s! "NON-VALID") then
	sh 'cat /usr/local/gifa/help/welcome.txt'
endif

; then check version
set macro_version = 4.3
if (head(tail($version)) < $macro_version ) then
  alert "WARNING : The version number of this binary is older than the current set of macro, and may be incompatible"
endif

; set up default plot size
cx 15 cy 15

; load environment
if $config_graph then

   set viewer := "netscape"    ; Adapt this one to your set-up

   pulldownmenu 1    ; choose menus instead of boxes
   button.g
; env_basic.g   ; uncomment this one and comment out button.g 
                ; if you want to get simplified menus as default

  disp1d 1  cdisp2d 1 ; open graphic windows

  zm 1               ; open zoom box
else
   print "No graphic environment"
endif

exit

