;set up a menu for UNIX tools
; 
; see also : button.g startup.g BUTTONBOX SH


buttonbox Unix \
   Pwd 'sh pwd' \
   Cd... 'dialogbox CD "Enter directory" file s " " * cd $s unset s pwd'\
   More... more \
   'Ls -l' 'sh "ls -l"' \
   separator \
   Shell 'sh "xterm -sb &"' \
   'Vi...' vi \
   'Vi macro/...' vim \
   'Vi ~/macro/...' vip *
