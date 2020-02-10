;set up a menu for plotting
; 
; see also : button.g startup.g BUTTONBOX


buttonbox 'Plot' \
   'Easy plot' easyplot \
   separator \
   'Plot...' plot? \
   separator \
   'Screen Dump' 'plot *pl' \
   'Title' title \
   'Plotaxis?' plotaxis? \
   'Plotaxis F1' 'plotaxis f1 *pl' \
   'Plotaxis F2' 'plotaxis f2 *pl' \
   'Grid F1' 'grid f1 *pl' \
   'Grid F2' 'grid f2 *pl' \
   'Page' 'page *pl' \
   separator \
   'Cindy...' fplot \
   *

