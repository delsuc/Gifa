A problem has been introduced with the 4.2 version of XFree.

The symptoms is that many Motif based application refuse to start, including Gifa,
with varying messages such 
"attempt to add non-widget child  to parent  which supports only widgets"
or
"zero width widget... "

You will find a discusison about this at
http://sourceforge.net/tracker/index.php?func=detail&aid=524215&group_id=18034&atid=118034

Since things are not fully settled yet, and Xfree evoluates faster than we can tackle.
As a short term solution, he best way is to change the libXt file of the Xfree distribution
back to the 4.1 version.
You will find here the libXt.a (needed for compiling)
and libXt.dylib (needed for running) from the 4.1 version.

copy them to your /usr/X11R6/lib/
(you may want to backup your own files before end)

M.A.Delsuc
