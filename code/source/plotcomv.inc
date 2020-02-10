C plotcom.inc
C
C this file contains the plot variables
C
       integer 		nfile
       parameter(nfile=10)    ! can plot on nfile in the same time
       integer		penup, plottextangle, init(nfile),
     *                  rotate, count
       real		offx, offy, cx, cy, sepax, sepay, vheight
       integer		current               ! index of the open plot file
       character*256	namecom(nfile)   ! can plot on nfile in the same time
       character*20     plotunit

       integer          pl_mode, post, hpgl
       parameter (post = 1, hpgl = 2)

       common /plotcom/ pl_mode,penup, plottextangle, init,
     *                  rotate, count,
     *                  offx, offy, cx, cy, sepax, sepay, vheight,
     *                  current, namecom, plotunit

