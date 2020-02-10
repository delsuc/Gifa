C This file is a part of the GIFA program
C     This software has been developped by the NMR Group in CBS/Montpellier
C
C     Authors :       M.A.Delsuc J.L.Pons
C                     C.B.S. Faculte de Pharmacie
C                     34060 Montpellier   FRANCE
C
C     This software cannot be used unless the user have been fully
C licensed to do so form the above laboratory.
C     This file cannot be copied, duplicated or used in any other program,
C without written permission from the authors.

c****************************************************************
      subroutine gcolor(vd_id,col_ind)
c IN	: vd_id,col_ind
c
c vd_id : id of the window
c col_ind : color index
c set the gc for the color of the window
c 1:white 2:red  3:orange 4:yellow
c 5:green 6:cian 7:blue   8:purple  9:black
c
      implicit none
#include "gcolor.inc"
      integer vd_id, col_ind, xx

         if (col_ind.eq.1) then
             xx = white
         elseif (col_ind.eq.2) then
             xx = red
         elseif (col_ind.eq.3) then
             xx = yellow
         elseif (col_ind.eq.4) then
             xx = green
         elseif (col_ind.eq.5) then
             xx = cian
         elseif (col_ind.eq.6) then
             xx = blue
         elseif (col_ind.eq.7) then
             xx = purple
         elseif (col_ind.eq.8) then
             xx = black
         else
             xx = white
         endif
         call win_fgcolor(vd_id,xx)
         return
         end
         
c****************************************************************
      subroutine bcolor(vd_id,col_ind)
c IN	: vd_id,col_ind
c
c vd_id : id of the window
c col_ind : color index
c set the gc for the background color of the window
c 1:white 2:red  3:orange 4:yellow
c 5:green 6:cian 7:blue   8:purple  9:black
c

#include "gcolor.inc"
      integer vd_id, col_ind, xx

         if (col_ind.eq.1) then
             xx = white
         elseif (col_ind.eq.2) then
             xx = red
         elseif (col_ind.eq.3) then
             xx = yellow
         elseif (col_ind.eq.4) then
             xx = green
         elseif (col_ind.eq.5) then
             xx = cian
         elseif (col_ind.eq.6) then
             xx = blue
         elseif (col_ind.eq.7) then
             xx = purple
         elseif (col_ind.eq.8) then
             xx = black
         else
             xx = white
         endif
         call win_bgcolor(vd_id,xx)
         return
         end
         



