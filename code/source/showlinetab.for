C This file is a part of the GIFA program
C
C     Authors :       M.A.Delsuc,
C                     Centre de Biochimie Structurale
C                     Faculte de Pharmacie
C                     34060 Montpellier - FRANCE
C
C     This software cannot be used unless the user have been fully 
C licensed to do so form the above laboratory.
C     This file cannot be copied, duplicated or used in any other program,
C without written permission from the author.
C
C First version 9 juin 1998
C
      subroutine showlinetab(vd_id,buffx,buffy,size,
     *       unt,offsetx,offsety)

c IN	: vd_id,buffx,buffy,size,unt,offsetx,offsety
c
C displays the content of (buffx,buffy) of length size
C buffx is interpreted in 'unt'
C buffy is interpreted as usual, with max0,scale,etc.. (from common)

      implicit none

      integer size,vd_id
      real buffx(size),buffy(size),offsetx,offsety
      character*256 unt
#include "sizebase.inc"
#include "disptypev.inc"
#include "paramfilev.inc"
#include "plotcomv.inc"
#include "zoomparamv.inc"
#include "unitextern.inc"
#include "laplacev.inc"

      real x(sizemax),y(sizemax)
#f2c      save x,y
      real zero,screen,zz1
      integer i

      if (max0.eq.0.0) max0=1.0

C plots
      zero = vheight
      screen = max0/(0.65)
      do i=1, size
         zz1 = buffx(i)+offsetx
         if (unt .eq. 'PPM') then
           zz1 = ptoir(zz1,sizeimage1d,sp1d,of1d,freq1d)
         else if (unt .eq. 'HZ') then
           zz1 = htoir(zz1,sizeimage1d,sp1d,of1d)
         else if (unt .eq. 'SECOND') then
           zz1 = stoir(zz1,sp1d)
         else if (unt .eq. 'DAMPING') then
           zz1 = ktoir(zz1,sizeimage1d,dmin,dmax,dfactor)
         elseif (unt.eq.'TAB') then
           zz1 = ttoir(zz1,sampling,szsamp)
         endif
         if (zoom.eq.0) then
            if (it1d.eq.0) then
                zz1 = (zz1-1) / (sizeimage1d-1)
            else
                zz1 = ((zz1-1)/2) / (sizeimage1d/2-1)
            endif
         else
              if (it1d.eq.0) then
                 zz1 = (zz1-zo1dl) / (zo1du-zo1dl)
              else
                 zz1 = ((zz1-zo1dl)/2) /
     *               float(int((zo1du-zo1dl+1)/2)-1)
              endif
         endif
         x(i) = zz1
         y(i) = scale*(buffy(i)+offsety)/screen + zero

      enddo

      call win_plot_array(vd_id,size,x,y)
      call win_update(vd_id)
      end
       

