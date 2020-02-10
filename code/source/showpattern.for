C This file is a part of the GIFA program
C     This software has been developped by the NMR Group Montpellier.
C
C     Author :       M.A.Delsuc
C     This software cannot be used unless the user have been fully 
C licensed to do so form the above laboratory.
C     This file cannot be copied, duplicated or used in any other program,
C without written permission from the authors.
C
      subroutine showpattern(vd_id,x,y,px,py,pat)
C IN : vd_id   the id of the window to use
C      x,y     coordinate of the pattern
C      px,py   size of the pattern
C      pat     type of pattern
C
C  dispay a given pattern on screen
C  coordinates are 'generalized' 0..1
C  px,py are the size in 'generalized'.
C  pat is :   x  cross
C             +  plus
C             *  star
C             s  square
C             d  diamond
C             o  circle  (16 facetted)
C             b  box ( + and s )
C             

      implicit none

      integer vd_id
      character*1 pat

      integer i
      real x,y,px,py
      real xmp,xpp,ymp,ypp,xar(17),yar(17),pi8,x1,y1,x2,y2

      if (x.lt.0.0 .or. x.gt.1.0 .or. y.lt.0.0 .or. y.gt.1.0) goto 10
      if (vd_id.eq.0) goto 10

      xmp = max(x-px,0)
      xpp = min(x+px,1)
      ymp = max(y-py,0)
      ypp = min(y+py,1)
      pi8 = atan(1.0)/2

      if (pat .eq. '+' .or. pat .eq. 'b') then       !  + pattern
      	call win_plot_1d(vd_id, xmp, y, xpp, y)
      	call win_plot_1d(vd_id, x, ymp, x, ypp)
      endif
      if (pat .eq. 's' .or. pat .eq. 'b') then  !  square
      	xar(1) = xmp
      	yar(1) = ymp
      	xar(2) = xmp
      	yar(2) = ypp
      	xar(3) = xpp
      	yar(3) = ypp
      	xar(4) = xpp
      	yar(4) = ymp
      	xar(5) = xmp
      	yar(5) = ymp
      	call win_plot_array(vd_id,5,xar,yar)
      else if (pat .eq. 'x') then       !  x pattern
       	call win_plot_1d(vd_id, xmp, ymp, xpp, ypp)
      	call win_plot_1d(vd_id, xmp, ypp, xpp, ymp)
      else if (pat .eq. 'd') then  !  tilted square
      	xar(1) = x
      	yar(1) = ymp
      	xar(2) = xmp
      	yar(2) = y
      	xar(3) = x
      	yar(3) = ypp
      	xar(4) = xpp
      	yar(4) = y
      	xar(5) = x
      	yar(5) = ymp
      	call win_plot_array(vd_id,5,xar,yar)
      else if (pat .eq. 'o') then  !  16 sides
         do i=0,16
            xar(i+1)=min(1,max(0,x+px*cos(i*pi8)))
            yar(i+1)=min(1,max(0,y+py*sin(i*pi8)))
         enddo
         call win_plot_array(vd_id,17,xar,yar)
      else if (pat .eq. '*') then  !  star
         do i=0,3
            x1=min(1,max(0,x+px*cos(i*2*pi8)))
            y1=min(1,max(0,y+py*sin(i*2*pi8)))
            x2=min(1,max(0,x-px*cos(i*2*pi8)))
            y2=min(1,max(0,y-py*sin(i*2*pi8)))
            call win_plot_1d(vd_id,x1,y1,x2,y2)
         enddo
      endif
c      call win_update(vd_id)
10    return
      end
