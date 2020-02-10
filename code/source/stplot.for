C
C     This file is a part of the GIFA program.
C     Version 2                           20-jan-1990 
C     This software was developed by the NMR Group of the
C     CNRS in GIF/Yvette France.
C
C     Programmers:    M.A. Delsuc, V. Stoven, M. Robin
C                     NMR Laboratory
C                     ICSN - CNRS
C                     91190 GIF/YVETTE   FRANCE
C
C     This software cannot be used unless the user has been fully 
C licensed by the above laboratory.
C     This file cannot be copied, transferred, or used for any other
C purpose without the written permission of the authors.
C                                                      
      subroutine stplot(pl_id,matrix,si1,si2,itype,
     *              scale,skip,shf,skew,cx,cy,dy)
C IN	: pl_id,matrix,si1,si2,itype,scale,skip,shf,skew,cx,cy,dy
C
C     This subr. plots a stacked plot of the matrix matrix(si2,si1).
C All measures are in cm
C The length of a single line is cx.
C if scale.eq.1 then the larger line of the 2D is cy high
C lines are clipped at cy before plotting.
C each line is shifted by shf point before plotting (might be fractional)
C and incremented in y by dy cm.
C The horizontal line may be skewed left or right with the skew param.
C the number of plotted lines is n, which depends on itype and skip,
C so that every skip lines is plotted.
C if pl_id .ne. 99  -> plot on graphic screen
C uses work1d(sizemax) found in sizeparam.inc as the skyline buffer

      implicit none
#include "sizeparam.inc"
#include "controlc.inc"

      integer si1,si2,itype,pl_id,skip
      real matrix(si2,si1),cx,cy,dy,scale,shf,skew

      integer di1,di2,shift,nline,i1,i2,mi1,mi2,title_length
      real max0,factor,dx,x,y,x0,y0,xx,yy,
     *     uisx,uisy,shiftr,ysk0
      real xfix,yfix,tx,ty,sx,sy,xmin,ymin
      logical penup,nextup
      character*1 cc
      character*256 win_title

C test input
      if(skip.lt.1 .or. scale.eq.0 ) then
          write(*,*) 'Invalid parameters'
          return
      endif

C compute matrix increments
      di1 = 1
      di2 = 1
      if (mod(itype,2).eq.1) di1 = 2
      if (itype.ge.2) di2 = 2
      di1 = di1*skip
      nline = si1/di1
      dx = cx*di2/si2
      ysk0 = max(0.0, cx*skew)      ! offset if skewing
      if (sizemax.lt.(si2 + int(shf*nline))) then
         write(*,*) 'Too many horizontal points'
         return
      endif

c      if (pl_id.ne.99) then     ! restrict number of point on screen
c        if (nline*si2/di2.gt.128*256) then
c           write(*,*) 'Too many points for on-screen plot, use STSKIP'
c           return
c        endif
c      endif

C find maximum
      max0 = 0.0
      do i1=1,si1,di1
        do i2=1,si2,di2
          max0 = max(abs(matrix(i2,i1)),max0)
        enddo
      enddo

      if (max0.eq.0.0) then
        write(*,*) 'Flat image!'
        return
      endif

      factor = scale*cy/max0
C set up sky line
      call prpvect(work1d,-0.1*abs(cy),sizemax)   ! put -0.1cy cm. in skyline
      if (shf.ge.0.0) then
        shiftr=0.0
      else
        shiftr = nline*abs(shf)
      endif
      shift = int(shiftr)
C
C then plot
      if (pl_id.ne.99) then
C        screen width and height in cms...
         uisx = cx + nline*dx*abs(shf)
         uisy = abs(cy) + nline*dy +abs(skew)*cx

         win_title = 'Stacked Plot'
         call trailing (win_title,title_length)
         call win_open_gene(pl_id,
     *       1.05*uisx,1.05*uisy,
     *       win_title,title_length,0.0,0.0,2)
         if (pl_id.eq.0) then
            call gifaout('*** Unable to open window')
            return
         endif
         xfix = (1.10 * uisx)
         yfix = (1.10 * uisy)
         xmin = (-0.1 * uisx)
         ymin = (-0.1 * uisy)
      else
         call plinit(pl_id)
      endif

      do i1=1, nline
         mi1 = 1 + di1*(i1-1)
         x0 = (shift+1)*dx
         y0 = matrix(1,mi1)*factor      ! compute starting point
         if (abs(y0).gt.abs(cy)) y0 = abs(cy)*y0/abs(y0)
         y0 = y0 + i1*dy + ysk0
         penup = (y0.lt.work1d(shift+1))   ! starting pen up?

         if (.not.penup) then
             if (pl_id.eq.99) call plmove(pl_id, x0, y0)
         endif

         do i2=2, si2/di2       ! scan thru the line
            mi2 = 1 + di2*(i2-1)
            x = (shift+i2)*dx
            y = matrix(mi2,mi1)*factor
            if (abs(y).gt.abs(cy)) y = abs(cy)*y/abs(y)  ! clip to cy
            y = y + i1*dy + ysk0 - (i2-1)*skew*dx
            nextup = (y.lt.work1d(shift+i2))  ! next up?
            if (.not.nextup .and. .not.penup) then
               if (pl_id.eq.99) then
                 call pldraw(pl_id,x,y)                   ! just draw
               else
                 tx = ((x-xmin)/xfix)
                 ty = ((y-ymin)/yfix)
                 sx = ((x0-xmin)/xfix)
                 sy = ((y0-ymin)/yfix)
                 call win_plot_1d(pl_id,sx,sy,tx,ty)
               endif
            elseif (penup .and. nextup) then
               continue                       ! do nothing
            else                ! intersection...
               call intsct(x0,x,y0,y,             ! compute intersect
     *            work1d(shift+i2-1),work1d(shift+i2),xx,yy)
               if (penup) then
                  if (pl_id.eq.99) then
                    call plmove(pl_id,xx,yy)    ! move to intersect
                    call pldraw(pl_id,x,y)      ! and draw
                  else
                    tx = ((x-xmin)/xfix)
                    ty = ((y-ymin)/yfix)
                    sx = ((xx-xmin)/xfix)
                    sy = ((yy-ymin)/yfix)
                    call win_plot_1d(pl_id,sx,sy,tx,ty)
                  endif
               else
                  if (pl_id.eq.99) then
                    call pldraw(pl_id,xx,yy)    ! just draw to intersect
                  else
                    tx = ((xx-xmin)/xfix)
                    ty = ((yy-ymin)/yfix)
                    sx = ((x0-xmin)/xfix)
                    sy = ((y0-ymin)/yfix)
                    call win_plot_1d(pl_id,sx,sy,tx,ty)
                  endif
               endif              ! intersect
            endif      ! different cases
C then keep track
            if (.not.penup) work1d(shift+i2-1) = y0
            penup = nextup
            x0 = x
            y0 = y
         enddo
         if (.not.penup) work1d(shift+i2) = y0
         shiftr = shiftr + shf
         shift = int(shiftr)
         if (control.eq.1) goto 99
         if (pl_id.ne.99 .and. mod(i1,16).eq.0)
     *		call win_update(pl_id)		! check what we're doing
      enddo
99    if (pl_id.eq.99) then
          call plhome(pl_id)
      else
          write(*,*) 'Type return to refresh'
          read(*,18) cc
          call win_update(pl_id)
          write(*,*) 'Type return to continue'
          read(*,18) cc
18        format(A1)
          call win_close(pl_id)
      endif
      call win_refresh()

      return
      end

      subroutine intsct(x1,x2,y11,y12,y21,y22,xx,yy)
c IN	: x1,x2,y11,y12,y21,y22
c OUT	: xx,yy
c
c used by stplot
c     intsct calculates intersection pt of 2 lines with endpoints
c     (x1,y11)->(x2,y12)   and   (x1,y21)->(x2,y22)
c     and returns this point in xx,yy

      implicit none
      real x1,x2,y11,y12,y21,y22,xx,yy
      real dx,sl1,sl2,b1,b2

      dx=x2-x1
      sl1=(y12-y11)/dx
      sl2=(y22-y21)/dx
      b1=(y11-sl1*x1)
      b2=(y21-sl2*x1)
      xx=(b2-b1)/(sl1-sl2)
      yy=sl1*xx+b1
      return
      end
