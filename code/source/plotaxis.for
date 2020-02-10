C This file is a part of the GIFA program
C     This software has been developped by the NMR Group in GIF/Yvette.
C
C     Authors :       M.A.Delsuc, M.Robin
C                     NMR Laboratory,
C                     ICSN - CNRS
C                     91190 GIF/YVETTE   FRANCE
C
C     This software cannot be used unless the user have been fully 
C licensed to do so form the above laboratory.
C     This file cannot be copied, duplicated or used in any other program,
C without written permission from the authors.


      subroutine plotaxis(plid,cx,cy,offsetx,offsety,
     *                    direc,cunit,sepax,sepay,grid)
c IN: all parameters
C   This subroutine plots axes around a square of cx by cy cm. on the 
C   current plotter.                                           

C if Direc is 2    plots x axis only
C if Direc is 1    plots y axis only
C if Direc is 3    plots both axes

C offsetx is the distance of x axis to the x=0 level (usually neg)
C offsety is the distance of y axis to the y=0 level (usually neg)

c grid 0 is axis drawing
c grid 1 , a grid is to drawn along with axis

C sw and off are spectral width and spectral offset (of the upper right 
C  point) of the spectrum to plot.

C 1 refers to y coordinates or values and 2 to x coordinates
C   and values.           
      implicit none
      
      integer angle
      integer direc,plid,grid
      real cx,cy,sepax,sepay
      real offsetx, offsety
      character*20 cunit		! tell the active unit(HZ,PPM,SECOND or INDEX)
#include "zoomparamv.inc"
#include "paramfilev.inc"

      real sw1,sw2,off1,off2	! active widthes and offset, in current unit
      integer i,j,k,test,sens
      real curx,cury,curv,tick,step,first,vfirst,z,zz1,zz2,round
      real plottextsize,cvtemp
      character*20 text,lunitx,lunity

C copy active unit apart
      lunitx = unitx
      lunity = unity
      unitx = cunit
      unity = cunit
C compute widthes and offset.
C off is the coordinate of the right (up) most point
C sw is the width right to left (up to down)
      if (dim.eq.1) then
        if (zoom.eq.0) then
          call currunit(off2,sizeimage1d,2)
          call currunit(sw2,1,2)
        else
          call currunit(off2,zo1du,2)
          call currunit(sw2,zo1dl,2)
        endif
        off1 = 100
        sw1 = -30
      else
        if (zoom.eq.0) then
          call currunit(off2,si2im,2)
          call currunit(off1,si1im,1)
          call currunit(sw2,1,2)
          call currunit(sw1,1,1)
        else
          call currunit(off2,zour2,2)
          call currunit(off1,zour1,1)
          call currunit(sw2,zolf2,2)
          call currunit(sw1,zolf1,1)
        endif
      endif
      sw2 = sw2-off2
      sw1 = sw1-off1
      unitx = lunitx 
      unity = lunity

c      write(*,*) sw1, sw2, off1, off2
C Set up the plot text angle.
      angle = 0  !0 degrees for horizontal text...
      call pltextdir(plid,angle)
                                       
      cvtemp = 0.0  ! Temporary variable to store the absolute value of
C                   !  the CurV during the plotting of the tick labels.
      tick = 0.2    ! Tick depth in cm.

C ***********************************************************************
C ***********************************************************************
C ******************** Handle the X Axis......... ***********************
C ***********************************************************************
C ***********************************************************************
      if (direc.ge.2) then      ! direc=2 or 3
C Check to see that the cx value is reasonable...
   
      angle = 0  !0 degrees for horizontal text...
      if (cx.lt.0.0) then
         call gifaout('Cannot have a negative x width for plot!')
         goto 999   !Exit this subroutine...
      endif

C compute vfirst, the first value in unit
      if (sw2.gt.0.0) then
        vfirst = int((off2+sw2)/sepax)*sepax
      else
        vfirst = int(1+((off2+sw2)/sepax))*sepax
      endif

C compute step and first, these are in cm. for the plotter...
C from sepa and off and sw in unit
      first = (sw2+off2-vfirst)*cx/sw2
      step = abs( sepax*cx/sw2 )
      i = int(10*first/step)
      curx = first -i*step/10   ! beginning, and initialize the
      if (curx.gt.cx) goto 99
      curv = vfirst             ! current counter variables.

C Size of plotted label text in cm. 
      plottextsize  = .15*(step/(1.0*20/12.5))
c      call pltexset(plid,plottextsize,0.0)

C First, draw a straight baseline...
      if (grid.eq.0) 
     *       call plmove(plid,0.0,offsetx) !Put the plotter at the
      if (grid.eq.0) call pldraw(plid,cx,offsetx)  

C repeat until (curx.gt.cx)
      if (i.ge.1) then
      do j = 1,i
        if (grid.eq.0) call plmove(plid,curx,offsetx)
        test = i-j
        if (test.eq.4) then
           z = 2*tick/3
        else 
           z = tick/3
        endif
        if (grid.eq.0) call pldraw(plid,curx,offsetx-z)
        curx = first-(i-j)*step/10
        if (curx.gt.cx) goto 99
      enddo
      endif

      i = int((cx-first)/step)
      sens = 0
      do j = 1,i+1
       if (grid.eq.0) then
         call plmove(plid,curx,offsetx)      !Move to current tick spot
         call pldraw(plid,curx,offsetx-tick) !Draw the tick...
       else
         if (sens.eq.0) then          ! make it back and forth
           call plmove(plid,curx,0.0)      !Move to current grid spot
           call pldraw(plid,curx,cy)       !Draw the grid...
           sens = 1
         else
           call plmove(plid,curx,cy)      !Move to current grid spot
           call pldraw(plid,curx,0.0)       !Draw the grid...
           sens = 0
         endif
       endif
         
                                          
       cvtemp = abs(curv)
       if (curv .eq. int(curv)) then
             write (text,30) curv
       elseif (10*curv .eq. int(10*curv)) then
             write (text,40) curv
       elseif (100*curv .eq. int(100*curv)) then
             write (text,50) curv
       else
             write (text,60) curv
       endif
30          format(' ',f15.0)
40          format(' ',f15.1)
50          format(' ',f15.2)
60          format(' ',f15.4)
                           

C for debugging re-insert this line.-->  
C      write(*,*) text    
C      write(*,*) curv

C Plot the text string at the current position with the following
C  offsets:     x position = current - 8 mm. 
C               y position =  offset - 5 mm. 
C                        
       call trailzero(text)
       if (grid.eq.0) 
     *       call pltext(plid,text,9,curx-plottextsize,offsetx-0.5)

       do k = 1,4
        curx = curx + step/10          !Increment the step counters for the
        if (curx.gt.cx) goto 99
        if (grid.eq.0) call plmove(plid,curx,offsetx)      !Move to current tick spot
        if (grid.eq.0) call pldraw(plid,curx,offsetx-tick/3) !Draw the tick...
       enddo
       curx = curx + step/10
       if (curx.gt.cx) goto 99
       if (grid.eq.0) call plmove(plid,curx,offsetx)      !Move to current tick spot
       if (grid.eq.0) 
     *       call pldraw(plid,curx,offsetx-2*tick/3) !Draw the tick...
       do k = 1,4
        curx = curx + step/10          !Increment the step counters for the
        if (curx.gt.cx) goto 99
        if (grid.eq.0)
     *        call plmove(plid,curx,offsetx)      !Move to current tick spot
        if (grid.eq.0) 
     *       call pldraw(plid,curx,offsetx-tick/3) !Draw the tick...
       enddo
       curx = curx + step/10
       if (curx.gt.cx) goto 99
       if (sw2.gt.0) then
         curv = curv - sepax         ! plotter, and for the axis values.
       else
         curv = curv + sepax
       endif
      enddo

C writes the cunit 
99    if (grid.eq.0) 
     *       call pltext(plid,cunit,9,cx/2,offsetx-1.0)
      endif

C ***********************************************************************
C ***********************************************************************
C ******************** Handle the Y Axis......... ***********************
C ***********************************************************************
C ***********************************************************************

      if (mod(direc,2).eq.1) then  

C Odd numbers plot the y axis...
C Check to see if the user wants the y axis plotted.


C Check to see that the cy value is reasonable...
      cvtemp = 0.0  ! Temporary variable to store the absolute value of
C                   !  the CurV during the plotting of the tick labels.
      if (cy.lt.0.0) then
         call gifaout('Cannot have a negative y width for plot!')
         goto 999   !Exit this subroutine...
      endif

      angle = 90  !90 degrees for vertical text...
      call pltextdir(plid,angle)

C compute vfirst, the first value
      if (sw1.gt.0.0) then
        vfirst = int((off1+sw1)/sepay)*sepay
      else
        vfirst = int(1+((off1+sw1)/sepay))*sepay
       endif

C compute step and first, these are in cm. for the plotter...
         step = abs( sepay*cy/sw1 )
         first = (sw1+off1-vfirst)*cy/sw1
         i = int(10*first/step)
         cury = first -i*step/10   ! beginning, and initialize the
         if (cury.gt.cy) goto 98
         curv = vfirst                        ! current counter variables.

C First, draw a straight baseline...
         if (grid.eq.0) call plmove(plid,cx-offsety,cy) !Put the plotter at the
         if (grid.eq.0) call pldraw(plid,cx-offsety,0.0)        !beginning and draw a line

C repeat until (cury.gt.cy)
      if (i.ge.1) then
      do j = 1,i
        if (grid.eq.0) call plmove(plid,cx-offsety,cury)       !Move to current tick spot
        test = i-j
        if (test.eq.4) then
           z = 2*tick/3
        else
           z = tick/3
        endif
        if (grid.eq.0) call pldraw(plid,cx-offsety+z,cury)  !Draw the tick...
        cury = first-(i-j)*step/10
        if (cury.gt.cy) goto 98
      enddo
      endif

      i = int((cy-first)/step)
      sens = 0
      do j = 1,i+1
        if (grid.eq.0) then
          call plmove(plid,cx-offsety,cury)       !Move to current tick spot
          call pldraw(plid,cx-offsety+tick,cury)  !Draw the tick...
        else
          if (sens.eq.0) then          ! make it back and forth
            call plmove(plid,0.0,cury)      !Move to current grid spot
            call pldraw(plid,cx,cury)       !Draw the grid...
            sens = 1
          else
            call plmove(plid,cx,cury)      !Move to current grid spot
            call pldraw(plid,0.0,cury)       !Draw the grid...
            sens = 0
          endif
        endif
                                            
        cvtemp = abs(curv)
        if (curv .eq. int(curv)) then
            write (text,30) curv
        elseif (10*curv .eq. int(10*curv)) then
            write (text,40) curv
        elseif (100*curv .eq. int(100*curv)) then
            write (text,50) curv
        else
            write (text,60) curv
        endif

                                         
C Plot the text string at the current position with the following
C  offsets:     x position = current - 3 mm. 
C               y position =  offset - 8 mm. 
C                        
         call trailzero(text)
         if (grid.eq.0) 
     *       call pltext(plid,text,9,cx-offsety+0.4,cury-plottextsize/2)

        do k = 1,4
          cury = cury + step/10          !Increment the step counters for the
          if (cury.gt.cy) goto 98
          if (grid.eq.0) call plmove(plid,cx-offsety,cury)       !Move to current tick spot
          if (grid.eq.0) call pldraw(plid,cx-offsety+tick/3,cury)  !Draw the tick...
        enddo
        cury = cury + step/10
        if (cury.gt.cy) goto 98
        if (grid.eq.0) call plmove(plid,cx-offsety,cury)       !Move to current tick spot
        if (grid.eq.0) call pldraw(plid,cx-offsety+2*tick/3,cury)  !Draw the tick...
        do k = 1,4
          cury = cury + step/10          !Increment the step counters for the
          if (cury.gt.cy) goto 98
          if (grid.eq.0) call plmove(plid,cx-offsety,cury)       !Move to current tick spot
          if (grid.eq.0) call pldraw(plid,cx-offsety+tick/3,cury)  !Draw the tick...
        enddo
        cury = cury + step/10
        if (cury.gt.cy) goto 98
        if (sw1.gt.0) then
          curv = curv - sepay         ! plotter, and for the axis values.
        else
          curv = curv + sepay
        endif
      enddo

C writes the unit 
98    if (grid.eq.0) 
     *       call pltext(plid,cunit,9,cx-offsety+1.0,cy/2)
      endif    


C     Arrive here in order to exit...  
      angle = 0
      call pltextdir(plid,angle)
999   return
      end     
C     End of subroutine plotaxis.
                                 

C This function only keeps the two most significant
C digits of the passed real number:
C for example:
C     round(1432.22) = 1400.0
C     round(1.302)   = 1.3
C     round(0.0001465) = 1.4e-04
      function round(x)
 
      implicit none
      integer i
      real x,round,z

      if (x.eq.0.0) then
        z = 0.0
      else
        i = 0
70      if(abs(x*10.0**float(i)).gt.100.0) then
           i = i-1
           goto 70
        elseif (abs(x*10.0**float(i)).lt.(10.0)) then
           i = i+1
           goto 70
        endif
        z = int(x*10.0**float(i))/10.0**float(i) 
      endif
      round = z
      return
      end

