C This file is a part of the GIFA program
C     This software has been developped by the NMR Group in CBS/Montpellier
C
C     Authors :       M.A.Delsuc
C                     C.B.S. Faculte de Pharmacie
C                     34060 Montpellier   FRANCE
C
C     This software cannot be used unless the user have been fully
C licensed to do so form the above laboratory.
C     This file cannot be copied, duplicated or used in any other program,
C without written permission from the authors.

      subroutine dorefresh()
c part of the GIFA command parser,
c determines whether the display should be recomputed.
c uses the global variable refresh to do so, and call accordingly
c the correct display routines.


      implicit none
#include "disptypev.inc"
#include "gifshellv.inc"
#include "gifabcom.inc"
#include "sizeparam.inc"
#include "paramfilev.inc"
       real mini,maxi
       integer i,zz

       if (input.eq.20 .or. refmacro.eq.1) then
          if (scale.eq.0.0) then
            call gifaout('*** no Display, SCALE is null')
            return
          endif
          if (dim.eq.1 .and. disp1d.ne.0) then
             call dorefresh1d(vd_id(1))
             if (axes.ne.0) then
                call gcolor(vd_id(1),color)
                call set_axis(axes,vd_id(1))
             endif
          else if (dim.eq.2) then       
            if (disp2d.ne.0) then
		call doref2d(vd_id(2))
                if (axes.ne.0) then
                   call gcolor(vd_id(2),color)
                   call set_axis(axes,vd_id(2))
                endif
	    endif
            if (cdisp2d.ne.0) then
		call doref2dcont(vd_id(3))
                if (axes.ne.0) then
                   call gcolor(vd_id(3),color)
                   call set_axis(axes,vd_id(3))
                endif
	    endif
          else        	! dim .eq. 3
            if (max0.eq.0.0) then
c                write(*,*) 'Looking for the Maximum intensity'
                call mnxvect(mini,maxi,zz,i,image,sizeimage3d)
c                write(*,*) maxi
                if (maxi .eq. 0.0) maxi = 1.0 
                max0 = maxi
            endif
            if (disp2d.ne.0) then 
		call doref2d(vd_id(2))
                if (axes.ne.0) then
                   call gcolor(vd_id(2),color)
                   dim = 2
                   call set_axis(axes,vd_id(2))
                   dim = 3
                endif
	    endif
            if (cdisp2d.ne.0) then
		call doref2dcont(vd_id(3))
                if (axes.ne.0) then
                   call gcolor(vd_id(3),color)
                   dim = 2
                   call set_axis(axes,vd_id(3))
                   dim = 3
                endif
	    endif
          endif
        endif

        return
        end


      subroutine dorefresh1d(win_id)
c IN : win_id
c refresh the 1D window called win_id
c
      implicit none
      integer win_id
#include "zoomparamv.inc"
#include "sizeparam.inc"
#include "paramfilev.inc"
#include "disptypev.inc"
#include "plotcomv.inc"

            if (zoom.eq.0) then
             call display1d(win_id,sizeimage1d,column,
     *                 max0,scale,it1d,vheight,clear)
            else
             call display1d(win_id,zo1du-zo1dl+1,
     *                column(zo1dl),max0,scale,it1d,vheight,clear)  
            endif
      return
      end

      subroutine doref2d(win_id)
c refresh the 2D-density window called win_id
c
      implicit none
      integer win_id
#include "zoomparamv.inc"
#include "sizeparam.inc"
#include "paramfilev.inc"
#include "disptypev.inc"

              call display2d(win_id,si2im,si1im,
     *                 plane2d,max0,scale,itype,sign,
     *                 zoom,zolf1,zolf2,zour1,zour2)
      return
      end

      subroutine doref2dcont(win_id)
c refresh the 2D-contour window called win_id
c
      implicit none
      integer win_id
#include "zoomparamv.inc"
#include "sizeparam.inc"
#include "plotcomv.inc"
#include "paramfilev.inc"
#include "disptypev.inc"

              call plot2d(0,win_id,si1im,si2im,plane2d,
     *                   max0,scale,itype,cx,cy,level,sign,loga,
     *                   ccolor,zoom,zolf1,zolf2,zour1,zour2,clear)

      return
      end
