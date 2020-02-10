C This file is a part of the GIFA program
C     This software has been developped by the NMR Group
C      in GIF/Yvette and in Montpellier
C
C     Authors :       M.A.Delsuc and others
C                     NMR Laboratory,
C                     CBS - Fac de Pharmacie
C                     34000 Montpellier   FRANCE
C
C     This software cannot be used unless the user have been fully
C licensed to do so form the above laboratory.
C     This file cannot be copied, duplicated or used in any other program,
C without written permission from the authors.

C you'll find here all the f77 entry points used from X_*.c
C 

      subroutine reset_all()
C reset all display parameters, and clear point_stack
c
      implicit none
#include "paramfilev.inc"
#include "zoomparamv.inc"
#include "disptypev.inc"
#include "plotcomv.inc"
      max0 = 0
      scale = 1
      zoom = 0
      clear = 1
      nclick = 0
      ccolor = 1
      color = 1
      scolor = 2
      vheight = 0.3
      return
      end

C==================================================
      subroutine setvcontext(vcont)
#include "gifshellv.inc"
      integer vcont

      vcontext = vcont
      return
      end

C==================================================
      subroutine getvcontext(vcont)
#include "gifshellv.inc"
      integer vcont

      vcont = vcontext 
      return
      end

C==================================================
C      subroutine set_select_arrea_var(x1,y1,x2,y2)
c
cc IN : x1,y1,x2,y2
c
c      implicit none
c#include "zoomparamv.inc"
c      real x1,y1,x2,y2
c
c	selectx1 = x1
c	selectx2 = x2
c	selecty1 = y1
c	selecty2 = y2
c
c      return
c      end

C==================================================
      subroutine set_zoom_coord(ll1, ll2, ur1, ur2)
c
c IN :  ll1, ll2, ur1, ur2
C	For 2 D Only

      implicit none
#include "zoomparamv.inc"
#include "paramfilev.inc"

      integer size,size1,size2,zl1,zl2,zu1,zu2
      real ur1,ur2,ll1,ll2

          if (dim.eq.2) then
             zolf2 = ll2*(si2im) + 1
             zour2 = ur2*(si2im) + 1
             zolf1 = ll1*(si1im) + 1
             zour1 = ur1*(si1im) + 1
             zolf1 = max(1,zolf1)
             zour1 = min(zour1,si1im)
             zolf2 = max(1,zolf2)
             zour2 = min(zour2,si2im)
             if (itype.eq.1 .or. itype.eq.3) then  ! check if complex in F2
               zolf2 = 2*(zolf2/2)+1
               zour2 = 2*(zour2/2)
             endif
             if (itype.eq.2 .or. itype.eq.3) then  ! check if complex in F1
               zolf1 = 2*(zolf1/2)+1
               zour1 = 2*(zour1/2)
             endif
          endif
        return
      end
 
C==================================================
      subroutine set_zoom_param(vd_id, zstat, ll1, ll2, ur1, ur2)
c
c IN : vd_id, zstat, ll1, ll2, ur1, ur2
c SIDE : zoom state
c
C    This subroutine allows an interactive display of a spectrum.
c
C   spectrum: spectrum to display
C   vd_id: window in which the new phased spectra is drawn
c   zstat, ul1,ul2,lr1,lr1 describe the zoom state, (in and out)
C
      implicit none
#include "zoomparamv.inc"
#include "paramfilev.inc"

      integer vd_id,zstat,size,size1,size2,zl1,zl2,zu1,zu2
      real ur1,ur2,ll1,ll2

          if (dim.eq.1) then
		zu1 = zo1du
                zl1 = zo1dl
             if (zoom.eq.0) then
		size = sizeimage1d
		zo1dl = int(ll2*sizeimage1d) + 1
                zo1du = int(ur2*sizeimage1d) + 1
             else
                size = zo1du-zo1dl
                zo1du = int(ur2*size) + zo1dl
                zo1dl = int(ll2*size) + zo1dl
             endif
             zo1dl = min(zo1dl,zo1du-1)
             zo1dl = max(1,zo1dl)
             zo1du = min(zo1du,sizeimage1d)
             if (it1d.eq.1) then		! check if complex
               zo1dl = 2*(zo1dl/2)+1
               zo1du = 2*(zo1du/2)
             endif
	     if(size.le.1) then
		zo1du = zu1
		zo1dl = zl1 
	     endif

	  elseif (dim.eq.2) then
		zl1 = zolf1
                zl2 = zolf2
                zu1 = zour1
                zu2 = zour2
             if (zoom.eq.0) then
		zolf2 = ll2*(si2im) + 1
		zour2 = ur2*(si2im) + 1 
		zolf1 = ll1*(si1im) + 1
		zour1 = ur1*(si1im) + 1
             else
                size1 = zour1-zolf1+1
                size2 = zour2-zolf2+1
                zour2 = int(ur2*size2) + zolf2
                zour1 = int(ur1*size1) + zolf1
                zolf2 = int(ll2*size2) + zolf2
                zolf1 = int(ll1*size1) + zolf1
             endif
             zolf1 = max(1,zolf1)
             zour1 = min(zour1,si1im)
             zolf2 = max(1,zolf2)
             zour2 = min(zour2,si2im)
             if (itype.eq.1 .or. itype.eq.3) then  ! check if complex in F2
               zolf2 = 2*(zolf2/2)+1
               zour2 = 2*(zour2/2)
             endif
             if (itype.eq.2 .or. itype.eq.3) then  ! check if complex in F1
               zolf1 = 2*(zolf1/2)+1
               zour1 = 2*(zour1/2)
             endif
	     size1 = zour2-zolf2
	     size2 = zour1-zolf1
	     if(size1.le.1.or.size2.le.1) then
		zolf1 = zl1
		zolf2 = zl2
		zour1 = zu1
		zour2 = zu2
	    endif
          endif
	zoom = zstat
        return
      end

C==================================================
	subroutine shift_image(vd_id, shift)
c
c IN : vd_id, shift
c
C   spectrum: 1D spectrum to display
C   vd_id: window in which the new image is drawn
C   shift orientation :
C		1 -> up	
C		2 -> down
C		3 -> right
C		4 -> left

      implicit none
#include "zoomparamv.inc"
#include "paramfilev.inc"
#include "plotcomv.inc"

      integer vd_id,size,lowim,upim,shift

          if (dim.eq.1) then
             if (zoom.ne.0) then
		if(shift.eq.3) then
			size = zo1du-zo1dl 
			lowim = zo1dl + (size/3)
			upim = zo1du + (size/3)
			if(upim.ge.sizeimage1d)then
				zo1dl = sizeimage1d - size
				zo1du = sizeimage1d
			else 
				zo1du = upim
				zo1dl = lowim
			endif

		elseif(shift.eq.4) then
			size = zo1du-zo1dl
                        upim = zo1du - (size/3)
                        lowim = zo1dl - (size/3)
                        if(lowim.le.0) then
				zo1du = size+1
                                zo1dl = 1
			else 
                                zo1du = upim
                                zo1dl = lowim
                        endif
		endif
	        if (it1d.eq.1) then             ! check if complex
                    zo1dl = 2*(zo1dl/2)+1
                    zo1du = 2*(zo1du/2)
               endif
             endif
             if (shift.eq.1) then
                   vheight = vheight + 0.1
             elseif (shift.eq.2) then
                   vheight = vheight - 0.1
             endif

          elseif (dim.eq.2) then
             if (zoom.ne.0) then
		if(shift.eq.2) then
                        size = zour1-zolf1
                        upim = zour1 - (size/3)
                        lowim = zolf1 - (size/3)

                       if(lowim.le.0) then
                               zour1 = zour1-(zolf1-1)
                               zolf1 = 1
			else
			       zour1 = upim
                               zolf1 = lowim

                       endif

                elseif(shift.eq.1) then
                        size = zour1-zolf1
                        upim = zour1 + (size/3)
                        lowim = zolf1 + (size/3)

                       if(upim.ge.si1im) then
                               zolf1 = zolf1 + (si1im-zour1) 
                               zour1 = si1im
			else
                               zour1 = upim
                               zolf1 = lowim
                       endif

                elseif(shift.eq.4) then
                        size = zour2-zolf2
                        upim = zour2 - (size/3)
                        lowim = zolf2 - (size/3)

                       if(lowim.le.0) then
                               zour2 = zour2-(zolf2-1)
                               zolf2 = 1
			else
                               zour2 = upim
                               zolf2 = lowim
                       endif

                elseif(shift.eq.3) then
                        size = zour2-zolf2
                        upim = zour2 + (size/3)
                        lowim = zolf2 + (size/3)

                       if(upim.ge.si2im) then
                               zolf2 = zolf2 + (si2im-zour2)
                               zour2 = si2im
			else
                               zour2 = upim
                               zolf2 = lowim
                       endif
		endif
	         if (itype.eq.1 .or. itype.eq.3) then
                     zolf2 = 2*(zolf2/2)+1
                     zour2 = 2*(zour2/2)
                endif
                if (itype.eq.2 .or. itype.eq.3) then
                     zolf1 = 2*(zolf1/2)+1
                     zour1 = 2*(zour1/2)
                endif

             endif
	  endif
        return
      end

C==================================================

        subroutine zoom_param_return()
c zoom out

        implicit none
#include "zoomparamv.inc"
#include "paramfilev.inc"
      integer size, lowim, upim, lowim2, upim2, size2, size3,
     *      lowim3, upim3

      if (zoom.ne.0) then
          if (dim.eq.1) then
               size = zo1du-zo1dl
               lowim = zo1dl - (size/2)
               upim = zo1du + (size/2)
               if(upim.ge.sizeimage1d) then
                      zo1du = sizeimage1d
	       else
	              zo1du = upim	
               endif
	       if(lowim.le.0) then
	              zo1dl = 1
               else
                      zo1dl = lowim
               endif
	       if (it1d.eq.1) then             ! check if complex
                     zo1dl = 2*(zo1dl/2)+1
                     zo1du = 2*(zo1du/2)
               endif
          elseif (dim.eq.2) then
               size = zour1-zolf1
	       size2 = zour2-zolf2
               upim = zour1 + (size/2)
               lowim = zolf1 - (size/2)
               upim2 = zour2 + (size2/2)
               lowim2 = zolf2 - (size2/2)
               if(lowim.le.0) then
                        zolf1 = 1
               else
                        zolf1 = lowim
	       endif
	       if(lowim2.le.0) then
                        zolf2 = 1
               else
                        zolf2 = lowim2
               endif
               if(upim.ge.si1im) then
                        zour1 = si1im
               else
                        zour1 = upim
               endif
               if(upim2.ge.si2im) then
                        zour2 = si2im
               else
                        zour2 = upim2
               endif
              if (itype.eq.1 .or. itype.eq.3) then
                 zolf2 = 2*(zolf2/2)+1
                 zour2 = 2*(zour2/2)
              endif
              if (itype.eq.2 .or. itype.eq.3) then
                 zolf1 = 2*(zolf1/2)+1
                 zour1 = 2*(zour1/2)
              endif

          elseif (dim.eq.3) then
               size = zo3df1u-zo3df1l
	       size2 = zo3df2u-zo3df2l
               size3 = zo3df3u-zo3df3l
               upim = zo3df1u + (size/2)
               lowim = zo3df1l - (size/2)
               upim2 = zo3df2u + (size2/2)
               lowim2 = zo3df2l - (size2/2)
               upim3 = zo3df3u + (size2/2)
               lowim3 = zo3df3l - (size2/2)

               zo3df1l = max(1,lowim)
               zo3df2l = max(1,lowim2)
               zo3df3l = max(1,lowim3)
               zo3df1u = min(si3d1,upim)
               zo3df2u = min(si3d2,upim2)
               zo3df3u = min(si3d3,upim3)
              if (it3d.eq.1 .or. it3d.eq.3 .or.
     *            it3d.eq.5 .or. it3d.eq.7) then
                 zo3df3l = 2*(zo3df3l/2)+1
                 zo3df3u = 2*(zo3df3u/2)
              endif
              if (it3d.eq.2 .or. it3d.eq.3 .or.
     *            it3d.eq.6 .or. it3d.eq.7) then
                 zo3df2l = 2*(zo3df2l/2)+1
                 zo3df2u = 2*(zo3df2u/2)
              endif
              if (it3d.eq.4 .or. it3d.eq.5 .or.
     *            it3d.eq.6 .or. it3d.eq.7) then
                 zo3df1l = 2*(zo3df1l/2)+1
                 zo3df1u = 2*(zo3df1u/2)
              endif

           endif
	endif 
        return
        end

C==================================================
        subroutine set_scale(flag)
c
c IN : flag
c
C   code de flag :
C               1 -> / 2
C               2 -> * 2
C               3 -> - 20%
C               4 -> + 20 %

        implicit none
#include "disptypev.inc"
	integer flag

      	if (flag.eq.1) then
		scale = scale/2
	elseif(flag.eq.2) then
	        scale = scale*2
	elseif(flag.eq.3) then
                scale = scale/1.2
	elseif(flag.eq.4) then
                scale = scale*1.2
        endif
      	return
        end

C==================================================
        subroutine set_zoom_zero()
C reset zoom parameters
        implicit none
#include "zoomparamv.inc"
      zoom = 0
      return
      end
C==================================================
        subroutine which_dim(dimback)
C returns current dim
        implicit none
#include "paramfilev.inc"
      integer dimback
      dimback = dim
      return
      end
C==================================================
        subroutine get_currunit(retunit,l,axe)
C IN : axe
C OUT : retunit,l
C
C returns current unit along axe
        implicit none
#include "zoomparamv.inc"
      character*8 retunit
      integer l,axe
      if (axe .eq. 1) then
         call trailing(unity,l)
         retunit = unity
      else
         call trailing(unitx,l)
         retunit = unitx
      endif
      return
      end

C==================================================
	subroutine which_prompt(cprompt)
C returns current prompt
	implicit none
#include "gifshellv.inc"
	character*256 cprompt
	cprompt = prompt
	return
	end

C==================================================
        subroutine which_int(inten, ldim, f1, f2, f3)
c returns the intensity of the point in buffer dim, at
C coordinates f1, f2, f3 (index)
	implicit none
#include "sizeparam.inc"
#include "paramfilev.inc"
	real inten
        integer ldim,f1,f2,f3
	if  (ldim .eq. 1) then
           inten = column(f1)
        else if (ldim .eq.2) then
           inten = plane2d((f1-1)*si2im + f2)
        else if (ldim .eq.3) then
           inten = image((f1-1)*si3d2*si3d3 + (f2-1)*si3d3 + f3 )
        endif
	return
	end

C==================================================
       subroutine set_point_xy(xmouse,ymouse)
C set point parameters:
C	
C
        implicit none
#include "zoomparamv.inc"
      real xmouse,ymouse
      if (nclick.eq.0) nclick = 1	! modif MAD
      clickt(1,1) = xmouse
      clickt(1,2) = ymouse
      return
      end

C==================================================
        subroutine set_last_butt(butt)
C set last variable paraneters
        implicit none
#include "zoomparamv.inc"
      integer butt
      clickbutton = butt
      return
      end

C==================================================
        subroutine set_phase_var(spectrum,x0,wid,pc0,p1,flag)
C set last variable parameters
C flag determines action :
C       init = 0
C       cancel = 1
C       phasing = 2
C       Ok = 3

        implicit none
#include "sizeparam.inc"
#include "paramfilev.inc"
#include "zoomparamv.inc"
#include "disptypev.inc"
#include "plotcomv.inc"
#include "gifabcom.inc"

      integer wid
      integer flag
      real spectrum(sizeimage1d),p0,p1
      real x0,pc0
      character*80 str

c  	lmax0 = 0
C this must be done if the guy says Ok with no correction!
	if(flag.eq.0) then
           call copvect(work1d,column,sizeimage1d)
C cancel callback !
	else if(flag.eq.1) then
            call copvect(column,work1d,sizeimage1d)
C phasing tests callback
	else if(flag.eq.2) then
		call copvect(column,work1d,sizeimage1d)
		p0 = pc0 + p1*(0.5-x0)
                ph0 = p0
                ph1 = p1
		call phase(column,p0,p1,sizeimage1d/2)
	        if (zoom.eq.0) then
        	   call display1d(wid,sizeimage1d,column,
     * 		        max0,scale,1,vheight,1)
        	else
                   call display1d(wid,zo1du-zo1dl+1,column(zo1dl),
     *         		max0,scale,1,vheight,1)
        	endif
C ok callback !
        else if(flag.eq.3) then
	    p0 = pc0 + p1*(0.5-x0)
            ph0 = p0
            ph1 = p1
            write (str,10) p0,p1
10          format(' 0 order =',f8.1,' 1 order =',f8.1,' ')
            call gifaout(str)
	endif
      return
      end

C==================================================
        subroutine display_pivot_point(temp1,p0,p1,x0,vd_id,flag)
C affichage du trait vertical durant le PH
        implicit none
#include "zoomparamv.inc"
#include "paramfilev.inc"
#include "disptypev.inc"

        integer vd_id,flag
        real delta,x0,xxl,xxu,temp1,p0,p1

	delta = 0.002

	if(flag.eq.3) then  		 ! recalcul du pivot
          if (zoom.ne.0)                 ! move in in zoom
     *          temp1 = (temp1*(zo1du-zo1dl+1)+zo1dl-1)/sizeimage1d
          p0=p0+p1*(temp1-x0)
          x0=temp1
	endif

        if (zoom.eq.0) then
		xxl = x0 - delta
		xxu = x0 + delta
        else
		xxl = (x0*sizeimage1d - zo1dl + 1) / (zo1du-zo1dl+1) - delta
		xxu = (x0*sizeimage1d - zo1dl + 1) / (zo1du-zo1dl+1) + delta
        endif

        call gcolor(vd_id, scolor)
	call win_plot_1d(vd_id,xxl,0.0,xxl,1.0)
	call win_plot_1d(vd_id,xxu,0.0,xxu,1.0)
        call gcolor(vd_id, color)
      return
      end

C==================================================
        subroutine get_var_3d(retalpha,retbeta,retgama,retsc3d,retz0,
     *                   retox,retoy,retoz,retdx3d,retdy3d,retdz3d)
        implicit none
        real retalpha,retbeta,retgama,retsc3d,retz0,
     *     retox,retoy,retoz,retdx3d,retdy3d,retdz3d
#include "disp3dv.inc"
#include "plotcomv.inc"
        retalpha = alpha
        retbeta = beta
        retgama = gama
        retsc3d = scale3d
        retz0 = znot
        retox = off3dx
        retoy = off3dy
        retoz = off3dz
        retdx3d = cx
        retdy3d = cy
        retdz3d = cz
      return
      end

C==================================================
        subroutine set_var_3d(vd_id,setalpha,setbeta,
     *        setgama,setsc3d,setz0,
     *        setox,setoy,setoz,setdx3d,
     *        setdy3d,setdz3d)

C set last variable parameters
        implicit none
	integer vd_id
        real setalpha,setbeta,setgama,setsc3d,setz0,
     *     setox,setoy,setoz,setdx3d,setdy3d,setdz3d,z
#include "constant.inc"
#include "disp3dv.inc"
#include "plotcomv.inc"
C set 3D param,
        cx = setdx3d
        cy = setdy3d
       	cz = setdz3d
        call g3d_setalpha(setalpha*pi/180)
        call g3d_setbeta(setbeta*pi/180)
        call g3d_setgama(setgama*pi/180)
        call g3d_setscale(setsc3d)
        call g3d_setznot(setz0)
        call g3d_setoff(setox,setoy,setoz)
        alpha = setalpha
        beta = setbeta
        gama = setgama
        scale3d = setsc3d
        znot = setz0
        off3dx = setox
        off3dy = setoy
        off3dz = setoz

C display cube
        call plot3d(0,vd_id,10,10,10,z,
     *    1.0,0.0,0,0,setdx3d,setdy3d,setdz3d,
     *    1,1,1.0,1,0,1,1,1,1,1,1,1)
      return
      end

C==================================================
        subroutine display_icon(simax,s1,s2,bitmap)
C used to set the 'catch spectrum' small icon

      implicit none
      integer simax
      integer s1,s2

#include "zoomparamv.inc"
#include "sizeparam.inc"
#include "paramfilev.inc"
#include "disptypev.inc"
#include "disp2d.inc"
#vms      byte bitmap(simax*simax)
#alliant      byte bitmap(simax*simax)
#sgi      byte bitmap(simax*simax)
#sun      byte bitmap(simax*simax)
#adec      byte bitmap(simax*simax)
#aix      integer*1 bitmap(simax*simax)
#hp      byte bitmap(simax*simax)
#f2c      byte bitmap(simax*simax)
      integer s1im, s2im
      call fill_bitmap(simax,si2im,si1im,plane2d,max0,
     *          scale,itype,sign,0,zolf1,zolf2,
     *          zour1,zour2,s1im,s2im,bitmap)
C and display
      s1 = s1im
      s2 = s2im
      return
      end

C==================================================
      subroutine get_zoom_coord(llx,lly,urx,ury,sizey,sizex)
C returns the current zoom coord.
      implicit none
#include "zoomparamv.inc"
#include "paramfilev.inc"

      real llx,lly,urx,ury
      integer*4 sizey,sizex
	sizey = si1im
	sizex = si2im
	if (zoom.ne.0) then
	  llx = real(zolf2)
	  lly = real(zolf1)
	  urx = real(zour2)
	  ury = real(zour1)
        else
          llx = 0.0
          lly = 0.0
          urx = 0.0
          ury = 0.0
        endif
      return
      end

C==================================================
      subroutine set_select_var(ll1, ll2, ur1, ur2)
c
c IN : ll1, llf2, ur1, ur2
c

      implicit none
#include "zoomparamv.inc"
#include "paramfilev.inc"

      integer size,size1,size2,zl1,zl2,zu1,zu2
      real ur1,ur2,ll1,ll2

          if (dim.eq.1) then
             if (zoom.eq.0) then
                selectx1 = ll2*(sizeimage1d) + 1
                selectx2 = ur2*(sizeimage1d) + 1
             else
                size = zo1du-zo1dl+1
                selectx2 = int(ur2*size) + zo1dl
                selectx1 = int(ll2*size) + zo1dl
             endif
             selectx1 = max(1,selectx1)
             selectx2 = min(selectx2,sizeimage1d)
             if (it1d.eq.1) then                ! check if complex
               selectx1 = 2*(zo1dl/2)+1
               selectx2 = 2*(zo1du/2)
             endif
          elseif (dim.eq.2) then
             if (zoom.eq.0) then
                zl1 = zolf1
                zl2 = zolf2
                zu1 = zour1
                zu2 = zour2
                selectx1 = ll2*(si2im) + 1
                selectx2 = ur2*(si2im) + 1
                selecty1 = ll1*(si1im) + 1
                selecty2 = ur1*(si1im) + 1
             else
                size1 = zour1-zolf1+1
                size2 = zour2-zolf2+1
                selectx2 = int(ur2*size2) + zolf2
                selecty2 = int(ur1*size1) + zolf1
                selectx1 = int(ll2*size2) + zolf2
                selecty1 = int(ll1*size1) + zolf1
             endif

             selectx1 = max(1,selectx1)
             selectx2 = min(selectx2,si2im)
             selecty1 = max(1,selecty1)
             selecty2 = min(selecty2,si1im)
             if (itype.eq.1 .or. itype.eq.3) then  ! check if complex in F2
               selectx1 = 2*(zolf2/2)+1
               selectx2 = 2*(zour2/2)
             endif
             if (itype.eq.2 .or. itype.eq.3) then  ! check if complex in F1
               selecty1 = 2*(zolf1/2)+1
               selecty2 = 2*(zour1/2)
             endif
             size1 = selectx2-selectx1
             size2 = selecty2-selecty1
             if(size1.le.1.or.size2.le.1) then
                selecty1 = zl1
                selectx1 = zl2
                selecty2 = zu1
                selectx2 = zu2
            endif
          endif
        return
      end

C==================================================
      subroutine which_view(id,st,st_l)
c
c IN : id
c OUT : st,st_l
c
c returns the view name from its id (found in variable table)
c
      implicit none
      integer id, st_l
      character*(256) st
      
      character*4 st4		! room for one int
      integer ist
      equivalence (ist,st4)
      integer i,j,k
#include "variable.inc"
      character*(varnmlen)  nom
  
      ist = id                ! string to find is now in st4
      nom='[not_found]'
      do i=1,varmax
         if (conttab(i).eq.20) then
            if (varval(i)(1:4).eq.st4) then
               nom=varname(i)
               goto 10
            endif
         endif
      enddo

 10   j=index(nom,'[')
      k=index(nom,']')
      st = nom(j+1:k-1)
      call trailing(st,st_l)
      return
      end
      


