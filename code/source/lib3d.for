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

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C library for 3D display, this one is based on the basic
C graphic library of GIFA --> will work where GIFA works.
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C  External part
C
C g3D : common to screen and plotter support
C
C g3d_init()
C g3d_setalpha(float *alpha)
C g3d_setbeta(float *beta)
C g3d_setgama(float *gama)
C g3d_setscale(float *scale)
C g3d_setscxyz(scx,scy,scz)
C g3d_setznot(float *znot)
C g3d_setoff(float *x,y,z)
C g3d_config(char *st)
C
C
C on screen support, natural unit is 0..1 on each axis
C
C win3d_open(int *vd_id, float *cx, float *cy,char *title int *length);
C win3d_close(int *vd_id);
C win3d_erase(int *vd_id);
C win3d_point(int *vd_id, float *x1, float *y1, float *z1)
C win3d_plot_line(int *vd_id, float *x1, float *y1, float *z1, float *x2,
C	float *y2, float *z2);
C win3d_write(vd_id,x,y,z,text,length);
C win3d_plot_array(int *vd_id,int *count, float *xv, float *yv, float *zv);
C win3d_update(int *vd_id);
C
C
C on plotter support, natural unit is in centimeters
C
C pl3d_move(int *vd_id, float *x1, float *y1, float *z1)
C pl3d_draw(int *vd_id, float *x1, float *y1, float *z1);
C pl3d_text(vd_id,x,y,z,text,length);
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine g3d_init
c common to all 3D routines
c 
C initializes the 3d space
	implicit none 
#include "lib3d.inc"
      alpha = 0.0
      beta = 0.0
      gama = 0.0
      ca = 1.0
      cb = 1.0
      cg = 1.0
      sa = 0.0
      sb = 0.0
      sg = 0.0
      xr = 0.0
      yr = 0.0
      zr = 0.0
      znot = 10.0
      scale = 0.5
      scx = 1.0
      scy = 1.0
      scz = 1.0
      call rrrup
      return
      end
	  
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine g3d_setalpha(a)
c IN	: a
c common to all 3D routines
c 
c set alpha angle
      real a
#include "lib3d.inc"
      alpha = a
      ca = cos(alpha)
      sa = sin(alpha)
      call rrrup
      return
      end
	  
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine g3d_setbeta(a)
c IN	: a
c common to all 3D routines
c 
c set beta angle
      real a
#include "lib3d.inc"
      beta = a
      cb = cos(beta)
      sb = sin(beta)
      call rrrup
      return
      end
	  
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine g3d_setgama(a)
c IN	: a
c common to all 3D routines
c 
c set gama angle
      real a
#include "lib3d.inc"
      gama = a
      cg = cos(gama)
      sg = sin(gama)
      call rrrup
      return
      end
	  
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine g3d_setscale(a)
c IN	: a
c common to all 3D routines
c 
c set global scale
      real a
#include "lib3d.inc"
      scale = a
      call rrrup
      return
      end
	  
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine g3d_setscxyz(a,b,c)
c IN	: a,b,c
c common to all 3D routines
c 
c set scale on axes x,y,z
      real a,b,c
#include "lib3d.inc"
      scx = a
      scy = b
      scz = c
      call rrrup
      return
      end
	  
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine g3d_setoff(x,y,z)
c IN	: x,y,z
c common to all 3D routines
c 
c set offset on axes x,y,z
      real x,y,z
#include "lib3d.inc"
      xr = x
      yr = y
      zr = z
      call rrrup
      return
	  end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine g3d_setznot(a)
c IN	: a
c common to all 3D routines
c 
c set z0 view parameter
      real a
#include "constant.inc"
#include "lib3d.inc"
      if (a.gt.epsilon) then
         znot = 1/a
      else
         znot = 0.0
      endif
      call rrrup
      return
	  end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine g3d_config(text,l)
c OUT	: text,l
c common to all 3D routines
c 
c returns name of 3D library
      character*(*) text
      integer l
      text = 'Basic 3D module'
      l = len(text)
      return
	  end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine win3d_open(vd_id, cx, cy,title,length)
C IN	: cx, cy,title,length
c OUT	: vd_id
c
c for 3D graphic display
c opens a 3D view port, size is "cx" by "cy" (in cm), name is 
c "title(length)"
c returns id of window, 0 if failed
      implicit none
#include "lib3d.inc"
      integer vd_id,length
      real cx,cy
      character*(*) title
      integer i
      i = 3
      call win_open_gene(vd_id, cx, cy,title,length,0.0,0.0,i)
      return
	  end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine win3d_close(vd_id)
C IN	: vd_id
c
c for 3D graphic display
c close a 3D view port
      integer vd_id
      call win_close(vd_id)
      return
	  end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine  win3d_erase(vd_id)
C IN	: vd_id
c
c for 3D graphic display
c erase the content of the window
      integer vd_id
      call win_erase(vd_id)
      return
	  end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine  win3d_point(vd_id, x1, y1, z1)
C IN	: vd_id,x1,y1,z1
c
c for 3D graphic display
c draws a point at location x1,y1,z1
      integer vd_id
      real x1,y1,z1,xl,yl
      call transf3d(xl,yl,x1,y1,z1)
      call win_plot_1d(vd_id, xl, yl, xl, yl)
      return
	  end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine win3d_plot_line(vd_id, x1, y1, z1, x2,	y2, z2)
C IN	: vd_id,x1,y1,z1,x2,y2,z2
c
c for 3D graphic display
c draws a line from x1,y1,z1 to x2,	y2, z2
      integer vd_id
      real x1,y1,z1,x2,y2,z2,xl1,yl1,xl2,yl2
      call transf3d(xl1,yl1,x1,y1,z1)
      call transf3d(xl2,yl2,x2,y2,z2)
      call win_plot_1d(vd_id, xl1, yl1, xl2, yl2)
      return
	  end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine win3d_write(vd_id,x,y,z,text,length)
C IN	: vd_id,x,y,z,text,length
c
c for 3D graphic display
c draws text at location x,y,z

      integer vd_id,length
	  character*(*) text
      real x,y,z,xl,yl
      call transf3d(xl,yl,x,y,z)
      call win_write(vd_id,xl,yl,text,length)
      return
	  end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine win3d_plot_array(vd_id,count,xv,yv,zv)
C IN	: vd_id,count,xv,yv,zv
c
c for 3D graphic display
c draws a polyline at locations "xv(count)" "yv(count)" "zv(count)"
      integer vd_id,count,i,buf_max
      parameter (buf_max=32*1024)
      real xv(count),yv(count),zv(count),xl(buf_max),yl(buf_max)
c
      do i =1,min(buf_max,count)
         call transf3d(xl(i),yl(i),xv(i),yv(i),zv(i))
      enddo
      call win_plot_array(vd_id,count,xl,yl)
      return
	  end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine win3d_update(vd_id)
C IN	: vd_id
c
c for 3D graphic display
c realizes contens of window
      integer vd_id
      call win_update(vd_id)
      return
	  end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine  pl3d_move(vd_id, x1, y1, z1)
C IN	: vd_id, x1, y1, z1
c
c for 3D plot
c move pen to x1, y1, z1
      integer vd_id
      real x1,y1,z1,xl,yl
      call transf3d(xl,yl,x1,y1,z1)
      call plmove(vd_id, xl, yl)
      return
	  end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine  pl3d_draw(vd_id, x1, y1, z1)
C IN	: vd_id, x1, y1, z1
c
c for 3D plot
c draw from current position to x1, y1, z1
      integer vd_id
      real x1,y1,z1,xl,yl
      call transf3d(xl,yl,x1,y1,z1)
      call pldraw(vd_id, xl, yl)
      return
	  end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine pl3d_text(vd_id,x,y,z,text,length)
C IN	: vd_id, x1, y1, z1
c
c for 3D plot
c draw text at position x1, y1, z1
      integer vd_id,length
       character*(*) text
      real x,y,z,xl,yl
      call transf3d(xl,yl,x,y,z)
      call pltext(vd_id,text,length,xl,yl)
      return
	  end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C  Internal part
C
C	Should not be used by a regular program
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


      subroutine rrrup()
c internal 3D library
c
c updates rotation matrix

      implicit none
#include "lib3d.inc"
c this one update rrr from parameters (p 96 & 104)

      rrr(1,1) = ca*cb*scale*scx
      rrr(1,2) = sa*cb*scale*scx

      rrr(2,1) = (ca*sb*sg - sa*cg)*scale*scy
      rrr(2,2) = (ca*cg + sa*sb*sg)*scale*scy
      rrr(3,1) = (sa*sg + ca*sb*cg)*scale*scz
      rrr(3,2) = (sa*sb*cg - ca*sg)*scale*scz

      rrr(4,1) = xr
      rrr(4,2) = yr

c      rrr(1,3) = sg*scale*znot*scx
c      rrr(2,3) = -cb*sg*scale*znot*scy
c      rrr(3,3) = -cb*cg*scale*znot*scz
      rrr(1,3) = -sb*scale*znot*scx
      rrr(2,3) = cb*sg*scale*znot*scy
      rrr(3,3) = cb*cg*scale*znot*scz

      rrr(4,3) = 1 - zr*znot
      return
      end


      subroutine transf3d(x2,y2,x3,y3,z3)
c IN	: x3,y3,z3
c OUT	: x2,y2
c internal 3D library
c
C This one will perform the 3D xyz to 2D x,y transform (both in 0..1 coord)
C works in homogenous coordinates.
C (ie its a mult of rrr with x3,y3,z3,t3 (t3 = 1))
c (11 mult & div) (9 plusses) ==> 20 Flop per point
      implicit none
#include "lib3d.inc"
      real x2,y2,x3,y3,z3,xloc,yloc,tloc
c
      xloc = 	rrr(1,1)*x3
     *		+ 	rrr(2,1)*y3
     *		+ 	rrr(3,1)*z3
     *		+ 	rrr(4,1)
      yloc = 	rrr(1,2)*x3
     *		+ 	rrr(2,2)*y3
     *		+ 	rrr(3,2)*z3
     *		+ 	rrr(4,2)
      tloc = 	rrr(1,3)*x3
     *		+ 	rrr(2,3)*y3
     *		+ 	rrr(3,3)*z3
     *		+ 	rrr(4,3)
      x2 = xloc / tloc
      y2 = yloc / tloc
      return
      end


