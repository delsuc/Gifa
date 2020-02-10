C This file is a part of the GIFA program
C     This software has been developped by the NMR Group in GIF/Yvette.
C
C     Authors :       M.A.Delsuc, V.Stoven, M.Robin
C                     NMR Laboratory,
C                     ICSN - CNRS
C                     91190 GIF/YVETTE   FRANCE
C
C     This software cannot be used unless the user have been fully 
C licensed to do so form the above laboratory.
C     This file cannot be copied, duplicated or used in any other program,
C without written permission from the authors.
C
            subroutine topcross(vd_id,x,y,size)
c IN	: vd_id,x,y,size
c
c display on window "vd_id" a cross at location "x" "y" and of size "size"
c
      implicit none

      integer vd_id

      real x,y,size

      if (x.lt.0.0 .or. x.gt.1.0 .or. y.lt.0.0 .or. y.gt.1.0) goto 10
      if (vd_id.eq.0) goto 10
      call win_plot_1d(vd_id,x-size,y,x+size,y)
      call win_plot_1d(vd_id,x,y-size,x,y+size)
      call win_update(vd_id)
10    return
      end

            subroutine topcross2(vd_id,x,y,size,i)
c IN	: vd_id,x,y,size,i
c
c display on window "vd_id" a cross at location "x" "y" and of size "size"
c and writes number "i"

      implicit none

      integer vd_id,i,j

      real x,y,size
      character*12 st

      if (x.lt.0.0 .or. x.gt.1.0 .or. y.lt.0.0 .or. y.gt.1.0) goto 10
      if (vd_id.eq.0) goto 10
      call win_plot_1d(vd_id,x-size,y,x+size,y)
      call win_plot_1d(vd_id,x,y-size,x,y+size)
      write(st,'(i10)')i
      call leading(st)
      call trailing(st,j)
      call win_write(vd_id,x+2*size,y,st,j)
      call win_update(vd_id)
10    return
      end

            subroutine topcross3(vd_id,x,y,size,st)
c IN	: vd_id,x,y,size,i
c
c display on window "vd_id" a cross at location "x" "y" and of size "size"
c and writes string st
      implicit none
      integer vd_id,j
      real x,y,size
      character*(*) st

      if (x.lt.0.0 .or. x.gt.1.0 .or. y.lt.0.0 .or. y.gt.1.0) goto 10
      if (vd_id.eq.0) goto 10
      call win_plot_1d(vd_id,x-size,y,x+size,y)
      call win_plot_1d(vd_id,x,y-size,x,y+size)
      j = len(st)
      call win_write(vd_id,x+2*size,y,st,j)
      call win_update(vd_id)
10    return
      end

            subroutine topcross3d(vd_id,x,y,z,size,i)
c IN	: vd_id,x,y,z,size,i
c
c display on 3D window "vd_id" a 3D cross at location "x" "y" "z"
c and of size "size" and writes number "i"
c
      implicit none
      integer vd_id,i,j
      real x,y,z,size
      character*12 st

      write (*,*) x,y,z,size
c      if (x.lt.0.0 .or. x.gt.1.0 .or. y.lt.0.0 .or. y.gt.1.0
c    *   .or. z.lt.0.0 .or. z.gt.1.0) goto 10
      if (vd_id.eq.0) goto 10
      call win3d_plot_line(vd_id,x-size,y,z,x+size,y,z)
      call win3d_plot_line(vd_id,x,y-size,z,x,y+size,z)
      call win3d_plot_line(vd_id,x,y,z-size,x,y,z+size)
      write(st,'(i10)')i
      call leading(st)
      call trailing(st,j)
      call win3d_write(vd_id,x+2*size,y,z,st,j)
      call win3d_update(vd_id)
      return
10    write (*,*) x,y,z,size,'Error in 3D format'
      return
      end


            subroutine topcross3d3(vd_id,x,y,z,size,st)
c IN	: vd_id,x,y,z,size,st
c
c display on 3D window "vd_id" a 3D cross at location "x" "y" "z"
c and of size "size" and writes string st
c
      implicit none
      integer vd_id,j
      real x,y,z,size
      character*(*) st

      write (*,*) x,y,z,size
c      if (x.lt.0.0 .or. x.gt.1.0 .or. y.lt.0.0 .or. y.gt.1.0
c    *   .or. z.lt.0.0 .or. z.gt.1.0) goto 10
      if (vd_id.eq.0) goto 10
      call win3d_plot_line(vd_id,x-size,y,z,x+size,y,z)
      call win3d_plot_line(vd_id,x,y-size,z,x,y+size,z)
      call win3d_plot_line(vd_id,x,y,z-size,x,y,z+size)
      j = len(st)
      call win3d_write(vd_id,x+2*size,y,z,st,j)
      call win3d_update(vd_id)
      return
10    write (*,*) x,y,z,size,'Error in 3D format'
      return
      end


      subroutine plotcross2(pl_id,x,y,size,i)
c IN	: pl_id,x,y,size,i
c
c draw on plot file "pl_id" a cross at location "x" "y" (in cm) and of size "size"
c and writes number "i"

      implicit none

      integer pl_id,i,j

      real x,y,size
      character*12 st

      call plmove(pl_id,x-size,y)
      call pldraw(pl_id,x+size,y)
      call plmove(pl_id,x,y-size)
      call pldraw(pl_id,x,y+size)
      write(st,'(i10)')i
      call leading(st)
      call trailing(st,j)
      call pltext(pl_id,st,j,x+2*size,y)

10    return
      end

