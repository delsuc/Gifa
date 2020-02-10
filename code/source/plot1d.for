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
      subroutine plot1d(pl_id,size,spectrum,max0,scaling,mode,cx,cy,vh)
c IN	: pl_id,size,spectrum,scaling,mode,cx,cy,vh
c INOUT	: max0
c
C display array spectrum(size) on plotter pl_id, with scaling
C with length cx and heigth cy
C if mode.eq.0 display everything, if mode.eq.1 only one over two  (real/complex)
c vh is the location of the zero level in the window

      implicit none

      integer size, pl_id, mode
      real spectrum(size),scaling,cx,cy,max0,vh

      integer i, di, dsize
      real screen, s1, dx, zero, xcur


#vms      call lib$flt_under(0)

c      if (scaling.eq.0) then
c        write (*,*) 'scaling is 0'
c       return
c   endif    

C computes step                            
      di = 1
      dsize = size
      if (mode.eq.1) then
         dsize = dsize/2
         di = 2
      endif
      dx = cx/(dsize-1)
C finds maximum and minimum in case no absolute scale has 
C previously been defined in max0
      if (max0.eq.0.0) then              
       do i=1,size,di
        max0 = max(abs(spectrum(i)),max0)
       enddo
      endif
      if (max0.eq.0.0) max0=1.0

      zero = vh
      screen = max0/(0.95-zero)
      if (screen.eq.0.0) screen = 1.0

      s1 = cy*(scaling*spectrum(1)/screen + zero)
      s1 = max(min(cy,s1),0.0)
      xcur = 0
      call plmove(pl_id,xcur,s1)

      do i=di+1,size,di
          xcur = xcur + dx
          s1 = cy*(scaling*spectrum(i)/screen + zero)
          s1 = max(min(cy,s1),0.0)
          call pldraw(pl_id,xcur,s1)
      enddo

      call plhome(pl_id)
      return
      end

