C This file is a part of the GIFA program
C     This software has been developped by the NMR Group in GIF/Yvette.
C
C     Authors :       M.A.Delsuc
C                     NMR Laboratory,
C                     ICSN - CNRS
C                     91190 GIF/YVETTE   FRANCE
C
C     This software cannot be used unless the user have been fully 
C licensed to do so form the above laboratory.
C     This file cannot be copied, duplicated or used in any other program,
C without written permission from the authors.
C
      subroutine display1d(vd_id,size,spectrum,max0,scaling,mode,vh,ref)

c IN	: vd_id,size,spectrum,max0,scaling,mode,vh,ref
c
C displays array spectrum(size) on visual display vd_id, with scaling 
C if mode.eq.0 display every thing, if mode.eq.1 only one over two
C modified to compute mean over undisplayed points
C   (more realistic on gif data-sets)

      implicit none

      integer size,vd_id,mode,ref
      real spectrum(size),scaling,max0,vh

      integer i, j, di, ddi, k, dsize
      real screen, dx, zero, ym
      real x(2048),y(2048)
      integer color


      if (scaling.eq.0) then
         write (*,*) 'no display scaling is 0'
         return
      endif                     

C compute step and size of image
      dsize = size
      di = 1
      ddi = 1

      if (mode.eq.1) then
         dsize = dsize/2
         di = di*2
      endif

      if (ref.eq.1) call win_erase(vd_id)

C finds maximum and minimum in case no absolute scale has 
C previously been defined in max0
      if (max0.eq.0.0) then              
       do i=1,size,di
        max0 = max(abs(spectrum(i)),max0)
       enddo
      endif
      if (max0.eq.0.0) max0=1.0

      do while (dsize.gt.2048)
         dsize = dsize/2
         ddi = ddi*2
      enddo
                     
C plots
      zero = vh
      screen = max0/(0.65)

      dx = 1.0/float(dsize-1)        ! when you display n points
      x(1) = 0.0                     ! that makes n-1 vectors !
      ym = 0.0
      do j = 1,di*ddi,di
         ym = ym + spectrum(j)
      enddo
      y(1) = scaling*ym/(screen*ddi) + zero
      k=1
      do i=di*ddi,size-1,di*ddi
          k=k+1
          x(k) = x(k-1)+dx
          ym = 0.0
          do j=1,ddi*di,di        ! loop over skipped points to display their mean
            ym = ym + spectrum(i+j)
          enddo
          y(k) = scaling*ym/(screen*ddi) + zero
      enddo
      color=64
      call win_plot_array(vd_id,k,x,y)
      call win_update(vd_id)
      end
       

