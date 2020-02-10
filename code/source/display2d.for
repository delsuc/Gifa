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
      subroutine display2d(vd_id,size2,size1,array,max0,
     *          scaling,mode,signl,
     *          zoom,zolf1,zolf2,zour1,zour2)
c IN    : vd_id,size2,size1,array,max0, scaling,mode,signl,zoom,zolf1,
c
C displays array array(size2,size1) on visual display vd_id, with scaling
C if mode.eq.0 display every thing, if mode.eq.1 only one over two in dim
C 1 if mode.eq.2 only one over two in dim 2, if mode.eq.3,only one over
C two in both dim
C if signl is -1 display only negative points...
C if signl is 1  display only positive points...
C if signl is 0 display both
C if zoom.eq.1  zolf1,zolf2,zour1,zour2 are used as window coordinates

      implicit none
      integer size2,size1,mode,vd_id,
     *    signl,zoom,zolf1,zolf2,zour1,zour2
      real array(size2,size1), scaling

#include "disptypev.inc"
#include "disp2d.inc"
      integer simax
      parameter (simax=512)
#aix      integer*1 bitmap(simax*simax)
#_aix      byte bitmap(simax*simax)
      integer s1im, s2im
      real max0
      call fill_bitmap(simax,size2,size1,array,max0,
     *          scaling,mode,signl,
     *          zoom,zolf1,zolf2,zour1,zour2,s1im,s2im,bitmap)
C and display
      call win_erase(vd_id)
      call win_image_2d(vd_id,s2im,s1im,8,bitmap)
      return
      end


      subroutine fill_bitmap(simax,size2,size1,array,max0,
     *          scaling,mode,signl,
     *          zoom,zolf1,zolf2,zour1,zour2,s1im,s2im,bitmap)
c IN	: size2,size1,array,max0, scaling,mode,signl,zoom,zolf1,zolf2,zour1,zour2
c
C displays array array(size2,size1) on visual display , with scaling 
C if mode.eq.0 display every thing, if mode.eq.1 only one over two in dim
C 1 if mode.eq.2 only one over two in dim 2, if mode.eq.3,only one over
C two in both dim
C if signl is -1 display only negative points...
C if signl is 1  display only positive points...
C if signl is 0 display both
C if zoom.eq.1  zolf1,zolf2,zour1,zour2 are used as window coordinates

      implicit none
      integer size2,size1,mode,s1im,s2im,
     *    signl,zoom,zolf1,zolf2,zour1,zour2
      real array(size2,size1), scaling

#include "disptypev.inc"
#include "disp2d.inc"

      integer simax, zmax, zmin
#aix      integer*1 bitmap(simax*simax)
#_aix      byte bitmap(simax*simax)
      integer di2, di1, i2, i1, k, sz1,
     *    sti1,sti2
      real max0,zz

C test parameters
      s1im = size1        !size of spectrum to display
      s2im = size2
C      if (dim.eq.2) max0=max2d
C      if (dim.eq.3) max0=max3d
      if (scaling.eq.0.0) then
         write(*,*) 'no display scaling is 0'
         return
      endif
      if (zoom.eq.1) then
         if (zolf1.lt.1 .or. zolf2.lt.1 .or.
     *       zour1.gt.size1 .or. zour2.gt.size2) then
            write(*,*) 'Wrong zoom window'
            return
         endif
         s1im = zour1-zolf1+1
         s2im = zour2-zolf2+1
      endif
C compute step
      sti2 = 1
      sti1 = 1
      if (mod(mode,2).eq.1)  sti2 = 2  ! minimum step
      if (mode/2.eq.1)       sti1 = 2
      di1 = sti1
      di2 = sti2
1     if ((s1im-1)/di1.ge.simax) then
         di1 = di1 + sti1
         goto 1
      endif
2     if ((s2im-1)/di2.ge.simax) then
         di2 = di2 + sti2
         goto 2
      endif
C compute last line imaged
      if (zoom.eq.0) then
          sz1 = di1*( (s1im-1)/di1 ) + 1
      else
          sz1 = di1*( (s1im-1)/di1 ) + zolf1
      endif
      s1im = (s1im-1)/di1 +1     ! size of bitmap image
      s2im = (s2im-1)/di2 +1
C find maximum and minimum in case no absolute scale has 
C previously been defined in max0
      if (max0.eq.0.0) then
       do i1=1,size1,di1
         do i2=1,size2,di2
          max0 = max(abs(array(i2,i1)),max0)
         enddo
       enddo
      endif
C      if (dim.eq.2) max2d=max0
C      if (dim.eq.3) max3d=max0
      if (max0.eq.0.0)  max0 = 1.0
C compute byte array                 

      zz = scaling*(vsize/2)/max0
      k = 1
      if (signl.eq.0) then
        zmin=0
        zmax=vsize
      else if (signl.eq.1) then
        zmin=(vsize/2) - 1
        zmax=vsize
      else if (signl.eq.-1) then
        zmin=0
        zmax=vsize/2
      else
        write(*,*) 'Error with parameters in Display2d'
        return
      endif
      if (zoom.eq.0) then
        do i1=sz1,1,-di1      ! backward to get 1,1 on lower left point
          do i2=1,size2,di2
            bitmap(k) =
     *        max( zmin, min( zmax, 
     *           ((vsize/2) +
     *             int(zz*array( i2, i1 )) ))-1 )
            k = k+1
          enddo
        enddo
      else
        do i1=sz1,zolf1,-di1
          do i2=zolf2,zour2,di2
            bitmap(k) =
     *        max( zmin, min( zmax, 
     *           ((vsize/2) +
     *             int(zz*array( i2, i1 )) ))-1 )
            k = k+1
          enddo
        enddo
      endif
      return
      end
