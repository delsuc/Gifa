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
       subroutine fftreal(fdata,size,sign,direc)
c IN	: size,sign,direc
c INOUT	: fdata
C This subroutine performs an in-place real fourier-transform
C on the real array fdata of length SIZE.
C SIGN = 1 for direct FT and -1 for inverse FT
c DIREC=1 FT real to complex, DIREC=-1 complex to real
C from numerical recipes, Cambridge univ. press W.H.Press et al.

      implicit none
      integer size,sign,direc
      real fdata(size)

#include "constant.inc"
      real*8 wr,wi,wpr,wpi,wtemp,theta
      real c1,c2,wrs,wis,h1r,h1i,h2r,h2i
      integer i, n2p3, i1,i2,i3,i4,size2


#vms      call lib$flt_under(0)
      size2 = size/2
      theta = sign*dpi/dble(size2)
      c1 = 0.5
      c2 = -direc*c1
      if (direc.eq.1) call fftcpx(fdata,size2,sign)
      
      wpr = -2.0d0*dsin(0.5d0*theta)**2
      wpi = dsin(theta)
      wr = 1.0d0+wpr
      wi = wpi
      n2p3 = size+3

      do 100 i=2,(size2/2)+1   ! runs:  2 --> (size/4)+1   i.e. size/4 times
         i1 = 2*i-1            ! runs:  3 --> size/2+1 
         i2 = i1+1             ! runs:  4 --> size/2+2
         i3 = n2p3-i2          ! runs:  size-1 --> size/2+1
         i4 = i3+1             ! runs:  size   --> size/2+2
         wrs = sngl(wr)
         wis = sngl(wi)
         h1r = c1*(fdata(i1)+fdata(i3))
         h1i = c1*(fdata(i2)-fdata(i4))
         h2r = -c2*(fdata(i2)+fdata(i4))
         h2i = c2*(fdata(i1)-fdata(i3))
         fdata(i1) = h1r + wrs*h2r - wis*h2i
         fdata(i2) = h1i + wrs*h2i + wis*h2r
         fdata(i3) = h1r - wrs*h2r + wis*h2i
         fdata(i4) = -h1i + wrs*h2i + wis*h2r
         wtemp = wr
         wr = wr*wpr - wi*wpi +wr
         wi = wi*wpr + wtemp*wpi + wi
100   continue

      if (direc.eq.-1)  fdata(2)=0
      h1r = c1*fdata(1)
      h1i = c1*fdata(2)
      h2r = -c2*fdata(2)
      h2i = c2*fdata(1)

      if (direc.eq.1) then
         fdata(1) = (h1r + h2r)
         fdata(2) = (h1i + h2i)
      else
         fdata(1) = 2*(h1r + h2r)
         fdata(2) = 2*(h1i + h2i)
         call fftcpx(fdata,size2,sign)
      endif
      return
      end
