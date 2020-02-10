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
      subroutine fftcpx(fdata,size,sign)
c IN	: size,sign
c INOUT	: fdata
C This subroutine performs an in-place complex fourier-transform
C on the complexe array fdata of length SIZE.
C SIGN = 1 for direct FT and -1 for inverse FT
C from numerical recipes, Cambridge univ. press W.H.Press et al.

      implicit none

      integer size,sign
      real fdata(2*size)

#include "constant.inc"
      real*8 wr,wi,costheta,sintheta,wtemp,theta,dtwopi
      real tempr,tempi,swr,swi
      integer i,j,n,m,mmax,step

#vms      call Lib$flt_under(0)
      dtwopi = sign*2.0d0*dpi
C first bit-reversal of the array (decimation in time)
      n = 2*size
      j = 1
      do 100 i=1,n,2
               if (j.gt.i) then
                   tempr = fdata(j)
                   tempi = fdata(j+1)
                   fdata(j) = fdata(i)
                   fdata(j+1) = fdata(i+1)
                   fdata(i) = tempr
                   fdata(i+1) = tempi
                endif
                m = n/2
  200           if ((m.ge.2).and.(j.gt.m)) then   ! while...do
                   j = j-m
                   m = m/2
                   goto 200
                endif
                j = j+m
100   continue

C here start the butterfly
      mmax = 2                       ! runs on power of two
300   if (n.gt.mmax) then                      ! while...do
           step = 2*mmax
           theta = dtwopi/mmax                 ! theta is the angle
           costheta = -2.D0*dsin(0.5D0*theta)**2
           sintheta = dsin(theta)
           wr = 1.D0                        ! used for recursion
           wi = 0D0
           swr = sngl(wr)
           swi = sngl(wi)
           do 400 m=1,mmax,2
              do 401 i=m,n,step
                j = i+mmax
                tempr = swr*fdata(j) - swi*fdata(j+1)
                tempi = swr*fdata(j+1) + swi*fdata(j)
                fdata(j) = fdata(i)-tempr
                fdata(j+1) = fdata(i+1)-tempi
                fdata(i) = fdata(i)+tempr
                fdata(i+1) = fdata(i+1)+tempi
  401         continue
              wtemp = wr
              wr = wr*costheta - wi*sintheta + wr
              wi = wi*costheta + wtemp*sintheta + wi
              swr = sngl(wr)
              swi = sngl(wi)
 400         continue
           mmax = step
      goto 300
      endif
      return
      end
