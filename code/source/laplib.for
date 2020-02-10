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
C

C Set of fortran routines for laplace transform.

C================================================================
      subroutine laplace(A,n,B,p,KK1,KK2,Tmax)
C performs the Laplace transform between dampings KK1 and KK2
C i.e. given a series of damping amplitudes,
C      exponentially spaced between KK1 and KK2
C      the time response F(t) 0..Tmax is computed
C           over linearly sampled time points
C
C     A(N) : the array of damping amplitudes
C     B(P) : the computed time response
C
C IN :  N, P, A(N), KK1, KK2, Tmax
C OUT : B(P)

      implicit none

      integer n, p
      real a(n), b(p), KK1, KK2, tmax

      real K, kcurr, sum, tt
      integer i,j

C KK_j = KK_1*exp(K*(j-1))
C KK_n = KK_2
C K = (log(KK_2)-log(KK_1))/(n-1)
      K = (log(KK2)-log(KK1))/(n-1)

      call prpvect(b, 0.0, p)

      do j = 1, n            ! loop over data values
         tt = KK1*exp(K*(j-1))
         kcurr = -tt*Tmax/(p-1)
c#d         write(*,*) j, A(j), tt, kcurr
         do i = 1, p            ! loop over time points
            b(i) = b(i) + A(j)*exp( (i-1) * kcurr)
c   Bi = sum(j=1..n) [ Aj exp( (i-1)/(p-1) Tmax Kj) ]
C   Bi = sum(j=1..n) [ Aj Lij ]
         enddo
      enddo
      
      return
      end

C================================================================
      subroutine tlaplace(A,n,B,p,KK1,KK2,samp)
C performs the Laplace transform between dampings KK1 and KK2
C i.e. given a series of damping amplitudes,
C      exponentially spaced between KK1 and KK2
C      the time response F(t(i)) is computed
C          over tabulated sample point t(i)
C
C     A(N) : the array of damping amplitudes
C     B(P) : the computed time response
C
C IN :  N, P, A(N), KK1, KK2, Tmax
C OUT : B(P)

      implicit none

      integer n, p
      real a(n), b(p), samp(p), KK1, KK2

      real K, kcurr, sum
      integer i,j

C KK_j = KK_1*exp(K*(j-1))
C KK_n = KK_2
C K = (log(KK_2)-log(KK_1))/(n-1)
      K = (log(KK2)-log(KK1))/(n-1)

      call prpvect(b, 0.0, p)

      do j = 1, n            ! loop over data values
         kcurr = -KK1*exp(K*(j-1))
c#d         write(*,*) j, A(j), kcurr
         do i = 1, p            ! loop over time points
            b(i) = b(i) + A(j)*exp( samp(i) * kcurr)
         enddo
      enddo
      
      return
      end

C================================================================
      subroutine translap(tdata,timage)

C     IN :timage
C     OUT:tdata
C
C implements the elementary transform step for MaxEnt inverse Laplace transform

C   Bi = sum(j=1..n) [ Aj Lij ]
C   B == tdata  /  A == timage

      implicit none
#include "sizeparam.inc"
#include "paramfilev.inc"
#include "paramdatav.inc"
#include "laplacev.inc"

      real tdata(*),timage(*)

      call laplace(timage,sizeimage1d,tdata,sizedata,
     *     dmin,dmax,(0.5*float(sizedata-1)/sp1d) )
      return
      end



C================================================================
      subroutine ttranslap(timage,tdata)

C     IN :tdata
C     OUT:timage
C
C implements the elementary transpose transform step
C     for MaxEnt inverse Laplace transform
C
C   Aj = sum(i=1..p) [ Bi Lij ]
C   avec Lij = exp( (i-1)/(p-1) Tmax Kj)

C   B == tdata  /  A == timage

      implicit none
#include "sizeparam.inc"
#include "paramfilev.inc"
#include "laplacev.inc"
#include "paramdatav.inc"


      real tdata(*),timage(*)

      integer j, i
      real k, tmax, kcurr, tt

      K = (log(dmax)-log(dmin))/(sizeimage1d-1)
      tmax = 0.5*(sizedata-1)/sp1d

      call prpvect(timage, 0.0, sizeimage1d)

      do j = 1, sizeimage1d            ! loop over output values
         tt = dmin*exp(K*(j-1))
         kcurr = -tt*Tmax/(sizedata-1)
         do i = 1, sizedata            ! loop over data points
            timage(j) = timage(j) + tdata(i)*exp( (i-1) * kcurr)
         enddo
      enddo

      return
      end

C================================================================
      subroutine transtlap(tdata,timage)

C     IN :timage
C     OUT:tdata
C
C implements the elementary transform step for MaxEnt inverse Laplace transform
C   for tabulated sampling value

      implicit none
#include "sizeparam.inc"
#include "paramfilev.inc"
#include "paramdatav.inc"
#include "laplacev.inc"

      real tdata(*),timage(*)

      call tlaplace(timage,sizeimage1d,tdata,sizedata,
     *     dmin,dmax,sampling )
      return
      end



C================================================================
      subroutine ttranstlap(timage,tdata)

C     IN :tdata
C     OUT:timage
C
C implements the elementary transpose transform step
C     for MaxEnt inverse Laplace transform

      implicit none
#include "sizeparam.inc"
#include "paramfilev.inc"
#include "laplacev.inc"
#include "paramdatav.inc"

      real tdata(*),timage(*)

      integer j, i
      real k, tmax, kcurr

      K = (log(dmax)-log(dmin))/(sizeimage1d-1)
      tmax = sizedata/sp1d

      call prpvect(timage, 0.0, sizeimage1d)

      do j = 1, sizeimage1d            ! loop over output values
         kcurr = -dmin*exp(K*(j-1))
         do i = 1, sizedata            ! loop over data points
            timage(j) = timage(j) + tdata(i)*exp( sampling(i) * kcurr)
         enddo
      enddo

      return
      end


