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
      subroutine autophase(p01,p11,p02,p12,iter,miter,mode,verb)
c IN	: iter,miter,mode,verb
c OUT	: p01,p11,p02,p12
c
C computes the optimum phase corrections for the 2D current 2D data-set.
C use conjugate gradient method. 
C If mode .eq.0   order 0 only will be corrected, order 0 and 1 otherwise
C iter and miter : number of iterations
C if verb.eq.1 verbose output

      implicit none

#include "constant.inc"
#include "sizeparam.inc"
#include "paramfilev.inc"

      real p01,p11,p02,p12
      real phasemerit,merit1d
      real  p(4),tol,fmini,cc,zz
      integer iter,miter,mode,i,j,verb

      external phasemerit,merit1d,dphasemerit       
                             
      if (max0.eq.0.0) 
     *        call mxavect(zz,max0,i,j,image,si1im*si2im)

      cc=pi/180
      p(1)=p01*cc
      p(2)=p02*cc
      p(3)=p11*cc
      p(4)=p12*cc
      tol=1.e-6
C minimize
      fmini=1.0                 ! used if miter .eq. 0
      if (mode.eq.0) then
        call conjgrad(phasemerit,merit1d,dphasemerit,2,p,iter,miter,
     *                tol,fmini,verb)
      else
        call conjgrad(phasemerit,merit1d,dphasemerit,4,p,iter,miter,
     *                tol,fmini,verb)
      endif                                                      

C go back to "normal" values in degrees
      p01=p(1)/cc 
      p11=p(3)/cc
      p02=p(2)/cc
      p12=p(4)/cc
C then apply
      call phase2d(image,si1im,si2im,p01,p11,1)
      call phase2d(image,si1im,si2im,p02,p12,2)

      return
      end
