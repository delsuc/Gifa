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

C this file contains every thing for exponential fitting
C fitting itself, is done by calling the generic 
c Levenberg-Maquardt minimiser

C MAD - first version 16 march 2001

C**********************************************************************
C initialisation for exp fitting
C**********************************************************************
	  subroutine fitexpinit(p,size,err)
C IN:   size
C OUT:  p
C
C This one initialize every thing for the multi decaying exp (FITEXP)
C
C initial and final values are found in $Pi, $DPi
C much as it is done for FITGENE
C
C size: The number of independent parameters
C p(size) : the fortran array that will hold the fitted parameters
C

      implicit none
#include "conjgrad.inc"
#include "minimize.inc"
#include "variable.inc"
      character*(varcontlen) loc1
      character*(varnmlen) loc2
      integer size,err
      real p(size)
      
      integer k,i
      real*8 zz

      actualsize = size
C get (or create) $Pi
      do i =1,size
         write(loc1,'(I6)') i
         call leading(loc1)
         call trailing(loc1,k)
         loc2 = '$P' // loc1(1:k)
         call checkvar(loc2,20,err)
         if (err.eq.0) then              ! does exist
            call getvar(loc2,20,loc1,err)
            call readval(loc1,zz,err)
            if (err.ne.0) goto 100
            p(i)=zz
         else                            ! otherwise
            loc1 = '1.0'
            err = 0
            call assign(loc2,20,loc1,err)
            if (err.ne.0) goto 100
            p(i) = 1.0
         endif
      enddo
      return

 100  call gifaout( '*** Error in FITEXP initialisation')
      return

      return
      end

C**********************************************************************
C function and derivatives for exp fitting
C**********************************************************************
 
      subroutine mexp(size,param,x,y) 
      implicit none
c IN	: size,param,x
c OUT	: y
c Computes the value of the multi decaying exp, the parameters are 
c found in param
c
c e = sum/i( A/i exp( - T/i * x ) )
c
c
      implicit none

      integer  size
      real     param(size),y,x

      integer  i

      y=0.0
      do i=1,size-1,2
         param(i+1) = abs(param(i+1))
         y = y + param(i)*exp(-param(i+1)*x)
      enddo
      return
      end

C**********************************************************************
      subroutine dmexp(size,param,x,dyda) 
c IN	: size,param,x
c OUT	: dyda
c Computes the derivative of the mexp function at x, the parameters are 
c found in param
c
c

      implicit none

      integer  size
      real     param(size),dyda(size),x

      real     a,b
      integer  i

      do i=1,size-1,2
         a        = param(i)
         b        = exp(-param(i+1) * x)
         dyda(i)  = b
         dyda(i+1)  = -a*x*b
      enddo
      return
      end


