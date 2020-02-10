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

      program t1fit

      implicit none
      external t1func,dt1func,t1func1d
      real t1func,t1func1d
#include "data.inc"
      integer iter, miter,i,j,k, verb,size
      real a,b,tol,param(3),val,maxt,maxf,minf

      call readdata()

      iter = 30
      miter = 30
      tol = 1e-5
      size = 3
      verb = 0

c  first guess
      maxt = time(1)
      maxf = fdata(1)
      minf = fdata(1)

      do i =2,sizedata
        maxt = max(time(i),maxt)
        minf = min(fdata(i),minf)
        maxf = max(fdata(i),maxf)
      enddo

      param(1) = maxf
      param(2) = (maxf - minf)/maxf
      param(3) = maxt/2.0

      Call Conjgrad(t1func,t1func1d,dt1func,
     *    size,param,Iter,Miter,tol,Val,verb)

      write(*,*) ' error : ',sqrt(val/sizedata)
      write(*,*)'           i        time       data     recomputed'
      do i=1,sizedata
        write(*,*) i,' ',time(i),' ',fdata(i),' ',
     *  param(1)*(1.0 - param(2)*exp(-time(i)/param(3)))
      enddo

      write(*,*)
      write(*,*) 'y = Mo . (1 - B.exp(-t/T1))'
      write(*,*) 'Mo = ',param(1)
      write(*,*) 'B  = ',param(2)
      write(*,*) 'T1 = ',param(3)
      

      end

c************************************************************8
      function t1func(param)
      implicit none
      real t1func,param(3),y,error
#include "data.inc"
      integer i

      error = 0.0
      do i=1,sizedata
         y = param(1)*(1.0 - param(2)*exp(-time(i)/param(3)))
         error = error + (fdata(i)-y)**2
      enddo
      t1func = error
      return
      end

c************************************************************8
      function t1func1d(x)
      implicit none
#include "conjgrad.inc"
      external t1func
      real t1func1d,x,t1func,copie(3)
#include "data.inc"
      integer i

      do i=1,3
         copie(i) = start(i) + x*axis(i)
      enddo
      t1func1d = t1func(copie)
      return
      end
c************************************************************8
      subroutine dt1func(param,dx)
      implicit none
      real param(3),dx(3),y,dm0,db,dt1,dydt,dydb,dydm
#include "data.inc"
      integer i

      db = 0.0
      dt1 = 0.0
      dm0 = 0.0
      do i=1,sizedata
        y = param(1)*(1.0 - param(2)*exp(-time(i)/param(3)))
        dydm = (1.0 - param(2)*exp(-time(i)/param(3)))
        dydb = -param(1)*exp(-time(i)/param(3))
        dydt = -param(1)*param(2)*exp(-time(i)/param(3))*time(i)
     *          /(param(3)**2)
        db = db - 2*(fdata(i)-y)*dydb
        dm0 = dm0 - 2*(fdata(i)-y)*dydm
        dt1 = dt1 - 2*(fdata(i)-y)*dydt
      enddo
      dx(1) = dm0
      dx(2) = db
      dx(3) = dt1
      return
      end

c************************************************************8
      subroutine readdata()
      implicit none
#include "data.inc"
      integer i

      write(*,*) 'number of time points'
      read (*,*) sizedata
      write(*,*) 'enter time value, then amplitude'
      do i=1,sizedata
        write(*,*) ' i =',i
        read(*,*) time(i),fdata(i)
      enddo
      write(*,*) '          i        time       data'
      do i=1,sizedata
        write(*,*) i,' ',time(i),' ',fdata(i)
      enddo
      return
      end
