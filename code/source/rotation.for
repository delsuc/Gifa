C This file is a part of the GIFA program
C     This software has been developped by the NMR Group in GIF/Yvette.
C
C     Authors :       D.Abergel M.A.Delsuc
C                     NMR Laboratory,
C                     ICSN - CNRS
C                     91190 GIF/YVETTE   FRANCE
C
C     This software cannot be used unless the user have been fully
C licensed to do so form the above laboratory.
C     This file cannot be copied, duplicated or used in any other program,
C without written permission from the authors.
C

          subroutine rotation(matrix,si1,si2,iter,miter,tol,verb)
c
c part of diagonalisation package, ROTATION command
c
          Implicit None
          integer si1,si2,iter,miter,verb
          real tol
          real matrix(si2,si1)

#include "rotation.inc"

           Integer i,j,l,g
           real fmini,tempmini,tempminj
           real ax,xx,bx,fa,fx,fb,minimum,famini
           external famini

          n = si2
          p = si1

          do g=1,iter
100         do i=1,p-1
              indicei = i
              do l = 1,n
                  wil(l) = matrix(l,indicei)
              enddo
              do j=i+1,p
                indicej = j
                do l = 1,n
                   wjl(l) = matrix(l,indicej)
                enddo
                ax = 0.0
                xx = 1.0
                bx = 2.0
                call braket(ax,xx,bx,fa,fx,fb,famini,verb)
                call brentmini(ax,xx,bx,famini,tol,miter,
     *                         minimum,fmini,verb)
c               fmini = famini(xx)
c calcul de la matrice obtenue: R(ij)* U
                do l=1,n
                  tempmini = matrix(l,i)
                  tempminj = matrix(l,j)
                  matrix(l,i) =  cos(minimum)*tempmini + 
     *                               sin(minimum)*tempminj
                  matrix(l,j) = -sin(minimum)*tempmini +  
     *                               cos(minimum)*tempminj 
                enddo
              enddo
           enddo
         enddo

        return
        End

c*************************************************
      function famini(x)
c
c part of diagonalisation package, ROTATION command
c

      implicit none
      real x,famini
#include "rotation.inc"
      integer l
      real tempi,tempj,s1
      real wil_loc(ld) , wjl_loc(ld)

      
      do l=1,n
         tempi = wil(l)
         tempj = wjl(l)
         wil_loc(l) =  cos(x)*tempi + sin(x)*tempj
         wjl_loc(l) = -sin(x)*tempi + cos(x)*tempj
      enddo 
      s1 = 0.0
c on calcule somme( || abs(x)-x || )
      do l=1,n
          s1 = s1 + (wil_loc(l)-abs(wil_loc(l)))**2 
     *            + (wjl_loc(l)-abs(wjl_loc(l)))**2
      enddo

      famini = s1
      return
      end

