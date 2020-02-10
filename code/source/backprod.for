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

         subroutine backprod(v,u,si1,si2) 

c IN	: u,si1,si2
c OUT	: v
c
c computes V = U . transp(U)
c where U is (si2,si1), and V (si2,si2)

         implicit none
         integer si1,si2,i,j,k
         real u(si2,si1),v(si2,si2),s

        do j=1,si2
         do i=1,si2
           s = 0.0
           do k=1,si1
              s = s + u(i,k)*u(j,k)
           enddo
           v(i,j) = s
         enddo
        enddo


        return
       end
