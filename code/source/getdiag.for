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

C*********************************************************************
        subroutine getdiag(diag,size,smx,si1,si2)
c IN	: size,smx,si1,si2
c OUT	: diag
C this subroutine extracts the main diagonal from the 2D array smx
C and puts it into the array diag

        implicit none

        integer size,si1,si2,i,j,z
        real diag(size),smx(si2*si1)

        if (si1.gt.si2) then
          z = si1/si2
          do i=1,si2
            do j=0,z-1
             diag((i-1)*z+j+1) = smx((j+(i-1)*z)*si2+i)
            enddo
          enddo
        else
          z = si2/si1
          do i=1,si1
            do j=1,z
             diag((i-1)*z+j) = smx((i-1)*(si2+z)+j)
            enddo
          enddo
        endif

100     return
        end 

C*****************************************************************
        subroutine getdiag2d(diag,size,smx,si1,si2,si3,zz)
c IN	: size,smx,si1,si2,si3,zz
c OUT	: diag
C this subroutine extracts one of the 3 diagonal planes form the 3D 
C array smx and puts it into the array plane2d
c zz determines which plane to be extracted
c zz = 3 : 
c zz = 5 : 
c zz = 6 : 

        implicit none

        integer size,si1,si2,si3,i,j,zz,z
        real diag(size),smx(si3*si2*si1)

        if (zz.eq.3) then
         if (si1.ge.si2) then
          z = si1/si2
          do i=1,si2
            do j=0,z-1
             call copvect(diag(si3*z*(i-1)+si3*j+1),
     *                    smx(si2*si3*(i+j-1)+si3*(i-1)+1),si3)
            enddo
          enddo
         else
          z = si2/si1
          do i=1,si1
            do j=0,z-1
             call copvect(diag(si3*z*(i-1)+j*si3+1),
     *                    smx((si2*si3+si3*z)*(i-1)+1),si3)
            enddo
          enddo
         endif
        elseif (zz.eq.5) then
         if (si1.ge.si3) then
           z = si1/si3
           do i=1,si3
             do j=0,z-1
              call getcol(diag(si2*z*(i-1)+si2*j+1),
     *                    smx(si2*si3*(i+j-1)+1),i,si2,si3)
             enddo
           enddo
         else
           z = si3/si1
           do i=1,si1
             do j=0,z-1
              call getcol(diag(si2*z*(i-1)+si2*j+1),
     *                    smx(si2*si3*(i-1)+1),z*(i-1)+j,si2,si3)
             enddo
           enddo
         endif
        elseif (zz.eq.6) then
         z = max(si2,si3)
         do i=1,si1
          call getdiag(diag((i-1)*z+1),z,
     *                 smx((i-1)*si2*si3+1),si2,si3)
         enddo
        else
          write(*,*) 'error with value'
        endif
100     return
        end 
