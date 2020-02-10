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
       subroutine searchamb(nopk,masktab,taille1,taille2)
c tab(taille1,taille2): 2D experiment array
c makstab(taille1,taille2): amobea array
c 
c IN	: tab,masktab,taille1,taille2,nopk
c
c search in masktab, for the pixels located in the amoeba of peak nopk
       implicit none

#include "sizebase.inc"
#include "peaksize.inc"

       integer taille1,taille2,i,j,k,zz,nopk,pixcrd(6,2)
       real    masktab(taille2,taille1)

c
c look for the pixels of amoeba of peak nopk
c
       write(*,*)"Pixel coordinates (F2,F1):"
       zz = 0
       do i=1,taille1
         do j=1,taille2
            if (masktab(j,i).eq.real(nopk)) then
              zz = zz + 1
              pixcrd(zz,1) = j
              pixcrd(zz,2) = i
              if (zz.eq.6) then
                write(*,1000)(pixcrd(k,1),pixcrd(k,2), k= 1,6)
                zz = 0
              endif
            endif
         enddo
       enddo
       write(*,1000)(pixcrd(k,1),pixcrd(k,2), k= 1,zz)
c 1000  format('(',i4,',',i4,')')
 1000  format('(',i4,',',i4,')','(',i4,',',i4,')','(',i4,',',i4,')',
     &'(',i4,',',i4,')','(',i4,',',i4,')','(',i4,',',i4,')')

       return
       end

      






















