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
       subroutine mskintg(tab,masktab,taille1,taille2,
     *    noise,shift,signpic)
c tab(taille1,taille2): 2D experiment array
c makstab(taille1,taille2): amobea array
c noise: noise level (calculated by EVALN)
c shift: shift (calculated by EVALN)
c 
c IN	: tab,masktab,taille1,taille2,noise,shift,signpic
c OUT	: peak2d
c
c calculate the volume of all the peaks from the amobea table information
c and put the volume in peak2d(*,3) and incertitude in peak2d(*,6)
       implicit none

#include "sizebase.inc"
#include "peaksize.inc"

       integer taille1,taille2,i,j,zz,signpic
       real    tab(taille2,taille1),
     *         masktab(taille2,taille1),noise,shift,val

       if (noise.eq.0.0)  
     * call gifaout('Warning: NOISE is 0.0, and the intensity errors will 
     * not be calculated: use EVALN')

       do i=1,nbpic2d
         peak2d(i,3) = 0.0
         peak2d(i,6) = 0.0
       enddo
c
c calculation of the volumes and of the number of points per peak
c
       do i=1,taille1
         do j=1,taille2
            if (masktab(j,i).ne.0.0) then
              zz = int(masktab(j,i))
c              write(*,*)'zz',zz,'i,j',i,j
              peak2d(zz,3) = peak2d(zz,3) + tab(j,i)
              peak2d(zz,6) = peak2d(zz,6) + 1.0
            endif
         enddo
       enddo
c
c incertitude calculation and volume correction using shift
c
       do i=1,nbpic2d
         if (peak2d(i,3).ne.0) then
            peak2d(i,3) = peak2d(i,3)-(signpic*peak2d(i,6)*shift)
            peak2d(i,6) = sqrt(peak2d(i,6))*noise*100/abs(peak2d(i,3))
         else
            peak2d(i,6) = -1000.0
         endif
       enddo

       return
       end

      
























