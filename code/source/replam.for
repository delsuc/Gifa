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
       subroutine replam(masktab,taille1,taille2,
     *    nopkold,nopknew)
c
c makstab(taille1,taille2): amobea array
c nopkold: old peak number
c nopknew: new peak number
c 
c IN    : masktab,taille1,taille2,nopkold,nopknew
c OUT   : masktab
c
c replace the amoeba of peak number nopkold by the peak number nopknew
c
       implicit none

#include "sizebase.inc"
#include "peaksize.inc"

       integer taille1,taille2,i,j,nopkold,nopknew
       real    masktab(taille2,taille1)

       do i=1,taille1
         do j=1,taille2
            if (masktab(j,i).eq.real(nopkold)) then
              masktab(j,i) = real(nopknew)
            endif
         enddo
       enddo

       return
       end

