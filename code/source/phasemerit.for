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
       function phasemerit(phase)
c IN	: phase
C merit function for auto phase
C phase(4) is the phase parameters
C phase(1), (2) are the oth order, (3),(4) are the 1st order.

       implicit none
                 
#include "constant.inc"
#include "conjgrad.inc"
#include "sizeparam.inc"
#include "paramfilev.inc"          

       real       phasemerit,phase(4),pm,cc,scal,penal
       integer   i,j         

       penal = 10.0
       scal = (si1im*si2im)*max0
       scal = 1.0/scal
       cc = 180.0/pi
       call copvect(work,image,si1im*si2im)
       call phase2d(work,si1im,si2im,phase(1)*cc,
     *              phase(3)*cc,1)
       call phase2d(work,si1im,si2im,phase(2)*cc,
     *              phase(4)*cc,2)

       pm= 0.0
       do j= 1,si1im,2
         do i= 1,si2im,2
C  This is a penalty for negative points
           if (work(i+(j-1)*si2im) .gt. 0) then
              pm= pm + work(i+(j-1)*si2im)    ! sum( rr(ij) )
           else
              pm= pm + penal*work(i+(j-1)*si2im)    ! sum( rr(ij) )
           endif
         enddo
       enddo
       phasemerit = -scal*pm
       return
       end
C********************************************************************
       function merit1d(x)
c IN	: x
C 1d version of phasemerit
C x a real, parameting along axis, starting from start (in conjgrad.inc)
       implicit none

#include "sizeparam.inc"
#include "paramfilev.inc"
#include "conjgrad.inc"

       real    merit1d,x,phase(4),phasemerit

       call addvect(phase,start,axis,x,4)
       merit1d=phasemerit(phase)

       return
       end
C********************************************************************
       subroutine dphasemerit(phase,dphase)
c IN	: phase
c OUT	: dphase
C  dphase is the gradient of the function phasemerit at point phase.
C the first two elements are order 0 and 1 in dim1
C the two others are order 0 and 1 in dim2.
 
       implicit none
                                               
#include "conjgrad.inc"
#include "constant.inc"
#include "sizeparam.inc"
#include "paramfilev.inc"

       integer   i,j
       real      phase(4),dphase(4)
       real      dp,cc,scal,dd,ee,penal


       penal = 10.0
       scal = (si1im*si2im)*max0
       scal = 1.0/scal
       cc = 180.0/pi
       dd=si1im/2
       ee=si2im/2
C first apply phase
       call copvect(work,image,si1im*si2im)
       call phase2d(work,si1im,si2im,phase(1)*cc,
     *              phase(3)*cc,1)
       call phase2d(work,si1im,si2im,phase(2)*cc,
     *              phase(4)*cc,2)
C sum on IR
       dp = 0.0
       do j=1,si1im,2
          do i=1,si2im,2
            if (work(i+(j-1)*si2im) .gt. 0) then
              dp= dp + work(i + j*si2im)
            else
              dp= dp + penal*work(i + j*si2im)
            endif
          enddo     
       enddo
       dphase(1) = dp*scal     !order 0 dim 1

       dp = 0.0            
       do j=1,si1im,2
          do i=1,si2im,2
            if (work(i+(j-1)*si2im) .gt. 0) then
              dp= dp + (j-dd)*work(i + j*si2im)
            else
              dp= dp + penal*(j-dd)*work(i + j*si2im)
            endif
          enddo     
       enddo
       dphase(3) =dp*scal    ! order 1 dim 1

C sum on RI
       dp = 0.0
       do j=1,si1im,2
          do i=1,si2im,2
            if (work(i+(j-1)*si2im) .gt. 0) then
               dp= dp + work(i+1+(j-1)*si2im)
            else
               dp= dp + penal*work(i+1+(j-1)*si2im)
            endif
          enddo     
       enddo
       dphase(2) = dp*scal               !order 0 dim 2 

       dp = 0.0
       do j=1,si1im,2   
         do i=1,si2im,2
           if (work(i+(j-1)*si2im) .gt. 0) then
             dp= dp + (i-ee)*work(i+1+(j-1)*si2im)
           else
             dp= dp + penal*(i-ee)*work(i+1+(j-1)*si2im)
           endif
         enddo
       enddo 
       dphase(4) =dp*scal            ! order 1 dim 2
       return
       end
 
