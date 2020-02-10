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
       subroutine picmax(comp,nombrepic)
c IN	: comp,nombrepic
c SIDE	: in peak2d
c
c concatenates peak in a radius of comp
       implicit none

#include "sizebase.inc"
#include "peaksize.inc"

       integer comp,nombrepic,no,i,j,colp,ligp,col,lig,dd
       real    intens,intp
                         
       do no=1,nombrepic-1
           col=int(peak2d(no,1))
           lig=int(peak2d(no,4))
           intens=peak2d(no,3)
           do i=no+1,nombrepic
               colp=int(peak2d(i,1))
               ligp=int(peak2d(i,4))
               intp=peak2d(i,3)
               dd = abs(col-colp) + abs(lig-ligp)
               if (dd.le.comp) then
                 if (intp.lt.intens) then
                   peak2d(i,5) = -no 
                   do j=1,nombrepic
                     if (peak2d(j,5).eq.-i) peak2d(j,5)=-no
                   enddo
                 else              
                   peak2d(no,5)=-i
                   do j=1,nombrepic
                     if (peak2d(j,4).eq.-no) peak2d(j,5)=-i
                   enddo
                 endif
               endif
           enddo
       enddo                          
       return
       end

c************************************************************************       
       subroutine picmax3d(comp,nombrepic)
c IN	: comp,nombrepic
c SIDE	: in peak3d
c
c concatenates peak in a radius of comp

       implicit none

#include "sizebase.inc"
#include "peaksize.inc"

       integer comp,nombrepic,no,i,j,fa1,fa2,fa3,fb1,fb2,fb3,dd
       real    intens,intp
                         
       do no=1,nombrepic-1
           fa1=int(peak3d(no,1))
           fa2=int(peak3d(no,4))
           fa3=int(peak3d(no,6))
           intens=peak3d(no,3)
           do i=no+1,nombrepic
	           fb1=int(peak3d(i,1))
	           fb2=int(peak3d(i,4))
	           fb3=int(peak3d(i,6))
               intp=peak3d(i,3)
               dd = abs(fa1-fb1) + abs(fa2-fb2) + abs(fa2-fb3)
               if (dd.le.comp) then
                 if (intp.lt.intens) then
                   peak3d(i,5) = -no 
                   do j=1,nombrepic
                     if (peak3d(j,5).eq.-i) peak3d(j,5)=-no
                   enddo
                 else              
                   peak3d(no,5)=-i
                   do j=1,nombrepic
                     if (peak3d(j,4).eq.-no) peak3d(j,5)=-i
                   enddo
                 endif
               endif
           enddo
       enddo                          
       return
       end
       
