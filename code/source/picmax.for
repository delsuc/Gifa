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

c Completely modified christmas 2000-may 2001 by M.A.Delsuc
c added the dist algo and the various options
c now associated with the PKFILTER command

       subroutine picmaxlink(comp,nombrepic,dist)
c IN	: comp,nombrepic,dist
c SIDE	: in peak2d
c
c link peak in a radius of comp
c dist determines which distance is used :
c   1 : dist = abs(x) + abs(y)  => square   was the default in previous version
c   2 : dist = sqrt(x^2 + y^2)  => circle
c   0 : dist = x+y              => diamond

       implicit none

#include "sizebase.inc"
#include "peaksize.inc"

       integer comp,nombrepic,dist,no,i,j,colp,ligp,col,lig
       real    intens,intp,dd

       if (dist .lt. 0 .or. dist.gt.2) return
       do no=1,nombrepic-1
           col=int(peak2d(no,1))
           lig=int(peak2d(no,4))
           intens=peak2d(no,3)
           do i=no+1,nombrepic
               colp=int(peak2d(i,1))
               ligp=int(peak2d(i,4))
               intp=peak2d(i,3)
               if (dist.eq.0) then
                  dd = max(abs(col-colp),abs(lig-ligp))
               else if (dist.eq.1) then
                  dd = abs(col-colp) + abs(lig-ligp)
               else if (dist.eq.2) then
                  dd = sqrt( float((col-colp)**2 + (lig-ligp)**2))
               endif
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
       subroutine picmaxlink3d(comp,nombrepic,dist)
c IN	: comp,nombrepic,dist
c SIDE	: in peak3d
c
c link peak in a radius of comp
c dist determines which distance is used :
c   1 : dist = abs(x) + abs(y)  => square   was the default in previous version
c   2 : dist = sqrt(x^2 + y^2)  => circle
c   0 : dist = x+y              => diamond

       implicit none

#include "sizebase.inc"
#include "peaksize.inc"

       integer comp,nombrepic,dist,no,i,j,fa1,fa2,fa3,fb1,fb2,fb3
       real    intens,intp,dd
                         
       if (dist .lt. 0 .or. dist.gt.2) return
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
               if (dist.eq.0) then
                  dd = max(abs(fa1-fb1), abs(fa2-fb2), abs(fa2-fb3))
               else if (dist.eq.1) then
                  dd = abs(fa1-fb1) + abs(fa2-fb2) + abs(fa2-fb3)
               else if (dist.eq.2) then
         dd = sqrt(float((fa1-fb1)**2 + (fa2-fb2)**2 + (fa2-fb3)**2))
               endif
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

c************************************************************************       
       subroutine picmaxrem(comp,nombrepic,dist)
c IN	   : comp,dist
c INOUT  : nombrepic
c SIDE   : in peak2d
c
c remove peak in a radius of comp
c dist determines which distance is used :
c   1 : dist = abs(x) + abs(y)  => square   was the default in previous version
c   2 : dist = sqrt(x^2 + y^2)  => circle
c   0 : dist = x+y              => diamond

       implicit none

#include "sizebase.inc"
#include "peaksize.inc"

       integer comp,nombrepic,dist,no,i,j,colp,ligp,col,lig,
     *     tindex(peakmax),tn
#f2c       save tindex      ! for C array
       real    intens,intp,dd

       if (dist .lt. 0 .or. dist.gt.2) return
       tn = 1
       do no=1,nombrepic-1
           col=int(peak2d(no,1))
           lig=int(peak2d(no,4))
           intens=peak2d(no,3)
           do i=no+1,nombrepic
               colp=int(peak2d(i,1))
               ligp=int(peak2d(i,4))
               intp=peak2d(i,3)
               if (dist.eq.0) then
                  dd = max(abs(col-colp),abs(lig-ligp))
               else if (dist.eq.1) then
                  dd = abs(col-colp) + abs(lig-ligp)
               else if (dist.eq.2) then
                  dd = sqrt(float((col-colp)**2 + (lig-ligp)**2))
               endif
               if (dd.le.comp) then
                  if (intp.lt.intens) then
                    tindex(tn) = i
                  else              
                    tindex(tn) = no
                  endif
                  tn=tn+1
               endif
           enddo
       enddo
       tn = tn-1
       call bsort(tindex,tn)
       do i=1,pk2d
         call cicopvect(tindex,peak2d(1,i),peak2d(1,i),nombrepic,tn)
         call cicopsvect(tindex,peak2d_id(i),peak2d_id(i),nombrepic,tn)
       enddo
       nombrepic = nombrepic - tn
       return
       end

c************************************************************************       
       subroutine picmaxave(comp,nombrepic,dist)
c IN	   : comp,dist
c INOUT  : nombrepic
c SIDE   : in peak2d
c
c remove all peaks in a radius of comp
c  and create a new one with coordinates at the barycenter of
c              the found peaks, and intensity equal to the sum
c
c dist determines which distance is used :
c   1 : dist = abs(x) + abs(y)  => square   was the default in previous version
c   2 : dist = sqrt(x^2 + y^2)  => circle
c   0 : dist = x+y              => diamond

       implicit none

#include "sizebase.inc"
#include "peaksize.inc"

       integer comp,nombrepic,dist,no,i,j,colp,ligp,col,lig,
     *     tindex(peakmax),tn
       real    intens,intp,dd
       real tintens(peakmax), ccoord(peakmax), lcoord(peakmax)
#f2c       save tindex,tintens,ccoord,lcoord      ! for C array

       if (dist .lt. 0 .or. dist.gt.2) return
       tn = 1
       call prpvect(tintens,0.0,nombrepic)
       call prpvect(ccoord,0.0,nombrepic)
       call prpvect(lcoord,0.0,nombrepic)
       do no=1,nombrepic-1
           col=int(peak2d(no,1))
           lig=int(peak2d(no,4))
           intens=peak2d(no,3)
           do i=no+1,nombrepic
               colp=int(peak2d(i,1))
               ligp=int(peak2d(i,4))
               intp=peak2d(i,3)
               if (dist.eq.0) then
                  dd = max(abs(col-colp),abs(lig-ligp))
               else if (dist.eq.1) then
                  dd = abs(col-colp) + abs(lig-ligp)
               else if (dist.eq.2) then
                  dd = sqrt(float((col-colp)**2 + (lig-ligp)**2))
               endif
               if (dd.le.comp) then
                  if (intp.lt.intens) then
                    tindex(tn) = i
                    ccoord(i)=tintens(no)*ccoord(no)+ccoord(i)
                    lcoord(i)=tintens(no)*lcoord(no)+lcoord(i)
                    tintens(no)=tintens(no)+tintens(i)
                  else
                    tindex(tn) = no
                    ccoord(no)=ccoord(no)+tintens(i)*ccoord(i)
                    lcoord(no)=lcoord(no)+tintens(i)*lcoord(i)
                    tintens(i)=tintens(no)+tintens(i)
                  endif
                  tn=tn+1
               endif
           enddo
       enddo
       tn = tn-1
       call bsort(tindex,tn)
       do i=1,pk2d
         call cicopvect(tindex,peak2d(1,i),peak2d(1,i),nombrepic,tn)
         call cicopsvect(tindex,peak2d_id(i),peak2d_id(i),nombrepic,tn)
       enddo
       nombrepic = nombrepic - tn
       do no=1,nombrepic
         peak2d(no,1) = peak2d(no,1)/peak2d(no,3)
         peak2d(no,4) = peak2d(no,4)/peak2d(no,3)
       enddo
       return
       end

c************************************************************************       
       subroutine picmaxrem3d(comp,nombrepic,dist)
c IN	   : comp,dist
c INOUT  : nombrepic
c SIDE   : in peak3d
c
c remove peak in a radius of comp
c dist determines which distance is used :
c   1 : dist = abs(x) + abs(y)  => square   was the default in previous version
c   2 : dist = sqrt(x^2 + y^2)  => circle
c   0 : dist = x+y              => diamond

       implicit none

#include "sizebase.inc"
#include "peaksize.inc"

       integer comp,nombrepic,dist,no,i,j,fa1,fa2,fa3,fb1,fb2,fb3,
     *     tindex(peakmax),tn
       real    intens,intp,dd
#f2c       save tindex     ! for C array
                         
       if (dist .lt. 0 .or. dist.gt.2) return
       tn = 1
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
               if (dist.eq.0) then
                  dd = max(abs(fa1-fb1), abs(fa2-fb2), abs(fa2-fb3))
               else if (dist.eq.1) then
                  dd = abs(fa1-fb1) + abs(fa2-fb2) + abs(fa2-fb3)
               else if (dist.eq.2) then
        dd = sqrt(float((fa1-fb1)**2 + (fa2-fb2)**2 + (fa2-fb3)**2))
               endif
               if (dd.le.comp) then
                  if (intp.lt.intens) then
                    tindex(tn) = i
                  else              
                    tindex(tn) = no
                  endif
                  tn=tn+1
               endif
           enddo
       enddo
       tn = tn-1
       call bsort(tindex,tn)
       do i=1,pk3d
         call cicopvect(tindex,peak3d(1,i),peak3d(1,i),nombrepic,tn)
         call cicopsvect(tindex,peak3d_id(i),peak3d_id(i),nombrepic,tn)
       enddo
       nombrepic = nombrepic - tn
       return
       end

c************************************************************************       
       subroutine picmaxave3d(comp,nombrepic,dist)
c IN	   : comp,dist
c INOUT  : nombrepic
c SIDE   : in peak3d
c
c remove all peaks in a radius of comp
c  and create a new one with coordinates at the barycenter of
c              the found peaks, and intensity equal to the sum
c dist determines which distance is used :
c   1 : dist = abs(x) + abs(y)  => square   was the default in previous version
c   2 : dist = sqrt(x^2 + y^2)  => circle
c   0 : dist = x+y              => diamond

       implicit none

#include "sizebase.inc"
#include "peaksize.inc"

       integer comp,nombrepic,dist,no,i,j,fa1,fa2,fa3,fb1,fb2,fb3,
     *     tindex(peakmax),tn
       real    intens,intp,dd
       real tintens(peakmax), fa1coo(peakmax), fa2coo(peakmax),
     *  fa3coo(peakmax)
#f2c       save tindex,tintens,fa1coo,fa2coo,fa3coo
                         
       if (dist .lt. 0 .or. dist.gt.2) return
       tn = 1
       call prpvect(tintens,0.0,nombrepic)
       call prpvect(fa1coo,0.0,nombrepic)
       call prpvect(fa2coo,0.0,nombrepic)
       call prpvect(fa3coo,0.0,nombrepic)
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
               if (dist.eq.0) then
                  dd = max(abs(fa1-fb1), abs(fa2-fb2), abs(fa2-fb3))
               else if (dist.eq.1) then
                  dd = abs(fa1-fb1) + abs(fa2-fb2) + abs(fa2-fb3)
               else if (dist.eq.2) then
        dd = sqrt(float((fa1-fb1)**2 + (fa2-fb2)**2 + (fa2-fb3)**2))
               endif
               if (dd.le.comp) then
                  if (intp.lt.intens) then
                    tindex(tn) = i
                    fa1coo(i)=tintens(no)*fa1coo(no)+fa1coo(i)
                    fa2coo(i)=tintens(no)*fa2coo(no)+fa2coo(i)
                    fa3coo(i)=tintens(no)*fa3coo(no)+fa3coo(i)
                    tintens(no)=tintens(no)+tintens(i)
                  else
                    fa1coo(no)=fa1coo(no)+tintens(i)*fa1coo(i)
                    fa2coo(no)=fa2coo(no)+tintens(i)*fa2coo(i)
                    fa3coo(no)=fa3coo(no)+tintens(i)*fa3coo(i)
                    tindex(tn) = no
                  endif
                  tn=tn+1
               endif
           enddo
       enddo
       tn = tn-1
       call bsort(tindex,tn)
       do i=1,pk3d
         call cicopvect(tindex,peak3d(1,i),peak3d(1,i),nombrepic,tn)
         call cicopsvect(tindex,peak3d_id(i),peak3d_id(i),nombrepic,tn)
       enddo
       nombrepic = nombrepic - tn
       do no=1,nombrepic
         peak3d(no,1) = peak3d(no,1)/peak3d(no,3)
         peak3d(no,4) = peak3d(no,4)/peak3d(no,3)
         peak3d(no,6) = peak3d(no,6)/peak3d(no,3)
       enddo
       return
       return
       end

