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
      subroutine stockpic(tablo,si1,si2,min,max,nombre,signpic,zq,
     *                    zoom,zolf1,zolf2,zour1,zour2)

      implicit none

#include "sizebase.inc"
#include "peaksize.inc"
    
      integer si1,si2,col,lig,nombre,nopic,signpic,zq,
     *        zoom,zolf1,zolf2,zour1,zour2,cd,cf,ld,lf
      real  tablo(si2,si1),som,somm1,somp1,scm1,scp1,min,max

      nopic=1
      if (zoom.eq.1) then
          cd = zolf2
          cf = zour2
          ld = zolf1
          lf = zour1
      else
          cd = 2
          cf = si2-1
          ld = 2
          lf = si1-1
      endif
      do col=cd,cf
        do lig=ld,lf
          som=tablo(col,lig)
          if (zq.eq.1) som = abs(som)
          if (som.ge.min .and. som.le.max) then
            if (zq.eq.0) then
              scm1=tablo(col-1,lig)
              scp1=tablo(col+1,lig)
              somm1=tablo(col,lig-1)
              somp1=tablo(col,lig+1)
            else
              scm1=abs(tablo(col-1,lig))
              scp1=abs(tablo(col+1,lig))
              somm1=abs(tablo(col,lig-1))
              somp1=abs(tablo(col,lig+1))
            endif
            if ((signpic.eq.1 .and.
     *              (som.gt.somm1 .and. som.gt.somp1 .and.
     *               som.gt.scm1 .and. som.gt.scp1))
     *     .or. (signpic.eq.-1 .and.
     *              (som.lt.somm1 .and. som.lt.somp1 .and.
     *               som.lt.scm1 .and. som.lt.scp1)))           then
                peak2d(nopic,1)=col
                peak2d(nopic,4)=lig
                peak2d(nopic,3)=som
                peak2d(nopic,2) = 3.0
                peak2d(nopic,5) = 3.0
                nopic=nopic+1
                if (nopic.gt.peakmax) then
            call gifaout('too many peaks,change parameters in MINIMAX')
                  go to 10 
                endif
            endif
          endif
        enddo
      enddo               
10    nombre=nopic-1                       
      return
      end


c*************************************************************
      subroutine copypic3d(n2d,ind3d,sign,zq)

c this sub makes an incremental load of the 3D peak table with the contents
c of the 2d peak table, removing non-maximum entries
c
c every thing is done within common blocks
c
c n2d   is the size of the current 2d peak table
c ind3d is the index of the current F1 plane being processed

      implicit none
      integer n2d,ind3d,sign,zq
      real x,y,a,pp
      integer i,j,k,l

#include "sizebase.inc"
#include "peaksize.inc"

c for each pic in 2d table
      do i = 1, n2d
         x = peak2d(i,1)
         y = peak2d(i,4)
         a = peak2d(i,3)
         if (zq.eq.1) a = abs(a)
c search for a match in 3d table
         do j = nbpic3d,1,-1
c break in case of going backward more than one plane
           if (peak3d(j,4).lt.(ind3d-1)) goto 100
           if ((x.eq.peak3d(j,1)) .and. (y.eq.peak3d(j,6))) then   ! found one match
             pp = peak3d(j,3)
             if (zq.eq.1) pp = abs(pp)
             if (((sign.eq.1)  .and. (a.gt.pp))
     *      .or. ((sign.eq.-1) .and. (a.lt.pp))) then			! remove 3D peak
                do k=j,nbpic3d-1
                  do l=1,7
                    peak3d(k,l) = peak3d(k+1,l)
                  enddo
                enddo
                nbpic3d = nbpic3d-1
             elseif (((sign.eq.1)  .and. (a.le.pp))
     *          .or. ((sign.eq.-1) .and. (a.ge.pp))) then		! remove 2D peak
                peak2d(i,5) = -1		! just mark it with neg width
                if (a.eq.pp) peak3d(j,4) = ind3d-0.5
             endif
           endif
         enddo
100      continue
      enddo
c then copy
      do i = 1,n2d
         if (peak2d(i,5).ne.-1) then
	         j = nbpic3d + 1
	         peak3d(j,1) = peak2d(i,1)
	         peak3d(j,2) = peak2d(i,2)
	         peak3d(j,3) = peak2d(i,3)
	         peak3d(j,4) = ind3d
	         peak3d(j,5) = 3.0
	         peak3d(j,6) = peak2d(i,4)
	         peak3d(j,7) = peak2d(i,5)
	         nbpic3d = nbpic3d+1
	     endif
       enddo
      return
      end

