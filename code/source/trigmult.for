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
C sinmul(vect,size,x,itype)
C sin2mul(vect,size,x,itype)
C trapeze(vect,size,tm1,tm2,itype)
C module(vect,size)
C 
C apply math apodization, itype = 1 means complex.

      subroutine sinmul(vect,size,x,itype)
c IN	: size,x,itype
c INOUT	: vect
c
c multply vect by a shifted sine bell
c 0<= x <= 0.5       x = 0 is pure cosine,
c                    x = 0.5 is pure sine
C itype = 1 means complex.

      implicit none
 
#include "constant.inc"

      integer   size,i,itype
      real   vect(size),zz,x,s,yy,a

C     x has its value between 0 and 0.5
C     x=0 corresponds to cosine, x=0.5 corresponds to sine

      s=2*(1-x)
      zz= pi/((size-1)*s)
      yy= pi*(s-1)/s         !yy has the dimension of a phase
      if (itype.eq.0) then       ! real
       do i=1,size
          a=sin((i-1)*zz + yy) 
          vect(i)=vect(i)*a
       enddo
      else                       ! complex
       do i=1,size,2
          a=sin((i-1)*zz + yy) 
          vect(i)=vect(i)*a
          vect(i+1)=vect(i+1)*a
       enddo
      endif

      return
      end
C***************************************************************
      subroutine sin2mul(vect,size,x,itype)
c IN	: size,x,itype
c INOUT	: vect
c
c multply vect by a shifted sine bell squared
c 0<= x <= 0.5       x = 0 is pure cosine,
c                    x = 0.5 is pure sine
C itype = 1 means complex.

      implicit none

#include "constant.inc"
 
      integer   size,itype,i
      real   vect(size),a,x,zz,yy,s

      s=2*(1-x)
      zz= pi/((size-1)*s)
      yy= pi*(s-1)/s         !yy has the dimension of a phase
      if (itype.eq.0) then       ! real
       do i=1,size
         a=sin((i-1)*zz+ yy)
         vect(i)=vect(i)*a*a
       enddo
      else
       do i=1,size,2
         a=sin((i-1)*zz+ yy)
         vect(i)=vect(i)*a*a
         vect(i+1)=vect(i+1)*a*a
       enddo
      endif

      return
      end
                            
C***************************************************************
      subroutine trapeze(vect,size,tm1,tm2,itype)
c IN	: size,tm1,tm2,itype
c INOUT	: vect
c
c multiply vect by a trapezoid function
c        1.0 ... 1.0
c 0 ...             ... 0
c 1 ... tm1 ... tm2 ... size
C itype = 1 means complex.

      implicit none

#include "constant.inc"
 
      integer   size,i,tm1,tm2,itype
      real   vect(size),ftm1,ftm2

      if (tm1.lt.1 .or. tm2.lt.tm1 .or. tm2.gt.size) then
        call gifaout( '*** Error with trapeze window')
        return
      endif
      if (itype.eq.1) then		! regularize starting & ending points
        tm1 = min(size, 2*(tm1/2)+1)
        tm2 = 2*(tm2/2)+1		! I want size to become size+1
      endif
      ftm1 = tm1
      ftm2 = size-tm2+1
      if (tm1.gt.1) then
        if (itype.eq.0) then
         do i=1,tm1
            vect(i)=vect(i)*float(i)/ftm1
         enddo
        else
         do i=1,tm1,2
            vect(i)=vect(i)*float(i)/ftm1
            vect(i+1)=vect(i+1)*float(i)/ftm1
         enddo
        endif
      endif
      if (tm2.lt.size) then
        if (itype.eq.0) then
         do i=tm2,size
            vect(i)=vect(i)*float(size-i+1)/ftm2
         enddo
        else
         do i=tm2,size,2
            vect(i)=vect(i)*float(size-i+1)/ftm2
            vect(i+1)=vect(i+1)*float(size-i+1)/ftm2
         enddo
        endif
      endif
      return
      end
                            
C***************************************************************
      subroutine module(image,size)
c INOUT	: image,size
c
C module computes the module of the complex array "image", and put it
C back in image changed to real, "size" is divided by two

      implicit none
      integer size,i,j
      real image(size)

      j = 1
      do 100,i=1,size/2
         image(i) = sqrt( image(j)**2 + image(j+1)**2)
         j = j+2
100   continue
      size = size/2
      return
      end

C***************************************************************
      subroutine gauscplx(vect,size)
c IN	: size,itype
c INOUT	: vect
c
c multiply  (cplx*8) vect(size) by the exp(i pi/n k^2) function
c which appears in deconvolution theory

      implicit none

#include "constant.inc"
      integer size
      complex vect(size)
      integer i
      real w,w0

      w0=pi/float(size)
      do i=1,size
         w = w0*(float(i-1)**2)
         vect(i)=vect(i)*cmplx(cos(w),sin(w))
      enddo
      return
      end



