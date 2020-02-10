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
C**********************************************************
      subroutine trap3d(smx,si1,si2,si3,mode,tm1,tm2,it3d)
C IN	: si1,si2,si3,mode,tm1,tm2,it3d
c INOUT	: smx
c
c 3D trapezoid apodisation
c
      implicit none

      integer   si1,si2,si3,mode,itp,it3d
      real smx(si3,si2,si1),tm1,tm2
      logical checkdim
      external checkdim
#include "sizeparam.inc"

      call prpvect(work1d,1.0,si1+si2+si3)

      if (mod(mode,2).eq.1) then	! in F1
        if (checkdim(1,it3d,3)) then
           itp = 1
        else
           itp = 0
        endif
        call trapeze(work1d(si3+si2+1),si1,tm1,tm2,itp)
      endif

      if (mod(mode/2,2).eq.1) then	! in F2
        if (checkdim(2,it3d,3)) then
           itp = 1
        else
           itp = 0
        endif
        call trapeze(work1d(si3+1),si2,tm1,tm2,itp)
      endif

      if (mode.ge.4) then		! in F3
        if (checkdim(4,it3d,3)) then
           itp = 1
        else
           itp = 0
        endif
        call trapeze(work1d,si3,tm1,tm2,itp)
      endif

      call mult3d(work1d,smx,si1,si2,si3)
      return

      end
C**************************************************
      subroutine sin3d(smx,si1,si2,si3,mode,x,it3d)
c IN	: si1,si2,si3,mode,x,it3d
c INOUT	: smx
c
c 3D sine bell apodisation
C mode = 1 for si1
C mode = 2 for si2
C mode = 4 for si3
C and corresponding sums

      implicit none

      integer   si1,si2,si3,mode,itp,it3d
      real x,smx(si3,si2,si1)
      logical checkdim
      external checkdim
#include "sizeparam.inc"

      call prpvect(work1d,1.0,si1+si2+si3)

      if (mod(mode,2).eq.1) then	! in F1
        if (checkdim(1,it3d,3)) then
           itp = 1
        else
           itp = 0
        endif
        call sinmul(work1d(si3+si2+1),si1,x,itp)
      endif

      if (mod(mode/2,2).eq.1) then	! in F2
        if (checkdim(2,it3d,3)) then
           itp = 1
        else
           itp = 0
        endif
        call sinmul(work1d(si3+1),si2,x,itp)
      endif

      if (mode.ge.4) then		! in F3
        if (checkdim(4,it3d,3)) then
           itp = 1
        else
           itp = 0
        endif
        call sinmul(work1d,si3,x,itp)
      endif

      call mult3d(work1d,smx,si1,si2,si3)
      return

      end
C**************************************************
      subroutine sinsq3d(smx,si1,si2,si3,mode,x,it3d)
c IN	: si1,si2,si3,mode,x,it3d
c INOUT	: smx
c
c 3D squared sine bell apodisation
C mode = 1 for si1
C mode = 2 for si2
C mode = 4 for si3
C and corresponding sums

      implicit none

      integer   si1,si2,si3,mode,itp,it3d
      real x,smx(si3,si2,si1)
      logical checkdim
      external checkdim
#include "sizeparam.inc"

      call prpvect(work1d,1.0,si1+si2+si3)

      if (mod(mode,2).eq.1) then	! in F1
        if (checkdim(1,it3d,3)) then
           itp = 1
        else
           itp = 0
        endif
        call sin2mul(work1d(si3+si2+1),si1,x,itp)
      endif

      if (mod(mode/2,2).eq.1) then	! in F2
        if (checkdim(2,it3d,3)) then
           itp = 1
        else
           itp = 0
        endif
        call sin2mul(work1d(si3+1),si2,x,itp)
      endif

      if (mode.ge.4) then		! in F3
        if (checkdim(4,it3d,3)) then
           itp = 1
        else
           itp = 0
        endif
        call sin2mul(work1d,si3,x,itp)
      endif

      call mult3d(work1d,smx,si1,si2,si3)
      return

      end
C**************************************************
      subroutine exp3d(smx,si1,si2,si3,lb1,lb2,lb3,it3d)
c IN	: si1,si2,si3,mode,lb1,lb2,lb3,it3d
c INOUT	: smx
c
c 3D exponential apodisation

      implicit none

      integer   si1,si2,si3,itp,it3d
      real smx(si3,si2,si1),lb1,lb2,lb3
      logical checkdim
      external checkdim
#include "sizeparam.inc"

      call prpvect(work1d,1.0,si1+si2+si3)

      if (lb1.ne.0.0) then		! in F1
        if (checkdim(1,it3d,3)) then
           itp = 1
        else
           itp = 0
        endif
        call expmult(work1d(si3+si2+1),si1,lb1,itp)
      endif

      if (lb2.ne.0.0) then		! in F2
        if (checkdim(2,it3d,3)) then
           itp = 1
        else
           itp = 0
        endif
        call expmult(work1d(si3+1),si2,lb2,itp)
      endif

      if (lb3.ne.0.0) then		! in F3
        if (checkdim(4,it3d,3)) then
           itp = 1
        else
           itp = 0
        endif
        call expmult(work1d,si3,lb3,itp)
      endif

      call mult3d(work1d,smx,si1,si2,si3)
      return

      end
C**************************************************
      subroutine gaus3d(smx,si1,si2,si3,gb1,gb2,gb3,it3d)
c IN	: si1,si2,si3,mode,gb1,gb2,gb3,it3d
c INOUT	: smx
c
c 3D gaussian apodisation
      implicit none

      integer   si1,si2,si3,itp,it3d
      real smx(si3,si2,si1),gb1,gb2,gb3
      logical checkdim
      external checkdim
#include "sizeparam.inc"

      call prpvect(work1d,1.0,si1+si2+si3)

      if (gb1.ne.0.0) then		! in F1
        if (checkdim(1,it3d,3)) then
           itp = 1
        else
           itp = 0
        endif
        call gausmult(work1d(si3+si2+1),si1,gb1,itp)
      endif

      if (gb2.ne.0.0) then		! in F2
        if (checkdim(2,it3d,3)) then
           itp = 1
        else
           itp = 0
        endif
        call gausmult(work1d(si3+1),si2,gb2,itp)
      endif

      if (gb3.ne.0.0) then		! in F3
        if (checkdim(4,it3d,3)) then
           itp = 1
        else
           itp = 0
        endif
        call gausmult(work1d,si3,gb3,itp)
      endif

      call mult3d(work1d,smx,si1,si2,si3)
      return

      end
C**********************************************************
      subroutine proj3dadd(proj,smx,si1,si2,si3,axis,from,to)
c IN	: smx,si1,si2,si3,axis,from,to
c OUT	: proj
C performs a 3D additive projection along axis (1,2,3)
      implicit none
      integer si1,si2,si3,axis,from,to
      real smx(si3*si2*si1)
      real proj(512*1024)
      integer i

      if (axis.eq.1) then
        call projadd(proj,smx,si1,si2*si3,1,from,to)
      else if (axis.eq.3) then
        call projadd(proj,smx,si1*si2,si3,2,from,to)
      else if (axis.eq.2) then
        do i=1,si1
          call projadd(proj(1+(i-1)*si3),
     *      smx(1+(i-1)*si2*si3),si2,si3,1,from,to)
        enddo
      endif
      return
      end
C**********************************************************
      subroutine proj3dmax(proj,smx,si1,si2,si3,axis,from,to)
c IN	: smx,si1,si2,si3,axis,from,to
c OUT	: proj
C performs a 3D skyline projection along axis (1,2,3)
      implicit none
      integer si1,si2,si3,axis,from,to
      real smx(si3*si2*si1)
      real proj(512*1024)
      integer i

      if (axis.eq.1) then
        call projmax(proj,smx,si1,si2*si3,1,from,to)
      else if (axis.eq.3) then
        call projmax(proj,smx,si1*si2,si3,2,from,to)
      else if (axis.eq.2) then
        do i=1,si1
          call projmax(proj(1+(i-1)*si3),
     *      smx(1+(i-1)*si2*si3),si2,si3,1,from,to)
        enddo
      endif
      return
      end
C**************************************************
      subroutine mult3d(vector,smx,si1,si2,si3)
c IN	: vector,si1,si2,si3
c INOUT	: smx
c
c 3D equivalent of mult2d
c
      implicit none
#include "controlc.inc"

      integer  si1,si2,si3,i,j,k
      real     vector(si1+si2+si3),smx(si3,si2,si1),a


      do i=1,si1
        do j=1,si2
          a=vector(si3+si2+i)*vector(si3+j)
          do k=1,si3
            smx(k,j,i)=smx(k,j,i)*vector(k)*a
          enddo
        enddo
        if (control.eq.1) return
      enddo
      return
      end
C***************************************************************

      subroutine modu3d(image,size1,size2,size3)
c INOUT	: image,size1,size2,size3
c
C modu3d computes the module of the 3D tricomplex array image, and put it
C back in image changed to 3D real, size1, size2 and size3 then refer to real.

      implicit none
      integer size1,size2,size3
      real image(size2*size1*size3)
      integer line,col,i,j,k,l,dep
      real z

C change sizes first
      size1 = size1/2
      size2 = size2/2
      size3 = size3/2
C then loop
      do 100,line=1,size1
         do 110,col=1,size2
           i = (line-1)*size2*size3*8 + (col-1)*size3*4 + 1
           j = i + 4*size2*size3
           k = i + 2*size3
           l = k + 4*size2*size3
           do 110, dep=1,size3
             z = image(i)**2 + image(i+1)**2 +
     *           image(j)**2 + image(j+1)**2 +
     *           image(k)**2 + image(k+1)**2 +
     *           image(l)**2 + image(l+1)**2
             image((line-1)*size2*size3+(col-1)*size3+dep) = sqrt(z)
           i = i+2
           j = j+2
           k = k+2
           l = l+2
110      continue
100   continue
      return
      end
