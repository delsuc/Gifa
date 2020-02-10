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
      subroutine sin2d(smx,size1,size2,mode,x,itype)
c IN	: size1,size2,mode,x,itype
c INOUT	: smx
c
c multiplies the matrix "smx" by a sine bell.
c mode = 1 / 2 / 3  for F1, F2 F12 axes
c x ranges from 0.0 (pur cosine) to 0.5 pure sine
c itype = 0 / 1 / 2 / 3  for complex type

      implicit none
 
#include "sizeparam.inc"                       

      integer   size1,size2,mode ,itype 
      real   x,smx(size2,size1) 

      call prpvect(work1d,1.0,size1+size2)

      if (mode/2.eq.1) then  !filter in dim2 if mode eq 2 or 3 
        call sinmul(work1d,size2,x,mod(itype,2))
      endif

      if (mod(mode,2).eq.1) then     !filter in dim1 if mode eq 1 or 3
        call sinmul(work1d(size2+1),size1,x,itype/2)
      endif
   
      call mult2d(work1d,smx,size1,size2)
      return
      end
C**********************************************************
      subroutine j2d(smx,size1,size2,j1,j2,type,itype)
c IN	: size1,size2,mode,x,itype
c INOUT	: smx
c
c multiplies the matrix "smx" by a sinusoide of frequency j1,j2.
c type is A or P for sine or cosine
c itype = 0 / 1 / 2 / 3  for complex type

      implicit none
 
#include "sizeparam.inc"                

      integer   size1,size2 ,itype 
      real   smx(size2,size1),j1,j2
      character*1  type 

      call prpvect(work1d,1.0,size1+size2)

      if (j2.ne.0.0) then  !j-deconv in dim2 if j non equal zero
        call jmult(work1d,size2,j2,type,mod(itype,2))
      endif

      if (j1.ne.0.0) then  !j-deconv in dim1 if j1 non equal zero
        call jmult(work1d(size2+1),size1,j1,type,itype/2)
      endif
   
      call mult2d(work1d,smx,size1,size2)
      return
      end
C**********************************************************
      subroutine exp2d(smx,size1,size2,x,y,itype)
c IN	: size1,size2,mode,x,itype
c INOUT	: smx
c
c multiplies the matrix "smx" by an damped exponential.
c x is damping on F1 y on F2
c itype = 0 / 1 / 2 / 3  for complex type

      implicit none
 
#include "sizeparam.inc"

      integer   size1,size2 ,itype 
      real   x,smx(size2,size1),y 

      call prpvect(work1d,1.0,size1+size2)

      if (y.ne.0.0) then  !filter in dim2 if lb non equal zero
        call expmult(work1d,size2,y,mod(itype,2))
      endif

      if (x.ne.0.0) then     !filter in dim1 if lb non equal zero
        call expmult(work1d(size2+1),size1,x,itype/2)
      endif
   
      call mult2d(work1d,smx,size1,size2)
      return
      end
C**********************************************************
      subroutine gaus2d(smx,size1,size2,x,y,itype)
c IN	: size1,size2,mode,x,itype
c INOUT	: smx
c
c multiplies the matrix "smx" by an damped gaussian.
c x is damping on F1 y on F2
c itype = 0 / 1 / 2 / 3  for complex type

      implicit none
 
#include "sizeparam.inc"

      integer   size1,size2 ,itype 
      real   x,smx(size2,size1),y 

      call prpvect(work1d,1.0,size1+size2)

      if (y.ne.0.0) then  !filter in dim2 if lb non equal zero
        call gausmult(work1d,size2,y,mod(itype,2))
      endif

      if (x.ne.0.0) then     !filter in dim1 if lb non equal zero
        call gausmult(work1d(size2+1),size1,x,itype/2)
      endif
   
      call mult2d(work1d,smx,size1,size2)
      return
      end
C**********************************************************
      subroutine sinsq2d(smx,size1,size2,mode,x,itype)
c IN	: size1,size2,mode,x,itype
c INOUT	: smx
c
c multiplies the matrix "smx" by a squared sine bell.
c mode = 1 / 2 / 3  for F1, F2 F12 axes
c x ranges from 0.0 (pur cosine) to 0.5 pure sine
c itype = 0 / 1 / 2 / 3  for complex type

      implicit none
 
#include "sizeparam.inc"
      integer   size1,size2,mode,itype
      real      smx(size2,size1),x
                            
      call prpvect(work1d,1.0,size1+size2)

      if (mode/2.eq.1) then  !filter in dim1 if mode eq 1 or 3 
        call sin2mul(work1d,size2,x,mod(itype,2))
      endif

      if (mod(mode,2).eq.1) then   !filter in dim2 if mode eq 2 or 3
        call sin2mul(work1d(size2+1),size1,x,itype/2)
      endif
                                   
      call mult2d(work1d,smx,size1,size2)
      return
      end
C**********************************************************
      subroutine trap2d(smx,size1,size2,mode,tm1,tm2,itype)
c IN	: size1,size2,mode,x,itype
c INOUT	: smx
c
c multiplies the matrix "smx" by an trapezoid function.
c goes from 0 to 1 for point 1 to tm1,
c goes from 1 to 0 for point tm2 to siz
c mode = 1 / 2 / 3  for F1, F2 F12 axes
c itype = 0 / 1 / 2 / 3  for complex type

      implicit none
 
#include "sizeparam.inc"
      integer   size1,size2,mode,tm1,tm2,itype
      real      smx(size2,size1)
                            
      call prpvect(work1d,1.0,size1+size2)

      if (mode/2.eq.1) then  !filter in dim1 if mode eq 1 or 3 
        call trapeze(work1d,size2,tm1,tm2,mod(itype,2))
      endif

      if (mod(mode,2).eq.1) then   !filter in dim2 if mode eq 2 or 3
        call trapeze(work1d(size2+1),size1,tm1,tm2,itype/2)
      endif
                                   
      call mult2d(work1d,smx,size1,size2)
      return
      end
C**************************************************
      subroutine mult2d(vector,smx,si1,si2)
c IN	: vector,si1,si2
c INOUT	: smx
c
c used by 2D apodisation.
c vector is holding function if F2 and in F1 concatenated
c smx(i,j) is multiplied by F2(i)*F1(j)
c
      implicit none
#include "controlc.inc"

      integer  si1,si2,i,j
      real     vector(si1+si2),smx(si2,si1),a


      do j=1,si1                                
        a=vector(si2+j)
        do i=1,si2        
          smx(i,j)=smx(i,j)*vector(i)*a
        enddo
        if (control.eq.1) return
      enddo
      return
      end
C***************************************************************

      subroutine modu2d(image,size1,size2)
c INOUT	: image,size1,size2
c
C modu2d computes the module of the 2D bicomplex array image, and put it
C back in image changed to 2D real, size1 and size2 then refer to real.
c

      implicit none
      integer size1,size2
      real image(size2*size1)
      integer line,col,i,j
      real z

C change sizes first
      size1 = size1/2
      size2 = size2/2
C then loop
      do 100,line=1,size1
         i = (line-1)*size2*4 + 1
         j = i + 2*size2
         do 110,col=1,size2
           z = image(i)**2 + image(i+1)**2 +
     *         image(j)**2 + image(j+1)**2
           image((line-1)*size2+col) = sqrt(z)
           i = i+2
           j = j+2
110      continue
100   continue
      return
      end
C***************************************************************
      subroutine real2d(image,size1,size2,mode)
c IN	: mode
c INOUT	: image,size1,size2
c
C real2d extract the RR part of the 2D bicomplex array image, and put it
C back in image changed to 2D real, size1 and size2 then refer to real.


      implicit none
      integer size1,size2,mode
      real image(size2*size1)
      integer i

      if (mode.eq.3) then
C change sizes first
         size1 = size1/2
         size2 = size2/2
C then  loop
         do i=1,size1               !lin number in a matrix
            call ctrvect(image((i-1)*size2+1),
     *              image(4*(i-1)*size2+1),2*size2)
         enddo
      else if (mode.eq.2) then
         size2 = size2/2
         do i=1,size1               !lin number in a matrix
            call ctrvect(image((i-1)*size2+1),
     *              image(2*(i-1)*size2+1),2*size2)
         enddo
      else if (mode.eq.1) then
         size1 = size1/2
         do i=2,size1               !lin number in a matrix
            call copvect(image((i-1)*size2+1),
     *              image(2*(i-1)*size2+1),size2)
         enddo
      endif
      return
      end
C*************************************************************
      subroutine real1d(image,size)
c INOUT	: image,size
c
c throw away the imaginaty part of the interleaved complex vector image
c
      implicit none
      integer size
      real image(size)
      integer i

C change sizes first
      size = size/2
C then loop
      do 100,i=1,size               
           image(i)=image(2*i-1)
100   continue
      return
      end
C*************************************************************
      subroutine projadd(vect,image,si1,si2,axis,from,to)
c IN	: image,si1,si2,axis,from,to
c OUT	: vect
c
C performs the additive projection of the 2D image, along axis (1=si1, 
C 2 = si2), for the subsection from-to, and put the result in vect.
c
      implicit none
      integer si1,si2,axis,from,to
      real image(si2,si1), vect(*)

      integer i,sizp
      real sum

      sizp = to-from+1
      if (axis.eq.2) then 
         do i=1,si1
           call sumvect(sum,image(from,i),sizp)
           vect(i) = sum
         enddo
c         call mltvect(vect,vect,(1.0/sizp),si1)
      else if (axis.eq.1) then
         call copvect(vect,image(1,from),si2)
         if (sizp.gt.1) then
           do i=from+1,to
             call addvect(vect,vect,image(1,i),1.0,si2)
           enddo
         endif
c         call mltvect(vect,vect,(1.0/sizp),si2)
      endif
      return
      end
C*************************************************************
      subroutine projmax(vect,image,si1,si2,axis,from,to)
c IN	: image,si1,si2,axis,from,to
c OUT	: vect
c
C performs the skyline projection of the 2D image, along axis (1=si1, 
C 2 = si2), for the subsection from-to, and put the result in vect.
      implicit none
      integer si1,si2,axis,from,to
      real image(si2,si1), vect(*)

      integer i,j,sizp
      real sum,a

      sizp = to-from+1
      if (axis.eq.2) then 
         do i=1,si1
           call mnxvect(a,sum,j,j,image(from,i),sizp)
           vect(i) = sum
         enddo
      else if (axis.eq.1) then
         call copvect(vect,image(1,from),si2)
         if (sizp.gt.1) then
           do i=from+1,to
              do j=1,si2
                vect(j) = max( vect(j), image(j,i) )
              enddo
           enddo
         endif
      endif
      return
      end
C*************************************************************
      subroutine symetrize(matrix,size,algo)
c IN	: size,algo
c INOUT	: matrix
c
C symetrize a sizexsize matrix
C depending on algo, symetrization will be :
C algo = 1 : mean value
C algo = 2 : smallest (mean if not the same sign)
C algo = 3 : hugues' special

      implicit none
      integer size,algo
      real matrix(size,size)
      
      integer i,j
      real a,b,c

      if (algo.eq.1) then
        do i=1,size-1
         do j=i+1,size
           a = (matrix(i,j)+matrix(j,i))*0.5
           matrix(i,j) = a
           matrix(j,i) = a
         enddo
        enddo
      elseif (algo.eq.2) then
        do i=1,size
         do j=i+1,size
           if (matrix(i,j)*matrix(j,i).lt.0.0) then
             a = 0.0
           else
             a = sign(min(abs(matrix(i,j)), abs(matrix(j,i))),
     *                matrix(j,i))
           endif
           matrix(i,j) = a
           matrix(j,i) = a
         enddo
        enddo
      elseif (algo.eq.3) then
        do i=1,size
         do j=i+1,size
           b = matrix(i,j)**2
           c = matrix(j,i)**2
           a = (b*matrix(j,i) + c*matrix(i,j)) / (b+c)
           matrix(i,j) = a
           matrix(j,i) = a
         enddo
        enddo
      endif
      return
      end
c****************************************************************
c
        subroutine moindo(spectre,n,min,max)
c IN	: n,min,max
c INOUT	: spectre
c
c local transpose of a matrix
c
c routine faite pour la symetrisation d'une bande d'eau

        implicit none
        integer n,min,max,i,j
        real spectre(n,n)
      
        do i=1,n
         do j=min,max
          spectre(j,i) = spectre (i,j)
         enddo
        enddo  

       return 
       end



