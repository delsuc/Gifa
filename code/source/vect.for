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
C    List of all the subroutines available for vector manipulation.
C 
C Unless otherwise noted, when a subroutine uses different vectors, these
C vectors may be the same. 
C i.e. call addvect(A,A,B,0.1,size) is legal.
C      call dotvect(chi,A,A,size) as well
C 
C All complex subroutines handle complex vectors as FORTRAN do: i.e. real and
C imaginary parts interlaced. 
C 
C 
C COPVECTI(A,B,N) copy the integer vector B, of size N into integer vector A 
C of size N
C 
C COPVECT(A,B,N) copy the real vector B, of size N into vector A of size N
C 
C RCOPVECT(A,B,N) copy the real vector B, of size N into vector A 
C of size N. The copy is done by starting at the end of the vector A
C
C CONJVECT(A,B,N) conjugate the complex*16 vector B, of size N and copy it 
C into complex*16 vector A of size N
C 
C SDCOPVECT(A,B,N) copy the real*4 vector B, of size N into real*8 vector A 
C of size N
C 
C DSCOPVECT(A,B,N) copy the real*8 vector B, of size N into real*4 vector A 
C of size N
C 
C DDCOPVECT(A,B,N) copy the real*8 vector B, of size N into real*8 vector A 
C of size N
C 
C SICOPVECT(A,B,N) copy the integer vector B, of size N into real vector A 
C 
C ISCOPVECT(A,B,N) copy the real vector B, of size N into integer vector A 
C
C CICOPVECT(INDX,A,B,N,N1) copy the complementary part of B(INDX(i)) into A
C Be careful! the index array INDX should be sorted in ascending order
C 
C CP2VECT(A,B,NA,NB) copy real vector of length NB into real vector A of length
C                    NA, NA and NB may be different, zero fill or truncate
C                    accordingly.
C 
C CPRVECT(A,B,NA,NB) copy real vector of length NB into real vector A of length
C                    NA, NA and NB may be different, zero fill or truncate
C                    accordingly. Starting form the end
C 
C CTRVECT(A,B,N) copy the real part of the complex vector B of length N into real
C                vector A.
C 
C RTCVECT(A,B,N) copy the real vector B into the real part of the complex
C                vector A, and put 0 in the imaginary part.
C                   A AND B CANNOT BE EQUAL
C 
C SWAVECT(A,N)   A is a real vector of length N, A is changed to a complex
C                vector of length N/2, when entering the first half of A holds
C                the real part of the complex vector, and the second part holds
C                the imaginary part. N must be power of two!!
C                       
C USWVECT(A,N)   A is a complex vector of length N/2, A is changed to a real
C                vector of length N where the first half holds the real
C                part of the complex vector, and the second part holds the
C                imaginary part. N must be power of two!!
C                       
C BRUVECT(A,B,N) copy real vector B of length N into A, changing the sign of
C                value with entries equal to 2 and 3 modulo 4.
C 
C BRU2VECT(A,B,N) copy real vector B of length N into A, changing the sign of
C                value with entries equal to 1 modulo 2.
C 
C MNXVECT(mini,maxi,imini,imaxi,A,N)  find values and locations of the largest
C          point (maxi,imaxi) and the smallest point (mini,imini) of vector
C          A of size N
C 
C MXAVECT(mini,maxi,imini,imaxi,A,N)   same as MNXVECT bu for the absolute
C          value of the vector A
C 
C REVVECT(A,N) reverse the order of the vector A and put the result back
C              into A
C 
C ZREVVECT(A,N) reverse the order of the vector A(complex*16) and put the 
C result back into A (complex*16)
C
C CREVVECT(A,N) reverse the order of the vector A(complex) and put the 
C result back into A (complex)
C 
C ADRVECT(A,L,N) add the scalar l to the content of vector A
C 
C ADDVECT(A,B,C,L,N) multiply the vector C by the scalar l, add this vector
C                    to B, and put the result into A
C 
C DADDVECT(A,B,C,L,N) multiply the vector C (real*8) by the scalar l,
C     add this vector to B(real*8), and put the result into A (real*8)
C 
C QADDVECT(A,B,C,L,N) put in A the vector (1-L)*B + L*C
C 
C DIAVECT(A,B,C,N) multiply the vector C by the diagonal matrix whose diagonal
C                  is in vector B, and put the result into A
C 
C DOTVECT(D,A,B,N) performs the dot product of vectors A and B and put the
C                  result into D
C 
C DDOTVECT(D,A,B,N) performs the dot product of vectors A (complex*16)and
C		    B (complex*8) and put the result into D (complex*8)
C 
C DDDOTVECT(D,A,B,N) performs the dot product of vectors A (real*8)and
C		    B (real*8) and put the result into D (real*8)
C 
C CDOTVECT(D,A,B,N) performs the dot product of vectors A (complex)and
C		    B (complex) and put the result into D (complex)
C 
C ZHERMVECT(D,A,B,N) performs the hermitian dot product of vectors 
C 	A (complex*16) and B (complex*16) and put the result into D (complex*16)
C 
C EUCVECT(E,A,B,N) compute the norm of the vector A weighted by the diagonal
C                  metric matrix whose diagonal is in B, and put the result into E
C 
C MLTVECT(A,B,L,N) multiply vector B by scalar l, and put it into A
C 
C PRPVECT(A,L,N) propagates scalar L into vector A
C 
C PRDVECT(A,L,N) propagates scalar L (real*8) into vector A (real*8)
C 
C PRCVECT(A,L,N) propagates scalar L (complex) into vector A (complex)
C 
C IPRPVECT(A,L,N) propagates scalar L integer into vector A integer
C 
C SUMVECT(S,A,N) S is the sum of all elements of vector A
C 
C SMAVECT(S,A,N) S is the sum of absolute value of element of vector A
C 
C CHIVECT(C,A,B,N) C is the euclidian distance between vectors A and B
C                  ( dot square product of (A-B) )
C 
C ENTVECT(E,A,S,N) E is the entropy of vector A for scaling S
C                  ( sum of -Z(i)*LOG(Z(i)), where  Z(i) = A(i)/S )
C 
C BCRVECT(A,N) This subroutine will base line correct vector A, by removing
C              any continuous contribution in A (i.e. removes the mean)
C 
C EXCVECT(A,B,N) exchange the values of vectors A and B.
C
C ABSVECT(A,B,N) puts the absolute value of vector B into A.
C
C INTVECT(A,N,T) Compute the running sum of the vect A
C                i.e. A'(I) = Sum( j=1,I-1;A(j) )
C                if T=0 data are real, if T=1 , data are complex
C
C PRODMAT(D,A,B,N,M) Calculate the product of matrix A(M,N) with 
C	diagonal matrix D(M) and put the result into B(M,N)
C
C BACKWARD(A,B,N) calculate the backward coeff vector from forward 
C
C INVNORM (B,A,N,L) transform the vector A(N) complex into the vector B(N) real
C composed of the inverse of complex norm of elements of A multiplied by the
C real L
C 
C LEFTPROD (VOUT,MAT,VIN,N,M)
C   computes the result of applying a left product with the
C   matrix mat(n x m) on the vect vin(m x 1)
C   i.e.  vout(n x 1) = mat(n x m) * vin(m x 1)
C
C*************************************************************
      subroutine absvect(array1,array2,size)
c IN	: array2,size
c OUT	: array1
c 
C ABSVECT(A,B,N) puts the absolute value of vector B into A.
C

      implicit none
                   
      integer  size,i
      real array1(*),array2(*)

#assert (size.gt.0)

      do 10 i=1,size  
        array1(i)=abs(array2(i))
10    continue
      return
      end
C*************************************************************
      subroutine  addvect(arrayout,B,C,l,size)
c IN	: B,C,l,size
c OUT	: arrayout
c 
C ADDVECT(A,B,C,L,N) multiply the vector C by the scalar l, add this vector
C                    to B, and put the result into A

      Implicit none

      integer i,size
      real b(size),c(size),l,arrayout(size)

#assert (size.gt.0)

      if (l.eq.1.0) then
         do 100,i=1,size
             arrayout(i) = b(i) + c(i)
100      continue
      elseif (l.eq.-1.0) then
         do 200,i=1,size
             arrayout(i) = b(i) - c(i)
200      continue
      else
         do 300,i=1,size
             arrayout(i) = b(i) + l*c(i)
300      continue
      endif
      return
      end

C*************************************************************
      subroutine  daddvect(arrayout,B,C,l,size)
c IN	: B,C,l,size
c OUT	: arrayout
c 
C DADDVECT(A,B,C,L,N) multiply the vector C (real*8) by the scalar l,
C     add this vector to B(real*8), and put the result into A (real*8)
C
C
      Implicit none

      integer i,size
      real*8 b(size),c(size),l,arrayout(size)

#assert (size.gt.0)

      if (l.eq.1.0) then
         do 100,i=1,size
             arrayout(i) = b(i) + c(i)
100      continue
      elseif (l.eq.-1.0) then
         do 200,i=1,size
             arrayout(i) = b(i) - c(i)
200      continue
      else
         do 300,i=1,size
             arrayout(i) = b(i) + l*c(i)
300      continue
      endif
      return
      end

C*************************************************************
      subroutine adrvect(array,reel,size)
c IN	: reel,size
c INOUT	: array
c
C ADRVECT(A,L,N) add the scalar l to the content of vector A
C 

      implicit none
                   
      integer  size,i
      real array(*),reel

#assert (size.gt.0)

      do 10 i=1,size  
        array(i)=array(i)+reel
10    continue
      return
      end
C*************************************************************
      subroutine bcrvect(array,size)
c IN	: size
c INOUT	: array
c
C BCRVECT(A,N) This subroutine will base line correct vector A, by removing
C              any continuous contribution in A (i.e. removes the mean)

      implicit none
      integer size
      real array(size)

      real mean

#assert (size.gt.0)

      if (size.le.0) return
      
      call sumvect(mean,array,size)
      mean = mean/size

      call adrvect(array,-mean,size)

      return
      end
C*************************************************************
      subroutine  bruvect(arrayout,arrayin,size)
c IN	: arrayin,size
c OUT	: arrayout
c
C BRUVECT(A,B,N) copy real vector B of length N into A, changing the sign of
C                value with entries equal to 2 and 3 modulo 4.

      Implicit none

      integer i,size
      real arrayin(size),arrayout(size)

#assert (size.gt.3)

      do 100,i=1,size-3,4
          arrayout(i) = arrayin(i)
          arrayout(i+1) = arrayin(i+1)
          arrayout(i+2) = -arrayin(i+2)
          arrayout(i+3) = -arrayin(i+3)
100   continue
      return
      end

C*************************************************************
      subroutine  bru2vect(arrayout,arrayin,size)
c IN	: arrayin,size
c OUT	: arrayout
c
C 
C BRU2VECT(A,B,N) copy real vector B of length N into A, changing the sign of
C                value with entries equal to 1 modulo 2.
C
      Implicit none

      integer i,size
      real arrayin(size),arrayout(size)

#assert (size.gt.1)

      do 100,i=1,size-1,2
          arrayout(i) = arrayin(i)
          arrayout(i+1) = -arrayin(i+1)
100   continue
      return
      end

C*************************************************************
      subroutine chivect(chi,A,B,size)
c IN	: A,B,size
c OUT	: chi
c
C CHIVECT(C,A,B,N) C is the euclidian distance between vectors A and B
C                  ( dot square product of (A-B) )

      implicit none
      integer size,i
      real A(size),b(size),chi
      real*8 cc

#assert (size.gt.0)

      cc = 0.0D0
      do 100,i=1,size
         cc = cc + ( a(i)-b(i) )**2
100   continue
      chi = cc
      return
      end

C*************************************************************
      subroutine  copvecti(arrayout,arrayin,size)
c IN	: arrayin,size
c OUT	: arrayout
c
C COPVECTI(A,B,N) copy the integer vector B, of size N into integer vector A 
C of size N
c
      Implicit none

      integer i,size
      integer arrayin(size),arrayout(size)

#assert (size.gt.0)

      do 100,i=1,size
          arrayout(i) = arrayin(i)
100   continue
      return
      end

C*************************************************************
      subroutine  copvect(arrayout,arrayin,size)
c IN	: arrayin,size
c OUT	: arrayout
c
C COPVECT(A,B,N) copy the real vector B, of size N into vector A of size N
c
      Implicit none

      integer i,size
      real arrayin(size),arrayout(size)

#assert (size.gt.0)

      do 100,i=1,size
          arrayout(i) = arrayin(i)
100   continue
      return
      end

C*************************************************************
      subroutine  rcopvect(arrayout,arrayin,size)
c IN	: arrayin,size
c OUT	: arrayout
c
C RCOPVECT(A,B,N) copy the real vector B, of size N into vector A 
c of size N. The copy is done by starting at the end of the vector A
c
      Implicit none

      integer i,size
      real arrayin(size),arrayout(size)

#assert (size.gt.0)

      do 100,i=size,1,-1
          arrayout(i) = arrayin(i)
100   continue
      return
      end

C*************************************************************
      subroutine  conjvect(arrayout,arrayin,size)
c IN	: arrayin,size
c OUT	: arrayout
c
C CONJVECT(A,B,N) conjugate the complex*16 vector B, of size N and copy it 
C into complex*16 vector A of size N
C 
      Implicit none

      integer i,size
      complex*16 arrayin(size),arrayout(size)

#assert (size.gt.0)

      do 100,i=1,size
          arrayout(i) = dconjg(arrayin(i))
100   continue
      return
      end

C*************************************************************
      subroutine  ddcopvect(arrayout,arrayin,size)
c IN	: arrayin,size
c OUT	: arrayout
c
C DDCOPVECT(A,B,N) copy the real*8 vector B, of size N into real*8 vector A 
C of size N
C
      Implicit none

      integer i,size
      real*8 arrayin(size),arrayout(size)

#assert (size.gt.0)

      do 100,i=1,size
          arrayout(i) = arrayin(i)
100   continue
      return
      end

C*************************************************************
      subroutine  sdcopvect(arrayout,arrayin,size)
c IN	: arrayin,size
c OUT	: arrayout
c
C SDCOPVECT(A,B,N) copy the real*4 vector B, of size N into real*8 vector A 
C of size N
C
      Implicit none

      integer i,size
      real arrayin(size)
      real*8 arrayout(size)

#assert (size.gt.0)

      do 100,i=1,size
          arrayout(i) = dble(arrayin(i))
100   continue
      return
      end

C*************************************************************
      subroutine  dscopvect(arrayout,arrayin,size)
c IN	: arrayin,size
c OUT	: arrayout

C DSCOPVECT(A,B,N) copy the real*8 vector B, of size N into real*4 vector A 
C of size N
C
      Implicit none

      integer i,size
      real arrayout(size)
      real*8 arrayin(size)

#assert (size.gt.0)

      do 100,i=1,size
          arrayout(i) = real(arrayin(i))
100   continue
      return
      end

C*************************************************************
      subroutine sicopvect (arrayout,arrayin,size)
c IN	: arrayin,size
c OUT	: arrayout
c
C SICOPVECT(A,B,N) copy the integer vector B, of size N into real vector A 
C
      Implicit none

      integer i,size
      real arrayout(size)
      integer arrayin(size)

#assert (size.gt.0)

      do 100,i=1,size
          arrayout(i) = float(arrayin(i))
100   continue
      return
      end

C*************************************************************
      subroutine iscopvect (arrayout,arrayin,size)
c IN	: arrayin,size
c OUT	: arrayout
c
C 
C ISCOPVECT(A,B,N) copy the real vector B, of size N into integer vector A 
C
      Implicit none

      integer i,size
      integer arrayout(size)
      real arrayin(size)

#assert (size.gt.0)

      do 100,i=1,size
          arrayout(i) = int(arrayin(i))
100   continue
      return
      end

C*************************************************************
      subroutine  cp2vect(arrayout,arrayin,sizeout,sizein)
c IN	: arrayin,sizeout,sizein
c OUT	: arrayout
c
C CP2VECT(A,B,NA,NB) copy real vector of length NB into real vector A of length
C                    NA, NA and NB may be different, zero fill or truncate
C                    accordingly.
c copy is done on direct order
c
      Implicit none

      integer i,sizeout,sizein
      real arrayin(sizein),arrayout(sizeout)

#assert (sizein.gt.0 .and. sizeout.gt.0)

      if (sizein.lt.sizeout) then
        do 100,i=1,sizein
            arrayout(i) = arrayin(i)
100     continue
        do 200,i=(sizein+1),sizeout
            arrayout(i) = 0.0
200     continue
      else
        do 300,i=1,sizeout
            arrayout(i)=arrayin(i)
300     continue
      endif

      return
      end

C*************************************************************
      subroutine  cprvect(arrayout,arrayin,sizeout,sizein)
c IN	: arrayin,sizeout,sizein
c OUT	: arrayout
c
C CPRVECT(A,B,NA,NB) copy real vector of length NB into real vector A of length
C                    NA, NA and NB may be different, zero fill or truncate
C                    accordingly. Starting form the end
C In reverse order.
c
      Implicit none

      integer i,sizeout,sizein
      real arrayin(sizein),arrayout(sizeout)

#assert (sizein.gt.0 .and. sizeout.gt.0)

      if (sizein.lt.sizeout) then
        do 100,i=sizein,1,-1
            arrayout(i) = arrayin(i)
100     continue
        do 200,i=(sizein+1),sizeout
            arrayout(i) = 0.0
200     continue
      else
        do 300,i=sizeout,1,-1
            arrayout(i)=arrayin(i)
300     continue
      endif

      return
      end

C*************************************************************
      subroutine  ctrvect(arrayout,arrayin,size)
c IN	: arrayin,size
c OUT	: arrayout
c
C CTRVECT(A,B,N) copy the real part of the complex vector B of length N into real
C                vector A.
C
      Implicit none

      integer i,j,size
      real arrayin(2*size),arrayout(size)

#assert (size.gt.0)

      j = 1
      do 100,i=1,size
          arrayout(i) = arrayin(j)
          j=j+2
100   continue
      return
      end

C*************************************************************
      subroutine  diavect(a,b,c,size)
c IN	: b,c,size
c OUT	: a
c
C DIAVECT(A,B,C,N) multiply the vector C by the diagonal matrix whose diagonal
C                  is in vector B, and put the result into A
c
c i.e. a(i) = b(i)*c(i)
C
      Implicit none

      integer i,size
      real a(size),b(size),c(size)

#assert (size.gt.0)

#vms      Call Lib$flt_under(0)
      do 100,i=1,size
         a(i) = b(i)*c(i)
100   continue
      return
      end

C*************************************************************
      subroutine  hdiavect(a,b,c,size1,size2)
c IN	: b,c,size1,size2
c OUT	: a
c
C HDIAVECT(A,B,C,N1,N2) multiply matrices hypercomplex (size1,size2) B and C
C point by point, and put the result into A
c
c     i.e. a(i) = b(i)*c(i)       where * is the 2D hypercomplex product
C
      Implicit none

      integer i,j,size1,size2
      real a(size2,size1),b(size2,size1),c(size2,size1)

#assert (size1.gt.0 .and. size2.gt.0)

#vms      Call Lib$flt_under(0)
      do 100,i=1,size1,2
         do 100,j=1,size2,2
            a(j,i) = b(j,i)*c(j,i) 
     *             - b(j+1,i)*c(j+1,i)
     *             - b(j,i+1)*c(j,i+1)
     *             + b(j+1,i+1)*c(j+1,i+1)
            a(j+1,i) = b(j,i)*c(j+1,i) 
     *             +   b(j+1,i)*c(j,i)
     *             -   b(j,i+1)*c(j+1,i+1)
     *             -   b(j+1,i+1)*c(j,i+1)
            a(j,i+1) = b(j,i)*c(j,i+1) 
     *             +   b(j,i+1)*c(j,i)
     *             -   b(j+1,i)*c(j+1,i+1)
     *             -   b(j+1,i+1)*c(j+1,i)
            a(j+1,i+1) = b(j,i)*c(j+1,i+1) 
     *             +     b(j+1,i+1)*c(j,i)
     *             +     b(j+1,i)*c(j,i+1)
     *             +     b(j,i+1)*c(j+1,i)
100   continue
      return
      end

C*************************************************************
      subroutine  cdiavect(a,b,c,size)
c IN	: b,c,size
c OUT	: a
c
C CDIAVECT(A,B,C,N) multiply the vector (complex*8) C by the diagonal matrix whose diagonal
C                  is in vector (complex*8) B, and put the result into(complex*8)  A
c
c i.e. a(i) = b(i)*c(i)
C
      Implicit none

      integer i,size
      complex a(size),b(size),c(size)

#assert (size.gt.0)

#vms      Call Lib$flt_under(0)
      do 100,i=1,size
         a(i) = b(i)*c(i)
100   continue
      return
      end

C*************************************************************
      subroutine  dotvect(d,a,b,size)
c IN	: a,b,size
c OUT	: d
C DOTVECT(D,A,B,N) performs the dot product of vectors A and B and put the
C                  result into D
C 
C
      Implicit none

      integer i,size
      real a(size),b(size),d
      real*8 dd

#assert (size.gt.0)

#vms      call lib$flt_under(0)
      dd = 0.0D0
      do 100,i=1,size
         dd = a(i)*b(i) + dd
100   continue
      d = dd
      return
      end

C*************************************************************
      subroutine  ddotvect(d,a,b,size)
c IN	: a,b,size
c OUT	: d
C DDOTVECT(D,A,B,N) performs the dot product of vectors A (complex*16)and
C		    B (complex*8) and put the result into D (complex*8)
C
      Implicit none

      integer i,size
      complex b(size),d
      complex*16 dd,bb,a(size)

#assert (size.gt.0)

#vms      call lib$flt_under(0)
      dd = (0.0D0,0.0D0)
      do 100,i=1,size
	 bb = b(i)
         dd = a(i)*bb + dd
100   continue
      d = dd
      return
      end

C*************************************************************
      subroutine  dddotvect(d,a,b,size)
c IN	: a,b,size
c OUT	: d
c
C DDDOTVECT(D,A,B,N) performs the dot product of vectors A (real*8)and
C		    B (real*8) and put the result into D (real*8)
C
      Implicit none

      integer i,size
      real*8 b(size),d,a(size)

#assert (size.gt.0)

#vms      call lib$flt_under(0)
      d = 0.0D0
      do 100,i=1,size
         d = a(i)*b(i) + d
100   continue
      return
      end

C*************************************************************
      subroutine  cdotvect(d,a,b,size)
c IN	: a,b,size
c OUT	: d
c
C CDOTVECT(D,A,B,N) performs the dot product of vectors A (complex) and
C		    B (complex) and put the result into D (complex)
C
      Implicit none

      integer i,size
      complex b(size),d,a(size)

#assert (size.gt.0)

#vms      call lib$flt_under(0)
      d = (0.0,0.0)
      do 100,i=1,size
         d = a(i)*b(i) + d
100   continue
      return
      end

C*************************************************************
      subroutine  zhermvect (d,a,b,size)
c IN	: a,b,size
c OUT	: d
c
C ZHERMVECT(D,A,B,N) performs the hermitian dot product of vectors 
C 	A (complex*16) and B (complex*16) and put the result into D (complex*16)
C
C i.e 		d =sum(a(i) . b(i)* )
c
      Implicit none

      integer i,size
      complex*16 b(size),d,a(size)

#assert (size.gt.0)

#vms      call lib$flt_under(0)
      d = (0.d0,0.d0)
      do 100,i=1,size
         d = a(i)*dconjg(b(i)) + d
100   continue
      return
      end

C*************************************************************
      subroutine entvect(ent,image,scale,size)
c IN	: image,scale,size
c OUT	: ent
c
c C ENTVECT(E,A,S,N) E is the entropy of vector A for scaling S
C                  ( sum of -Z(i)*LOG(Z(i)), where  Z(i) = A(i)/S )
c
      implicit none
      integer size
      real image(size),scale,ent

      integer i
      real*8 ee,tt

#assert (size.gt.0 .and. scale.GT.0.0)

#vms      Call Lib$flt_under(0)

      ee = 0.0D0
      if (scale.eq.0.0) then
         ent = 0.0
         return
      endif
      do 100,i=1,size
         tt = abs(image(i))/scale
         if (tt.gt.0.0D0) ee = ee - tt*log(tt)
100   continue
      ent = ee
      return
      end
C*************************************************************
      subroutine eucvect(n,a,b,size)
c IN	: a,b,size
c OUT	: n
c
C EUCVECT(E,A,B,N) compute the norm of the vector A weighted by the diagonal
C          metric matrix whose diagonal is in B, and put the result into E
C i.e.  E = sum( a(i)^2 . b(i) ) 
c

      Implicit none

      integer size,i
      real a(size), b(size), n
      real*8 z,nn

#assert (size.gt.0)

      nn = 0.0
      do 100,i=1,size
        z =  a(i)
        nn = nn + z*z*b(i)
100   continue
      n = nn
      return
      end
C*************************************************************
      subroutine  excvect(arrayout,arrayin,size)
C IN	: size
C INOUT	: arrayout,arrayin
c
C EXCVECT(A,B,N) exchange the values of vectors A and B.
C
C
      Implicit none

      integer i,size
      real arrayin(size),arrayout(size),temp

#assert (size.gt.0)

      do 100,i=1,size
          temp = arrayout(i)
          arrayout(i) = arrayin(i)
          arrayin(i) = temp
100   continue
      return
      end

C*************************************************************
      subroutine  mltvect(a,b,l,size)
c IN	: b,l,size
c OUT	: a
c
C MLTVECT(A,B,L,N) multiply vector B by scalar l, and put it into A
C 
C
      Implicit none

      integer i,size
      real a(size),b(size),l

#assert (size.gt.0)

      do 100,i=1,size
         a(i) = l*b(i)
100   continue
      return
      end

C*************************************************************
      subroutine mnxvect(mini,maxi,imini,imaxi,A,size)
c IN	: A,size
c OUT	: mini,maxi,imini,imaxi
c
C MNXVECT(mini,maxi,imini,imaxi,A,N)  find values and locations of the largest
C          point (maxi,imaxi) and the smallest point (mini,imini) of vector
C          A of size N
c
      implicit none
      integer size,imini,imaxi
      real a(size),mini,maxi

      integer i,imn,imx
      real s,mn,mx
      
#assert (size.gt.0)

      mn = a(1)
      mx = mn
      imn = 1
      imx = 1
      if (size.le.1) return
      do 100,i=2,size
          s = a(i)
          if (s.lt.mn) then
             mn = s
             imn = i
          else if (s.gt.mx) then
             mx = s
             imx = i
          endif
100   continue
      imini = imn
      imaxi = imx
      mini = mn
      maxi = mx
      return
      end
C*************************************************************
      subroutine mxavect(mini,maxi,imini,imaxi,A,size)
c IN	: A,size
c OUT	: mini,maxi,imini,imaxi
c
C MXAVECT(mini,maxi,imini,imaxi,A,N)   same as MNXVECT but for the absolute
C          value of the vector A
C 
c
      implicit none
      integer size,imini,imaxi
      real a(size),mini,maxi

      integer i
      real s
      
#assert (size.gt.0)

      mini = abs(a(1))
      maxi = mini
      imini = 1
      imaxi = 1
      if (size.le.1) return
      do 100,i=2,size
          s = abs(a(i))
          if (s.lt.mini) then
             mini = s
             imini = i
          else if (s.gt.maxi) then
             maxi = s
             imaxi = i
          endif
100   continue
      return
      end
C*************************************************************
      subroutine  prpvect(a,l,size)
C IN	: l,size
c OUT	: a
c
C PRPVECT(A,L,N) propagates the value of the scalar L into vector A
C 
      Implicit none

      integer i,size
      real a(*),l

#assert (size.gt.0)

      if (size.lt.1) then
         return
      else
        do 100,i=1,size
          a(i) = l
100     continue
      endif
      return
      end

C*************************************************************
      subroutine  prdvect(a,l,size)
C IN	: l,size
c OUT	: a
c
C PRDVECT(A,L,N) propagates scalar L (real*8) into vector A (real*8)
C 
      Implicit none

      integer i,size
      real*8 a(*),l

#assert (size.gt.0)

      if (size.lt.1) then
         return
      else
        do 100,i=1,size
          a(i) = l
100     continue
      endif
      return
      end

C*************************************************************
      subroutine  prcvect(a,l,size)
C IN	: l,size
c OUT	: a
c
C PRCVECT(A,L,N) propagates scalar L (complex) into vector A (complex)
C 
      Implicit none

      integer i,size
      complex a(*),l

#assert (size.gt.0)

      if (size.lt.1) then
         return
      else
        do 100,i=1,size
          a(i) = l
100     continue
      endif
      return
      end

C*************************************************************
      subroutine  iprpvect(a,l,size)
C IN	: l,size
c OUT	: a
c
C IPRPVECT(A,L,N) propagates scalar L integer into vector A integer
C 
C
      Implicit none

      integer i,size
      integer a(*),l

#assert (size.gt.0)

      if (size.lt.1) then
         return
      else
        do 100,i=1,size
          a(i) = l
100     continue
      endif
      return
      end

C*************************************************************
      subroutine  qaddvect(arrayout,B,C,l,size)
c IN	: B,C,l,size
c OUT	: arrayout
c
C QADDVECT(A,B,C,L,N) put in A the vector (1-L)*B + L*C
C negative values in are forbidden by exponential smoothing for L > 1
c
C Arrayout, B and C may be the same vector
C
      Implicit none

      integer i,size
      real b(size),c(size),l,arrayout(size),l2,alpha
#d       real dd
#include "constant.inc"

#assert (size.gt.0)

#vms      Call Lib$flt_under(0)
      l2 = 1-l
c      if (l .le. 1.0) then
      if (1 .eq. 1) then
         do 300,i=1,size
            arrayout(i) = max(epsilon, l2*b(i) + l*c(i))
 300     continue
      else
         do 400,i=1,size
#assert (c(i).gt.0.0)
            alpha = (c(i)-b(i))/c(i)       ! constant for exp smoothing
            if (alpha.gt.0) then          ! doit only if neg slope
               arrayout(i) = max(0.0, l2*b(i) + l*c(i))
            else
               arrayout(i) = c(i)*exp(l2*alpha)
            endif
#assert (arrayout(i).gt.0.0)
 400     continue
      endif
#d      call sumvect(dd,arrayout,size)
#assert (dd.gt.0.0)

      return
      end

C*************************************************************
      subroutine  revvect(array,size)
c IN	: size
c INOUT	: array
c
C REVVECT(A,N) reverse the order of the vector A and put the result back
C              into A
C
      Implicit none

      integer i,size,st
      real array(size),zz

#assert (size.gt.1)

      st = size+1
      do 100,i=1,size/2
          zz = array(i)
          array(i) = array(st-i)
          array(st-i) = zz
100   continue
      return
      end

C*************************************************************
      subroutine  zrevvect(array,size)
c IN	: size
c INOUT	: array
c
C ZREVVECT(A,N) reverse the order of the vector A(complex*16) and put the 
C result back into A (complex*16)
c
      Implicit none

      integer i,size,st
      complex*16 array(size),zz

#assert (size.gt.1)

      st = size+1
      do 100,i=1,size/2
          zz = array(i)
          array(i) = array(st-i)
          array(st-i) = zz
100   continue
      return
      end

C*************************************************************
      subroutine  crevvect(array,size)
c IN	: size
c INOUT	: array
c
C CREVVECT(A,N) reverse the order of the vector A(complex) and put the 
C result back into A (complex)
C 
      Implicit none

      integer i,size,st
      complex array(size),zz

#assert (size.gt.1)

      st = size+1
      do 100,i=1,size/2
          zz = array(i)
          array(i) = array(st-i)
          array(st-i) = zz
100   continue
      return
      end

C*************************************************************
      subroutine  rtcvect(arrayout,arrayin,size)
c IN	: arrayin,size
c OUT	: arrayout
c
C RTCVECT(A,B,N) copy the real vector B into the real part of the complex
C                vector A, and put 0 in the imaginary part.
C                   A AND B CANNOT BE EQUIVALENCED
C
      Implicit none

      integer i,j,size
      real arrayin(size),arrayout(2*size)

#assert (size.gt.0)

      j = 1
      do 100,i=1,size
          arrayout(j) = arrayin(i)
          arrayout(j+1) = 0.0
          j=j+2
100   continue
      return
      end

C*************************************************************
      subroutine smavect(sum,array,size)
C IN	: array,size
C OUT	: sum
c
C SMAVECT(S,A,N) S is the sum of absolute value of element of vector A
c
      implicit none
      integer size
      real array(size),sum
      real*8 ss

      integer i

#assert (size.gt.0)

      ss = 0.0D0
      do 100,i=1,size
         ss = ss+abs(array(i))
100   continue
      sum = ss
      return
      end
C*************************************************************
      subroutine sumvect(sum,array,size)
C IN	: array,size
C OUT	: sum
c
C SUMVECT(S,A,N) S is the sum of all elements of vector A
C

      implicit none
      integer size
      real array(size),sum
      real*8 ss

      integer i

#assert (size.gt.0)

      ss = 0.0D0
      do 100,i=1,size
         ss = ss+array(i)
100   continue
      sum = ss
      return
      end
C*************************************************************
      subroutine  swavect(array,size)
C IN	: size
c INOUT	: array
c
C SWAVECT(A,N)   A is a real vector of length N, A is changed to a complex
C                vector of length N/2, when entering the first half of A holds
C                the real part of the complex vector, and the second part holds
C                the imaginary part. N must be power of two!!
c
      Implicit none

      integer i,j,size,step,n,sz,off
      real array(size),temp

#assert (size.gt.1)

      if (size.le.2) return
      n = 1
      sz = size
1       sz = sz/2
        n = n+1
        if (sz.gt.2) goto 1
#assert (2**n .eq. size)

      if (2**n.ne.size) return       ! must be power of two

      step = size
      off = size/4

2        do 3,i=1,size,step
            do 4,j=off,2*off-1
               temp = array(off+i+j)
               array(i+j+off) = array(i+j)
               array(i+j) = temp
4           continue
3        continue
         step = step/2
         off = off/2
         if (off.gt.0) goto 2

      return
      end

C*************************************************************
      subroutine  uswvect(array,size)
C IN	: size
c INOUT	: array
c
C USWVECT(A,N)   A is a complex vector of length N/2, A is changed to a real
C                vector of length N where the first half holds the real
C                part of the complex vector, and the second part holds the
C                imaginary part. N must be power of two!!
C

      Implicit none

      integer i,j,size,step,n,sz,off
      real array(size),temp

#assert (size.gt.1)

      if (size.le.2) return
      n = 1
      sz = size
1       sz = sz/2
        n = n+1
        if (sz.gt.2) goto 1
#assert (2**n .eq. size)

      if (2**n.ne.size) return       ! must be power of two

      step = 4
      off = 1

2        do 3,i=1,size,step
            do 4,j=off,2*off-1
               temp = array(off+i+j)
               array(i+j+off) = array(i+j)
               array(i+j) = temp
4           continue
3        continue
         step = step*2
         off = 2*off
         if (step.lt.size+1) goto 2

      return
      end
C*************************************************************
      subroutine intvect(array,size,itype)
c IN	: size,itype
c INOUT	: array
C INTVECT(A,N,T) Compute the running sum of the vect A
C                i.e. A'(I) = Sum( j=1,I-1;A(j) )
C                if T=0 data are real, if T=1 , data are complex
      implicit none
      integer size,itype
      real array(size)
      integer i
      real zz

#assert (size.gt.0)

C modif 6 aout 1997, remove zz scaling
      if (size.le.0) return
c      zz = 1.0/float(size)
      if (itype.eq.0) then
c         array(1)=array(1)*zz
         do 3,i=2,size
c            array(i) = array(i)*zz + array(i-1)
            array(i) = array(i) + array(i-1)
3        continue
      elseif (itype.eq.1) then
         do 4,i=3,size,2
c            array(i) = array(i)*zz + array(i-2)
c            array(i+1) = array(i+1)*zz + array(i-1)
            array(i) = array(i) + array(i-2)
            array(i+1) = array(i+1) + array(i-1)
4        continue
      endif
      return
      end

C*************************************************************
      subroutine prodmat (diag,arrayin,arrayout,size,sized,sizdat)
c IN	: diag,arrayin,size,sized,sizdat
c OUT	: arrayout
c
C PRODMAT(D,A,B,N,M,P) Calculate the product of matrix A(M,N) (leading dimension P)
c with diagonal matrix D(M) and put the result into B(M,N) (leading dimension P)
C
c
      Implicit none

      integer size,i,sized,j,sizdat
      complex*16 arrayin(sizdat,sizdat),
     &		arrayout(sizdat,sizdat),diag(sizdat)

#assert (size.gt.0 .and. sized.gt.0)

      do i = 1,sized
	 do j = 1,size
	     arrayout(i,j) = diag(i)*arrayin(i,j)
	 enddo
      enddo
      return
      end

C*************************************************************
      subroutine backward (back,forw,size)
c IN	: for,size
c OUT	: back
c
C BACKWARD(A,B,N) calculate the backward coeff vector from forward 
C
      Implicit none

      integer size,i
      complex*16 forw(size),back(size)

#assert (size.gt.1)

      do i = 1,size-1
         back(i) = dconjg(forw(size-i)/forw(size))
      enddo
      back(size) = dconjg(1/forw(size))
      return
      end

C*************************************************************
      subroutine invnorm (arrayout,arrayin,size,scale)
c IN	: arrayin,size,scale
c OUT	: arrayout
c
C INVNORM (B,A,N,L) transform the vector A(N) complex into the vector B(N) real
C composed of the inverse of complex norm of elements of A multiplied by the
C real L
C 
      Implicit none
#include "constant.inc"

      integer size,i
      complex arrayin(size)
      real arrayout(size),scale

#assert (size.gt.0)

      do i = 1,size
         if (cabs(arrayin(i)).GT.epsilon) then
           arrayout(i) = scale/cabs(arrayin(i))
         else
           arrayout(i) = 0.0
         endif
      enddo

      return
      end

C*************************************************************
      subroutine  icopvect(index,a,b,size)
c IN	: index,b,size
c OUT	: a
c
C   This subroutine copy b(index(i)) into a
C
      Implicit none

      integer i,size,index(size)
      real a(size),b(size)

#assert (size.gt.0)

      do 100,i=1,size
         a(i) = b(index(i))
100   continue
      return
      end

C*************************************************************
      subroutine  cicopvect(index,a,b,size,size1)

C CICOPVECT(INDX,A,B,N,N1) copy the complementary part of B(INDX(i)) into A
C Be careful! the index array INDX should be sorted in ascending order
C
      Implicit none

      integer i,j,k,size,size1,index(size1)
      real a(size),b(size)

#assert (size.gt.0)

      k = 1
      j = 1
      do 100,i=1,size
	 if (i.ne.index(k)) then
            a(j) = b(i)
c            write(*,*)'i,j',i,j
	    j = j + 1
	 else
	    k = k + 1
	 endif
100   continue
      return
      end

C*************************************************************
      subroutine  idotvect(index,d,a,b,size)
c IN	: index,a,b,size
c OUT	: d
c
C   This subroutine performs the dot product between vectors
c a(index()) and b()
C
      Implicit none

      integer i,size,index(size)
      real a(size),b(size),d
      real*8 dd

#assert (size.gt.0)

#vms      call lib$flt_under(0)
      dd = 0.0D0
      do 100,i=1,size
         dd = a(index(i))*b(i) + dd
100   continue
      d = dd
      return
      end

C*************************************************************
      subroutine  i2dotvect(index,d,a,b,size)
c IN	: index,a,b,size
c OUT	: d
c
C   This subroutine performs the dot product between vectors
c a(index()) and b(index())
C
      Implicit none

      integer i,size,index(size)
      real a(size),b(size),d
      real*8 dd

#assert (size.gt.0)

#vms      call lib$flt_under(0)
      dd = 0.0D0
      do 100,i=1,size
         dd = a(index(i))*b(index(i)) + dd
100   continue
      d = dd
      return
      end


C*************************************************************
      subroutine  idiavect(index,a,b,c,size)
c IN	: index,b,c,size
c OUT	: a
c
C   This subroutine multiplies the vector C(index()) by the diagonal matrix, whose
C diagonal is in B(index()), and put it in A
C
      Implicit none

      integer i,size,index(size)
      real a(size),b(size),c(size)

#assert (size.gt.0)

#vms      Call Lib$flt_under(0)
      do 100,i=1,size
         a(i) = b(index(i))*c(index(i))
100   continue
      return
      end

C*************************************************************
      subroutine  imltvect(index,a,b,l,size)
c IN	: index,b,l,size
c OUT	: a
C   This subroutine multiplies vector B(index()) by l and put it in A(index())
C
      Implicit none

      integer i,size,index(size)
      real a(size),b(size),l

#assert (size.gt.0)

      do 100,i=1,size
         a(index(i)) = l*b(index(i))
100   continue
      return
      end

C************************************************************* 
      subroutine  iaddvect(index,arrayout,B,C,l,size)
c IN	: index,B,C,l,size
c OUT	: arrayout
C   This subroutine put B + l*C(index()) into arrayout,
C A, B and C may be equal
C
      Implicit none

      integer i,size,index(size)
      real b(size),c(size),l,arrayout(size)

#assert (size.gt.0)

      if (l.eq.1.0) then
         do 100,i=1,size
             arrayout(i) = b(i) + c(index(i))
100      continue
      elseif (l.eq.-1.0) then
         do 200,i=1,size
             arrayout(i) = b(i) - c(index(i))
200      continue
      else
         do 300,i=1,size
             arrayout(i) = b(i) + l*c(index(i))
300      continue
      endif
      return
      end
C**********************************************************
       subroutine entier(espec1,rspec1,size,fact)
C IN	: rspec1,size,fact
c OUT	: espec1
c
C   This subroutine, given real vector "rspec1(size)", returns
C the corresponding integer vector "espec1(size)" such that
c espec1 = int( fact*rspec1 ) .
     
     
       implicit none
 
       integer size
       integer espec1(size)
       real    rspec1(size)
       real*8  fact
     
       integer j
                  
       do j=1,size
        espec1(j) = nint(rspec1(j)*fact)
       enddo
       return
       end
C*************************************************************
      subroutine  minvect(arrayout,B,C,size)
c IN	: B,C,size
c OUT	: arrayout
c 
C MAXVECT(A,B,C,N) put into the vector A, the smallest value of vector B and C

      Implicit none

      integer i,size
      real b(size),c(size),arrayout(size)

#assert (size.gt.0)

         do 100,i=1,size
             arrayout(i) = min(b(i),c(i))
100      continue

      return
      end
C*************************************************************
      subroutine  maxvect(arrayout,B,C,size)
c IN	: B,C,size
c OUT	: arrayout
c 
C MAXVECT(A,B,C,N) put into the vector A, the largest value of vector B and C

      Implicit none

      integer i,size
      real b(size),c(size),arrayout(size)

#assert (size.gt.0)

         do 100,i=1,size
             arrayout(i) = max(b(i),c(i))
100      continue

      return
      end
C*************************************************************
      subroutine  leftprod(vout,mat,vin,n,m)
c IN	: mat,vin,n,m
c OUT	: vout
c 
C computes the result of applying a left product with the
C matrix mat(n x m) on the vect vin(m x 1)
C i.e.  vout(n x 1) = mat(n x m) * vin(m x 1)

      Implicit none

      integer i,j,n,m
      real vout(n), mat(m,n), vin(m)
      real*8 s

#assert ((n*m).gt.0)

         do i=1,n
            s = 0.0d0
            do j=1,m
               s = s + mat(j,i)*vin(j)
            enddo
            vout(i) = s
         enddo

      return
      end
