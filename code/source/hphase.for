C This file is a part of the GIFA program
C     This software has been developped by the NMR Group in GIF/Yvette.
C
C     Authors :       M.A.Delsuc
C                     NMR Laboratory,
C                     ICSN - CNRS
C                     91190 GIF/YVETTE   FRANCE
C
C     This software cannot be used unless the user have been fully 
C licensed to do so form the above laboratory.
C     This file cannot be copied, duplicated or used in any other program,
C without written permission from the authors.
C

      subroutine hphase(rspec,p0,p1,n)
c IN	: p0,p1,n
c INOUT	: rspec
C  this subroutine applies a NMR-type phase correction to the REAL
C data A(i) of length N, with p0 as zero-order correction and p1 as first-order
C correction. P0 and P1 are both in degree.
C First the imaginary part is reconstructed from A: A is inverse Fourier 
C transformed,(real to complex), and dephased by 90 degrees, then Fourier 
C transformed (complex to real). The result is considered as the imaginary
C part of A: B
C  
C then  C(i)=(A(i),B(i)) is (complex) multiplied by exp(2*pi*(P0 + P1*(i-1/N))
C to give C': the result of this subroutine is the real part of C'
C

      Implicit none

      Integer n
      real rspec(n),p0,p1

#include "sizebase.inc"
      real workloc(sizemax)

      if (2*n .gt. sizemax) then
         call gifaout("*** Size too big for operation")
         return
      endif
      call copvect(workloc,rspec,n)
      call copvect(workloc(n+1),rspec,n)
      call fftreal(workloc(n+1),n,-1,1)
      call mltvect(workloc(n+1),workloc(n+1),2.0/float(n),n)
      call phase(workloc(n+1),-90.0,0.0,n/2)
      call fftreal(workloc(n+1),n,1,-1)
      call swavect(workloc,2*n)
      call phase(workloc,p0,p1,n)
      call ctrvect(rspec,workloc,n)

      return
      end


