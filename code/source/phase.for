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
      subroutine phase(A,P0,P1,N)
c IN	: P0,P1,N
c INOUT	: A
c
C  this subroutine applies a NMR-type phase correction to the complex
C data A(i) of length N, with p0 as zero-order correction and p1 as first-order
C correction. P0 and P1 are both in degree.
c
C i.e    A(i) is (complex) multiplied by exp(2*pi*(P0 + P1*(i-1/N))

      Implicit none

      Integer N
      complex A(N)
      real P0, P1
 
      Integer i
      Real pp1, pp0, pi, z
      complex zz

      parameter (pi=3.1415926)

      pp1 = p1*pi/180.0
      pp0 = p0*pi/180.0 - pp1/2.0

      if (p1.ne.0.0) then
        do 100,i=1,n
           z = pp0 + pp1*float((i-1))/(n-1)
           zz = cmplx( cos(z), sin(z) )
           a(i) = a(i)*zz
100     continue
      else
        zz = cmplx( cos(pp0), sin(pp0) )
        do 110,i=1,n
           a(i) = a(i)*zz
110     continue
      endif
      return
      end

