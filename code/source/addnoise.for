C This file is a part of the GIFA program
C     This software has been developped by the NMR Group in CBS/Montpellier
C
C     Authors :       T.E. Malliavin M.A.Delsuc 
C                     C.B.S. Faculte de Pharmacie
C                     34060 Montpellier   FRANCE
C
C     This software cannot be used unless the user have been fully
C licensed to do so form the above laboratory.
C     This file cannot be copied, duplicated or used in any other program,
C without written permission from the authors.
C

      subroutine addnoise(tdata,size,noise,error)
c INOUT	: tdata
c IN	: size,noise
c OUT   : error
c
c add noise to the data
c
      implicit none

#include "paramfilev.inc"	

      integer i,seed,size,error
      real tdata(*)
      real  noise,gasdev,zz

      save  zz,seed

C init tdata                    

      call message( 'amount of noise')
      call getreal2( zz,error)
      if (error.ne.0) goto 62
      noise=zz
      if (noise.lt.0.0) goto 61
      if (noise.ne.0.0) then
         call message( 'enter seed')
         call getint2( seed,error)
         if (error.ne.0) goto 62
      endif

      if (noise.ne.0.0) then
         do i=1,size
            tdata(i) = tdata(i) + (noise*gasdev(seed))
         enddo
      endif
      return

c negative or nul t2	
 61   call gifaout('noise should be positive')
      error = 1
      return

c error in getting parameter
62    call gifaout('Error with value')
      return	
	
      end
