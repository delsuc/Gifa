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
      subroutine evaln(array,size,noise,shift)
c IN	: array,size
C OUT	: noise,shift
c
C evaluates the noise from the data
c the mean is returned in shift
c the standard deviation is returned in noise

      implicit none          

      integer size
      real   array(size),noise,shift
      integer i

      shift=0
      call sumvect(shift,array,size) 
      shift=shift/size
      noise=0
      do 10 i=1,size
        noise=noise + (array(i)-shift)**2
10    continue
      noise = sqrt(noise/size)
      return
      end
