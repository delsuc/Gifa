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
       subroutine lhare(tdata,size,name,noise,error)
c INOUT	: name
c OUT	: tdata,size,noise,error
c
c reads a file called "name" in 1D D.Hare format
       implicit none

       character*256   fichier,name
       integer     i,error,size
       real        tdata(*),noise,shift
#include "sizeparam.inc"

       data fichier/'noname'/
        
       if (name.eq.' ') then
          write(*,20)
          call getstring(fichier)
20        format (' filename: ')
       else
          fichier = name
       endif
       open(unit=10,file=fichier,iostat=error,
#vms     *     readonly,
     *     status='old',form='unformatted')
       if (error.ne.0) return
101    read(10,iostat=error) size,(tdata(i),i=1,min(smxbig,size*2))
       if (error.ne.0) return
c       if (size.le.0) goto 101    ! modif MAD 5 mai 1995
       if (size.lt.0) then
           write (*,*) '*** Wrong size '
           error = 1
           return
       endif

       size=size*2
       if (size.gt.smxbig) goto 97
       write (*,*) 'size of data: ',size
C backspace / param removed for speed -> use cache file format
c       call readparam1d(10)
       close(10)
 
C evaluate noise 
       noise=0
       shift=0
       i=size*9/10
       call evaln(tdata(i),size-i+1,noise,shift)
       return

97    write(*,*) '*** Size of FID is too big'
      error=1
      return
      end
