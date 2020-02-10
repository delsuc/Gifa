C This file is a part of the GIFA program
C     This software has been developped by the NMR Group in GIF/Yvette.
C
C     Authors :       M.A.Delsuc, T.Malliavin
C                     NMR Laboratory,
C                     ICSN - CNRS
C                     91190 GIF/YVETTE   FRANCE
C
C     This software cannot be used unless the user have been fully 
C licensed to do so form the above laboratory.
C     This file cannot be copied, duplicated or used in any other program,
C without written permission from the authors.
C
       subroutine writec (name,spec,size1,size2,specw1,
     &		specw2,off1,off2,itype,rate,sar) 

c IN	: name,spec,size1,size2,specw1,specw2,off1,off2,itype,rate,sar
c
c writes the 
       implicit none         

#include "sizebase.inc"
#include "controlc.inc"
       integer size1,size2,itype,zz
       integer error,i,totsize,sar,delta(sizemax)
       real spec(size1*size2),ar(2*ldmax),fid(sizemax)	
       real specw1,specw2,off1,off2,rate
       real*8 power
       complex*16 afloc(ldmax),wk1loc(sizemax)
       complex*16 wk2loc(sizemax)
       character*256 fichier,name
        
       if (name.eq.' ') then
          write(*,20)
          call getstring(fichier)
20        format (' filename: ')
       else
          fichier = name
       endif
       call lpcopen (name,error)
       if (error.ne.0) then
          write(*,*) 'Unable to open the file!'
          return
       endif

       call burgc (spec,size2/2,sar,power,afloc,wk1loc,wk2loc,error)
c
c Need a double size because ar is declared as real and used as complex
c
       zz = 2*sar
       if (error.eq.1) return
       call dscopvect (ar,afloc,zz)
       call lpcwrthead (size1,size2,zz,ar,
     &		specw1,specw2,off1,off2,itype,error)
c
c Compact and write the FIDs
c first, let's reverse ar() to be able to use dotvect inside lpcomp
c
       call crevvect(ar,sar)
       do i = 1,size1
	  if (mod(i,16).eq.0) write(*,*)i,size2
	  call lpcomp (fid,ar,sar,spec(((i-1)*size2)+1),size2/2) 	
	  call iscopvect (delta,fid(zz+1),size2-zz)
c	  call copvect (spec(((i-1)*size2)+1),fid,size2)
          call lpcwrtfid (fid,delta,error) 
       enddo
       call lpcclose (totsize) 
       rate = 100.0*(1.-(float(totsize)/float(size1*size2)))
       return
       end


