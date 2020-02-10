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
       subroutine readc (name,spec,size1,size2,specw1,
     &		specw2,off1,off2,itype) 
c IN	: name
c OUT	: spec,size1,size2,specw1,specw2,off1,off2,itype
c
c reads-in a file in LPC compressed format
c
       implicit none                 

#include "sizebase.inc"
#include "controlc.inc"
       integer size1,size2,zz
       integer error,i,totsize,sar,itype,delta(sizemax)
       real spec(*),ar(2*ldmax),fid(sizemax)
       real specw1,specw2,off1,off2
       character*256 fichier,name
        
       if (name.eq.' ') then
          write(*,20)
          call getstring(fichier)
20        format (' filename: ')
       else
          fichier = name
       endif
       call lpcopen (fichier,error)
       if (error.ne.0) then
          write(*,*) 'Unable to open the file!'
          return
       endif

       call lpcreadhead(size1,size2,zz,ar,
     &		specw1,specw2,off1,off2,itype,error)
c
c The ar coeff read are taken real, then the prediction order is the half 
c of the polynomial size zz
c
       sar = zz/2
c
c Read and decompact the FIDs
c first, let's reverse ar() to be able to use dotvect
c
       call crevvect(ar,sar)
       do i = 1,size1
	  if (mod(i,16).eq.0) write(*,*)i,size2
          call lpcreadfid (fid,delta,error) 
	  call sicopvect (fid(1+zz),delta,size2-zz)
c	  call copvect (spec(((i-1)*size2)+1),fid,size2)
	  call lpdecomp(fid,ar,sar,spec(((i-1)*size2)+1),
     &		size2/2) 	
       enddo
       call lpcclose(totsize) 
       return
       end


