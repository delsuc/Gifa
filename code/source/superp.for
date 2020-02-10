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
       subroutine superp(plane2d,data)
c
c oldtab(taille1,taille2): old data array
c newtab(taille1,taille2): new data array
c nopkold: old peak number
c nopknew: new peak number
c 
c IN    : oldtab
c OUT   : newtab
c
c create the newtab array in the data workspace, corresponding to the
c spectral widthes,sizes, frequencies and offsets of the data array,
c from the plane2d array, and its parameters (specw, freq, offset, size).
c
       implicit none

#include "paramfilev.inc"
#include "paramdatav.inc"
       real plane2d(si2im,si1im),data(si2dat,si1dat)
       real p1new,p2new,itopr,ptoir,xi,xj,tmp
       integer i,j,iold,jold,closest

       do i=1,si1dat
         xi = real(i)
         p1new = itopr(xi,si1dat,sp1dat,of1dat,freq1dat)
         tmp = ptoir(p1new,si1im,specw1,offset1,freq1)
         iold = closest(tmp)
         do j=1,si2dat
            xj = real(j)
            p2new = itopr(xj,si2dat,sp2dat,of2dat,freq2dat)
            tmp = ptoir(p2new,si2im,specw2,offset2,freq2)
            jold = closest(tmp)
c            write(*,*)'iold',iold,'jold',jold
            data(i,j) = plane2d(iold,jold)
         enddo
       enddo       
c
       return
       end
