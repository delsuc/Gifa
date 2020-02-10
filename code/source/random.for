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
      function randomd(idum)
c INOUT	: idum
c
C return a uniform random deviate between 0 and 1
C form Num. Recipes p197
c reinitialized if idum<0
c

      implicit none
      real randomd,rm
      integer ir(97),iff,iy,j,idum,m,ia,ic
      parameter (m=714025,ia=1366,ic=150889,rm=1.0/m)
      save
c      save ir,iff,iy,j
      data iff /0/

C initialize if idum is neg, or if it is the first time

      if (idum.lt.0.or.iff.eq.0) then
         iff=1
         idum=mod(ic-idum,m)
         do j=1,97
            idum=mod(ia*idum+ic,m)
            ir(j)=idum
         enddo
         idum=mod(ia*idum+ic,m)
         iy=idum
      endif
      j=1+(97*iy)*rm
      if (j.gt.97.or.j.lt.1)pause
      iy=ir(j)
      randomd=iy*rm
      idum=mod(ia*idum+ic,m)
      ir(j)=idum
      return
      end
C
C****************************************************************************
      function gasdev(idum)
c INOUT	: idum
c
C this function returns a normally distributed deviate with 0 mean and unit
C variance, usins RANDOMD(IDUM)
C form Num. Recipes p203
      
      implicit none
      real gasdev,v1,v2,r,fac,gset,randomd
      external randomd
      integer idum,iset
      save
c      save iset,gset

      data iset/0/
      if (iset.eq.0) then
1           v1=2.0*randomd(idum)-1.0
            v2=2.0*randomd(idum)-1.0
            R=v1**2+v2**2
            if (R.ge.1.0) goto 1
         fac=sqrt(-2.0*log(r)/r)
         gset=v1*fac
         gasdev=v2*fac
         iset=1
      else
         gasdev=gset
         iset=0
      endif
      return
      end
