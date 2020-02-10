c This file is a part of the GIFA program
C     This software has been developped by the NMR Group in GIF/Yvette.
C
C     Author :       A.Rouh
C                    NMR Laboratory,
C                    ICSN - CNRS
C                    91190 GIF/YVETTE   FRANCE
C
C     This software cannot be used unless the user have been fully
C licensed to do so form the above laboratory.
C     This file cannot be copied, duplicated or used in any other program,
C without written permission from the authors.
C

      subroutine initclassif(verbose,src,origine,size,classe,
     *nbclasse,rep,nbrep,nbpt)
c
c used by linecor2
c
      implicit none

#include "sizeparam.inc"
#include "linecor2v.inc"
#include "constant.inc"

      integer verbose,size,nbclasse,nbpt(nbclasse),nbrep
      real src(*),rep(nbclasse,nbrep),classe(size),
     *     origine(*)

      integer imin,imax,k,i,j
      real min,max,a,b,rsize

      do 10 k=1,nbrep
         i = (k-1)*bcmax
c         call mnxvect(min,max,imin,imax,src(1+i),size)
         call dotvect(max,src(1+i),src(1+i),size)
         rsize = size
         max = max / rsize
         max = sqrt(max)
         imin = i
c         max = abs(max)
c         min = abs(min)
c         if (min.gt.max) max = min
         if (max.lt.epsilon) max = 1
         a = nueefactor  / max
         call sumvect(b,src(i+1),size)
         b = b / rsize
         do 15 i=1,size
            min = (src(i+imin)-b)*a
            if (min.gt.0) then
               origine(i+imin) = log(1.0+min)
            else
               origine(i+imin) = log(1.0-min)
            endif
15       continue


10    continue
      imin = 1
      imax = 1
      a = 0
      if (nueedistance.eq.1) then
         do 20 k=1,nbrep
            j = (k-1)*bcmax
            a = a + origine(j+1) * origine(j+1)
20       continue
         a = sqrt(a)
      else
         do 30 k=1,nbrep
            j = (k-1)*bcmax
            a = a + abs(origine(j+1))
30       continue
      endif
      min = a
      max = a

      do 40 i=2,size
         a = 0
         if (nueedistance.eq.1) then
            do 50 k=1,nbrep
            j = (k-1)*bcmax
            a = a + origine(j+i) * origine(j+i)
50          continue
            a = sqrt(a)
         else
            do 60 k=1,nbrep
            j = (k-1)*bcmax
            a = a + abs(origine(j+i))
60          continue
         endif
         if (a.gt.max) then
            max = a
            imax = i
         endif
         if (a.lt.min) then
            min = a
            imin = i
         endif
40    continue

      do 70 k=1,nbrep
         j = (k-1)*bcmax
         rep(1,k) = origine(j+imax)
         rep(2,k) = origine(j+imin)
70    continue


      if (verbose.eq.1) then
         do 80 k=1,nbrep
            write(*,*)  'rep(1,' , k , ')=' , max
            write(*,*) 'rep(2,' , k , ')=' , min
80       continue
      endif

      return
      end

C********************************************************************
      subroutine classif(verbose,origine,size,classe,
     *          nbclasse,rep,nbrep,nbpt)
c
c used by linecor2
c


      implicit none

#include "sizeparam.inc"
#include "linecor2v.inc"

  
      integer size,nbclasse,nbpt(nbclasse),verbose,nbrep
      real origine(*),rep(nbclasse,nbrep),
     *      classe(size)
    
      integer it,stable,iclasse,k,i,j,maxit
      real rep2(2,3),dclasse,dclasse2,temp


      it = 0
      maxit = nueeiter

10    continue

c     repartition des points dans les classes

      call iprpvect(nbpt,0,nbclasse)
      do 20 i=1,size
         iclasse = 1
         dclasse = 0
         do 30 k=1,nbrep
            temp = (origine(i+(k-1)*bcmax)-rep(1,k))
            if (nueedistance.eq.1.0) then
            dclasse = dclasse + temp*temp
            else
            dclasse = dclasse + abs(temp)
            endif
30       continue
         if (nueedistance.eq.1.0) then
            dclasse = sqrt(dclasse)
         endif
         do 40 j=2,nbclasse
            dclasse2 = 0
            do 50 k=1,nbrep
               temp = (origine(i+(k-1)*bcmax)-rep(j,k))
               if (nueedistance.eq.1.0) then
               dclasse2 = dclasse2 + temp*temp
               else
               dclasse2 = dclasse2 + abs(temp)
               endif   
50          continue
         if (nueedistance.eq.1.0) then
            dclasse2 = sqrt(dclasse2)
         endif
            if (dclasse2.lt.dclasse) then
               dclasse = dclasse2
               iclasse = j
            endif
40       continue
         classe(i) = iclasse-1
         nbpt(iclasse) = nbpt(iclasse) + 1
20    continue

c     calcul des represantants des classes


      call copvect(rep2,rep,nbrep*nbclasse)
      call prpvect(rep,0.0,nbrep*nbclasse)

      do 60 i=1,nbclasse
         nbpt(i) = 0
60    continue

      do 70 i=1,size
         j = classe(i) + 1
         do 80 k=1,nbrep
            rep(j,k) = rep(j,k) + origine(i+(k-1)*bcmax)
80       continue
         nbpt(j) = nbpt(j) + 1
70    continue     

      if (verbose.eq.1) then
      do 81 i=1,nbclasse
         write(*,*) 'nbpt(',i,')=',nbpt(i)
81    continue
      endif


      do 90 i=1,nbclasse
         if (nbpt(i).ne.0) then
         do 100 j=1,nbrep
            rep(i,j) = rep(i,j) / nbpt(i)
100      continue
         endif
90    continue

      if (verbose.eq.1) then
         do 101 i=1,nbclasse
            do 102 j=1,nbrep
               write(*,*) 'rep(',i,';',j,')=',rep(i,j)
102         continue
101      continue
      endif


      stable = 1
      i = 0
110   continue
      i = i + 1
      do 120 k=1,nbrep
         if(rep2(i,k).ne.rep(i,k)) stable = 0
120   continue
      if (stable.ne.0 .and. i.lt.nbclasse) goto 110
      it = it + 1
      if (verbose.eq.1) write(*,*) 'it=',it
      if (stable.eq.0 .and. it.lt.maxit) goto 10

      return
      end

         
     
