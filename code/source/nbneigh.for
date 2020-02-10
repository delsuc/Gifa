C This file is a part of the GIFA program
C     This software has been developped by the NMR Group in CBS/Montpellier
C
C     Authors :       Therese Malliavin
C                     C.B.S. Faculte de Pharmacie
C                     34060 Montpellier   FRANCE
C
C     This software cannot be used unless the user have been fully
C licensed to do so form the above laboratory.
C     This file cannot be copied, duplicated or used in any other program,
C without written permission from the authors.

      function nbneigh (masktab,taille2,taille1,crdf2,crdf1,
     & pkno)
c
c IN masktab,taille2,taille1,crdf1,crdf2,pkno
c OUT nbneigh
c
c For a given pixel(crdf2,crdf1) and a given peak number
c pkno, calculate
c the number of the neighboring pixels belonging to the 
c peak amoeba
c if (x,y) are the pixel coordinates, the considered pixel
c neighbors have the following coordinates: (x-1,y),(x+1,y),
c (x,y-1) and (x,y+1)
c
      implicit none

      integer crdf1,crdf2,taille2,taille1
      integer pkno,nbneigh,nbtmp
      real masktab(taille2,taille1)
      
      nbtmp = 0
      if (masktab(crdf2-1,crdf1).eq.pkno) then
         nbtmp = nbtmp + 1
      endif
      if (masktab(crdf2+1,crdf1).eq.pkno) then
         nbtmp = nbtmp + 1
      endif
      if (masktab(crdf2,crdf1-1).eq.pkno) then
         nbtmp = nbtmp + 1
      endif
      if (masktab(crdf2,crdf1+1).eq.pkno) then
         nbtmp = nbtmp + 1
      endif
      nbneigh = nbtmp

      return
      end
c
      function nbneigha (masktab,taille2,taille1,crdf2,crdf1,
     & pkno)
c
c IN masktab,taille2,taille1,crdf1,crdf2,pkno
c OUT nbneigh
c
c For a given pixel(crdf2,crdf1) and a given peak number
c pkno, calculate
c the number of the neighboring pixels belonging to the 
c peak amoeba
c if (x,y) are the pixel coordinates, the considered pixel
c neighbors have the following coordinates: (x-1,y),(x+1,y),
c (x,y-1),(x,y+1),(x-1,y-1),(x+1,y+1),(x-1,y+1),(x+1,y-1)
c
      implicit none

      integer crdf1,crdf2,taille2,taille1
      integer pkno,nbneigha,nbtmp
      real masktab(taille2,taille1)
      
      nbtmp = 0
      if (masktab(crdf2-1,crdf1).eq.pkno) then
         nbtmp = nbtmp + 1
      endif
      if (masktab(crdf2+1,crdf1).eq.pkno) then
         nbtmp = nbtmp + 1
      endif
      if (masktab(crdf2,crdf1-1).eq.pkno) then
         nbtmp = nbtmp + 1
      endif
      if (masktab(crdf2,crdf1+1).eq.pkno) then
         nbtmp = nbtmp + 1
      endif
      if (masktab(crdf2+1,crdf1+1).eq.pkno) then
         nbtmp = nbtmp + 1
      endif
      if (masktab(crdf2-1,crdf1-1).eq.pkno) then
         nbtmp = nbtmp + 1
      endif
      if (masktab(crdf2-1,crdf1+1).eq.pkno) then
         nbtmp = nbtmp + 1
      endif
      if (masktab(crdf2+1,crdf1-1).eq.pkno) then
         nbtmp = nbtmp + 1
      endif
      nbneigha = nbtmp

      return
      end


