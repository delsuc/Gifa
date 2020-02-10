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
      subroutine intrec(smx,si1,si2,somme,nbpoint,err)
c IN	: smx,si1,si2,err
c OUT	: somme,nbpoint
c
c computes the sum of the points in an area of the 2D data "smx"
c area is prompted from the user,
      implicit none
      
      integer si1, si2, llcol, lllin, urcol, urlin,
     *        line,nbcol,nblin,nbpoint,err
      real smx( si2*si1 ),somme,som
      save llcol,lllin,urcol,urlin

c asks for the area on which volume will be computed
      call message (' area where to integrate: ')
      call getrect(llcol,lllin,urcol,urlin,err)
      somme = 0.0
      if (err.ne.0) then
         return
      endif
      nbcol=urcol-llcol+1
      nblin=urlin-lllin+1
      nbpoint=nbcol*nblin

c calculates the vertical offset
      do 10 line=lllin,urlin
         call sumvect(som,smx(llcol+si2*(line-1)),nbcol)
         somme=somme+som
10    continue
      return
      end
