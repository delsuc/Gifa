C This file is a part of the GIFA program
C
C     Authors :       M.A.Delsuc
C
C     This software cannot be used unless the user have been fully 
C licensed to do so form the above laboratory.
C     This file cannot be copied, duplicated or used in any other program,
C without written permission from the authors.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine intrec1d(vect,si,somme,nbpoint,err)
c IN	: vect,si,err
c OUT	: somme,nbpoint
c
c computes the sum of the points in an area of the 1D data "vect"
c area is prompted from the user,
      implicit none
      
      integer si, llcol, lllin, urcol, urlin,
     *        nbpoint,err
      real vect( si ),somme,som
      save llcol,lllin,urcol,urlin

c asks for the area on which volume will be computed
      call message (' area to integrate: ')
      call getrect(llcol,lllin,urcol,urlin,err)
      somme = 0.0
      if (err.ne.0) then
         return
      endif
      nbpoint=urcol-llcol+1

c calculates the vertical offset
         call sumvect(som,vect(llcol),nbpoint)
         somme=som
      return
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine intrec2d(smx,si1,si2,somme,nbpoint,err)
c IN    : smx,si1,si2,err
c OUT   : somme,nbpoint
c
c computes the sum of the points in an area of the 2D data "smx"
c area is prompted from the user,
      implicit none

      integer si1, si2, llcol, lllin, urcol, urlin,
     *        line,nbcol,nblin,nbpoint,err
      real smx( si2*si1 ),somme,som
      save llcol,lllin,urcol,urlin

c asks for the area on which volume will be computed
      call message (' area to integrate: ')
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

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine maxinbox1d(vect,si,maxi,err)
c IN	: vect,si,err
c OUT	: max
c
c computes the largest of the points in an area of the 1D data "vect"
c area is prompted from the user,
      implicit none

      integer si, llcol, lllin, urcol, urlin, err, imaxi, imini
      real vect(si), maxi, mini
      save llcol,lllin,urcol,urlin

c asks for the area on which volume will be computed
      call message (' area to integrate: ')
      call getrect(llcol,lllin,urcol,urlin,err)
      if (err.ne.0) then
         return
      endif
      call mnxvect(mini,maxi,imini,imaxi,vect(llcol),urcol-llcol+1)
      return
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      subroutine maxinbox2d(smx,si1,si2,maxi,err)
c IN    : smx,si1,si2,err
c OUT   : maxi
c
c computes the largest of the points in an area of the 2D data "smx"
c area is prompted from the user,
      implicit none

      integer si1, si2, llcol, lllin, urcol, urlin,
     *        err, imaxi, imini, line, nbcol
      real smx(si2*si1), maxi, lmax, mini
      save llcol,lllin,urcol,urlin

c asks for the area on which volume will be computed
      call message (' area to integrate: ')
      call getrect(llcol,lllin,urcol,urlin,err)
      if (err.ne.0) then
         return
      endif
      nbcol=urcol-llcol+1

      maxi = 0.0
      do 10 line=lllin,urlin
        call mnxvect(mini,lmax,imini,imaxi,
     *          smx(llcol+si2*(line-1)),nbcol)  
        if (maxi.lt.mini) maxi=mini
        maxi = max(lmax,maxi)
10    continue
      return
      end

