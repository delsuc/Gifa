C This file is a part of the GIFA program
C     This software has been developped by the NMR Group in CBS/Montpellier
C
C     Authors :       T.E.Malliavin
C                     C.B.S. Faculte de Pharmacie
C                     34060 Montpellier   FRANCE
C
C     This software cannot be used unless the user have been fully
C licensed to do so form the above laboratory.
C     This file cannot be copied, duplicated or used in any other program,
C without written permission from the authors.

      subroutine doslide (indxx,indxy,x0,y0,rad)
c
c IN x0,y0,rad
c OUT indxx,indxy
c
c find the coordinates of the pixels belonging to
c the square number rad around the pixel (x0,y0)
c
      implicit none

      integer rad
      integer indxx(8*rad),indxy(8*rad)
      integer x0,y0,k,u

c find the coordinates of the side pixels
c from 1 to 2rad - 1
      do k = -rad+1,rad-1,1
	 u = k + rad 
 	 indxx(u) = x0 + rad
 	 indxy(u) = y0 + k
      enddo
c from 2rad to 4rad - 2
      do k = -rad+1,rad-1,1
	 u = k + (3*rad) - 1
 	 indxx(u) = x0 - rad
 	 indxy(u) = y0 + k
      enddo
c from 4rad - 1 to 6rad - 3
      do k = -rad+1,rad-1,1
	 u = k + (5*rad) - 2
 	 indxx(u) = x0 + k
 	 indxy(u) = y0 + rad
      enddo
c from 6rad - 2 to 8rad - 4
      do k = -rad+1,rad-1,1
	 u = k + (7*rad) - 3
 	 indxx(u) = x0 + k
 	 indxy(u) = y0 - rad
      enddo
c find the coordinates of the angle pixels
      indxx((8*rad) - 3) = x0 + rad
      indxy((8*rad) - 3) = y0 + rad
      indxx((8*rad) - 2) = x0 - rad
      indxy((8*rad) - 2) = y0 + rad
      indxx((8*rad) - 1) = x0 - rad
      indxy((8*rad) - 1) = y0 - rad
      indxx(8*rad) = x0 + rad
      indxy(8*rad) = y0 - rad

      return
      end
