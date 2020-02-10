C This file is a part of the GIFA program
C     This software has been developped by the NMR Group in CBS/Montpellier
C
C     Authors :       M.A.Delsuc
C                     C.B.S. Faculte de Pharmacie
C                     34060 Montpellier   FRANCE
C
C     This software cannot be used unless the user have been fully
C licensed to do so form the above laboratory.
C     This file cannot be copied, duplicated or used in any other program,
C without written permission from the authors.

      subroutine grid2(pl_id,cx,cy,n)
c IN	: pl_id,cx,cy,n
C plots a grid of n vertical stripes on a cx by cy square
c
      implicit none
      integer pl_id,n
      real cx,cy
      integer i
      real x,y1,y2,dx

      if (n.le.1) return
      dx = cx/n
      x = 0.0
      y1 = 0.0
      y2 = cy
      do i=1,n+1
        call plmove(pl_id,x,y1)
        call pldraw(pl_id,x,y2)
        x = x+dx
      enddo
      return
      end


      subroutine grid1(pl_id,cx,cy,n)
c IN	: pl_id,cx,cy,n
C plots a grid of n horizontal stripes on a cx by cy square
      implicit none
      integer pl_id,n
      real cx,cy
      integer i
      real x1,x2,y,dy

      if (n.le.1) return
      dy = cy/n
      y = 0.0
      x1 = 0.0
      x2 = cx
      do i=1,n+1
        call plmove(pl_id,x1,y)
        call pldraw(pl_id,x2,y)
        y = y+dy
      enddo
      return
      end


