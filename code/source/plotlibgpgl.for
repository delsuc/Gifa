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
C Drivers for a GP-GL Plotter
C***********************************************************************
C pl_config identifies plotter driver
      subroutine pl_config(st)
      character*80 st
      st = 'GP-GL'
      return
      end
C***********************************************************************
C plhome send back the pen to home position
      subroutine plhome(pl_id)
      integer pl_id
      write(pl_id,10)
10    format('H')
      return
      end
C***********************************************************************
C plmove move the pen of plotter pl_id by dx and dy centimeters, starting
C from current position, coordinates in absolute.
      subroutine plmove(pl_id,dx,dy)
      implicit none
      integer pl_id
      real dx,dy
      integer idx,idy

      idx = 100*dx
      idy = 100*dy
      write(pl_id,10) idx,idy,char(3)
10    format('M',i5,',',i5,A1,$)
      return
      end

C***********************************************************************
C pldraw draw a line of dx and dy centimeters, starting
C from current position, coordinates in absolute.
      subroutine pldraw(pl_id,dx,dy)
      implicit none
      integer pl_id
      real dx,dy
      integer idx,idy

      idx = 100*dx
      idy = 100*dy
      write(pl_id,10) idx,idy,char(3)
10    format('D',i5,',',i5,A,$)
      return
      end
