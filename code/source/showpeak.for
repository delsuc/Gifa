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

      subroutine showpeak1d(zz,vd_id)
C display the peak zz on the 1D display vd_id
      implicit none
      integer zz, vd_id
#include "sizebase.inc"
#include "disptypev.inc"
#include "paramfilev.inc"
#include "zoomparamv.inc"
#include "peaksize.inc"
      real zz1, zz2, zz3
      integer j

       if (peak1d(zz,2).ge.0.0) then
               call gcolor(vd_id,scolor)
               zz1 = 0.96 - 0.03*mod(zz-1,4)
               if (zoom.eq.0) then
                if (it1d.eq.0) then
                  zz2 = (peak1d(zz,1)-1) / (sizeimage1d-1)
                else
                  zz2 = ((peak1d(zz,1)-1)/2) / (sizeimage1d/2-1)
                endif
               else
                if (it1d.eq.0) then
                   zz2 = (peak1d(zz,1)-zo1dl) / (zo1du-zo1dl)
                else
                   zz2 = ((peak1d(zz,1)-zo1dl)/2) /
     *                   float(int((zo1du-zo1dl+1)/2)-1)
                endif
               endif
               zz3 = 0.01
               call showpattern(vd_id,zz2,zz1,zz3,zz3,peakpat)
               j = len(peak1d_id(zz))
               call win_write(vd_id,zz2+2*zz3,zz1,peak1d_id(zz),j)
c               call topcross3(vd_id,zz2,zz1,zz3,peak1d_id(zz))
      endif
      call gcolor(vd_id,color)
      return
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine showpeak2d(zz,vd_id)
C display the peak zz on the 2D display vd_id
      implicit none
      integer zz, vd_id
#include "sizebase.inc"
#include "disptypev.inc"
#include "paramfilev.inc"
#include "zoomparamv.inc"
#include "peaksize.inc"
      real zz1, zz2, zz3
      integer j

       if (peak2d(zz,5).ge.0.0) then
               call gcolor(vd_id,scolor)
               if (zoom.eq.0) then
                    zz1 = (peak2d(zz,4)-0.5)/si1im
                    zz2 = (peak2d(zz,1)-0.5)/si2im
               else
                    zz2 = (peak2d(zz,1)-zolf2+0.5) / (zour2-zolf2+1)
                    zz1 = (peak2d(zz,4)-zolf1+0.5) / (zour1-zolf1+1)
               endif
               zz3 = 0.01
               call showpattern(vd_id,zz2,zz1,zz3,zz3,peakpat)
               j = len(peak2d_id(zz))
               call win_write(vd_id,zz2+2*zz3,zz1,peak2d_id(zz),j)
c               call topcross3(vd_id,zz2,zz1,zz3,peak2d_id(zz))
               call gcolor(vd_id,color)
         endif
         return
         end
         
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine showpeak3d(zz,vd_id)
C display the peak zz on the 3D display vd_id
      implicit none
      integer zz, vd_id
#include "sizebase.inc"
#include "disptypev.inc"
#include "paramfilev.inc"
#include "zoomparamv.inc"
#include "peaksize.inc"
      real zz1, zz2, zz3, zz4


      if (peak3d(zz,5).ge.0.0) then
               call gcolor(vd_id,scolor)
               if (zoom.eq.0) then
                zz1 = (peak3d(zz,4)-0.5)
                zz2 = (peak3d(zz,6)-0.5)
                zz3 = (peak3d(zz,1)-0.5)
               else
                zz1 = (peak3d(zz,4)-zo3df1l+0.5)
                zz2 = (peak3d(zz,6)-zo3df2l+0.5)
                zz3 = (peak3d(zz,1)-zo3df3l+0.5)
               endif
               zz4 = 1
               call topcross3d3(vd_id,zz3,zz2,zz1,zz4,peak3d_id(zz))
               call gcolor(vd_id,color)
       endif
       return
       end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine showpeak3d2d(zz,vd_id)
C display the 3D peak zz on the 2D display vd_id
      implicit none
      integer zz, vd_id
#include "sizebase.inc"
#include "disptypev.inc"
#include "paramfilev.inc"
#include "zoomparamv.inc"
#include "peaksize.inc"
      real zz1, zz2, zz3
      integer j

      zz3 = 0.01
      if (peak3d(zz,5).ge.0.0) then
          call gcolor(vd_id,scolor)
          if (axis2d.eq.1 .and.
     *           (abs(n2d-peak3d(zz,4)).lt.0.5)) then
            if (zoom.eq.0) then
                zz1 = (peak3d(zz,6)-0.5)/si1im
                zz2 = (peak3d(zz,1)-0.5)/si2im
            else
                zz2 = (peak3d(zz,1)-zolf2+0.5) / (zour2-zolf2+1)
                zz1 = (peak3d(zz,6)-zolf1+0.5) / (zour1-zolf1+1)
            endif
          elseif (axis2d.eq.2 .and.
     *            (abs(n2d-peak3d(zz,6)).lt.0.5)) then
            if (zoom.eq.0) then
                zz1 = (peak3d(zz,4)-0.5)/si1im
                zz2 = (peak3d(zz,1)-0.5)/si2im
            else
                zz2 = (peak3d(zz,1)-zolf2+0.5) / (zour2-zolf2+1)
                zz1 = (peak3d(zz,4)-zolf1+0.5) / (zour1-zolf1+1)
            endif
          elseif (axis2d.eq.3 .and. 
     *          (abs(n2d-peak3d(zz,1)).lt.0.5)) then
            if (zoom.eq.0) then
                zz1 = (peak3d(zz,4)-0.5)/si1im
                zz2 = (peak3d(zz,6)-0.5)/si2im
            else
                zz2 = (peak3d(zz,6)-zolf2+0.5) / (zour2-zolf2+1)
                zz1 = (peak3d(zz,4)-zolf1+0.5) / (zour1-zolf1+1)
            endif
            call showpattern(vd_id,zz2,zz1,zz3,zz3,peakpat)
            j = len(peak3d_id(zz))
            call win_write(vd_id,zz2+2*zz3,zz1,peak3d_id(zz),j)
c            call topcross2(vd_id,zz2,zz1,zz3,peak3d_id(zz))
          endif
          call gcolor(vd_id,color)
       endif
       return
       end
       
