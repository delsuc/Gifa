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
C Drivers for a HP-GL Plotter
C Open pl_id with:
C           open(unit=pl_id,file='name_of_plotter',status='new',
C     *         carriagecontrol='list')
C
C commands available are :
C  pl_config_hp(st)
C  plinit_hp(pl_id)
C  plhome_hp(pl_id)
C  plmove_hp(pl_id,x,y)
C  pldraw_hp(pl_id,x,y)
C  pldotdraw_hp(pl_id,x,y)
C  pltext_hp(pl_id,text,length_of_text,x,y)
C  pltexset_hp(pl_id,size_of_char,writing_angle)
C  plchpen_hp(pl_id,#pen)
C  plcolor_hp(pl_id,#color)
C  plchpage_hp(pl_id)
C  pltextdir_hp(pl_id,text_angle)
C penup keeps track of the position of the pen,   (1=down)
C***********************************************************************
C pl_config identifies plotter driver
      subroutine pl_config_hp(st)
c HPGL version of the generic plotter call
      implicit none
      character*(*) st
      st = 'HPGL'
      return
      end
C***********************************************************************
C plhome sends back the pen to home position
      subroutine plhome_hp(pl_id)
c HPGL version of the generic plotter call
      implicit none
#include "plotcomv.inc"
      integer pl_id
      if (penup.eq.0) then
        write(pl_id,*) 'PA0,0'
      else
        write(pl_id,*) 'PU;PA0,0'
        penup = 0
      endif
      return
      end
C***********************************************************************
C plinit sends back the pen to home position, initializes the plotter,
C and sets current default values for plotting,
      subroutine plinit_hp(pl_id)
c HPGL version of the generic plotter call
      implicit none
#include "plotcomv.inc"
#include "gifabcom.inc"
      integer pl_id
      if (license(1:ilenc).eq.'NON-VALID') then
         call plhome_hp(pl_id)
         call pltexset_hp(pl_id,3.0,30.0)
         call pltextdir_hp(pl_id,45)
         call pltext_hp(pl_id,'No Valid License', 16, 5.0, 5.0)
      endif
      write(pl_id,*) 'PU;PA0,0;IN;DF;'
      penup = 0
      call pltexset_hp(pl_id,.15,0.0)
      call pltextdir_hp(pl_id,0)
      return
      end
C***********************************************************************
C plmove move the pen of plotter pl_id to dx and dy centimeters, starting
C from current position, coordinates in absolute.
      subroutine plmove_hp(pl_id,dx,dy)
c HPGL version of the generic plotter call
      implicit none
#include "plotcomv.inc"
      integer pl_id
      real dx,dy

      integer lenx,leny
      character*80 stx,sty

      call convxy_hp(dx,dy,stx,sty,lenx,leny)

      if (penup.eq.0) then
         write(pl_id,*) 'PA', stx(1:lenx), ',', sty(1:leny)
      else
         penup = 0
         write(pl_id,*) 'PU;PA', stx(1:lenx), ',', sty(1:leny)
      endif
      return
      end

C***********************************************************************
C pldraw draw a solid line of dx and dy centimeters, starting
C from current position, coordinates in absolute.
      subroutine pldraw_hp(pl_id,dx,dy)
c HPGL version of the generic plotter call
      implicit none
#include "plotcomv.inc"
      integer pl_id
      real dx,dy

      integer lenx,leny
      character*80 stx,sty

      call convxy_hp(dx,dy,stx,sty,lenx,leny)

      if (penup.eq.0) then
         penup=1
         write(pl_id,*) 'PD;PA', stx(1:lenx), ',', sty(1:leny)
      else
         write(pl_id,*) 'PA', stx(1:lenx), ',', sty(1:leny)
      endif
      return
      end
C***********************************************************************
C pldotdraw draw a dotted line of dx and dy centimeters, starting
C from current position, coordinates in absolute.
      subroutine pldotdraw_hp(pl_id,dx,dy)
c HPGL version of the generic plotter call
      implicit none
#include "plotcomv.inc"
      integer pl_id
      real dx,dy

      integer lenx,leny
      character*80 stx,sty

      call convxy_hp(dx,dy,stx,sty,lenx,leny)

      if (penup.eq.0) then
        penup = 1
        write(pl_id,*) 'LT2,0.4;PD;PA', stx(1:lenx), ',', sty(1:leny)
      else
        write(pl_id,*) 'LT2,0.4;PA', stx(1:lenx), ',', sty(1:leny)
      endif
      write(pl_id,*) 'LT'
      return
      end
C***********************************************************************
C pltext writes the character string 'tex' of length 'lentext' at the
C position defined by 'xtext' and 'ytext' with current values for size
C and slope of characters
      subroutine pltext_hp(pl_id,tex,lentext,xtext,ytext)
c HPGL version of the generic plotter call
      implicit none
#include "plotcomv.inc"
      integer pl_id,lentext,ll
      real xtext,ytext
      character*256 tex
      call plmove_hp(pl_id,xtext,ytext)
      ll = lentext
      call trailing(tex(1:lentext),ll)
      write(pl_id,*) 'LO1;LB',tex(1:ll),char(3)
      return
      end
C***********************************************************************
C pltexset sets the values of the size of character strings to be
C written on the plotter, angl is for slant char
      subroutine pltexset_hp(pl_id,size,angl)
c HPGL version of the generic plotter call
      implicit none
#include "plotcomv.inc"
      integer pl_id
      real size,vsize,hbase,angl
      hbase=tan(angl*3.1415/180.0)
      vsize = 1.4*size

      write(pl_id,*) 'CS0;SI',size,vsize,';'
      write(pl_id,*) 'SL',hbase,';'
      return
      end
C***********************************************************************
C plchpen selects another pen for plotting,colour is the number of the selected
C pen , it depends of your choice in positionning the pens on the plotter!
C our choice is 1 = black , 2 = red , 3 = blue , 4 = green , 5 = violet
C , 6 = brown , 7 = orange.
      subroutine plchpen_hp(pl_id,colour)
c HPGL version of the generic plotter call
      implicit none
#include "plotcomv.inc"
      integer pl_id,colour
      write(pl_id,*) 'SP0;SP',colour,';'
      return
      end

      subroutine plcolor_hp(pl_id,colour)
C on hpgl, plcolor and plchpen are equivalent
      implicit none
      integer pl_id,colour
      call plchpen_hp(pl_id,colour)
      return
      end
C***********************************************************************
C plchpage changes the page on a plotter installed to do so
      subroutine plchpage_hp(pl_id)
c HPGL version of the generic plotter call
      implicit none
      integer pl_id
      write(pl_id,*) 'PG1;'
      return
      end
 
C***********************************************************************
C pltextdir changes the orientation of text for labels...
C 0  = horizontal text...
C 90 = vertical text...
      subroutine pltextdir_hp(pl_id,text_angle)
c HPGL version of the generic plotter call
      implicit none
#include "plotcomv.inc"
      integer pl_id
      integer text_angle,deltax,deltay

      if (text_angle.eq.0) then
        deltax = 0
        deltay = 1
      else
        deltax = int(100*cos(3.14159*text_angle/180.0))
        deltay = int(100*sin(3.14159*text_angle/180.0))
      endif

      write(pl_id,*) 'DI',deltay,deltax,';'
      return
      end

C***********************************************************************
      subroutine wtshort_hp(num,st,len)
c IN	: num
c OUT	: st,len
c
C wtshort_hp is used by the HPGL plot driver, removes leading blanks
c for numeric outputs.
C num is the number, return value is st, length of return value is len.
      implicit none
      integer num,len
      character*80 st

      write(st,10) num
10    format(i80)

      call leading(st)
      call trailing(st,len)
      return
      end

C***********************************************************************
      subroutine convxy_hp(x,y,stx,sty,lenx,leny)
 
      implicit none
      real x,y
      integer lenx,leny
      character*(*) stx,sty
#include "plotcomv.inc"
      integer xx,yy
 
      if (rotate.eq.1) then
         xx = int(400.0*(y+offy))
         yy = -int(400.0*(x+offx))
      else
         xx = int(400.0*(x+offx))
         yy = int(400.0*(y+offy))
      endif
 
      call wtshort_hp(xx,stx,lenx)
      call wtshort_hp(yy,sty,leny)

      return
      end
