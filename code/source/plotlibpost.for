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
C Drivers for a PostScript device.
C
C commands available are :
C  pl_config_ps(st)
C  plinit_ps(pl_id)
C  plhome_ps(pl_id)
C  plmove_ps(pl_id,x,y)
C  pldraw_ps(pl_id,x,y)
C  pldotdraw_ps(pl_id,x,y)
C  pltext_ps(pl_id,text,length_of_text,x,y)
C  pltexset_ps(pl_id,size_of_char,slant_angle)
C  plchpen_ps(pl_id,#pen)
C  plcolor_ps(pl_id,#color)
C  plchpage_ps(pl_id)
C  pltextdir_ps(pl_id,text_angle)
C***********************************************************************
C pl_config_ps identifies plotter driver
      subroutine pl_config_ps(st)
c Postscript version of the generic plotter call
      implicit none
      character*(*) st
      st = 'postscript'
      return
      end
C***********************************************************************
C plinit sends back the pen to home position, initializes the plotter,
C and sets current default values for plotting,
      subroutine plinit_ps(pl_id)
c Postscript version of the generic plotter call
      implicit none

#include "plotcomv.inc"
#include "gifabcom.inc"

      integer pl_id
#vms      character*9 dte
#vms      character*8 tme
 
#vms      call date(dte)
#vms      call time(tme)
 
      penup = 1
#vms          write(pl_id,*) '%!PS-Adobe-2.0'
#_vms          write(pl_id,10)
#_vms 10       format('%!PS-Adobe-2.0')
      write(pl_id,*) 'initgraphics initmatrix'
 
      write(pl_id,*) '%%Creator: GIFA_V4'
      write(pl_id,*) '%%DocumentFonts: Helvetica'
      write(pl_id,*) '%%BoundingBox: 17 22 554 820'
      write(pl_id,*) '%%Pages: (atend)'
      write(pl_id,*) '%%EndComments'
      write(pl_id,*) '/l /lineto load def'
      write(pl_id,*) '/m /moveto load def'
      write(pl_id,*) '/s /stroke load def'
      write(pl_id,*) '/n /newpath load def'
      write(pl_id,*) '/wp$x 600 def'
      write(pl_id,*) '/wp$y 900 def'
      write(pl_id,*) '/printDraft'
      write(pl_id,*) '{0 0 moveto TextToPrint show} def'
      write(pl_id,*) '%%EndProlog'
      write(pl_id,*) '%%Page: 1 1'
      if (license(1:ilenc).eq.'NON-VALID') then
        write(pl_id,*) 'gsave'
        write(pl_id,*) '/Times-BoldItalic findfont'
        write(pl_id,*) '/FontSize 48 def'
        write(pl_id,*) '/TextToPrint (No Valid License) def'
        write(pl_id,*) 'FontSize scalefont setfont'
        write(pl_id,*) 'wp$x 2 div wp$y 2 div translate'
        write(pl_id,*) '-45 rotate'
        write(pl_id,*) 'TextToPrint stringwidth pop 2 div neg'
        write(pl_id,*) 'FontSize 2 mul 3 div 2 div neg'
        write(pl_id,*) 'translate'
        write(pl_id,*) '.95 -.03 .8'
        write(pl_id,*) '{setgray printDraft -1 .5 translate} for'
        write(pl_id,*) '.95 setgray printDraft'
        write(pl_id,*) 'grestore'
      endif
      call pltexset_ps(pl_id,0.25,0)
      call pltextdir_ps(pl_id,0)
      call plchpen_ps(pl_id,1)
      write(pl_id,*) '0 setgray' !Reset to pen 1=Black 100% grayscaling...
      write(pl_id,*) 'n'
 
      return
      end
C***********************************************************************
      subroutine convxy_ps(x,y,retx,rety)
c Postscript version of the generic plotter call
      implicit none
      real x, y, retx, rety
 
#include "plotcomv.inc"
      real psminx,psmaxx,psmaxy
      real cmtodpi, xx, yy
      real psminy
 
      data  psminx, psmaxx, psminy, psmaxy, cmtodpi
     */      17.0,  575.0,  22.0,   820.0, 28.34645669/

      if (rotate.eq.1) then
         xx = y + offy
         yy = psmaxx/cmtodpi-x-offx
      else
         xx = x+offx
         yy = y+offy
      endif
 
      rety = psmaxy-(xx*cmtodpi)
      retx = (yy*cmtodpi)+psminx
 
c      if (rety.lt.psminy) rety = psminy
c      if (rety.gt.psmaxy) rety = psmaxy
c      if (retx.lt.psminx) retx = psminx
c      if (retx.gt.psmaxx) retx = psmaxx
 
      return
      end
C***********************************************************************
C plhome sends back the pen to home position
      subroutine plhome_ps(pl_id)
c Postscript version of the generic plotter call
      implicit none

#include "plotcomv.inc"
 
      integer pl_id
 
      call plmove_ps(pl_id,0.0,0.0)
      return
      end
C***********************************************************************
C plmove move the pen of plotter pl_id to dx and dy centimeters, starting
C from current position, coordinates in absolute.
      subroutine plmove_ps(pl_id,dx,dy)
c Postscript version of the generic plotter call
      implicit none
 
#include "plotcomv.inc"
 
      integer pl_id
      real dx,dy
      real absx,absy
 
      call convxy_ps(dx,dy,absx,absy)
      if (penup.eq.0) then
         write(pl_id,*) 's'
         penup = 1
      endif
      write(pl_id,10) absx,absy
10    format(x,'n',2f8.2,' m')
      count = 0
      return
      end
 
C***********************************************************************
C pldraw draw a solid line of dx and dy centimeters, starting
C from current position, coordinates in absolute.
      subroutine pldraw_ps(pl_id,dx,dy)
c Postscript version of the generic plotter call
      implicit none
#include "plotcomv.inc"
      integer pl_id
      real dx,dy
      real absx, absy
 
      call convxy_ps(dx,dy,absx,absy)
 
      penup = 0
      count = count+1
      if (count.ge.520) then
         write(pl_id,10) absx,absy,absx,absy
10       format(x,2f8.2,' l s n ',2f8.2,' m')
         count = 0
      else
         write(pl_id,11) absx,absy
11       format(x,2f8.2,' l')
      endif
      return
      end
C***********************************************************************
C pldotdraw draw a dotted line of dx and dy centimeters, starting
C from current position, coordinates in absolute.
      subroutine pldotdraw_ps(pl_id,dx,dy)
c Postscript version of the generic plotter call
      implicit none
 
#include "plotcomv.inc"
 
      integer pl_id
      real dx,dy
      real absx, absy
 
      call convxy_ps(dx,dy,absx,absy)
 
      write(pl_id,*) 'gsave [2 2] 0 setdash '
      write(pl_id,10) absx,absy
10    format(x,2f8.2,' l s')
      write(pl_id,*) ' grestore '
      penup = 0
 
      return
      end
C***********************************************************************
C pltext writes the character string 'tex' of length 'lentext' at the
C position defined by 'xtext' and 'ytext' with current values for size
C and slope of characters
      subroutine pltext_ps(pl_id,tex,lentext,xtext,ytext)
c Postscript version of the generic plotter call
      implicit none
 
#include "plotcomv.inc"
 
      integer pl_id,lentext,ll
      real xtext,ytext
      real absx, absy
      integer angle
      character*256 tex
      character*512 textloc
      character*80 stangle
      integer lena,i,j
 
      if (rotate.eq.1) then
         angle = plottextangle - 180
      else
         angle = plottextangle - 90
      endif
      call convxy_ps(xtext,ytext,absx,absy)
c     Offsets to correct where labels appear on page...
c      if (angle.eq.0) then
c         absx = absx + 40.0
c         absy = absy - 25.0
c      else
c         absx = absx + 25.0
c         absy = absy - 40.0
c      endif
 
      call wtshort_ps(angle,stangle,lena)
 
      call trailing(tex(1:lentext),ll)
c copy tex to textloc and protect all ) and ( with \
      j = 0
      do i = 1, ll
         if (tex(i:i) .eq. '(' .or. tex(i:i) .eq. ')') then
           j = j+1
#hp           textloc(j:j) = '\'
#sgi           textloc(j:j) = '\\'
#sun           textloc(j:j) = '\\'
#f2c           textloc(j:j) = '\\'
#aix           textloc(j:j) = '\\'
#adec           textloc(j:j) = '\\'
         endif
         j = j+1
         textloc(j:j)  = tex(i:i)
      enddo
      ll = j
      
      if (penup.eq.0) then
         penup = 1
         write(pl_id,*) 's'
      endif
c      write(pl_id,*) '% Label: ',tex(1:ll)
c     write(pl_id,*) stx(1:lenx),' ',sty(1:leny),' m'
      write(pl_id,*) absx,' ',absy,' m'
      if (angle.ne.0) then
         write(pl_id,*) 'gsave ',stangle(1:lena),' rotate'
      endif
      write(pl_id,*) '(',textloc(1:ll),') show'
      if (angle.ne.0) then
         write(pl_id,*) ' grestore '
      endif
      return
      end
C***********************************************************************
C pltexset sets the values of the size of character strings to be
C written on the plotter
      subroutine pltexset_ps(pl_id,size,angl)
c Postscript version of the generic plotter call
      implicit none
      integer pl_id
 
#include "plotcomv.inc"
 
      real size
      integer angl
      integer vsize,dpi
      character*80 vsizestr
      character*18 font
      integer vsizelen
 
      dpi = 72  !72 dpi for Apple LaserWriter
      if (angl.ne.0) then
        font = '/Helvetica-Oblique'
      else
        font = '/Helvetica        '
      endif
 
      vsize = int((size/2.54)*dpi)
 
      call wtshort_ps(vsize,vsizestr,vsizelen)
 
c      write(pl_id,*) '% *************************'
c      write(pl_id,*) '% * Pltext Font Setup...  *'
c      write(pl_id,*) '% *************************'
 
      write(pl_id,*) font,' findfont'
      write(pl_id,*) vsizestr(1:vsizelen),' scalefont'
      write(pl_id,*) 'setfont'
      penup = 1
      return
      end
C***********************************************************************
C pltextdir sets the values of the angle.. strings to be
C written on the plotter
      subroutine pltextdir_ps(pl_id,angle)
c Postscript version of the generic plotter call
      implicit none
      integer pl_id
 
#include "plotcomv.inc"
 
      integer angle
      plottextangle = angle
      return
      end
C***********************************************************************
C plchpen selects another pen for plotting,colour is the number of the selected
C pen ,
C in postscript it becomes :
C 1 normal, 2 small dash, 3 big dash, 4 asymetric dash, 5-8 same effect with
C larger line.
C
      subroutine plchpen_ps(pl_id,colour)
c Postscript version of the generic plotter call

      implicit none
 
#include "plotcomv.inc"
 
      integer pl_id,colour,colorlen
      character*80 colorstr
 
      call wtshort_ps(colour,colorstr,colorlen)
 
      if (penup.eq.0) then
         write(pl_id,*) 's'
         penup = 1
      endif
      if (colour.eq.1) then
         write(pl_id,*) '.25 setlinewidth'
         write(pl_id,*) '[] 0 setdash'       ! no dash
      elseif (colour.eq.2) then
         write(pl_id,*) '.25 setlinewidth'
         write(pl_id,*) '[4 2] 0 setdash'    ! small gaps
      elseif (colour.eq.3) then
         write(pl_id,*) '.25 setlinewidth'
         write(pl_id,*) '[2 4] 0 setdash'    ! big gaps
      elseif (colour.eq.4) then
         write(pl_id,*) '.25 setlinewidth'
         write(pl_id,*) '[8 2 2 2] 4 setdash' ! 2 gaps
      elseif (colour.eq.5) then
         write(pl_id,*) '1 setlinewidth'     ! big line
         write(pl_id,*) '[] 0 setdash'       ! no dash
      elseif (colour.eq.6) then
         write(pl_id,*) '1 setlinewidth'
         write(pl_id,*) '[4 2] 0 setdash'    ! small gaps
      elseif (colour.eq.7) then
         write(pl_id,*) '1 setlinewidth'
         write(pl_id,*) '[2 4] 0 setdash'    ! big gaps
      elseif (colour.eq.8) then
         write(pl_id,*) '1 setlinewidth'
         write(pl_id,*) '[8 2 2 2] 4 setdash' ! 2 gaps
      endif
 
      return
      end
 
C***********************************************************************
C plcolor selects another color, specific to postscript
C
      subroutine plcolor_ps(pl_id,colour)
c Postscript version of the generic plotter call
      implicit none
 
#include "plotcomv.inc"
 
      integer pl_id,colour

      if (penup.eq.0) then
         write(pl_id,*) 's'
         penup = 1
      endif
      if (colour.eq.1) then
         write(pl_id,*) '0.0 0.0 0.0 setrgbcolor'
      elseif (colour.eq.2) then
         write(pl_id,*) '1.0 0.0 0.0 setrgbcolor'
      elseif (colour.eq.3) then
         write(pl_id,*) '1.0 1.0 0.0 setrgbcolor'
      elseif (colour.eq.4) then
         write(pl_id,*) '0.0 1.0 0.0 setrgbcolor'
      elseif (colour.eq.5) then
         write(pl_id,*) '0.0 1.0 1.0 setrgbcolor'
      elseif (colour.eq.6) then
         write(pl_id,*) '0.0 0.0 1.0 setrgbcolor'
      elseif (colour.eq.7) then
         write(pl_id,*) '1.0 0.0 1.0 setrgbcolor'
      elseif (colour.eq.8) then
         write(pl_id,*) '1.0 1.0 1.0 setrgbcolor'
      endif
 
      return
      end
 
C***********************************************************************
C plchpage changes the page on a plotter installed to do so
C***********************************************************************
      subroutine plchpage_ps(pl_id)
c Postscript version of the generic plotter call
      implicit none
      integer pl_id
#include "plotcomv.inc"
 
      if (penup.eq.0) then
         write(pl_id,*) 's'
         penup = 1
      endif
c      write(pl_id,*) '% *********************'
c      write(pl_id,*) '% *  Change Pages...  *'
c      write(pl_id,*) '% *********************'
      write(pl_id,*) 'gsave showpage grestore'
       write(pl_id,*) '%%Trailer'
       write(pl_id,*) '%%Pages: 1'

      return
      end
 
C***********************************************************************
      subroutine wtshort_ps(num,st,len)
c IN	: num
c OUT	: st,len
c
C wtshort_ps is used by the postscript plot driver, removes leading blanks
c for numeric outputs.
C num is the number, return value is st, length of return value is len.
      implicit none
      integer num,len
      character*80 st
      integer i
 
      write(st,10) num          !Convert the number to a string
10    format(i80)               !Format with this line...
 
      i = 1                     !Count the number of spaces
20    if (st(i:i).eq.' ') then  ! at the beginning.
         i = i+1
         goto 20
      endif
      len = 80-i+1
      st(1:len) = st(i:80)      !Chop off i spaces from beginning.
      return
      end

