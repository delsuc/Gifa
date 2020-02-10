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
C implements a software interface between GIFA plotter calls and plot libraries
C specific to a given plotter language

C  pl_config(st)
C  plinit(pl_id)
C  plhome(pl_id)
C  plmove(pl_id,x,y)
C  pldraw(pl_id,x,y)
C  pldotdraw(pl_id,x,y)
C  pltext(pl_id,text,length_of_text,x,y)
C  pltexset(pl_id,size_of_char,slant_angle)
C  plchpen(pl_id,#pen)
C  plcolor(pl_id,#color)
C  plchpage(pl_id)
C  pleject(pl_id)
C  pltextdir(pl_id,text_angle)
C  plopen(pl_id,name,error)  str int
C  plclose(pl_id)
C***********************************************************************
      subroutine pl_config(st)
c OUT	: st
C pl_config identifies plotter driver currently used by the generic 
c plotter library
c mode currently active : postscript and HP-GL
c
      implicit none
#include "plotcomv.inc"
      character*(*) st
      if (pl_mode.eq.post) then
        st = 'postscript'
      elseif (pl_mode.eq.hpgl) then
        st = 'HP-GL'
      endif 
      return
      end
C***********************************************************************
      subroutine plinit(pl_id)
c IN	: pl_id
c generic plotter library
C plinit sends back the pen to home position, initializes the plotter,
C and sets current default values for plotting,
      implicit none

#include "plotcomv.inc"
 
      integer pl_id
      if (pl_mode.eq.post) then
         call plinit_ps(pl_id)
      elseif (pl_mode.eq.hpgl) then
         call plinit_hp(pl_id)
      endif 
      return
      end
C***********************************************************************
      subroutine plhome(pl_id)
c IN	: pl_id
c generic plotter library
C plhome sends back the pen to home position
      implicit none
#include "plotcomv.inc"
 
      integer pl_id
 
      if (pl_mode.eq.post) then
         call plhome_ps(pl_id)
      elseif (pl_mode.eq.hpgl) then
         call plhome_hp(pl_id)
      endif

      return
      end
C***********************************************************************
      subroutine plmove(pl_id,dx,dy)
c IN	: pl_id,dx,dy
c generic plotter library
C plmove move the pen of plotter pl_id to dx and dy centimeters, starting
C from current position, coordinates in absolute.
      implicit none
 
#include "plotcomv.inc"
 
      integer pl_id
      real dx,dy

      if (pl_mode.eq.post) then
         call plmove_ps(pl_id,dx,dy)
      elseif (pl_mode.eq.hpgl) then
         call plmove_hp(pl_id,dx,dy)
      endif
 
      return
      end
 
C***********************************************************************
      subroutine pldraw(pl_id,dx,dy)
c IN	: pl_id,dx,dy
c generic plotter library
C pldraw draw a solid line of dx and dy centimeters, starting
C from current position, coordinates in absolute.
      implicit none
#include "plotcomv.inc"
      integer pl_id
      real dx,dy
 
      if (pl_mode.eq.post) then
         call pldraw_ps(pl_id,dx,dy)
      elseif (pl_mode.eq.hpgl) then
         call pldraw_hp(pl_id,dx,dy)
      endif

      return
      end
C***********************************************************************
      subroutine pldotdraw(pl_id,dx,dy)
c IN	: pl_id,dx,dy
c generic plotter library
C pldotdraw draw a dotted line of dx and dy centimeters, starting
C from current position, coordinates in absolute.
      implicit none
 
#include "plotcomv.inc"
 
      integer pl_id
      real dx,dy

      if (pl_mode.eq.post) then
         call pldotdraw_ps(pl_id,dx,dy)
      elseif (pl_mode.eq.hpgl) then
         call pldotdraw_hp(pl_id,dx,dy)
      endif
 
      return
      end
C***********************************************************************
      subroutine pltext(pl_id,tex,lentext,xtext,ytext)
c IN	: pl_id,tex,lentext,xtext,ytext
c generic plotter library
C pltext writes the character string 'tex' of length 'lentext' at the
C position defined by 'xtext' and 'ytext' with current values for size
C and slope of characters
      implicit none
 
#include "plotcomv.inc"
 
      integer pl_id,lentext
      real xtext,ytext
      character*256 tex
 
      if (pl_mode.eq.post) then
         call pltext_ps(pl_id,tex,lentext,xtext,ytext)
      elseif (pl_mode.eq.hpgl) then
         call pltext_hp(pl_id,tex,lentext,xtext,ytext)
      endif

      return
      end
C***********************************************************************
      subroutine pltexset(pl_id,size,angl)
c IN	: pl_id,size,angl
c generic plotter library
C pltexset sets the values of the size of character strings to be
C written on the plotter
      implicit none
      integer pl_id
      real size
      integer angl
#include "plotcomv.inc"
 
      if (pl_mode.eq.post) then
         call pltexset_ps(pl_id,size,angl)
      elseif (pl_mode.eq.hpgl) then
         call pltexset_hp(pl_id,size,angl)
      endif

      return
      end
C***********************************************************************
      subroutine pltextdir(pl_id,angle)
c IN	: pl_id,angle
c generic plotter library
C pltextdir sets the values of the angle.. strings to be
C written on the plotter
      implicit none
      integer pl_id,angle
 
#include "plotcomv.inc"
 
      if (pl_mode.eq.post) then
         call pltextdir_ps(pl_id,angle)
      elseif (pl_mode.eq.hpgl) then
         call pltextdir_hp(pl_id,angle)
      endif

      return
      end
C***********************************************************************
      subroutine plchpen(pl_id,colour)
c IN	: pl_id,colour
c generic plotter library
C plchpen selects another pen for plotting,colour is the number of the selected
C pen ,
C in postscript it becomes :
C 1 normal, 2 small dash, 3 big dash, 4 asymetric dash, 5-8 same effect with
C larger line.
C
      implicit none
 
#include "plotcomv.inc"
 
      integer pl_id,colour
 
      if (pl_mode.eq.post) then
         call plchpen_ps(pl_id,colour)
      elseif (pl_mode.eq.hpgl) then
         call plchpen_hp(pl_id,colour)
      endif

      return
      end
 
C***********************************************************************
      subroutine plcolor(pl_id,colour)
c IN	: pl_id,colour
c generic plotter library
C plcolor selects another color, specific to postscript
C
      implicit none
 
#include "plotcomv.inc"
 
      integer pl_id,colour

      if (pl_mode.eq.post) then
         call plcolor_ps(pl_id,colour)
      elseif (pl_mode.eq.hpgl) then
         call plcolor_hp(pl_id,colour)
      endif

      return
      end
 
C***********************************************************************
      subroutine plchpage(pl_id)
c IN	: pl_id
c generic plotter library
c
C plchpage changes the page on a plotter installed to do so
      implicit none
      integer pl_id
#include "plotcomv.inc"
 
      if (pl_mode.eq.post) then
         call plchpage_ps(pl_id)
      elseif (pl_mode.eq.hpgl) then
         call plchpage_hp(pl_id)
      endif

      return
      end
 
C***********************************************************************
      subroutine pleject(pl_id)
c IN	: pl_id
c generic plotter library
c
C pleject changes the page on a plotter installed to do so
C reinitialize the plotter as well
C and send the file to the plotter through a shell call
C forgets about the file name
C closes the file, so no need for a close(plid)
      implicit none
      integer pl_id
#include "plotcomv.inc"
      integer ll
      character*256 st

      call plhome(pl_id)
      call plchpage(pl_id)
      call plclose(pl_id)

      ll=256
      call trailing(namecom(current),ll)
      if (pl_mode.eq.post) then
         st = 'gifaplot '//namecom(current)(1:ll)//' postscript '
      elseif (pl_mode.eq.hpgl) then
         st = 'gifaplot '//namecom(current)(1:ll)//' HPGL '
      endif
C gifaplot is an OS command that send the file to
C the plotter, destroying it if it is called Gifa_Temp.Plot
      call shell(st)
      namecom(current) = ' '
      current = 0
      return
      end
 
C***********************************************************************
       block data initplot
#include "plotcomv.inc"
       integer i
       data (namecom(i),i=1,nfile)/ nfile*' '/
       end


C***********************************************************************
       subroutine plopen(plid,nm,err)
c IN	: pl_id,nm
c OUT	: err
c
c generic plotter library
c
c opens the file called nm, and conects it to the plotter d 'pl_id'
 
       implicit none
       integer plid,err
       character*(*) nm
#include "plotcomv.inc"
       character*256 line
       integer ll,i,errloc
       logical new
 
       if (nm.eq.'*PLOTTER') nm = 'Gifa_Temp.Plot'
       ll=256
       call trailing(nm,ll)
       new=.true.
       do i=1,nfile            ! search if that name is already known
          if (nm(1:ll).eq.namecom(i)) then
             new = .false.
             current = i
          endif
       enddo
       if (new) then
          do i=1,nfile         ! search an empty slot
             if (namecom(i)(1:1).eq.' ') goto 5
          enddo
          err = 1              ! no more slots
          call gifaout('Too many opened plot files at the same time')
          return
  5       current = i
          open(plid,file=nm(1:ll),form='formatted',
     *         status='new',iostat=err)
          if (err.ne.0) return
          namecom(i)(1:256) = nm(1:ll)
          call plinit(plid)
       else            ! append to already existing file
c          line = 'appending to file '//namecom(current)
c          call gifaout(line)
          open(plid,file=namecom(current),form='formatted',
     *         status='old',iostat=err)
          if (err.ne.0) return
          i = 0
  20      read(plid,25,iostat=errloc,end=30,err=30)line
  25      format(a)
             i = i+1
             goto 20
  30      continue
       endif
       return
       end
C***********************************************************************
C  plforget(name,err)
       subroutine plforget(name,err)
c IN	: pl_id
c OUT	: err
c
c generic plotter library
c
c "forgets" the plotter file "pl_id", i.e. removes it from the list f 
c currently opened plotter file
       implicit none
       integer i,err,l
       character*(*) name
#include "plotcomv.inc"

       call trailing(name,l)
       do i=1,nfile            ! search if that name is known
          if (name(1:256).eq.namecom(i)(1:256)) then
             namecom(i) = ' '
          endif
       enddo
       current = 0
       return
       end

C***********************************************************************
C  plclose(pl_id)
c IN	: pl_id
c
c generic plotter library
c
c used to close an opened plotter file "pl_id"

       subroutine plclose(plid)
       integer plid,error
 
       close(plid,
#vms     * status='save',
C#sgi     * status='save',
#sun     * status='keep',
#alliant     * status='save',
#adec     * status='save',
#hp     * status='keep',
#f2c     * status='save',
     * iostat=error)
       end

