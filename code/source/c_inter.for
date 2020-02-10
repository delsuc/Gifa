C This file is a part of the GIFA program
C     This software has been developped by the NMR Group in GIF/Yvette.
C
C     Authors :       L.Ladaviere, M.A.Delsuc
C
C  Creation M.A.D. 10-07-94
C
C     This software cannot be used unless the user have been fully
C licensed to do so form the above laboratory.
C     This file cannot be copied, duplicated or used in any other program,
C without written permission from the authors.
C
C **************************************************************
C
C Ces routines gerent l interactivite des commandes GETC et PUTC
C
C **************************************************************

      subroutine get3d_1d(axis,indexea,indexeb,valmi,valma,err)
c INOUT : axis,indexea,indexeb
c OUT   : valmi,valma,err

C gets coordinates for a line from a 3D.
c 3D -> 1D

      implicit none

#include "cachev.inc"

      integer axis, indexea,indexeb,valmi,valma,err,maximum,
     *         maxinda,maxindb

        if (axis.eq.3) axis = 4
        call message('line parallel to which axis (F1,F2,or F3)')
        call getdim(axis,err)
        if (axis.eq.4) axis = 3
        if (axis.lt.1 .and. axis.gt.3) goto 102

        call message('which index in first dimension:')
        call getint2(indexea,err)
        if (err.ne.0) goto 102
        call message('which index in second dimension:')
        call getint2(indexeb,err)
        if (err.ne.0) goto 102

        if(axis.eq.1) then
            maxinda = c_sizef2
            maxindb = c_sizef3
            maximum = c_sizef1
        elseif(axis.eq.2) then
            maxinda = c_sizef1
            maxindb = c_sizef3
            maximum = c_sizef2
        elseif (axis.eq.3) then
            maxinda = c_sizef1
            maxindb = c_sizef2
            maximum = c_sizef3
        endif

        if ((indexea.le.0).OR.(indexea.gt.maxinda)) goto 102
        if ((indexeb.le.0).OR.(indexeb.gt.maxindb)) goto 102
        call get1d_gene(valmi,valma,maximum,err)
        if (err.ne.0) goto 102
        call chkborder3d(valmi,valma,axis,c_type)
       return

102    call gifaout( '*** error with axis')
       err = 2
       return
       end 
C **************************************************************
      subroutine get2d_1d(axis,indexe,valmi,valma,err)
c INOUT : axis, indexe
c OUT   : valmi,valma,err

C gets coordinates for a line from a 2D.
c 2D -> 1D

      implicit none

#include "cachev.inc"

      integer axis, indexe,valmi,valma,err,maximum,
     *         maxind

        call message('line parallel to which axis (F1 or F2)')
        call getdim(axis,err)
        if (axis.lt.1 .and. axis.gt.2) goto 102

        call message('which index :')
        call getint2(indexe,err)
        if (err.ne.0) goto 102

        if(axis.eq.1) then
            maxind = c_sizef2
            maximum = c_sizef1
        elseif(axis.eq.2) then
            maxind = c_sizef1
            maximum = c_sizef2
        endif

        if ((indexe.le.0).OR.(indexe.gt.maxind)) goto 102
        call get1d_gene(valmi,valma,maximum,err)
        if (err.ne.0) goto 102
        call chkborder2d(valmi,valma,axis,c_type)
       return

102    call gifaout( '*** error with axis')
       err = 2
       return
       end 

C **************************************************************
      subroutine get1d_1d(valmi,valma,err)
c OUT   : valmi,valma,err

C gets coordinates for a line
c 1D -> 1D

      implicit none

#include "cachev.inc"

      integer valmi,valma,err

        call get1d_gene(valmi,valma,c_sizef1,err)
        if (err.ne.0) goto 102
        call chkborder2d(valmi,valma,2,1)
       return

102    call gifaout( '*** error with window')
       err = 2
       return
       end 

C **************************************************************

      subroutine get3d_2d(axis,index,lf1,lf2,ur1,ur2,err)

c INOUT : axis,index
c OUT   : lf1,lf2,ur1,ur2,err

C gets coordinates for a plane from a 3D.
c 3D -> 2D

      implicit none

#include "cachev.inc"

      integer axis,index,lf1,lf2,ur1,ur2,err

      if (axis.eq.3) axis = 4
      call message('plane orthogonal to which axis ?')
      call getdim(axis,err)
      if (err.ne.0) goto 98
      if (axis.eq.4) axis = 3
      if (axis.lt.1 .and. axis.gt.3) goto 98
      call message('which index :')
      call getint2(index,err)
      if (index.le.0) goto 98
      if (axis.eq.1) then
         if (index.gt.c_sizef1) goto 98
         call get2d_gene (lf1,lf2,ur1,ur2,c_sizef2,c_sizef3,err)
         if (err.ne.0) goto 98
      elseif (axis.eq.2) then
         if (index.gt.c_sizef2) goto 98
         call get2d_gene (lf1,lf2,ur1,ur2,c_sizef1,c_sizef3,err)
      elseif (axis.eq.3) then
         if (index.gt.c_sizef3) goto 98
         call get2d_gene (lf1,lf2,ur1,ur2,c_sizef1,c_sizef2,err)
      endif
      return

98    err = 2
      return
      end

C **************************************************************
      subroutine get2d_2d(lf1,lf2,ur1,ur2,err)

c INOUT : axis,index
c OUT   : lf1,lf2,ur1,ur2,err
C gets coordinates for a plane from a 2D.
c 2D -> 2D
      implicit none
#include "cachev.inc"

      integer lf1,lf2,ur1,ur2,err

      call get2d_gene (lf1,lf2,ur1,ur2,c_sizef1,c_sizef2,err)
      return
      end

C **************************************************************
      subroutine get3d_3d(lf1,lf2,lf3,ur1,ur2,ur3,err)
c OUT   : lf1,lf2,lf3,ur1,ur2,lf3,err
C gets coordinates for a subcube from a 3D.
c 3D -> 3D

      implicit none

#include "cachev.inc"

      integer lf1,lf2,lf3,ur1,ur2,ur3,err

      lf1 = 1
      lf2 = 1
      lf3 = 1
      ur1 = c_sizef1
      ur2 = c_sizef2
      ur3 = c_sizef3

       call message(
     *     'enter lower left coordinates (F1,F2,F3)')
       call getint2(lf1,err)
       call getint2(lf2,err)
       call getint2(lf3,err)
       if (err.ne.0) goto 112

       call message(
     *     'enter upper right coordinates (F1,F2,F3)')
       call getint2(ur1,err)
       call getint2(ur2,err)
       call getint2(ur3,err)
       if (err.ne.0) goto 112

      if (lf1.lt.1 .or. lf1.ge.c_sizef1
     *   .or. ur1.gt.c_sizef1 .or. ur1.lt.lf1) goto 112

      if (lf2.lt.1 .or. lf2.ge.c_sizef2
     *   .or. ur2.gt.c_sizef2 .or. ur2.lt.lf2) goto 112

      if (lf3.lt.1 .or. lf3.ge.c_sizef3
     *   .or. ur3.gt.c_sizef3 .or. ur3.lt.lf3) goto 112

C be carefull with complex data
       call chkborder3d(lf1,ur1,1,c_type)
       call chkborder3d(lf2,ur2,2,c_type)
       call chkborder3d(lf3,ur3,3,c_type)
       return

112   err = 2
      lf1 = 1
      lf2 = 1
      lf3 = 1
      ur1 = c_sizef1
      ur2 = c_sizef2
      ur3 = c_sizef3
      call gifaout ('*** error whith window')
      return 
      end

C************************************************************************

      subroutine chkborder3d(lf1,ur1,axis,it)
c IN    : axis,it
c INOUT : lf1,ur1
C this subroutine verify the border of a window on a 3D spectra,
C to be coherent with itype, i.e. begins on a real (odd) address, end
C imaginary (even) address (if complex in that direction)
       implicit none


       integer lf1,ur1,axis,it


       if (axis.eq.2) then
         if (it.eq.2 .or. it.eq.3 .or. it.eq.6
     *      .or. it.eq.7) then
             lf1=(lf1/2)*2+1
             ur1=(ur1/2)*2
         endif
       elseif(axis.eq.1) then
         if (it.ge.4) then
             lf1=(lf1/2)*2+1
             ur1=(ur1/2)*2
         endif
       elseif(axis.eq.3) then
         if (mod(it,2) .ne. 0) then
             lf1=(lf1/2)*2+1
             ur1=(ur1/2)*2
         endif
       endif
       return
       end


C************************************************************************

      subroutine chkborder2d(lf1,ur1,axis,it)
c IN    : axis,it
c INOUT : lf1,ur1
C this subroutine verify the border of a window on a 2D spectra,
C to be coherent with itype, i.e. begins on a real (odd) address, end
C imaginary (even) address (if complex in that direction)
       implicit none


       integer lf1,ur1,axis,it


       if (axis.eq.2) then
         if (it.eq.1 .or. it.eq.3) then
           lf1=(lf1/2)*2+1
           ur1=(ur1/2)*2
         endif
       elseif (axis.eq.1) then
         if (it.eq.2 .or. it.eq.3) then
           lf1=(lf1/2)*2+1
           ur1=(ur1/2)*2
         endif
       endif
       return
       end

C **************************************************************
      subroutine get1d_gene (lf1,ur1,valmax,err)
C
C IN    : valmax
c OUT   : err,lf1,ur1
C used for the GIFA cache interface to enter a spectral interval;
c preset the coordinates, ask the user, and verify coherence

      Implicit none
      integer lf1,ur1,valmax,err,error

      lf1 = 1
      ur1 = valmax
      call message('enter left limit')
      call getint2(lf1,error)
      call message('enter right limit')
      call getint2(ur1,error)

      if (lf1.lt.1 .or. lf1.ge.valmax
     *   .or. ur1.gt.valmax .or. ur1.lt.lf1 .or. error.ne.0) then
        err = 1
        call gifaout ('*** error whith window')
        lf1 = 1
        ur1 = valmax
        return
      endif
      return
      end

C **************************************************************
      subroutine get2d_gene(lf1,lf2,ur1,ur2,max1,max2,err)
c IN    : max1,max2
c OUT   : lf1,lf2,ur1,ur2,err
C used for the GIFA cache interface to enter a 2D spectral interval;
c preset the coordinates, ask the user, and verify coherence

      Implicit none

      integer lf1,ur1,lf2,ur2,max1,max2,err

      lf1 = 1
      lf2 = 1
      ur1 = max1
      ur2 = max2

       call message(
     *     'enter lower left corner coordinates (row,col)')
       call getint2(lf1,err)
       call getint2(lf2,err)
       if (err.ne.0) goto 112

       call message(
     *        'enter upper right corner coordinates')
       call getint2(ur1,err)
       call getint2(ur2,err)
       if (err.ne.0) goto 112

      if (lf1.lt.1 .or. lf1.ge.max1
     *   .or. ur1.gt.max1 .or. ur1.lt.lf1) goto 112

      if (lf2.lt.1 .or. lf2.ge.max2
     *   .or. ur2.gt.max2 .or. ur2.lt.lf2) goto 112

       return

112   err = 2
      lf1 = 1
      lf2 = 1
      ur1 = max1
      ur2 = max2
      call gifaout ('*** error whith window')
      return 
      end
C **************************************************************
      subroutine c_to_1d(axis,size,err)
c IN :	axis,size
c OUT : err
c
c copy the parameters of the cache file to the working 1D buffer

      implicit none
#include "cachev.inc"
#include "sizeparam.inc"
#include "paramfilev.inc"
#include "laplacev.inc"
      integer axis,size,err
      logical bool1

      if (size.gt.smxbig+smxmax+sizemax) then
          call gifaout( '*** Size too big for operation')
          err = 1
      endif
      call checkunderflow(size,bool1)
      if (bool1) then
          err = 1
      endif 

      sizeimage1d = size
      frequency = c_freq
         dmin = c_dmin
         dmax = c_dmax
         dfactor = c_dfactor
      if (c_dimmanip.eq.3) then
	      if (axis.eq.1) then
	            sp1d = c_specwf1
	            of1d = c_offsf1
	            freq1d = c_freq1
	            it1d = (c_type/4)
	      elseif (axis.eq.2) then
	            sp1d = c_specwf2
	            of1d = c_offsf2
	            freq1d = c_freq2
	            it1d = mod((c_type/2),2)
	      elseif (axis.eq.3) then
	            sp1d = c_specwf3
	            of1d = c_offsf3
	            freq1d = c_freq3
	            it1d = mod(c_type,2)
	       endif
	  elseif (c_dimmanip.eq.2) then
	      if (axis.eq.1) then
	            sp1d = c_specwf1
	            of1d = c_offsf1
	            freq1d = c_freq1
	            it1d = (c_type/2)
	      elseif (axis.eq.2) then
	            sp1d = c_specwf2
	            of1d = c_offsf2
	            freq1d = c_freq2
	            it1d = mod(c_type,2)
	       endif
	  elseif (c_dimmanip.eq.1) then
	            sp1d = c_specwf1
	            of1d = c_offsf1
	            freq1d = c_freq1
	            it1d = (c_type/2)
	  endif
      return
      end

C **************************************************************
      subroutine c_to_2d(axis,si1,si2,err)
c IN :	axis
c
c copy the parameters of the cache file to the working 2D buffer
c axis is orthogonal to the 3D plane to copy
c i.e. axis=1 means F2-F3 plane
c thus axis=3 is for regular 2d
      implicit none
      integer axis,si1,si2,err
#include "cachev.inc"
#include "sizeparam.inc"
#include "paramfilev.inc"
#include "laplacev.inc"
      logical bool1

      if (si1*si2.gt.smxbig)  then
          call gifaout( '*** Size too big for operation')
          err = 1
      endif
      call checkunderflow(si1*si2,bool1)
      if (bool1) then
          err = 1
      endif 

      si1im = si1
      si2im = si2
      sizeimage = si1*si2
      frequency = c_freq
         dmin = c_dmin
         dmax = c_dmax
         dfactor = c_dfactor
      if (c_dimmanip.eq.3) then
	      if (axis.eq.3) then
	         itype = c_type/2
	         specw1 = c_specwf1
	         specw2 = c_specwf2
	         freq1 = c_freq1
	         freq2 = c_freq2
	         offset1 = c_offsf1
	         offset2 = c_offsf2
	       elseif (axis.eq.2) then
	         itype = 2*(c_type/4) + mod(c_type,2)
	         specw1 = c_specwf1
	         specw2 = c_specwf3
	         freq1 = c_freq1
	         freq2 = c_freq3
	         offset1 = c_offsf1
	         offset2 = c_offsf3
	       elseif (axis.eq.1) then
	         itype = mod(c_type,4)
	         specw1 = c_specwf2
	         specw2 = c_specwf3
	         freq1 = c_freq2
	         freq2 = c_freq3
	         offset1 = c_offsf2
	         offset2 = c_offsf3
	       endif
      elseif (c_dimmanip.eq.2) then
             itype = c_type
	         specw1 = c_specwf1
	         specw2 = c_specwf2
	         freq1 = c_freq1
	         freq2 = c_freq2
	         offset1 = c_offsf1
	         offset2 = c_offsf2
       endif
       return
       end

C **************************************************************
      subroutine c_to_3d(si1,si2,si3,error)
c copy the parameters of the cache file to the working 3D buffer
      implicit none
#include "cachev.inc"
#include "sizeparam.inc"
#include "paramfilev.inc"
#include "laplacev.inc"
      integer si1,si2,si3,error
      logical bool1

      if (si1*si2*si3.gt.smxbig)  then
          call gifaout( '*** Size too big for operation')
          error = 1
      endif
      call checkunderflow(si1*si2*si3,bool1)
      if (bool1) then
          error = 1
      endif 

      si3d1 = si1
      si3d2 = si2
      si3d3 = si3
      sizeimage3d = si1*si2*si3
         it3d = c_type
         sp3d1 = c_specwf1
         sp3d2 = c_specwf2
         sp3d3 = c_specwf3
         freq3d1 = c_freq1
         freq3d2 = c_freq2
         freq3d3 = c_freq3
         of3d1 = c_offsf1
         of3d2 = c_offsf2
         of3d3 = c_offsf3
         frequency = c_freq
         dmin = c_dmin
         dmax = c_dmax
         dfactor = c_dfactor
      return
      end
