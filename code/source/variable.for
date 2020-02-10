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
C

C set of routine for managing the variables in GIFA
C
C
C the top-level entry points are :
C
C assign(name,context,value,err)
C      assign a value to the variable called name, allocate if does
C      not exist yet. Context is used for allocation
C
C getvar(name,context,value,err)
C      get the value of the variable called name/context
C
C setvar(name,context,value,err)
C      set the value of the variable called name/context
C
C checkvar(name,context,err)
C      check if the variable called name/context exists, returns with err
C
C existvar(name,context,err)
C      determines whether the variable name/context can be evaluated (real var, dbm entry,..)
C
C nextelem(array,context,index,next,err)
C      if index is an entry in the associative array $array/context,
C      next will be the next elemen,
C      if index = ' ' will return the first elem
C      if index is the last, will return ' '
C      err if array does not exist
C
C dealloc(name,context,err)
C	forget variable name/context
C
C contclear(context)
c       dealloc all variable within context
C
C dumpvar()
C	prints the content of the variable table
C
C initvar()
C	Block data
C
C hash(st,len) can eventually be used as a generic hash function
C
C*********************************************************************

       subroutine assign(name,context,value,err)
C IN	: name,value,context
c OUT	: err
c
C      assign a value to the variable called name, allocate if does
C      not exist yet. Context is used for allocation and research.
c      Variables in different context are independent.
c      In GIFA, contexts are related to read unit, thus 20 to 29
c      20 is held as a global context.

       implicit none
#include "variable.inc"
       character*(varnmlen)namel,key,name
       character*(varcontlen) lst,array,value
       integer err,context,cont
       integer addr

       namel = name
       call find_var(namel,context,addr)
       if (addr.eq.0) then            ! check it is a DBM associative array
         call pickkey(array,key,namel,err)
         if (err.ne.0) goto 100       ! the current variable is not an array
         lst = '%'//array
         cont = 20
         call checkvar(lst,cont,err)  ! the current variable is not a DBM array
         if (err.ne.0) goto 100
         call dbmput(array,key,value,err)
         goto 999
100      continue                     ! allocate a new variable
         call allocate(namel,context,addr,err)
         if (err.ne.0) then
           call gifaout('Error in allocating variable : ' // namel)
           return
         endif
       endif
       call setvar(namel, context, value, err)
999    continue
       return
       end

C*********************************************************************
       subroutine checkvar(vname,context,err)
c IN	: vname,context
c INOUT : err
c
C      check if the variable called name exists in "context", returns with err :
c err = 0 means found
c err = 1 means not found
c
       implicit none
#include "variable.inc"
       character*(varnmlen) vname
       integer err,context
       integer addr

       call find_var(vname,context,addr)
       if (addr.ne.0) then
         err = 0
       else
         err = 1
       endif
       return
       end
C*********************************************************************
       subroutine existvar(vname,context,err)
c IN	: vname,context
c INOUT : err
c
C      check if the variable called name exists in "context", returns with err :
c err = 0 means found
c err = 1 means not found
c
       implicit none
#include "variable.inc"
       character*(varnmlen) vname,key,array
       integer err,context
       integer addr,i,len


       if (err.ne.0) goto 999
       call trailing(vname,len)

c first check $var_name[] which is a special syntax for seaching a whole array
       if (vname(len-1:len) .eq. '[]') then
           array = "%"//vname(2:len-2)         ! first search for dbm (faster)
           call find_var(array,context,addr)
           if (addr .ne. 0) then
              goto 100
           else                                ! now, scan whole variable table for an entry 
              array = vname(1:len-1)
              call uppercase(array,len-1)
              do i=1,varmax
                key = varname(i)
                if (key(1:len-1) .eq. array) then   ! "$var["
                   goto 100
                endif
              enddo
              goto 999
           endif

C else search for simple variables (or array entry)
       else

          call find_var(vname,context,addr)          ! first search as a regular variable
          if (addr.ne.0) then 
            goto 100

          else
             call pickkey(array,key,vname,err)       ! then as a dbm entry
             if (err.ne.0) return
             call dbmexist(array,key,err)
             if (err.eq.0) then
                goto 100
             else
                goto 999
             endif
          endif
       endif

 100   err = 0
       return
 999   err = 1
       return
       end
C*********************************************************************
       subroutine nextelem(array,context,indx,next,err)
c IN	: array,context,indx,err
c OUT	: next
c
C      if index is an entry in the associative array $array, in "context"
C      next will be the next elemen,
C      if index = \127 or ' ' will return the first elem
C      if index is the last, will return \127
C      err <> 0 if array does not exist

       implicit none
#include "variable.inc"
       character*(*) array,indx,next
       character*(varnmlen) st,star
       integer err,context,i,lo,lc,addr,l1,l2,addr_db

       call trailing(array,l1)
       call trailing(indx,l2)
       st = '$'//array(1:l1)//'['//indx(1:l2)//']'
       call find_var(st,context,addr)
       star = '$'//array
       call uppercase(star,l1+1)
       if ((indx(1:l2).ne.char(127)) .and. (indx(1:l2).ne.' ')) then  ! if not first entry
         if (addr.ne.0) then				! if found
          do i=addr+1,varmax
            if (conttab(i) .eq. context .or.
     *          conttab(i) .eq. 20 ) then	! in context
             lo = index(varname(i),'[')
             if (lo.ne.0) then
                st = varname(i)
                if (star.eq.st(1:lo-1)) then		! found next
                   lc = index(st,']')
                   if (lc.lt.lo) goto 100
                   next = st(lo+1:lc-1)
                   goto 999
                endif
             endif
            endif
          enddo
          next = char(127)				! no next avail.
          goto 999
         endif
         goto 110
       else
c here if searching for first entry
          do i=1,varmax					! search for first
            if (conttab(i) .eq. context .or.
     *          conttab(i) .eq. 20 ) then	! in context
             lo = index(varname(i),'[')
             if (lo.ne.0) then
                st = varname(i)
                if (star.eq.st(1:lo-1)) then	! found first
                   lc = index(st,']')
                   if (lc.lt.lo) goto 100
                   next = st(lo+1:lc-1)
                   goto 999
                endif
             endif
           endif
          enddo
          next = char(127)	                ! no first = no array !
          goto 110
       endif


100    call gifaout('*** undeciferable variable name'//varname(i))
       err = 1
       goto 999

c here if no entry found, could it be a dbm?
110    continue
       call dbmcheck(array,addr_db)
       if (addr_db.ne.0) goto 120
       if (indx(1:l2).eq.char(127)) then
          call dbmfirst(array,next,err)
       else
          call dbmnext(array,next,err)
       endif
       if (err.eq.0) goto 999
c error with DBM
       call gifaout('*** error in getting first or next DBM key')
       goto 999
c the variable does not exist
120    continue
c       call gifaout('*** array : '//star//' does not exists')
       err = 1
       goto 999

999    return
       end
C*********************************************************************
       subroutine getvar(vname,context,value,err)
c IN	: vname,context
c OUT	: value,err
C      get the value of the variable called name in the context "context"
c search order is : in current context
c                   in global context (20)
c                   as predefined internal GIFA variables
c

c added in-line documentation on 20/05/99 MAD

       implicit none
#include "variable.inc"
#include "paramfilev.inc"
#include "gifshellv.inc"
#include "sizeparam.inc"
#include "gifabcom.inc"
#include "version.inc"
#include "peakintv.inc"
#include "peaksize.inc"
#include "plotcomv.inc"
#include "zoomparamv.inc"
#include "maxentv.inc"
#include "disptypev.inc"
#include "transform.inc"
#include "disp3dv.inc"
#include "cachev.inc"
#include "linpredv.inc"
#include "laplacev.inc"


       character*(varcontlen) lst,array,value
       character*(varnmlen) key,vname
       integer err,li,i,context
       integer addr
       logical table
       real randomd,gasdev
       external randomd,table,gasdev
       integer seed
       save seed
       data seed /-1/

       call find_var(vname,context,addr)
       if (addr.ne.0) then
          lst = varval(addr)
       else
          call uppercase(vname,varnmlen)
          if (table(vname,'$VAL',li)) then
C value of the ith data point in the current working buffer
             if (li.lt.1 .or. li.gt.sizeimo) goto 100
             write(lst,30) imago(imdim(dim)+li-1)
          elseif (vname.eq.'$SI1_1D') then
C size of the 1D buffer.
             write(lst,20) sizeimage1d
          elseif (vname.eq.'$SI1_2D') then
C size in F1 of the 2D buffer
             write(lst,20) si1im
          elseif (vname.eq.'$SI2_2D') then
C size in F2 of the 2D buffer
             write(lst,20) si2im
          elseif (vname.eq.'$SI1_3D') then
C size in F1 of the 3D buffer
             write(lst,20) si3d1
          elseif (vname.eq.'$SI2_3D') then
C size in F2 of the 3D buffer
             write(lst,20) si3d2
          elseif (vname.eq.'$SI3_3D') then
C size in F3 of the 3D buffer
             write(lst,20) si3d3
          elseif (vname.eq.'$ITYPE_1D') then
C value of itype in 1D
             write(lst,20) it1d
          elseif (vname.eq.'$ITYPE_2D') then
C value of itype in 2D
             write(lst,20) itype
          elseif (vname.eq.'$ITYPE_3D') then
C value of itype in 3D
             write(lst,20) it3d
          elseif (vname.eq.'$DIM') then
C current value of DIM
             write(lst,20) dim
          elseif (vname.eq.'$ABSMAX') then
C current value of ABSMAX
             write(lst,30) max0
          elseif (vname.eq.'$PLANE[1]') then
C axis of the last extracted plane in 3D
             if (axis2d.eq.3) then
                lst = 'F3'
             elseif  (axis2d.eq.2) then
                lst = 'F2'
             elseif  (axis2d.eq.1) then
                lst = 'F1'
             else
                lst = ' '
             endif
          elseif (vname.eq.'$PLANE[2]') then
C index of the last extracted plane in 3D
             write(lst,20) nplane
          elseif (vname.eq.'$ROW[1]') then
C axis of the last extracted row in 3D
             if (axis1d.eq.2) then
                lst = 'F2'
             elseif  (axis1d.eq.1) then
                lst = 'F1'
             else
                lst = ' '
             endif
          elseif (vname.eq.'$ROW[2]') then
C F1 index of the last extracted row in 3D
             write(lst,20) n1d1
          elseif (vname.eq.'$ROW[3]') then
C F2 index of the last extracted row in 3D
             write(lst,20) n1d2
          elseif (vname.eq.'$NAME') then
C name of the last read data-set
             lst = name
          elseif (vname.eq.'$NAME_1D') then
C name of the last read 1D data-set
             lst = name1d
          elseif (vname.eq.'$NAME_2D') then
C name of the last read 2D data-set
             lst = name2d
          elseif (vname.eq.'$NAME_3D') then
C name of the last read 3D data-set
             lst = name3d
          elseif (vname.eq.'$PKPATTERN') then
C The the value of PKPATTERN command 
             lst = peakpat
          elseif (vname.eq.'$PKNAME') then
C The name used by the last PKREAD / PKWRITE command 
             lst = pkname
          elseif (vname.eq.'$ROW') then
C index of the last extracted row in 2D
             write(lst,20) row
          elseif (vname.eq.'$COL') then
C index of the last extracted column in 2D
             write(lst,20) col
          elseif (vname.eq.'$PH0') then
C 0th order of the last phase correction
             write(lst,30) ph0
          elseif (vname.eq.'$PH1') then
C 1st order of the last phase correction
             write(lst,30) ph1
          elseif (vname.eq.'$LB1') then
C value of lb in F1
             write(lst,30) lb
          elseif (vname.eq.'$LB2') then
C value of lb in F2
             write(lst,30) lb1
          elseif (vname.eq.'$LB3') then
C value of lb in F3
             write(lst,30) lb2
          elseif (vname.eq.'$GB1') then
C value of gb in F1 
             write(lst,30) gb
          elseif (vname.eq.'$GB2') then
C value of gb in F2 
             write(lst,30) gb1
          elseif (vname.eq.'$GB3') then
C value of gb in F3 
             write(lst,30) gb2
          elseif (vname.eq.'$MAX[1]') then
C value of last computed max computed with the command MAX
             write(lst,30) maxi
          elseif (vname.eq.'$MAX[2]') then
C value of last computed min computed with the command MAX
             write(lst,30) mini
          elseif (vname.eq.'$SPECW_1D') then
C spectral width in 1D (in Hz)
             write(lst,30) sp1d
          elseif (vname.eq.'$SPECW_1_2D') then
C spectral width in 2D in F1 (in Hz)
             write(lst,30) specw1
          elseif (vname.eq.'$SPECW_2_2D') then
C spectral width in 2D in F@ (in Hz)
             write(lst,30) specw2
          elseif (vname.eq.'$SPECW_1_3D') then
C spectral width in 3D in F1 (in Hz)
             write(lst,30) sp3d1
          elseif (vname.eq.'$SPECW_2_3D') then
C spectral width in 3D in F2 (in Hz)
             write(lst,30) sp3d2
          elseif (vname.eq.'$SPECW_3_3D') then
C spectral width in 3D in F3 (in Hz)
             write(lst,30) sp3d3
          elseif (vname.eq.'$OFFSET_1D') then
C spectral offset in 1D (in Hz)
             write(lst,30) of1d
          elseif (vname.eq.'$OFFSET_1_2D') then
C spectral offset in 2D in F1 (in Hz)
             write(lst,30) offset1
          elseif (vname.eq.'$OFFSET_2_2D') then
C spectral offset in 2D in F2 (in Hz)
             write(lst,30) offset2
          elseif (vname.eq.'$OFFSET_1_3D') then
C spectral offset in 3D in F1 (in Hz)
             write(lst,30) of3d1
          elseif (vname.eq.'$OFFSET_2_3D') then
C spectral offset in 3D in F2 (in Hz)
             write(lst,30) of3d2
          elseif (vname.eq.'$OFFSET_3_3D') then
C spectral offset in 3D in F3 (in Hz)
             write(lst,30) of3d3
          elseif (vname.eq.'$FREQ') then
C main frequency (1H) of the spectrometer
             write(lst,40) frequency
          elseif (vname.eq.'$FREQ_1D') then
C spectrometer frequency in 1D (in MHz)
             write(lst,40) freq1d
          elseif (vname.eq.'$FREQ_1_2D') then
C spectrometer frequency in 2D along F1 (in MHz)
             write(lst,40) freq1
          elseif (vname.eq.'$FREQ_2_2D') then
C spectrometer frequency in 2D along F2 (in MHz)
             write(lst,40) freq2
          elseif (vname.eq.'$FREQ_1_3D') then
C spectrometer frequency in 3D along F1 (in MHz)
             write(lst,40) freq3d1
          elseif (vname.eq.'$FREQ_2_3D') then
C spectrometer frequency in 3D along F2 (in MHz)
             write(lst,40) freq3d2
          elseif (vname.eq.'$FREQ_3_3D') then
C spectrometer frequency in 3D along F3 (in MHz)
             write(lst,40) freq3d3
          elseif (vname.eq.'$UNIT') then
C current value for UNIT
             lst = unitx
          elseif (vname.eq.'$UNITY') then
C current value for UNIT_Y
             lst = unity
          elseif (vname.eq.'$AXIS') then
C value of the AXIS parameter 
             if (axes.eq.0) lst = 'NONE'
             if (axes.eq.1) lst = 'F1'
             if (axes.eq.2) lst = 'F2'
             if (axes.eq.3) lst = 'F12'
          elseif (vname.eq.'$CX') then
C current value for CX
             write(lst,30) cx
          elseif (vname.eq.'$CY') then
C current value for CY
             write(lst,30) cy
          elseif (vname.eq.'$CZ') then
C current value for CZ
             write(lst,30) cy
          elseif (vname.eq.'$VHEIGHT') then
C current value of command VHEIGHT
             write(lst,30) vheight
          elseif (vname.eq.'$PLOTOFFSET[1]') then
C current value for plot offset on X axis
             write(lst,30) offx
          elseif (vname.eq.'$PLOTOFFSET[2]') then
C current value for plot offset on Y axis
             write(lst,30) offy
          elseif (vname.eq.'$PLOTAXIS[1]') then
C current value for the unit used in PLOTAXIS
             lst = plotunit
          elseif (vname.eq.'$PLOTAXIS[2]') then
C current value for the tick distance in x axis
             write(lst,30) sepax
          elseif (vname.eq.'$PLOTAXIS[3]') then
C current value for the tick distance in y axis
             write(lst,30) sepay
          elseif (vname.eq.'$SIGN') then
C current value of context SIGN
             write(lst,20) sign
          elseif (vname.eq.'$LEVEL') then
C current value of command LEVEL
             write(lst,20) level
          elseif (vname.eq.'$SCALE') then
C current value of the context SCALE
             write(lst,30) scale
          elseif (vname.eq.'$LOGA') then
C current value of the context LOGA
             write(lst,30) loga
          elseif (vname.eq.'$ZONE[1]') then
C lower F1 coord. of the mouse-selected region (in index)
             write(lst,20) selecty1
          elseif (vname.eq.'$ZONE[2]') then
C lower F2 coord. of the mouse-selected region (in index)
             write(lst,20) selectx1
          elseif (vname.eq.'$ZONE[3]') then
C upper F1 coord. of the mouse-selected region (in index)
             write(lst,20) selecty2
          elseif (vname.eq.'$ZONE[4]') then
C upper F2 coord. of the mouse-selected region (in index)
             write(lst,20) selectx2
          elseif (vname.eq.'$ZOOM') then
C 1 if in ZOOM mode
             write(lst,20) zoom
          elseif (vname.eq.'$ZOOM_1D[1]') then
C left coordinate of the 1D zoom window (in index)
             write(lst,20) zo1dl
          elseif (vname.eq.'$ZOOM_1D[2]') then
C right coordinate of the 1D zoom window (in index)
             write(lst,20) zo1du
          elseif (vname.eq.'$ZOOM_2D[1]') then
C lower F1 coordinate of the 2D zoom window (in index)
             write(lst,20) zolf1
          elseif (vname.eq.'$ZOOM_2D[2]') then
C left F2 coordinate of the 2D zoom window (in index)
             write(lst,20) zolf2
          elseif (vname.eq.'$ZOOM_2D[3]') then
C upper F1 coordinate of the 2D zoom window (in index)
             write(lst,20) zour1
          elseif (vname.eq.'$ZOOM_2D[4]') then
C right F2 coordinate of the 2D zoom window (in index)
             write(lst,20) zour2
          elseif (vname.eq.'$ZOOM_3D[1]') then
C "left" F1 coordinate of the 3D zoom window (in index)
             write(lst,20) zo3df1l
          elseif (vname.eq.'$ZOOM_3D[2]') then
C "right" F1 coordinate of the 3D zoom window (in index)
             write(lst,20) zo3df1u
          elseif (vname.eq.'$ZOOM_3D[3]') then
C "left" F2 coordinate of the 3D zoom window (in index)
             write(lst,20) zo3df2l
          elseif (vname.eq.'$ZOOM_3D[4]') then
C "right" F2 coordinate of the 3D zoom window (in index)
             write(lst,20) zo3df2u
          elseif (vname.eq.'$ZOOM_3D[5]') then
C "left" F3 coordinate of the 3D zoom window (in index)
             write(lst,20) zo3df3l
          elseif (vname.eq.'$ZOOM_3D[6]') then
C "right" F3 coordinate of the 3D zoom window (in index)
             write(lst,20) zo3df3u

          elseif (vname.eq.'$REFPEAKS') then
C 1 if the display of peaks is active
             write(lst,20) refpeaks
          elseif (vname.eq.'$REFSPECTRA') then
C 1 if the display is active
             write(lst,20) refspectra
          elseif (vname.eq.'$REFMACRO') then
C 1 if the display is active during macro
             write(lst,20) refmacro

          elseif (vname.eq.'$DISP1D') then
C 1 if the 1D window is on screen
             write(lst,20) disp1d
          elseif (vname.eq.'$DISP2D') then
C 1 if the 2D bitmap window is on screen
             write(lst,20) disp2d
          elseif (vname.eq.'$CDISP2D') then
C 1 if the 2D controur plot window is on screen
             write(lst,20) cdisp2d
          elseif (vname.eq.'$COLOR') then
C current value for COLOR
             write(lst,20) color
          elseif (vname.eq.'$SCOLOR') then
C current value for SCOLOR
             write(lst,20) scolor
          elseif (vname.eq.'$CCOLOR') then
C current value for CCOLOR
             write(lst,20) ccolor
          elseif (vname.eq.'$BCOLOR') then
C current value for BCOLOR
             write(lst,20) bcol
          elseif (vname.eq.'$CLEAR') then
C current value for CLEAR
             write(lst,20) clear
          elseif (vname.eq.'$DISP3D') then
C 1 if the 3D controur plot window is on screen
             write(lst,20) disp3d
          elseif (vname.eq.'$ALPHA') then
C current value for ALPHA
             write(lst,30) alpha
          elseif (vname.eq.'$BETA') then
C current value for BETA
             write(lst,30) beta
          elseif (vname.eq.'$GAMA') then
C current value for GAMA
             write(lst,30) gama
          elseif (vname.eq.'$SCALE3D') then
C current value for SCALE3D
             write(lst,30) scale3d
          elseif (vname.eq.'$ZNOT') then
C current value for ZNOT
             write(lst,30) znot
          elseif (table(vname,'$OFFSET3D',li)) then
C current value for OFFSET3D
             if (li.eq.1) then
               write(lst,30) off3dx
             else if (li.eq.2) then
               write(lst,30) off3dy
             else if (li.eq.3) then
               write(lst,30) off3dz
             else
               goto 100
             endif
          elseif (vname.eq.'$AXIS3D') then
C value of the AXIS3D parameter 
             if (axis3d.eq.0) lst = 'NONE'
             if (axis3d.eq.1) lst = 'F1'
             if (axis3d.eq.2) lst = 'F2'
             if (axis3d.eq.3) lst = 'F12'
             if (axis3d.eq.4) lst = 'F3'
             if (axis3d.eq.5) lst = 'F13'
             if (axis3d.eq.6) lst = 'F23'
             if (axis3d.eq.7) lst = 'F123'
C   ! MaxEnt controls
          elseif (vname.eq.'$ENTROPY') then
C value of the entropy computed by the last command : MAXENT, INVLAP, INVTLAP, ...
             if (dim.eq.1) then
                write(lst,30) (ent/log(float(sizeimage1d)))
             else
                write(lst,30) (ent/log(float(sizeimage)))
             endif
          elseif (vname.eq.'$CHI2') then
C value of the chi2 returned by the last command : MAXENT, LINEFIT, RT->PK
             write(lst,30) leastsq
          elseif (vname.eq.'$ITER') then
C current value for ITER
             write(lst,20) itermax
          elseif (vname.eq.'$ITERDONE') then
C number of iteration actually realised in last iterative process
             write(lst,20) iterdone
          elseif (vname.eq.'$LAMBSP') then
C current value for LAMBSP
             write(lst,30) lambdasp
          elseif (vname.eq.'$FILTER') then
C current value for FILTER
             write(lst,20) filter
          elseif (vname.eq.'$NCHANNEL') then
C current value for NCHANNEL
             write(lst,20) nchannel
          elseif (vname.eq.'$NDISP') then
C current value for NDISP
             write(lst,20) nndisp
          elseif (vname.eq.'$LAMBDA') then
C context
             write(lst,30) lambda
          elseif (vname.eq.'$LAMBCONT') then
C context
             write(lst,30) lambcont
          elseif (vname.eq.'$MINITER') then
C context
             write(lst,20) miniter
          elseif (vname.eq.'$ALGO') then
C context
             write(lst,20) algo
          elseif (vname.eq.'$NOISE') then
C context
             write(lst,30) noise
          elseif (vname.eq.'$SHIFT') then
C context
             write(lst,30) shift
C  ! click tableprint vname
          elseif (table(vname,'$POINTX',li)) then
C X coordinates (in index) of ith point in the point stack
             if (li.gt.nclick .or. li.lt.1) goto 100
             write(lst,30) clickt(li,1)
          elseif (table(vname,'$POINTY',li)) then
C Y coordinates (in index) of ith point in the point stack
             if (li.gt.nclick .or. li.lt.1) goto 100
             write(lst,30) clickt(li,2)
          elseif (vname.eq.'$NPOINT') then
C The number of entries in the point stack
             write(lst,20) nclick
          elseif (vname.eq.'$BUTTON') then
C 1, 2 or 3 depending on which mouse button was last clicked
             write(lst,20) clickbutton
C  ! LP internals
          elseif (vname.eq.'$ORDER') then
C context
             write(lst,20) order
          elseif (vname.eq.'$NSVD') then
C context
             write(lst,20) nsvd
          elseif (vname.eq.'$NAR') then
C context
             write(lst,20) szar
          elseif (vname.eq.'$NRT') then
C context
             write(lst,20) szzn
C  ! Build-up curve module
          elseif (vname.eq.'$RELAX') then
C context
             write(lst,30) redf
          elseif (vname.eq.'$RCRYST') then
C context
             write(lst,30) rfactor
          elseif (vname.eq.'$CALIBDI[1]') then
C the current calibrating distance, as defined with the CALIBDI command
             write(lst,30) dcalib
          elseif (vname.eq.'$CALIBDI[2]') then
C the current calibrating relaxation rate, as defined with the CALIBDI command
             write(lst,30) redfcalib
          elseif (vname.eq.'$DIST') then
C context
             write(lst,30) dist
C  ! peak table
          elseif (vname.eq.'$PKRADIUS') then
C The nvalue of the last radius argument in the PEAK cmde
             write(lst,20) pkradius
          elseif (vname.eq.'$NPK1D') then
C The number of entries in the 1D peak table
             write(lst,20) nbpic1d
          elseif (table(vname,'$PK1D_ID',li)) then
C The peak id
             lst = peak1d_id(li)
          elseif (table(vname,'$PK1D_F',li)) then
C position (in index) of the ith 1D entry
             if (li.gt.nbpic1d .or. li.lt.1) goto 100
             write(lst,30) peak1d(li,1)
          elseif (table(vname,'$PK1D_W',li)) then
C width (in index) of the ith entry in the 1D peak table
             if (li.gt.nbpic1d .or. li.lt.1) goto 100
             write(lst,30) peak1d(li,2)
          elseif (table(vname,'$PK1D_A',li)) then
C amplitude of the ith entry in the 1D peak table
             if (li.gt.nbpic1d .or. li.lt.1) goto 100
             write(lst,30) peak1d(li,3)
          elseif (table(vname,'$PK1D_F_ERR',li)) then
C error on the previous quantity
             if (li.gt.nbpic1d .or. li.lt.1) goto 100
             write(lst,30) peak1d(li,6)
          elseif (table(vname,'$PK1D_W_ERR',li)) then
C error on the previous quantity
             if (li.gt.nbpic1d .or. li.lt.1) goto 100
             write(lst,30) peak1d(li,7)
          elseif (table(vname,'$PK1D_A_ERR',li)) then
C error on the previous quantity
             if (li.gt.nbpic1d .or. li.lt.1) goto 100
             write(lst,30) peak1d(li,8)
          elseif (table(vname,'$PK1D_P',li)) then
C phase of the ith entry in the 1D peak table
             if (li.gt.nbpic1d .or. li.lt.1) goto 100
             write(lst,30) peak1d(li,4)
          elseif (table(vname,'$PK1D_T',li)) then
C analytical form of the ith entry in the 1D peak table GAUSS, LORENTZ, or UNKNOWN
             if (li.gt.nbpic1d .or. li.lt.1) goto 100
             if (peak1d(li,5).eq.2.0) then
               lst = 'GAUSS'
             elseif (peak1d(li,5).eq.1.0) then
               lst = 'LORENTZ'
             else
               lst = 'UNKNOWN'
             endif
          elseif (vname.eq.'$NPK2D') then
C The number of entries in the 2D peak table
             write(lst,20) nbpic2d
          elseif (table(vname,'$PK2D_ID',li)) then
C The peak id
             lst = peak2d_id(li)
          elseif (table(vname,'$PK2D_F2F',li)) then
C F2 position (in index) of the ith entry in the 2D peak table
             if (li.gt.nbpic2d .or. li.lt.1) goto 100
             write(lst,30) peak2d(li,1)
          elseif (table(vname,'$PK2D_F2W',li)) then
C F2 width (in index) of the ith entry in the 2D peak table
             if (li.gt.nbpic2d .or. li.lt.1) goto 100
             write(lst,30) peak2d(li,2)
          elseif (table(vname,'$PK2D_A',li)) then
C amplitude of the ith entry in the 2D peak table
             if (li.gt.nbpic2d .or. li.lt.1) goto 100
             write(lst,30) peak2d(li,3)
          elseif (table(vname,'$PK2D_F1F',li)) then
C F1 position (in index) of the ith entry in the 2D peak table
             if (li.gt.nbpic2d .or. li.lt.1) goto 100
             write(lst,30) peak2d(li,4)
          elseif (table(vname,'$PK2D_F1W',li)) then
C F1 width (in index) of the ith entry in the 2D peak table
             if (li.gt.nbpic2d .or. li.lt.1) goto 100
             write(lst,30) peak2d(li,5)
          elseif (table(vname,'$PK2D_A_ERR',li)) then
C error on the previous quantity
             if (li.gt.nbpic2d .or. li.lt.1) goto 100
             write(lst,30) peak2d(li,6)
          elseif (table(vname,'$PK2D_F2F_ERR',li)) then
C error on the previous quantity
             if (li.gt.nbpic2d .or. li.lt.1) goto 100
             write(lst,30) peak2d(li,8)
          elseif (table(vname,'$PK2D_F2W_ERR',li)) then
C error on the previous quantity
             if (li.gt.nbpic2d .or. li.lt.1) goto 100
             write(lst,30) peak2d(li,9)
          elseif (table(vname,'$PK2D_F1F_ERR',li)) then
C error on the previous quantity
             if (li.gt.nbpic2d .or. li.lt.1) goto 100
             write(lst,30) peak2d(li,10)
          elseif (table(vname,'$PK2D_F1W_ERR',li)) then
C error on the previous quantity
             if (li.gt.nbpic2d .or. li.lt.1) goto 100
             write(lst,30) peak2d(li,11)
          elseif (table(vname,'$PK2D_T',li)) then
C analytical form of the ith entry in the 2D peak table GAUSS, LORENTZ, or UNKNOWN
             if (li.gt.nbpic2d .or. li.lt.1) goto 100
             if (peak2d(li,7) .eq. 2.0) then
               lst = 'GAUSS'
             elseif (peak2d(li,7) .eq. 1.0) then
               lst = 'LORENTZ'
             else
               lst = 'UNKNOWN'
             endif
          elseif (vname.eq.'$NPK3D') then
C The number of entries in the 3D peak table
             write(lst,20) nbpic3d
          elseif (table(vname,'$PK3D_ID',li)) then
C The peak id
             lst = peak3d_id(li)
          elseif (table(vname,'$PK3D_F3F',li)) then
C F3 position (in index) of the ith entry in the 3D peak table
             if (li.gt.nbpic3d .or. li.lt.1) goto 100
             write(lst,30) peak3d(li,1)
          elseif (table(vname,'$PK3D_F3W',li)) then
C F3 width (in index) of the ith entry in the 3D peak table
             if (li.gt.nbpic3d .or. li.lt.1) goto 100
             write(lst,30) peak3d(li,2)
          elseif (table(vname,'$PK3D_A',li)) then
C amplitude of the ith entry in the 3D peak table
             if (li.gt.nbpic3d .or. li.lt.1) goto 100
             write(lst,30) peak3d(li,3)
          elseif (table(vname,'$PK3D_F1F',li)) then
C F1 position (in index) of the ith entry in the 3D peak table
             if (li.gt.nbpic3d .or. li.lt.1) goto 100
             write(lst,30) peak3d(li,4)
          elseif (table(vname,'$PK3D_F1W',li)) then
C F1 width (in index) of the ith entry in the 3D peak table
             if (li.gt.nbpic3d .or. li.lt.1) goto 100
             write(lst,30) peak3d(li,5)
          elseif (table(vname,'$PK3D_F2F',li)) then
C F2 position (in index) of the ith entry in the 3D peak table
             if (li.gt.nbpic3d .or. li.lt.1) goto 100
             write(lst,30) peak3d(li,6)
          elseif (table(vname,'$PK3D_F2W',li)) then
C F2 width (in index) of the ith entry in the 3D peak table
             if (li.gt.nbpic3d .or. li.lt.1) goto 100
             write(lst,30) peak3d(li,7)
          elseif (vname.eq.'$PK_FND') then
C The index of the entry found by the last FIND command
             lst = found_pk
          elseif (vname.eq.'$PK_FND_DST') then
C The distance between the last entry found with FIND and the actual location given
             write(lst,30) found_dist
          elseif (vname.eq.'$MAXINBOX') then
C value returned by the last MAXINBOX command
             write(lst,30) maxinbox
          elseif (vname.eq.'$SOMREC') then
C value returned by the last SUMREC command
             write(lst,30) volume
          elseif( vname.eq.'$SUMREC') then
Cvalue returned by the last SUMREC command
             write(lst,30) volume
          elseif (vname.eq.'$SOMREC_ERR') then
C error on the previous quantity
             write(lst,30) incert
          elseif (vname.eq.'$SUMREC_ERR') then
C error on the previous quantity
             write(lst,30) incert
          elseif (vname.eq.'$SIGN_PEAK') then
C context
             write(lst,20) int(signpic)
C   ! static config
          elseif (vname.eq.'$GIFAPATH') then
C current PATH used for macro, set by th SETPATH command
             lst = path
          elseif (vname.eq.'$HOME') then
C equivalent to the $HOME variable in UNIX
             lst = home
          elseif (vname.eq.'$TIMER') then
C value of the last TIMER timing
             lst = sttimer
          elseif (vname.eq.'$RETURNED') then
C context set by the RETURN command
             lst = returned
          elseif (vname.eq.'$MACRONAME') then
C the name of the current running macro
             if (input.eq.20) then
               lst = 'command line'
             else
               lst = currinp(input-20)
             endif 
          elseif (vname.eq.'$MEM_PRO_LP') then
C The size of the protected 1D area, as returned by the CONFIG command
             write(lst,20) ldmax
          elseif (vname.eq.'$MEM_PRO_1D') then
C The size of the protected 1D area, as returned by the CONFIG command
             write(lst,20) sizemax
          elseif (vname.eq.'$MEM_PRO_2D') then
C The size of the protected 2D area, as returned by the CONFIG command
             write(lst,20) smxmax
          elseif (vname.eq.'$MEM_PRO_3D') then
C The size of the protected 3D area, as returned by the CONFIG command
             write(lst,20) 4*smxmax
          elseif (vname.eq.'$VAR_MAX') then
C The total number of user variable available, as returned by the CONFIG command
             write(lst,20) varmax
          elseif (vname.eq.'$MEM_MAX') then
C The larger data set available, as returned by the CONFIG command
             write(lst,20) smxbig
          elseif (vname.eq.'$CONFIG_OS') then
C the system type, as returned by the CONFIG command
             call os_config(lst)
          elseif (vname.eq.'$CONFIG_WIN') then
C The window system, as returned by the CONFIG command
             call win_config(lst)
          elseif (vname.eq.'$CONFIG_GRAPH') then
C is true if graphic is possible (X_Windows is connected)
             if (on_x_win.eq.1) then
               lst = '1'
             else
               lst = '0'
             endif
          elseif (vname.eq.'$CONFIG_PLOT') then
C The plot driver, as returned by the CONFIG command
             call pl_config(lst)
          elseif (vname.eq.'$VERSION') then
C The current version, as returned by the CONFIG command
             lst = version
          elseif (vname.eq.'$LICENCE') then
C The licence as returned by the CONFIG command
             lst = license(1:ilenc)
          elseif (vname.eq.'$PULLDOWNMENU') then
C The PULLDOWNMENU state
             write(lst,20) pdmenu
          elseif (vname.eq.'$VERBOSE') then
C The VERBOSE state
             write(lst,20) verbose
          elseif (vname.eq.'$DEBUG') then
C The DEBUG state
             write(lst,20) debug

          elseif (vname.eq.'$RANDOMG') then
C a random variable with normal law, unit variance and zero mean
             write (lst,30) gasdev(seed)
          elseif (vname.eq.'$RANDOM') then
C a random variable in the range 0..1 with equiprobable distribution
             write (lst,30) randomd(seed)
          elseif (vname.eq.'$RANDOMZ') then
C same as $RANDOM but resets the random series
             seed = -1
             write (lst,30) randomd(seed)
C support for cache system
          elseif (vname.eq.'$C_JOINED') then
C true if there is a currently JOINed dataset
             if (id_cache.eq.0) then
                lst = "0"
             else
                lst = "1"
             endif
          elseif (vname.eq.'$C_HEADER') then
C last value accessed with the GETHEADER command
             lst = c_header
          elseif (vname.eq.'$C_NAME') then
C name of the currently JOINed dataset
             lst = c_name
          elseif (vname.eq.'$C_DIM') then
C value of DIM of the currently JOINed dataset
             write(lst,20) c_dimmanip
          elseif (vname.eq.'$C_TYPE') then
C value of ITYPE of the currently JOINed dataset
             write(lst,20) c_type
          elseif (vname.eq.'$C_SIZEF1') then
C value of SIZE_F1 of the currently JOINed dataset
             write(lst,20) c_sizef1
          elseif (vname.eq.'$C_SIZEF2') then
C value of SIZE_F2 of the currently JOINed dataset
             write(lst,20) c_sizef2
          elseif (vname.eq.'$C_SIZEF3') then
C value of SIZE_F3 of the currently JOINed dataset
             write(lst,20) c_sizef3
          elseif (vname.eq.'$C_OFFSF1') then
C value of OFFSET_1 of the currently JOINed dataset
             write(lst,30) c_offsf1
          elseif (vname.eq.'$C_OFFSF2') then
C value of OFFSET_2 of the currently JOINed dataset
             write(lst,30) c_offsf2
          elseif (vname.eq.'$C_OFFSF3') then
C value of OFFSET_3 of the currently JOINed dataset
             write(lst,30) c_offsf3
          elseif (vname.eq.'$C_SPECWF1') then
C value of SPECW_1 of the currently JOINed dataset
             write(lst,30) c_specwf1
          elseif (vname.eq.'$C_SPECWF2') then
C value of SPECW_2 of the currently JOINed dataset
             write(lst,30) c_specwf2
          elseif (vname.eq.'$C_SPECWF3') then
C value of SPECW_3 of the currently JOINed dataset
             write(lst,30) c_specwf3
          elseif (vname.eq.'$C_FREQ') then
C value of FREQ of the currently JOINed dataset
             write(lst,40) c_freq
          elseif (vname.eq.'$C_FREQ1') then
C value of FREQ_1 of the currently JOINed dataset
             write(lst,40) c_freq1
          elseif (vname.eq.'$C_FREQ2') then
C value of FREQ_2 of the currently JOINed dataset
             write(lst,40) c_freq2
          elseif (vname.eq.'$C_FREQ3') then
C value of FREQ_3 of the currently JOINed dataset
             write(lst,40) c_freq3
           elseif (vname.eq.'$C_DMIN') then
C value of DMIN of the currently JOINed dataset
             write(lst,30) c_dmin*c_dfactor
           elseif (vname.eq.'$C_DMAX') then
C value of DMAX of the currently JOINed dataset
             write(lst,30) c_dmax*c_dfactor
           elseif (vname.eq.'$C_DFACTOR') then
C value of DFACTOR of the currently JOINed dataset
             write(lst,30) c_dfactor
          elseif (vname.eq.'$C_ABSMAX') then
C value of ABSMAX of the currently JOINed dataset
             write(lst,30) c_max0
C  ! Laplace transform
          elseif (vname.eq.'$DMIN') then
C context
             write(lst,30) dmin*dfactor
          elseif (vname.eq.'$DMAX') then
C context
             write(lst,30) dmax*dfactor
          elseif (vname.eq.'$DFACTOR') then
C context
             write(lst,30) dfactor
          elseif (vname.eq.'$SI_TAB') then
C size of the TAB buffer
             write(lst,20) szsamp
          elseif (table(vname,'$TAB',li)) then
C value of the ith point in the TAB buffer
             if (li.gt.szsamp .or. li.lt.1) goto 100
             write(lst,30) sampling(li)
          elseif (vname.eq.'$ARG') then
C True if arguments are present on the calling line (within macro only)
             if (input.gt.20) then
               if (buffer(input-20).eq.' ') then
                 lst = '0'
               else
                 lst = '1'
               endif
             else
               lst = '0'
             endif
c
C $_ will be the next parameter from the calling sequence if the 
C Fortran compiler permits recursion !
c (getstring2 calls getvar which calls getstring2 etc... )
c
ccunix          elseif (vname.eq.'$_') then		! pop parameter
ccunix             if (input.gt.20) then
ccunix                parloc = param			! save param
ccunix                param = buffer(input-20)
ccunix                call getstring2(lst,err)
ccunix                buffer(input-20) = param	! restore
ccunix                param = parloc
ccunix             else
ccunix                lst = ' '
ccunix                err = 1
ccunix                call gifaout('Parameters not available')
ccunix                return
ccunix             endif
	  else                  ! check if it is a DBM associative array
	     call pickkey(array,key,vname,err)
	     if (err.ne.0) goto 100
	     call dbmget(array,key,value,err)	  
             if (err.ne.0) goto 100
             goto 999
          endif


       endif
       value = lst
999    continue       
       return
100       lst = vname
          call trailing(lst,i)
          call gifaout(lst(1:i)//' : Not allocated variable')
          err = 1
          value = ' '
          return
20     format(i12)
30     format(G24.12)
40     format(G26.14)
       end
C*********************************************************************
       subroutine setvar(name,context,value,err)
c IN	: name,value,err,context
C      set the content of the variable called "name"/"context" to "value"
       implicit none
#include "variable.inc"
       character*(varcontlen) value
       character*(varnmlen) st,name
       integer err,context
       integer addr,i

       call find_var(name,context,addr)
       if (addr.eq.0) then
          st = name
          call trailing(st,i)
          call gifaout(st(1:i)//' : Not allocated variable')
          err = 1
          value = ' '
          return
       endif
       varval(addr) = value 
       return
       end

C*********************************************************************
       subroutine dealloc(name,context,err)
c IN	: name,err,context
c
C	forget variable name in context
c
       implicit none
#include "variable.inc"
       character*(varnmlen) namel,key,name
       character*(varcontlen) lst,array
       integer err,addr,context,cont

       namel = name
       call find_var(namel,context,addr)
       if (addr.eq.0) then            ! check it is a DBM associative array
         call pickkey(array,key,namel,err)
         if (err.ne.0) goto 100       ! the current variable is not an array
         lst = '%'//array
         cont = 20
         call checkvar(lst,cont,err)  ! the current variable is not a DBM array
         if (err.ne.0) goto 100
         call dbmdel(array,key,err)
         return
100      continue                     ! the variable does not exist
         call gifaout(namel//' : Not allocated variable')
         err = 1
         return
       endif
       if (varval(addr).eq.'$reserved') then
          call gifaout(namel//' : Reserved variable')
          err = 1
          return
       endif

       varname(addr) = ' '
       conttab(addr) = 0
       return
       end

C*********************************************************************
       subroutine contclear(context)
c IN	: context
c
c       dealloc all variable within context
       implicit none
#include "variable.inc"
       integer context,i

       do i=1,varmax
          if (conttab(i).eq.context) then
             varname(i) = ' '
             conttab(i) = 0
          endif
       enddo
       return
       end

C*********************************************************************
       subroutine dumpvar()
C	prints the content of the variable table
       implicit none
#include "variable.inc"
       integer i,lv,lv2
       character*14 st
       character*300 st2
       do i=1,varmax
         if (varname(i).ne.' ') then
           write(st,'(i14)') conttab(i)
           call trailing(varname(i),lv)
           call trailing(varval(i),lv2)
           st2 = varname(i)(1:lv) // ' /' // st // ' : '
     *                  //varval(i)(1:lv2)
           call trailing(st2,lv2)
           call cleanst(st2,lv2)
           call gifaout(st2(1:lv2))
         endif
       enddo
       return
       end

C*********************************************************************
C**********Should not be used for normal use**************************
C*********************************************************************
C initialize structure
       block data initvar_data
#include "variable.inc"
       integer i
       data (varname(i),i=1,varmax)/ varmax*' '/
       data collision/0/
       data (conttab(i),i=1,varmax)/ varmax*0/
       end

C*********************************************************************

       subroutine find_var(name,context,addr)
c IN	: name,context
c OUT	: addr
c
c find the variable called "name" in "context" or in gloabl context 
c in the internal table, and returns its address
c
c
c addr is 0 if not found
c
C correction 13-dec-1996  MAD
C removed the tests varname .eq. ' '
C this may happend when a collision occured and the colliding var
C was then unset.
C
C**********Should not be used for normal use**************************
       implicit none
#include "variable.inc"
       character*(varnmlen) name
       integer context,addr

       integer i,l,hash,here,z,found

       call trailing(name,l)
       call uppercase(name,l)
       z = context
30       here = abs(mod(hash(name,l)+z,varmax)) + 1

         do i=here,varmax
           if (varname(i).eq.name
     *        .and. conttab(i).eq.z ) then
               found = i
               goto 10
           endif
C           if (varname(i).eq.' ') goto 20
           if (varname(i).eq.' ' .and. collision.eq.0) goto 20
         enddo
         do i=1,here-1
           if (varname(i).eq.name
     *        .and. conttab(i).eq.z ) then
               found = i
               goto 10
           endif
C           if (varname(i).eq.' ') goto 20
           if (varname(i).eq.' ' .and. collision.eq.0) goto 20
         enddo

20     if (z.ne.20) then     ! redo in global if not found 
          z = 20
          goto 30
       endif
       addr = 0
       return

10     addr = found			! found at i
       return
       end

C*********************************************************************
       subroutine allocate(name,context,addr,err)
c IN	: name,context
c OUT	: addr,err
c
c allocate the variable called "name" in the internal table,
c with context "context"
c
c the table is held as a indexed hash table with rolling collision
c
c returns its address
c addr is 0 if not allocated
c
C**********Should not be used for normal use**************************
       implicit none
#include "variable.inc"
       character*(*) name
       integer addr,err,i,l,hash,here,context,found

       call trailing(name,l)
       call uppercase(name,l)
       here = abs(mod(hash(name,l)+context,varmax)) + 1
c       if (varname(here)(1:1).eq.' ') then
c            found = here
c            goto 10
c       endif
       do i=here, varmax
         if (varname(i)(1:1).eq.' ') then
            found = i
            goto 10
         endif
       enddo
       do i=1,here-1
         if (varname(i)(1:1).eq.' ')  then
            found = i
            goto 10
         endif
       enddo
       err = 1
       addr = 0
       return

10     continue
       if (here.ne.found) then
          collision = collision+1
#dc        write(*,*) ' collision : ', here, ' ',name(1:l),' ',varname(here)
       endif
       varname(found) = name
       conttab(found) = context
       addr = found
       err = 0
       return
       end
C*********************************************************************
      function hash(st,l)
C IN	: st,l
c
C implements a hash function, "st(l)" -> integer.
C this one comes from Chris Torek, said to be very good!
C
C hash(st,l) can eventually be used as a generic hash function

      implicit none
      integer hash,l
      character*(*) st
      integer i,hh
      hh = 12798
      do i=1,l
        hh = hh*33 + ichar(st(i:i))
      enddo
      hash = abs(hh)
      return
      end
C*********************************************************************
      function table(var,name,ind)
c IN	: var,name
c OUT	: ind
c 
C true if "var" is an entry into the table "name",
c   i.e. of the form name[index]
c   with ind a generic string (associative arrays)
C used by getvar to extract base name and ind from index entries
c ind is set to the index as a side effect
      implicit none
      logical table

      character*(*) var,name
      integer ind,i,j

      i=index(var,'[')
      if (i.lt.2) then
         table = .false.
         return
      endif
      if (var(1:i-1) .ne.name) then
         table = .false.
         return
      endif

      j=index(var,']')
      read(var(i+1:j-1),*,err=100) ind
      table = .true.
      return

100   table = .false.
      return
      end





