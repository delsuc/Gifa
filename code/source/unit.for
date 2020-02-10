C This file is a part of the GIFA program
C
C     Authors :       M.A.Delsuc, and others
C                     C.B.S. Fac de Pharmacy
C                     34000 Montpellier
C
C     This software cannot be used unless the user have been fully
C licensed to do so form the above laboratory.
C     This file cannot be copied, duplicated or used in any other program,
C without written permission from the authors.
C
C this file contains a serie of  tools for converting units
C



c************************************************************
      function itohr(index,size,specw,off)
c IN	: index,size,specw,off
c
C returns hertz from index value

      implicit none
      real specw,off,itohr,index
      integer size
      if (size.gt.1) then
         itohr = (size-index+1)*specw/(size) + off
      else
         itohr = 0.0
      endif
      return
      end
c************************************************************
      function itopr(index,size,specw,off,freq)
c IN	: index,size,specw,off,freq
c
C returns ppm from index value

      implicit none
      real specw,off,freq,itohr,index,itopr
      external itohr
      integer size

      itopr = itohr(index,size,specw,off)/freq
      return
      end

c************************************************************
      function ptoir(ppm,size,specw,off,freq)
c IN	: ppm,size,specw,off,freq
c
C returns index from ppm value

      implicit none
      real specw,off,freq,ppm,ptoir,htoir
      integer size
      external htoir

      ptoir = htoir(ppm*freq,size,specw,off)
      return
      end
c************************************************************
      function htoir(hertz,size,specw,off)
c IN	: hertz,size,specw,off
c
C returns index from hertz value

      implicit none
      real specw,off,hertz,htoir
      integer size

      htoir = size*(off - hertz)/specw + size + 1
      return
      end
c************************************************************
      function stoir(second,specw)
c IN	: second,specw
c
C returns index from second value

      implicit none
      real specw,second,stoir

      stoir = second*2*specw+1
      return
      end
c************************************************************
      function itosr(index,specw)
c IN	: index,size,specw
c
C returns second from index value

      implicit none
      real specw,index,itosr

      itosr = (index-1)/(2*specw)
      return
      end
c************************************************************
      function ktoir(k,size,kmin,kmax,kfact)
c IN	: k, size,kmin,kmax,kfact
c
C returns index from damping value
C
C dampings are exponentially spaced Kmin x kfact .. Kmax  x kfact
C =>     k = Kmin*exp(cst*(i-1))
C        log(k) = log(Kmin) + cst*(i-1)
C        cst = (Log(Kmax)-log(Kmin)) / (size-1)
C        i = 1+ ( log(k)-log(kmin) ) / cst

      implicit none
      integer size
      real k,kmin,kmax,ktoir,kfact

      real cst
      cst = (log(kmax)-log(kmin))/(size-1)
      ktoir = 1.0 + (log(k/kfact)-log(kmin)) / cst
      return
      end
c************************************************************
      function itokr(index,size,kmin,kmax,kfact)
c IN	: index,size,kmin,kmax,kfact
c
C returns damping from index value
C

      implicit none
      integer size
      real index,kmin,kmax,itokr,kfact

      real cst
      cst = (log(kmax)-log(kmin))/(size-1)
      itokr = kfact*kmin*exp(cst*(index-1.0))
      return
      end
c************************************************************
      function ttoir(val,tab,sztab)
c IN	: val,tab,sztab
c
C returns index from linear interpolation of tabulated values.
C
C ASSUMES regularly spaced tabulated values
C
      implicit none
      integer sztab
      real val, tab(sztab), ttoir

      integer i
      real tt
      if (tab(1).gt.tab(sztab)) then    ! decreasing case !
         do i=1,sztab
            if (tab(i).le.val) goto 100   ! find crossing
         enddo
         goto 100
      else                      ! growing case
         do i=1,sztab
            if (tab(i).ge.val) goto 100 ! find crossing
         enddo
      endif
 100  if (i.gt.1) then
         tt = tab(i-1)-tab(i)
         if (tt.ne.0.0) then
            ttoir = float(i)+(tab(i)-val)/tt
         else
            ttoir = float(i)
         endif
      else
         ttoir = 1.0
      endif

      return
      end
c************************************************************
      function itotr(index,tab,sztab)
c IN	: index,tab,sztab
c
C returns tabulated from index value
C
C ASSUMES regularly spaced tabulated values
C

      implicit none
      integer sztab

      real index, tab(sztab), itotr
      integer i

      if (index.le.1.0) then
         itotr = tab(1)
      else if (index.ge. float(sztab)) then
         itotr = tab(sztab)
      else
         i = int(index)
         itotr = tab(i) + (index-i)*(tab(i+1)-tab(i))
      endif
      return
      end

c************************************************************
      function d_itohr(index,dime,axis,error)
c IN	: index, dime, axis
c OUT   : error
c
c realize the transformation index to hz
c with the coord system of the dim data-set, along axis.
C if dim is 0 ==> use joined exp

      implicit none

      integer dime,axis,error
      real index,val,d_itohr
#include "cachev.inc"
#include "paramfilev.inc"
#include "sizebase.inc"
      external itohr
      real itohr

      if (dime .eq. 0) then
                 if (axis.eq.1) then		! switch on axis
                   val = itohr(index,
     *                   c_sizef1,c_specwf1,c_offsf1)
                 elseif (axis.eq.2 .and. c_dimmanip.gt.1) then
                   val = itohr(index,
     *                   c_sizef2,c_specwf2,c_offsf2)
                 elseif (axis.eq.3 .and. c_dimmanip.gt.2) then
                   val = itohr(index,
     *                   c_sizef3,c_specwf3,c_offsf3)
                 else
                   error=1
                 endif
      elseif (dime.eq.1) then
                 val = itohr(index,sizeimage1d,sp1d,of1d)
      elseif (dime.eq.2) then
                 if (axis.eq.1) then		! switch on axis
                   val = itohr(index,si1im,specw1,offset1)
                 elseif (axis.eq.2) then
                   val = itohr(index,si2im,specw2,offset2)
                 else
                   error = 1
                 endif
      elseif (dime.eq.3) then
                 if (axis.eq.1) then		! switch on axis
                   val = itohr(index,si3d1,sp3d1,of3d1)
                 elseif (axis.eq.2) then
                   val = itohr(index,si3d2,sp3d2,of3d2)
                 elseif (axis.eq.3) then
                   val = itohr(index,si3d3,sp3d3,of3d3)
                 else
                   error = 1
                 endif
      else
                 error = 1
      endif
      d_itohr = val
      return
      end
c************************************************************
      function d_htoir(hz,dime,axis,error)
c IN	: hz, dime, axis
c OUT   : error
c
c realize the transformation hz to index
c with the coord system of the dim data-set, along axis.
C if dim is 0 ==> use joined exp

      implicit none

      integer dime,axis,error
      real hz,val,d_htoir
#include "cachev.inc"
#include "paramfilev.inc"
#include "sizebase.inc"
      external htoir
      real htoir

      if (dime .eq. 0) then
                 if (axis.eq.1) then		! switch on axis
                   val = htoir(hz,
     *                   c_sizef1,c_specwf1,c_offsf1)
                 elseif (axis.eq.2 .and. c_dimmanip.gt.1) then
                   val = htoir(hz,
     *                   c_sizef2,c_specwf2,c_offsf2)
                 elseif (axis.eq.3 .and. c_dimmanip.gt.2) then
                   val = htoir(hz,
     *                   c_sizef3,c_specwf3,c_offsf3)
                 else
                   error=1
                 endif
      elseif (dime.eq.1) then
                 val = htoir(hz,sizeimage1d,sp1d,of1d)
      elseif (dime.eq.2) then
                 if (axis.eq.1) then		! switch on axis
                   val = htoir(hz,si1im,specw1,offset1)
                 elseif (axis.eq.2) then
                   val = htoir(hz,si2im,specw2,offset2)
                 else
                   error = 1
                 endif
      elseif (dime.eq.3) then
                 if (axis.eq.1) then		! switch on axis
                   val = htoir(hz,si3d1,sp3d1,of3d1)
                 elseif (axis.eq.2) then
                   val = htoir(hz,si3d2,sp3d2,of3d2)
                 elseif (axis.eq.3) then
                   val = htoir(hz,si3d3,sp3d3,of3d3)
                 else
                   error = 1
                 endif
      else
                 error = 1
      endif
      d_htoir = val
      return
      end

c************************************************************
      function d_itopr(index,dime,axis,error)
c IN	: index, dime, axis
c OUT   : error
c
c realize the transformation index to ppm
c with the coord system of the dim data-set, along axis.
C if dim is 0 ==> use joined exp

      implicit none

      integer dime,axis,error
      real index,val,d_itopr
#include "cachev.inc"
#include "paramfilev.inc"
#include "sizebase.inc"
      external itopr
      real itopr

      if (dime .eq. 0) then
                 if (axis.eq.1) then		! switch on axis
                   val = itopr(index,
     *                   c_sizef1,c_specwf1,c_offsf1,c_freq1)
                 elseif (axis.eq.2 .and. c_dimmanip.gt.1) then
                   val = itopr(index,
     *                   c_sizef2,c_specwf2,c_offsf2,c_freq2)
                 elseif (axis.eq.3 .and. c_dimmanip.gt.2) then
                   val = itopr(index,
     *                   c_sizef3,c_specwf3,c_offsf3,c_freq3)
                 else
                   error=1
                 endif
      elseif (dime.eq.1) then
                 val = itopr(index,sizeimage1d,sp1d,of1d,freq1d)
      elseif (dime.eq.2) then
                 if (axis.eq.1) then		! switch on axis
                   val = itopr(index,si1im,specw1,offset1,freq1)
                 elseif (axis.eq.2) then
                   val = itopr(index,si2im,specw2,offset2,freq2)
                 else
                   error = 1
                 endif
      elseif (dime.eq.3) then
                 if (axis.eq.1) then		! switch on axis
                   val = itopr(index,si3d1,sp3d1,of3d1,freq3d1)
                 elseif (axis.eq.2) then
                   val = itopr(index,si3d2,sp3d2,of3d2,freq3d2)
                 elseif (axis.eq.3) then
                   val = itopr(index,si3d3,sp3d3,of3d3,freq3d3)
                 else
                   error = 1
                 endif
      else
                 error = 1
      endif
      d_itopr = val
      return
      end


c************************************************************
      function d_ptoir(ppm,dime,axis,error)
c IN	: ppm, dime, axis
c OUT   : error
c
      implicit none

      integer dime,axis,error
      real ppm,val,d_ptoir
#include "cachev.inc"
#include "paramfilev.inc"
#include "sizebase.inc"
      external ptoir
      real ptoir

      if (dime .eq. 0) then
                 if (axis.eq.1) then		! switch on axis
                   val = ptoir(ppm,
     *                   c_sizef1,c_specwf1,c_offsf1,c_freq1)
                 elseif (axis.eq.2 .and. c_dimmanip.gt.1) then
                   val = ptoir(ppm,
     *                   c_sizef2,c_specwf2,c_offsf2,c_freq2)
                 elseif (axis.eq.3 .and. c_dimmanip.gt.2) then
                   val = ptoir(ppm,
     *                   c_sizef3,c_specwf3,c_offsf3,c_freq3)
                 else
                   error=1
                 endif
      elseif (dime.eq.1) then
                 val = ptoir(ppm,sizeimage1d,sp1d,of1d,freq1d)
      elseif (dime.eq.2) then
                 if (axis.eq.1) then		! switch on axis
                   val = ptoir(ppm,si1im,specw1,offset1,freq1)
                 elseif (axis.eq.2) then
                   val = ptoir(ppm,si2im,specw2,offset2,freq2)
                 else
                   error = 1
                 endif
      elseif (dime.eq.3) then
                 if (axis.eq.1) then		! switch on axis
                   val = ptoir(ppm,si3d1,sp3d1,of3d1,freq3d1)
                 elseif (axis.eq.2) then
                   val = ptoir(ppm,si3d2,sp3d2,of3d2,freq3d2)
                 elseif (axis.eq.3) then
                   val = ptoir(ppm,si3d3,sp3d3,of3d3,freq3d3)
                 else
                   error = 1
                 endif
      else
                 error = 1
      endif
      d_ptoir = val
      return
      end
     

c************************************************************
      subroutine currunitr(val,x,dd)
c IN	: x,dd
c OUT	: val
C
C   index => current
C return the x position (in index) expressed in current unit
c based on current file parameters
c dd is the axis when in nD (1, 2, (3) )

      implicit none
      integer dd	! position and axis to use
      real val,x          ! returned value
      character*8  unit
#include "zoomparamv.inc"
#include "paramfilev.inc"
#include "unitextern.inc"
#include "sizebase.inc"
#include "laplacev.inc"

      integer size
      real specw,off,freq

      if (dim.eq.1) then
        freq = freq1d
        specw = sp1d
        off = of1d
        size = sizeimage1d
        unit = unitx
      elseif (dim.eq.2) then
        if (dd.eq.1) then
          freq = freq1
          specw = specw1
          off = offset1
          size = si1im
          unit = unity
        elseif (dd.eq.2) then
          freq = freq2
          specw = specw2
          off = offset2
          size = si2im
          unit = unitx
        endif
      else
        if (dd.eq.1) then
          freq = freq3d1
          specw = sp3d1
          off = of3d1
          size = si3d1
          unit = unity
        elseif (dd.eq.2) then
          freq = freq3d2
          specw = sp3d2
          off = of3d2
          size = si3d2
          unit = unitx
        elseif (dd.eq.3) then
          freq = freq3d3
          specw = sp3d3
          off = of3d3
          size = si3d3
          unit = unitx
        endif
      endif

      if (unit.eq.'INDEX') then
        val = x
      elseif (unit.eq.'HZ') then
        val = itohr(x,size,specw,off)
      elseif (unit.eq.'PPM') then
        val = itopr(x,size,specw,off,freq)
      elseif (unit.eq.'SECOND') then
        val = itosr(x,specw)
      elseif (unit.eq.'DAMPING') then
        val = itokr(x,size,dmin,dmax,dfactor)
      elseif (unit.eq.'TAB') then
        val = itotr(x,sampling,szsamp)
      else
        val = 0.0
      endif
      return
      end

C************************************************************
C************************************************************

c************************************************************
      subroutine unit2ir(val,x,dd)
c IN    : x,dd
c OUT   : val
C 
C    current => index
C
C return the x position (in current unit) expressed in index
c based on current file parameters
c dd is the axis when in nD (1, 2, (3) )

      implicit none
      integer dd        ! position and axis to use
      real val,x          ! returned value
      character*8  unit
#include "zoomparamv.inc"
#include "paramfilev.inc"
#include "unitextern.inc"
#include "sizebase.inc"
#include "laplacev.inc"

      integer size
      real specw,off,freq

      if (dim.eq.1) then
        freq = freq1d
        specw = sp1d
        off = of1d
        size = sizeimage1d
        unit = unitx
      elseif (dim.eq.2) then
        if (dd.eq.1) then
          freq = freq1
          specw = specw1
          off = offset1
          size = si1im
          unit = unity
        elseif (dd.eq.2) then
          freq = freq2
          specw = specw2
          off = offset2
          size = si2im
          unit = unitx
        endif
      else
        if (dd.eq.1) then
          freq = freq3d1
          specw = sp3d1
          off = of3d1
          size = si3d1
          unit = unity
        elseif (dd.eq.2) then
          freq = freq3d2
          specw = sp3d2
          off = of3d2
          size = si3d2
          unit = unitx
        elseif (dd.eq.3) then
          freq = freq3d3
          specw = sp3d3
          off = of3d3
          size = si3d3
          unit = unitx
        endif
      endif

      if (unit.eq.'INDEX') then
        val = x
      elseif (unit.eq.'HZ') then
        val = htoir(x,size,specw,off)
      elseif (unit.eq.'PPM') then
	val = ptoir(x,size,specw,off,freq)
      elseif (unit.eq.'SECOND') then
        val = stoir(x,specw)
      elseif (unit.eq.'DAMPING') then
        val = ktoir(x,size,dmin,dmax,dfactor)
      elseif (unit.eq.'TAB') then
        val = ttoir(x,sampling,szsamp)
      else
        val = 0.0
      endif
      return
      end

C==================================================
C      subroutine recup_unit(x,y,retx,rety)
      subroutine win2indexr(retval,winarg,dime,axe)
C IN  : win,dime,axe
C OUT : retval
C
C     window (0..1) => index
C
C recuperation des coordonnees dans l'unite index
C
C modif MAD 5 sep 1997
C modif MAD jul `99 (recreated form recup_unit and added dime)
C
C bug : not available in 3D yet

        implicit none
#include "zoomparamv.inc"
#include "paramfilev.inc"
	real retval,winarg,win
        integer dime,axe

        integer si1,si2,zlf1,zlf2,zur1,zur2

        win = min(winarg,1.0)    ! truncate to 0..1  -might not be a good idea-
        win = max(win,0.0)

        if (dime.eq.2) then
           if (axe.eq.1) then
              if (itype.eq.2.or.itype.eq.3) then ! fake imag as if there is
                 si1 = si1im/2 ! half points
                 zlf1 = (zolf1+1)/2
                 zur1 = zour1/2
              else
                 si1=si1im
                 zlf1 = zolf1
                 zur1 = zour1
              endif
              if (zoom.eq.0) then
                    retval =  win*si1 + 0.5
              else
                    retval =  win*(zur1-zlf1+1)  + zlf1 - 0.5
              endif
              if (itype.eq.2.or.itype.eq.3) retval = 2*retval-1

           elseif (axe.eq.2) then
              if (itype.eq.1.or.itype.eq.3) then ! fake imag as if there is
                 si2=si2im/2    ! half points
                 zlf2 = (zolf2+1)/2
                 zur2 = zour2/2
              else
                 si2 = si2im
                 zlf2 = zolf2
                 zur2 = zour2
              endif
              if (zoom.eq.0) then
                    retval =  win*si2 + 0.5
              else
                    retval =  win*(zur2-zlf2+1)  + zlf2 - 0.5
              endif
              if (itype.eq.1.or.itype.eq.3) retval = 2*retval-1
           endif
        elseif (dime.eq.1) then
           if (it1d.eq.1) then
              si1 = sizeimage1d/2
              zlf1 = (zo1dl+1)/2
              zur1 = zo1du/2
           else
              si1 = sizeimage1d
              zlf1 = zo1dl
              zur1 = zo1du
           endif
           if (zoom.eq.0) then
              retval = win*(si1-1) + 1.0
           else          ! if zoom on
              retval = win*(zur1-zlf1) + zlf1
           endif
           if (it1d.eq.1) retval = 2*retval-1
        endif
      return
      end

C==================================================
      subroutine unit_2_wincoord(currval,retval,axe)
c
c IN :  currval (current unit), axe (1:f1, 2:f2)
c OUT:  retval	(window unit 0-1)
C
C     current => window 

      implicit none
#include "zoomparamv.inc"
#include "paramfilev.inc"

      integer axe
      real currval,retval,indexval

      call unit2ir(indexval,currval,axe)
      call index2winr(retval,indexval,dim,axe)
      end

C==================================================
      subroutine index2winr(retval,indexval,dime,axe)
c
c IN :  indexval (index unit), axe (1:f1, 2:f2), dime
c OUT:  retval  (window unit 0-1)
C
C   index => window 
C modif MAD jul `99 (added dime)

      implicit none
#include "zoomparamv.inc"
#include "paramfilev.inc"

      integer dime,axe
      real retval,indexval

C IN 1D
      if (dime.eq.1) then
           if (zoom.eq.0) then
              if (it1d.eq.0) then
                retval = (indexval-1) / (sizeimage1d-1)
              else
                retval = ((indexval-1)/2) / (sizeimage1d/2-1)
              endif
           else
              if (it1d.eq.0) then
                 retval = (indexval-zo1dl) / (zo1du-zo1dl)
              else
                 retval = ((indexval-zo1dl)/2) /
     *                 float(int((zo1du-zo1dl+1)/2)-1)
              endif
           endif
C in 2D
      elseif (dime.eq.2) then
C EN F2:
         if (axe.eq.2) then
             if (zoom.eq.0) then
		retval = (indexval-0.5) / si2im
             else
		retval = (indexval-zolf2+0.5) / (zour2-zolf2+1)
             endif
C EN F1:
         elseif (axe.eq.1) then
             if (zoom.eq.0) then
		retval = (indexval-0.5) / si1im
             else
		retval = (indexval-zolf1+0.5) / (zour1-zolf1+1)
             endif
         endif
      endif
      end

C************************************************************
C************************************************************
c integer versions :
      function itoh(index,size,specw,off)
c integer version of itohr (i.e. index is integer)
      implicit none
      real specw,off,itohr,itoh
      integer size,index
      itoh = itohr(float(index),size,specw,off)
      return
      end
c ------------------------
      function itop(index,size,specw,off,freq)
c integer version of itopr (i.e. index is integer)
      implicit none
      real specw,off,itop,freq,itohr
      external itohr
      integer size,index
      itop = itohr(float(index),size,specw,off)/freq
      return
      end
c ------------------------
      function ptoi(ppm,size,specw,off,freq)
c integer version of ptoir (i.e. index is integer)
      implicit none
      real specw,off,freq,ppm,htoir
      integer size,ptoi
      external htoir
      ptoi = htoir(ppm*freq,size,specw,off)
      return
      end
c ------------------------
      function htoi(hertz,size,specw,off)
c integer version of hotir (i.e. index is integer)
      implicit none
      real specw,off,hertz
      integer size,htoi
      htoi = int((size-1)*(off - hertz)/specw + size)
      return
      end
c ------------------------
      function stoi(second,specw)
c integer version of stoir (i.e. index is integer)
      implicit none
      real specw,second
      integer stoi

      stoi = int(second*2*specw)+1
      return
      end
c ------------------------
      function itos(index,specw)
c integer version of itosr (i.e. index is integer)
      implicit none
      real specw,itos
      integer index

      itos = float(index-1)/(2*specw)
      return
      end
c ------------------------
      function ktoi(k,size,kmin,kmax,kfact)
c integer version of ktoir
      implicit none
      integer size,ktoi
      real k,kmin,kmax,ktoir,kfact
      external ktoir
      ktoi = int( ktoir(k,size,kmin,kmax,kfact) )
      return
      end
c ------------------------
      function itok(index,size,kmin,kmax,kfact)
c integer version of itokr
      implicit none
      integer size,index
      real kmin,kmax,itokr,itok,kfact
      external itokr
      itok = itokr(float(index),size,kmin,kmax,kfact)
      return
      end
c ------------------------
      function ttoi(val,tab,sztab)
c integer version of ttoir
      implicit none
      integer sztab,ttoi
      real val,tab(sztab),ttoir
      external ttoir
      ttoi = int( ttoir(val,tab,sztab) )
      return
      end
c ------------------------
      function itot(index,tab,sztab)
c integer version of itotr
      implicit none
      integer sztab,index
      real tab(sztab),itotr,itot
      external itotr
      itot = itotr(float(index),tab,sztab)
      return
      end
c ------------------------
      subroutine currunit(val,x,dd)
c integer version of currunitr
      implicit none
      integer x,dd	! position and axis to use
      real val          ! returned value
      call currunitr(val,float(x),dd)
      return
      end

