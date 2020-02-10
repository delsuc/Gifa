C
C     This file is a part of the GIFA program.
C     This software was developed by the NMR Group of the
C     CNRS in GIF/Yvette France.
C
C     Programmers:    M.A. Delsuc, T.Malliavin
C                     NMR Laboratory
C                     ICSN - CNRS
C                     91190 GIF/YVETTE   FRANCE
C
C     This software cannot be used unless the user has been fully
C licensed by the above laboratory.
C     This file cannot be copied, transferred, or used for any other
C purpose without the written permission of the authors.
C
C
C Basic module for LP.
C***************************************************************
      subroutine lpforw(rec,szrec,ar,szar,data,szdata,error)
c IN	: szrec,ar,szar,data,szdata,error
c OUT	: rec, error
C lpforw reconstruct the data after the last point, using the
C linear prediction equation.
C i.e. :  data(i) = -sum(k=1 to szar, ar(k)*data(i-k) )
C         data(i) = -sum(s=i-1 to i-szar, ar(i-s)*data(s) )
C
C rec(szrec) final data.
C ar(szar) the autoregressive polynome.
C data(szdata) experimental data
C error error signal for overflow
 
      implicit none
#include "constant.inc"
      integer szrec,szar,szdata,error
      complex rec(szrec),data(szdata),tmp
      complex*16 ar(szar)
      integer i
 
      call copvect(rec,data,2*szdata)
C let's reverse ar() to be able to use dotvect.
      call zrevvect(ar,szar)
      do i=szdata+1,szrec
         call ddotvect(tmp,ar,rec(i-szar),szar)
	 if (abs(tmp).gt.maxexp) then
	    error = 1
   	    return		
	 endif
	 rec(i) = -tmp
      enddo
      call zrevvect(ar,szar)
      return
      end
 
C***************************************************************
      subroutine lpback(rec,szrec,ar,szar,data,szdata,error)
c IN	: szrec,ar,szar,data,szdata
c OUT	: rec,error
C lpback reconstruct the data before the first point, using the
C linear prediction equation.
C i.e. :  data(i) = -sum(k=1 to szar, ar(k)*data(i+k) )
C
C rec(szrec) final data.
C ar(szar) the autoregressive polynome.
C data(szdata) experimental data
C error error signal for overflow
 
      implicit none
#include "constant.inc"
      integer szrec,szar,szdata,error
      complex rec(szrec),data(szdata),tmp
      complex*16 ar(szar)
      integer i,n
 
      n = szrec-szdata
      if (n.le.0) return
      call rcopvect(rec(n+1),data,2*szdata)
      do i=n,1,-1
         call ddotvect(tmp,ar,rec(i+1),szar)
	 if (abs(tmp).gt.maxexp) then
	    error = 1
   	    return		
	 endif
	 rec(i) = -tmp
      enddo
      return
      end
 
C***************************************************************
C
      subroutine lpfilter(rec,szrec,ar,szar,data,szdata)
c IN	: szrec,ar,szar,data,szdata
c OUT	: rec
C filter the water signal with LP
C rec(szrec) final data.
C ar(szar) the autoregressive polynome.
C data(szdata) experimental data
      implicit none
      integer szrec,szar,szdata
      complex rec(szrec),data(szdata),temp
      complex*16 ar(szar)
      integer i,k
c
      do i = szdata-szar,1,-1
	 temp = (0.d0,0.d0)
	 do k = szar,1,-1
C dcmplx is bugged in xlf AIX v2.0 !
#aix	    temp = temp + 
#aix     *   (ar(k)*dcmplx(dble(real(data(i+k))),dble(imag(data(i+k)))))
#_aix        temp = temp + (ar(k)*dcmplx(data(i+k)))
	 enddo
	 rec(i) = data(i) + temp
      enddo
      return
      end
 
C***************************************************************
      subroutine lpcomp (rec,ar,szar,data,szdata)
c IN	: ar,szar,data,szdata
c OUT	: rec
c calculate deltas for compaction of data
C
C rec(szdata) first szar exp.data and delta between exp. data
c and calculated data with ar(szar)
C ar(szar) the autoregressive polynome.
C data(szdata) experimental data
      implicit none
#include "constant.inc"
      integer szar,szdata
      complex rec(szdata),data(szdata)
      complex ar(szar)
      integer i,zz1,zz2
      complex zz
 
      call copvect (rec,data,2*szar)
C let's reverse ar() to be able to use dotvect.
C not necessary to reverse ar(), because it was done in writec
c      call crevvect(ar,szar)
      do i = szar+1,szdata
         call cdotvect(zz,ar,data(i-szar),szar)
c	 zz = (0.0,0.0)
c	 do j = 1,szar
c	    zz = zz + (data (i-j)*ar(j))
c	 enddo
	 if (abs(zz).gt.maxexp) then
	    call gifaout('Overflow during prediction !')
   	    return		
	 endif
	 zz1 = int(real(zz))
	 zz2 = int(aimag(zz))
	 rec(i) = data(i) + cmplx(zz1,zz2)
      enddo
c      call crevvect(ar,szar)
 
      return
      end
 
C***************************************************************
      subroutine lpdecomp (rec,ar,szar,data,szdata)
c IN	: rec,ar,szar,szdata
c OUT	: data
c calculate exp. data from data compacted
C
c rec(szdata) data compacted : first exp.data (szar), and deltas (rec(i+szar))
c between exp. data and calculated data with ar(szar)
C ar(szar) the autoregressive polynome.
C data(szdata) experimental data
      implicit none
#include "constant.inc"
      integer szar,szdata
      complex rec(szdata),data(szdata)
      complex ar(szar)
      integer i
      integer zz1,zz2
c      real zz1,zz2
      complex zz
 
      call copvect(data,rec,2*szar)
C let's reverse ar() to be able to use dotvect.
C not necessary to reverse ar(), because it was done in readc
c      call crevvect(ar,szar)
      do i = szar+1,szdata
         call cdotvect(zz,ar,data(i-szar),szar)
	 if (abs(zz).gt.maxexp) then
	    call gifaout('Overflow during prediction !')
   	    return		
	 endif
	 zz1 = int(real(zz))
	 zz2 = int(aimag(zz))
	 data(i) = rec(i) - cmplx(zz1,zz2)
      enddo
c      call crevvect(ar,szar)
 
      return
      end
 
C***************************************************************
      subroutine codedelta (rec,szar,szdata)
c IN	: rec,szar,szdata
c 
c calculate size of compacted data
C
C szar : 2*order of burg
c rec(szdata) data compacted : first exp.data (szar), and deltas (rec(i+szar))
c between exp. data and calculated data with ar(szar)
c
      implicit none
      integer szar,szdata
      real rec(szdata),temp
      integer i,sizecomp,taille(64),nbits,deltasnul
 
      do i = 1,64
	 taille(i) = 0
      enddo
      sizecomp = 0
      deltasnul = 0
 
      do i = szar+1,szdata
	 if (rec(i).ne.0.0) then
	    temp = 1 + (log(abs(rec(i))+1)/log(2.0))
	    if (float(int(temp)).eq.temp) then
	       nbits = int(temp)
            else	
	       nbits = int(temp) + 1
	    endif
	    taille(nbits) = taille(nbits) + 1
	 else
	    deltasnul = deltasnul + 1
	 endif
      enddo
 
      do i = 1,64
	if (taille(i).ne.0) then
	 write(*,*)'nbre d''entiers a ',i,' bits : ',taille(i)
	 sizecomp = sizecomp + (taille(i)*i)
	endif
      enddo
      write(*,*)'Nbre d''entiers nuls : ',deltasnul
 
      return
      end
 
C***************************************************************
      subroutine sizecompact (rec,szar,szdata,code)
c IN	: rec,szar,szdata,code
c
c calculate size of compacted data
C
C szar : 2*order of burg
c rec(szdata) data compacted : first exp.data (szar), and deltas (rec(i+szar))
c between exp. data and calculated data with ar(szar)
c
      implicit none
      integer szar,szdata
      real rec(szdata),temp
      integer i,sizecomp,nbits,compacted,code,deltasnul
 
      compacted = 0
      sizecomp = 0
      deltasnul = 0
 
      do i = szar+1,szdata
	 if (rec(i).ne.0.0) then
	    temp = 1 + (log(abs(rec(i))+1)/log(2.0))
	    if (float(int(temp)).eq.temp) then
	       nbits = int(temp)
            else	
	       nbits = int(temp) + 1
	    endif
	    if (nbits.le.code) then
		compacted = compacted + 1
	    endif
	 else
	    deltasnul = deltasnul + 1
	 endif
      enddo
      write(*,*)'nbre de deltas compactes : ',compacted+deltasnul
      write(*,*)'nbre de deltas codes sur 32 bits : ',
     &			szdata-szar-compacted-deltasnul
 
      sizecomp = ((compacted+deltasnul)*code) + ((32+code)*
     &		(szdata-szar-compacted-deltasnul)) + (64*szar)
       write(*,*)'taille du fichier compacte : ',sizecomp+32
     &				,' bits.'
 
      return
      end
 
C*************************************************************
      subroutine listdcpx(array,size,indx1,indx2)
c IN	: array,size,indx1,indx2
c
C   This subroutine  outputs to user the vector array, complex*16, typed real
C  in gifa.for, from indx1 to indx2 element
C
      Implicit none
#include "gifaout.inc"

      integer i,size,indx1,indx2
      complex*16 array(size)

      write(*,122) (array(i),i=indx1,indx2)
      write(journal,122) (array(i),i=indx1,indx2)
122   format (F15.6)
      return
      end

C*************************************************************
      subroutine listfreq(array,size,dfreq,freq0,indx,specw)
c IN	: array,size,dfreq,freq0,indx,specw
c   This subroutine  outputs to user the elements of vector array, complex*16
C   whose argumant is in the interval [freq0-dfreq,freq0+dfreq]
C
      Implicit none
#include "gifaout.inc"
 
#include "constant.inc"
      integer i,size,k,indx(size)
      real dfreq,freq0,temp,temp1,specw,tpi
      real*8 zr,zi,freq,amp
      complex*16 array(size)

 
      k = 1
      tpi = 2.0*pi
      do i = 1,size
	temp = real(dimag(array(i)))
	zr=dble(array(i))
	zi=dimag(array(i))
c
c Computation of frequences in spectrometer's scale. DO NOT USE ATAN2!
c If zr.ne.0, no problem, else there is special values for frequences.
c
	freq=atan2(zi,zr)
	if (zr.ne.0.d0) then
	  temp1 = real(datan(zi/zr))*specw/tpi
	   if (zr.lt.0.d0)  freq = temp1 + specw/2
	   if ((zi.gt.0.d0).and.(zr.gt.0.d0)) 
     &	 		 freq = temp1 
	   if ((zi.lt.0.d0).and.(zr.gt.0.d0)) 
     &	 		 freq = temp1 + specw 
	else
          if (zi.gt.0.0)  freq = specw/4
          if (zi.lt.0.0)  freq = 3*specw/4
	endif
#_f2c      amp = cdabs(array(i))
#f2c      amp = zabs(array(i))
	temp = real(freq)
	if (abs(temp-freq0).le.dfreq) then
	  write(*,123) array(i),i,freq,amp
	  write(journal,123) array(i),i,freq,amp
	  indx(k) = i
	  k = k + 1
	endif
      enddo
      indx(k) = 0
123   format (2F15.6,i4,2F15.6)
      return
      end
C***************************************************************
	subroutine predloc(rdata,order,szdata,szfin,verbose,error)
c IN	: order,szdata,szfin,verbose,error
c OUT	: rdata
c
c Predict the following points of the FID from LP parameters
c without using any working space
c
	implicit none
 
#include "sizebase.inc"
#include "constant.inc"
	integer order,szdata,szfin,error,i,j,verbose
        character*(256) st
	complex rdata(szfin),tmp
	real*8 power
	complex*16 afloc(ldmax),wk1loc(sizemax/2)
	complex*16 wk2loc(sizemax/2)
 
c	error = 0
	call burgc(rdata,szdata,order,power,afloc,
     *		wk1loc,wk2loc,error)
	if (error.ne.0) then
	  call gifaout('Error in processing Burg coefficients')
          return
        endif
	call cp2vect(rdata,rdata,2*szfin,2*szdata)
C reconstruct the data after the last point, using the
C linear prediction equation. Doesn't any work space.
C i.e. :  rdata(i) = -sum(k=1 to szar, ar(k)*rdata(i-k) )
C         rdata(i) = -sum(s=i-1 to i-szar, ar(i-s)*rdata(s) )
        if (verbose.eq.1) then
	  do i = 1,order
	   write(st,*)'afloc',afloc(i),i
           call gifaout(st)
	  enddo
        endif
      	do i=szdata+1,szfin
 	  tmp = (0.0,0.0)
	  do j = 1,order
C dcmplx is bugged in xlf AIX v2.0 !
#aix	   tmp = tmp + (afloc(j)*
#aix     * dcmplx(dble(real(rdata(i-j))),dble(imag(rdata(i-j)))))
#_aix       tmp = tmp + (afloc(j)*dcmplx(rdata(i-j)))
	  enddo
	  if (cabs(tmp).gt.maxexp) then
	    call gifaout('Overflow during prediction!')
            error = 1
   	    return		
	  endif
	  rdata(i) = -tmp
      	enddo
 
	return
	end



