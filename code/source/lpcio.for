C This file is a part of the GIFA program
C     This software has been developped by the NMR Group in CBS/Montpellier
C
C     Authors :       T.E.Malliavin M.A.Delsuc
C                     C.B.S. Faculte de Pharmacie
C                     34060 Montpellier   FRANCE
C
C     This software cannot be used unless the user have been fully
C licensed to do so form the above laboratory.
C     This file cannot be copied, duplicated or used in any other program,
C without written permission from the authors.

C For bit I/O for LPC in GIFA
C lpcopen(name,error) open the lpc file
c lpcclose(totsize)    close it
c lpcwrthead(szi1,szi2,sar,ar(sar),specw1,spcw2,off1,off2,itype) 
c write the header
c lpcwrtfid(fid(sar),delta(szi2-sar))    write the fid and the delta
c lpcreadhead(szi1,szi2,sar,ar(sar),specw1,spcw2,off1,off2,itype) 
c read the header
c lpcreadfid(fid(sar),delta(szi2-sar))    read the fid and the delta
c
c
c if your compiler misses the ishft, ior, ibits routines
c uncomment the following line :
#f2c#include "lpcio_util.inc"
c
C***************************************************************
	subroutine lpcopen(name,error)
c IN	: name
c OUT	: error
c
c opens a LPC coded file, connect to unit 10
c
	implicit none
#include "lpcio.inc"
	character*256 name	! name of the file
	integer error		! .NE.0 if error

	open (10,file=name,status='unknown',
     *        iostat=error,form='unformatted')
	total = 0
	return
	end

C***************************************************************
	subroutine lpcclose(totsize)
c OUT	: totsize
c
c close the currently opend lpc coded file,
c returns the size of the file
c
	implicit none
	integer totsize
C totsize is the size of the coded file
#include "lpcio.inc"
	close(10)
	totsize = total
	return
	end

C***************************************************************
	subroutine lpcwrthead(szi1,szi2,sar,ar,specw1,specw2,
     *		off1,off2,itype,error)
c IN	: szi1,szi2,sar,ar,specw1,specw2,off1,off2,itype
c OUT	: error
c
c write the header of the opened lpc coded file.
c
	implicit none
	integer szi1	! size in F1
	integer szi2	! size in F2
	integer sar	! size of AR polynomial
	real ar(sar)	! the AR polynomial
	real specw1,specw2,off1,off2	! spectral parameters
        integer itype ! parameter describing the type of data : real,cpx,...
	integer error	! .NE.0 if error
	integer i
#include "lpcio.inc"
c
	write (10,iostat=error) specw1,specw2,off1,off2,itype,szi1,szi2,
     *          sar,(ar(i),i=1,sar) 
	size1 = szi1	! copy into common block
	size2 = szi2
	sizear = sar
	total = sar + 7
	return
	end

C***************************************************************
	subroutine lpcreadhead(szi1,szi2,sar,ar,specw1,specw2,
     *		off1,off2,itype,error)
c IN	: szi1,szi2,sar,ar,specw1,specw2,off1,off2,itype
c OUT	: error
c
c read the header of the currently opened lpc coded file
c
	implicit none
	integer szi1	! size in F1
	integer szi2	! size in F2
	integer sar	! size of AR polynomial
	real ar(*)	! the AR polynomial
	real specw1,specw2,off1,off2	! spectral parameters
        integer itype     ! parameter describing the type of data : real,cpx...
	integer error	! .NE.0 if error
	integer i
#include "lpcio.inc"
c
	read (10,iostat=error) specw1,specw2,off1,off2,itype,szi1,szi2,
     *          sar,(ar(i),i=1,sar) 
	size1 = szi1	! copy into common block
	size2 = szi2
	sizear = sar
	total = sar + 7
	return
	end

C***************************************************************
	subroutine lpcwrtfid(fid,delta,error)
c IN	: fid,delta
c OUT	: error
c
c write a fid to the opened lpc coded file
	implicit none
	real fid(*)
	integer delta(*)
	integer error,endbuf
#include "lpcio.inc"
	integer i,bit,maxb
C prepare buffer.
	do i=1,sizear
		rbuffer(i) = fid(i)
	enddo
	offset = 32*(sizear)
	do i=sizear+1,size2
		buffer(i)=0
	enddo
C il faut determiner bit ici ...
	call codoptim(delta,size2-sizear,bit)
	maxb = 2**(bit-1) - 1
	do i=1,size2-sizear
	   if (abs(delta(i)).le.maxb) then
		call putbit(delta(i),bit)
	   else
		call putbit(maxb+1,bit)
		call putbit(delta(i),32)
	   endif
	enddo
	endbuf = (offset+31)/32 
	write(10,iostat=error) bit,endbuf,(buffer(i),i=1,endbuf)
	total = total + endbuf+2
	return
	end

C***************************************************************
	subroutine lpcreadfid(fid,delta,error)
c OUT	: fid,delta,error
c
c reads a fid from the opened lpc coded file
	implicit none
	real fid(*)
	integer delta(*)
	integer error,endbuf
#include "lpcio.inc"
	integer i,maxb,bit
	read(10,iostat=error) bit,endbuf,(buffer(i),i=1,endbuf)
	maxb = 2**(bit-1) - 1
	if (error.ne.0) return
	do i=1,sizear
		fid(i) = rbuffer(i) 
	enddo
	offset = 32*(sizear)
	do i=1,size2-sizear
	   call getbit(delta(i),bit)
	   if (abs(delta(i)).eq.maxb+1) call getbit(delta(i),32)
	enddo
	total = total + endbuf+2
	return
	end

C***************************************************************
	subroutine putbit(a,bit)
c IN	: a,bit
c SIDE	: offset, buffer
c
c put the the word "a" of length "bit" bits using variables offset and buffer
c in common
	implicit none
	integer a,bit
#include "lpcio.inc"
#f2c        integer ishft,ior
#f2c        external ishft,ior
	integer doff,moff,aa,b1,b2
	moff = mod(offset,32)
	doff = offset/32  + 1
	if (moff+bit.le.32) then	! easy case
	   call shift1(aa,a,moff,bit)
	   buffer(doff) = ior(buffer(doff),aa)
	   offset = offset + bit
	else if (moff.eq.31) then	! don't want 1bit code
	   call shift1(aa,a,0,bit)
	   buffer(doff+1) = ior(buffer(doff+1),aa)
	   offset = offset+bit+1
	else				 ! we'll have to split
	   b1 = 32-moff		! left on this entry
	   b2 = bit - b1	! on next entry
	   if (b2.eq.1) b2=2
	   call shift1(aa,a,moff,b1)
	   buffer(doff) = ior(buffer(doff),aa)
	   call shift1(aa,ishft(a,-b1),0,b2)
	   buffer(doff+1) = aa
	   offset=offset+b1+b2
	endif
	return
	end

C***************************************************************
	subroutine getbit(a,bit)
c IN	: bit
c OUT	: a
c SIDE	: offset, buffer
c get the the word "a" of length "bit" bits using variables offset and buffer
c in common
	implicit none
	integer a,bit
#include "lpcio.inc"
	integer doff,moff,a1,a2,b1,b2

	moff = mod(offset,32)
	doff = offset/32  + 1
	if (moff+bit.le.32) then	! easy case
	   call shift2(a,buffer(doff),moff,bit)
	   offset = offset+bit
	elseif (moff.eq.31) then
	   call shift2(a,buffer(doff+1),0,bit)
	   offset = offset+bit+1
	else				! we'll have to split
	   b1 = 32-moff		! left on this entry
	   b2 = bit - b1	! on next entry
	   if (b2.eq.1) b2=2
	   call shift3(a1,buffer(doff),moff,b1)
	   call shift2(a2,buffer(doff+1),0,b2)
	   a = a2*2**b1 + a1
	   offset = offset+b1+b2
	endif
	return
	end

C***************************************************************
	subroutine shift1(aa,a,b,bb)
c IN	: a,b,bb
c OUT	: aa
C aa is a shifted left by b bits, masked to bb bits wide
C used by putbit
	implicit none
	integer aa,a,b,bb,ta
#f2c        integer ishft
#f2c        external ishft
c	   if (a.ge.0) then
c		aa = a * 2**b
c	   else
c		aa = (2**bb + a) * 2**b
c	   endif
c	ta = iand(a,(2**bb - 1))  ! mask
c	aa = ishft(ta,b)          ! then shift
	ta = ishft(a,32-bb)       ! flush left
	aa = ishft(ta,b+bb-32)    ! bring back
	return
	end

C***************************************************************
	subroutine shift2(aa,a,b,bb)
c IN	: a,b,bb
c OUT	: aa
C aa is a shifted right by b bits, masked to bb bits wide
C and negative values are propagated
C used by getbit
	implicit none
	integer aa,a,b,bb,ta,tta
#f2c        integer ishft,ibits
#f2c        external ishft,ibits
	tta = ibits(a,b,bb)
	ta = ishft(tta,32-bb)
	aa = ta /(2**(32-bb))
	return
	end

C***************************************************************
	subroutine shift3(aa,a,b,bb)
c IN	: a,b,bb
c OUT	: aa
C same as shift2, but negative values are not propagated
C used by getbit
	implicit none
	integer aa,a,b,bb
#f2c        integer ibits
#f2c        external ibits
	aa = ibits(a,b,bb)
	return
	end

C***************************************************************
      subroutine codoptim (delta,size,code)
c IN	: delta,size
c OUT	: code
C computes optimal codage for compacted data 
C
c code : optimum number of bits
c delta(size) : tot be compacted
C
C Builds a table (sumloc(ncode)) of all possiible coding, and choses
C the best one
c
      implicit none

      integer ncode
      parameter(ncode = 32)

      integer size,delta(size)
      integer i,nbits,code,imin
      integer sumloc(ncode)
      real temp

      do i = 1,ncode
	 sumloc(i) = 0
      enddo

      do i = 1,size
	 if (delta(i).ne.0) then
	    temp = 1 + ( log(abs(float(delta(i)))) / log(2.0) )
	    nbits = int(temp) + 1
	 else
	    nbits = 1
	 endif
	 call misaj (nbits,sumloc,ncode)
      enddo

      imin = 1
      do i = 1,ncode
	 if (sumloc(i).lt.sumloc(imin)) then
 	    imin = i
	 endif
      enddo
      code = imin
      return
      end
C**************************************************************************
      subroutine misaj (nbits,sumloc,ncode)
c IN	: nbits,ncode
c OUT	: sumloc
c
c internaly used by codoptim to update the optimal code table.
c
      implicit none

      integer nbits,i,ncode
      integer sumloc(ncode)

      do i = 1,ncode
	 sumloc(i) = sumloc(i) + i
	 if (i.le.(nbits-1)) then
	    sumloc(i) = sumloc(i) + 32
	 endif
      enddo
      return
      end
