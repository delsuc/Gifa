C This file is a part of the GIFA program
C     This software has been developped by the NMR Group in GIF/Yvette.
C
C     Authors :       M.A.Delsuc, T.E.Malliavin
C                     NMR Laboratory,
C                     ICSN - CNRS
C                     91190 GIF/YVETTE   FRANCE
C
C     This software cannot be used unless the user have been fully
C licensed to do so form the above laboratory.
C     This file cannot be copied, duplicated or used in any other program,
C without written permission from the authors.
C

	subroutine burgspectrum (data,siz,pow,ar,temp,work,order,specw2)

c IN	: siz,pow,ar,temp,work,order,specw2
c OUT	: data
c WORK	: temp,work
c data(siz) ar(order)
c computes pow/fft(ar)  and put it in data
c
	implicit none

	integer order,siz
	real specw2,scale
	complex*16 ar(order)
	real temp(2*order),work(0:(2*siz)-1),data(siz)
	real*8 pow

	scale = real(pow)
#d	write(*,*)'scale',scale
	call dscopvect(temp,ar,2*order)
        work(0) = 1.0
        work(1) = 0.0
	call cp2vect (work(2),temp,(2*siz)-1,2*order)
	call fftcpx (work(0),siz,1)
        call invnorm (data,work,siz,scale)

	return
	end


