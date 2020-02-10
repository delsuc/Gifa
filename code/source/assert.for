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

	subroutine assert (file, subr, test, line)
c
c IN	: file,subr,test,line
c
c Part of the ASSERT subsystem
c issue to the user a error msg, giving the name of the faulty code.
c and what to do it.
c to be used with the fpp program.

	implicit none
	character*(*) file,subr,test
	integer line, count, maxcount
	parameter (maxcount=10)
	save count
	data count/0/

	write(*,*) '---assertion failed -------------------'
	write(*,*) '  an internal bug was detected in the program'
	write(*,*)
     *  '  please transfer the following informations to the programmer'
	write (*,*)
     * '   along with the conditions in which it happened'
	write(*,*) '     FILE NAME   : ', file
	write(*,*) '     ENTRY       : ', subr
	write(*,*) '     LINE        : ', line
	write(*,*) '     FAILED TEST : ', test
	count = count+1
	if (count.gt.maxcount) then
		write(*,*)	
     *	'Too many assertions, Aborting...'
     	stop
	else
		write(*,*)	
     *	'Proceed with caution, and save your data as soon as possible'
		write(*,*) '---------------------------------------'
	endif
	return
	end
