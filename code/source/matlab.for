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

ccccccccccccccccccccccccccccccccccccccccccccc
	subroutine save_matlab(file_nm, matrix, line, col, error)
c IN	: file_nm, matrix, line, col,
c OUT	: error
c
c write matrix(col, line) as a matlab compatible file

	implicit none
	character*(80) file_nm
	integer line, col, error, i, j
	real matrix(col,line)
        character*(20) form

#assert (line.gt.0 .and. col.gt.0)
        i = 0
	open(10, file=file_nm, form='formatted',
     *       status='new', iostat=error)
	if (error.ne.0) goto 999
        write(form,'(A1,I2,A6)') '(',col,'E16.8)'
c       write(*,*) form
	do i=1, line
c		write(10, col, iostat=error) (matrix(j,i), j=1, col)
		write(10, '(1000E16.8)', iostat=error)
     *                   (matrix(j,i), j=1, col)
		if (error.ne.0) goto 999
	enddo
	close(10, iostat=error)
	if (error.ne.0) goto 999
	return

999	write (*,*) 'Error at line ',i,' in file ',file_nm
	error = 1
	return
	end

ccccccccccccccccccccccccccccccccccccccccccccc
	subroutine load_matlab(file_nm, sizemax,
     *                         matrix, line, col, error)
c IN	: file_nm, sizemax
c OUT	: error,  matrix, line, col,
c
c read matrix(col, line) from a matlab compatible file
c
c typical use is through recombinant array
c eg :
c	integer sizemax, line, col, error
c	parameter (sizemax=some large number)
c	real matrix(sizemax)
c	...
c	file_nm = 'my_file.dat'
c	call load_matlab(file_nm, sizemax, matrix, line, col, error)
c	if (error.ne.0) you should do something
c	call work(matrix, line, col, other param ...)
c	....
c and work is such :
c	subroutine work(matrix, line, col, other param ...)
c	integer line, col
c	real work(col,line)
c	... do your work here
c
c
	implicit none
	character*(80) file_nm
	integer sizemax, linemax, line, col, error, i, j, ib
#_aix	parameter (linemax=16020) 	! this is 1000 values max
#aix	parameter (linemax=500) 	! this is 30 values max
	real matrix(sizemax)
	character*(linemax) ll
#f2c        save ll       ! needed for C array

#aix	write(*,*) 'Limited to 30 entries on IBM/AIX version !'
	i = 0
	open(10, file=file_nm, form='formatted',
     *          status='old', iostat=error)
	if (error.ne.0) goto 999
c determine col ...
	ll = ' '		! skip empty lines
	do while(ll.eq.' ')
		read(10, '(A)', iostat=error) ll
		if (error.ne.0) goto 999
	enddo
	col = 0
	ib=linemax		! scan starting at the end
c#d 	write (*,*) ll
	do while(ib.ge.1)
		if (ll(ib:ib) .ne. ' ') then
			col = col+1	! found one
			do while(ll(ib:ib).ne.' ' .and. ib.gt.1)
				ib = ib-1
			enddo
		else
			ib = ib-1
		endif
	enddo
#assert (col.gt.0)
	rewind(10, iostat=error)
	if (error.ne.0) goto 999

	i = 0
	do while( .true. )
		if (i*col .gt. sizemax) then
			error = 2
			goto 999
		endif
		read(10, *, iostat=error, end=100)
     *               (matrix((i)*col +j), j=1, col)
		if (error.ne.0) goto 999
		i = i+1
	enddo
100	line = i
	error = 0
	close(10, iostat=error)
	if (error.ne.0) goto 999
	error = 0
	return
	
999	write (*,*) 'Error at line ',i,' in file ',file_nm
	error = 1
	return
	end

