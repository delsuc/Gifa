C This file is a part of the GIFA program
C     This software has been developped by the NMR Group in CBS/Montpellier
C
C     Authors :       T.E.Malliavin
C                     C.B.S. Faculte de Pharmacie
C                     34060 Montpellier   FRANCE
C
C     This software cannot be used unless the user have been fully
C licensed to do so form the above laboratory.
C     This file cannot be copied, duplicated or used in any other program,
C without written permission from the authors.

c**********************************************************
	subroutine dbmassign(dbm_name,array,error)
c
c IN: dbm_name,array
c OUT: error
c
c to open a DBM file
c
c input:
c dbm_name: DBM name
c array: associative array name
c
c output:
c error: error flag
c
	implicit none
#include "variable.inc"
c variables used to store integers in string variables
	character*4 st4
      	integer ist
      	equivalence (ist,st4)
	
	integer error,long_db,context,addr
	character*(*) array,dbm_name
	character*(varnmlen) local
	character*(varcontlen) name,value

	local = "$"//array//"[]"
	call existvar(local,20,addr)
	if (addr.eq.0) then
	   call gifaout('The array ' // local // ' is already assigned')
	   error = 1
	   return
	endif

	call trailing(dbm_name,long_db)
	call dbmopen(dbm_name,long_db,ist,error)
	if (error.ne.0) then
	   call gifaout('***Error during opening DBM file')
	   return
	endif

	name = "%"//array
	context = 20
	value = st4
	call assign(name,context,value,error)
	if (error.ne.0) then
	   call gifaout('***Error with variable assignment')
	   return
	endif

	return
	end
c**********************************************************
	subroutine dbmdealloc(array,error)
c
c IN: array
c OUT: error
c
c to close a DBM file
c
c input:
c array: associative array name
c
c output:
c error: error flag
c
	implicit none
#include "variable.inc"
c variables used to store integers in string variables
        character*4 st4
        integer ist
        equivalence (ist,st4)
	
	integer error,context,addr
	character*(*) array	
	character*(varcontlen) name,value
	
        call dbmcheck(array,addr)
        if (addr.eq.1) then
           call gifaout('***The DBM associative array is not assigned')
           error = 1
           return
        endif
	
	name = "%"//array
	context = 20
	call getvar(name,context,value,error)
	st4 = value(1:4)
	call dbmclose(ist)

	call dealloc(name,context,error)

	return
	end
c**********************************************************
	subroutine dbmput(array,key,value,error)
c
c IN: array,key,value
c OUT: error
c
c to store an element (key,value) in a DBM file
c
c input:
c array: associative array name
c key: database element key
c value: database element value
c
c output:
c error: error flag
c
	implicit none
#include "variable.inc"
c variables used to store integers in string variables
	character*4 st4
      	integer ist
      	equivalence (ist,st4)
	
	integer error,long_key,long_val,context,addr
	character*(*) array,key,value
	character*(varcontlen) name,dbvalue

        call dbmcheck(array,addr)
        if (addr.eq.1) then
           call gifaout('***The DBM associative array is not assigned')
           error = 1
           return
        endif
	
	name = "%"//array
	context = 20
	call getvar(name,context,dbvalue,error)
	if (error.ne.0) then
	   call gifaout('***DBM id not found')
	   return
	endif
	st4 = dbvalue(1:4)

	call trailing(key,long_key)
	call trailing(value,long_val)
	call dbmstore(ist,key,long_key,value,long_val,error)
	if (error.ne.0) then
	   call gifaout('***Error in storing DBM element')
	   return
	endif

	return
	end
c**********************************************************
	subroutine dbmget(array,key,value,error)
c
c IN: array,key
c OUT: error,value
c
c to get an element value from a DBM file
c
c input:
c array: associative array name
c key: database element key
c
c output:
c error: error flag
c value: database element value
c
	implicit none
#include "variable.inc"
c variables used to store integers in string variables
	character*4 st4
      	integer ist
      	equivalence (ist,st4)
	
	integer error,long_key,long_val,context,addr
	character*(*) array,key,value
	character*(varcontlen) name,dbvalue,stloc

        call dbmcheck(array,addr)
        if (addr.eq.1) then
c           call gifaout('***The DBM associative array is not assigned')
           error = 1
           return
        endif
	
	name = "%"//array
	context = 20
	call getvar(name,context,dbvalue,error)
        if (error.ne.0) then
           call gifaout('***DBM id not found')
           return
        endif
	st4 = dbvalue(1:4)

	call trailing(key,long_key)
	call dbmfetch(ist,key,stloc,long_key,long_val,error)
	if (error.eq.0) then
	   value = stloc(1:long_val)
	else
           call gifaout('***Error in getting DBM element')
           return
        endif

	return
	end
c**********************************************************
	subroutine dbmdel(array,key,error)
c
c IN: array,key
c OUT: error
c
c to delete an element in a DBM file
c
c input:
c array: associative array name
c key: database element key
c
c output:
c error: error flag
c
	implicit none
#include "variable.inc"
c variables used to store integers in string variables
	character*4 st4
      	integer ist
      	equivalence (ist,st4)
	
	integer error,long_key,context,addr
	character*(*) array,key
	character*(varcontlen) name,dbvalue

        call dbmcheck(array,addr)
        if (addr.eq.1) then
           call gifaout('***The DBM associative array is not assigned')
           error = 1
           return
        endif

	name = "%"//array
	context = 20
	call getvar(name,context,dbvalue,error)
        if (error.ne.0) then
           call gifaout('***DBM id not found')
           return
        endif
	st4 = dbvalue(1:4)

	call trailing(key,long_key)
	call dbmrem(ist,key,long_key,error)
        if (error.ne.0) then
           call gifaout('***Error in deleting a DBM element')
           return
        endif

	return
	end
c**********************************************************
	subroutine dbmnext(array,next_key,error)
c
c IN: array
c OUT: error,next_key
c
c to find the next element key in a DBM file
c
c input:
c array: associative array name
c
c output:
c error: error flag
c next_key: next database element key
c
	implicit none
#include "variable.inc"
c variables used to store integers in string variables
	character*4 st4
      	integer ist
      	equivalence (ist,st4)
	
	integer error,long_next_key,context,end,addr
	character*(*) array,next_key
	character*(varcontlen) name,dbvalue,stloc

        call dbmcheck(array,addr)
        if (addr.eq.1) then
           call gifaout('***The DBM associative array is not assigned')
           error = 1
           return
        endif
	
	name = "%"//array
	context = 20
	call getvar(name,context,dbvalue,error)
        if (error.ne.0) then
           call gifaout('***DBM id not found')
           return
        endif
	st4 = dbvalue(1:4)

        end = 0
	call dbmnxt(ist,stloc,long_next_key,error,end)
	if (end.eq.0) then      
c the pointeur is not at the DBM end
	   next_key = stloc(1:long_next_key)
	elseif (end.ne.0) then  
c the pointer is at the DBM end
           next_key = ' '
        endif

	return
	end
c**********************************************************
	subroutine dbmfirst(array,first_key,error)
c
c IN: array
c OUT: error,first_key
c
c to find the first element key in a DBM file
c
c input:
c array: associative array name
c
c output:
c error: error flag
c first_key: first database element key
c
	implicit none
#include "variable.inc"
c variables used to store integers in string variables
	character*4 st4
      	integer ist
      	equivalence (ist,st4)
	
	integer error,long_first_key,context,addr
	character*(*) array,first_key
	character*(varcontlen) name,dbvalue,stloc

        call dbmcheck(array,addr)
        if (addr.eq.1) then
           call gifaout('***The DBM associative array is not assigned')
           error = 1
           return
        endif

	name = "%"//array
	context = 20
	call getvar(name,context,dbvalue,error)
        if (error.ne.0) then
           call gifaout('***DBM id not found')
           return
        endif
	st4 = dbvalue(1:4)

	call dbmfrst(ist,stloc,long_first_key,error)
	if (error.eq.0) then
	   first_key = stloc(1:long_first_key)
        else
           call gifaout('***Error in getting DBM first element')
           return
        endif

	return
	end
c**********************************************************
	subroutine pickkey(array,key,vname,error)
c
c IN: vname
c OUT: error,array,key
c
c to get the key and array name from an array element
c
c input:
c vname: array element variable
c
c output:
c array: associative array name
c key: database element key
c error: error flag
c
	implicit none
#include "variable.inc"
	
	integer error,i,j
	character*(varnmlen) array,key,vname

        i = index(vname,'[')
        if (i.eq.0) then
           error = 1
           return
        endif
        j = index(vname,']')
        array = vname(2:i-1)        ! 2 parce que le nom de la var commence par $
        key = vname(i+1:j-1)

	return
	end
c**********************************************************
        subroutine dbmcheck(array,error)
c
c IN: array
c OUT: error
c
c to check if the array name correspond to a DBM file
c
c input:
c array: associative array name
c
c output:
c error: flag = 0 if found = 1 if not found
c
        implicit none

#include "variable.inc"
c variables used to store integers in string variables

        integer error,context
        character*(*) array
        character*(varcontlen) name

        context = 20
        name = '%' // array
        call checkvar(name,context,error)

	return
	end
c**********************************************************
	subroutine dbmexist(array,key,error)
c
c IN: array,key
c OUT: error
c
c to determines whether the key key exist in a DBM file
c
c input:
c array: associative array name
c key: database element key
c
c output:
c error: error flag
c
	implicit none
#include "variable.inc"
c variables used to store integers in string variables
	character*4 st4
      	integer ist
      	equivalence (ist,st4)
	
	integer error,long_key,long_val,context,addr
	character*(*) array,key
	character*(varcontlen) name,dbvalue,stloc

        call dbmcheck(array,addr)
        if (addr.eq.1) then
c           call gifaout('***The DBM associative array is not assigned')
           error = 1
           return
        endif
	
	name = "%"//array
	context = 20
	call getvar(name,context,dbvalue,error)
        if (error.ne.0) then
c           call gifaout('***DBM id not found')
           return
        endif
	st4 = dbvalue(1:4)

	call trailing(key,long_key)
	call dbmfetch(ist,key,stloc,long_key,long_val,error)

	return
	end








