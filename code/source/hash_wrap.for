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

C Fortran wrapper for the hash_pk library

c      subroutine hash_code(var, context, dim, err)

c realizes the hashing code
c   var/context is the name of the array to code
c      var is given as : ARRAY for  $ARRAY[ENTRY]
c      can be user Gifa variable array or dbm array
c   dim (= 1, 2 or 3) is the number of hashing keys
c   err will be non zero on return if an error occured



C================================================================
c      subroutine hash_dim(stp, context, xx, error)
c check wether var/context is currently hashed, and if so, returns the 
c   the current dim.
c   err will non zero on return if an error occured or if the var is not hashed


C================================================================
      subroutine hash_find(var, context, dim, zz1, zz2, zz3,
     *                   id, bestdist, error)
c  returns in id the peak hashed in var/context
c   which is matched by coordinates z1 ( z2 ( z3 ) )
c   err will non zero on return if an error occured :

c version temporaire, calcul sans table de hash

      implicit none
#include "variable.inc"
      character*(varnmlen) var, id
      integer context, dim, error
      real zz1, zz2, zz3, bestdist

      integer len, addr, i, j, ier
      logical dbm
      character*(varnmlen) tt, entry, key, array, bestkey
      character*(varcontlen) value
      real dist

c first determines which array it is
      call trailing(var,len)

      array = "%"//var         ! first search for dbm (faster)
      call find_var(array,context,addr)
      if (addr .ne. 0) then
      	dbm = .true.
      else                     ! now, scan whole variable table for an entry 
           array = "$"//var(1:len)//"["
           call uppercase(array,len+2)
           do i=1,varmax
                entry = varname(i)
                if (entry(1:len+2) .eq. array) then   ! "$var["
                   dbm = .false.
                   goto 100
                endif
           enddo
           error=1
           return   ! not found
      endif

C found one !
100   continue

C now do the coding
      bestdist = -1.0
      bestkey = " "

c if dbm
      if (dbm) then
         call dbmfirst(var,key,error)
         if (error.ne.0) return
         do while(key.ne.' ')
            call dbmget(var,key,value,error)
            if (error.ne.0) return
            call hdist(value,dim,zz1,zz2,zz3,dist,ier)
            if (ier.eq.0) then
                 if (bestdist .eq. -1 .or. bestdist .gt. dist) then
                    bestdist = dist
                    bestkey = key
                    if (dist .eq. 0.0) goto 200
                 endif
            endif
            call dbmnext(var,key,error)
            if (error.ne.0) return
         enddo


c if user var
      else
        do j = i, varmax
          entry = varname(j)
          if (entry(1:len+2) .eq. array) then   ! "$var["
            ier = 0
            call pickkey(tt, key, entry, ier)
            if (ier.eq.0) then	! found one
              value = varval(j)
              call hdist(value,dim,zz1,zz2,zz3,dist,ier)
              if (ier.eq.0) then
                 if (dist .ne. -1 .and. bestdist .gt. dist) then
                    bestdist = dist
                    bestkey = key
                    if (dist .eq. 0.0) goto 200
                 endif
              endif
            endif
          endif
        enddo
      endif

 200  if (bestdist .ne. -1) then
         id = bestkey
      else
         id = ' '
      endif
      return
      end

      subroutine hdist(value,dim,zz1,zz2,zz3,dist,err)
C IN :  value,di,zz1,zz2,zz3
C OUT : dist,err
C
C computes the distance between zz1,zz2,zz3
C and the (dim) coordinates coded in string st
c
      implicit none

      character*(*) value
      integer dim, err
      real zz1,zz2,zz3,dist
      real*8 x, z(3)

      integer ii, k
 
      z(1) = zz1
      z(2) = zz2
      z(3) = zz3
      dist = 0.0
      do k=1,dim
         call leading(value)		! remove leading blanks
         ii = index(value,' ')
         call readval(value(1:ii),X,err) ! read 1st entry
         if (err.ne.0) GOTO 200
         dist = dist + (x-z(k))**2
         value = value(ii+1:)
      enddo


      dist = sqrt(dist)
      return

200   dist = -1    ! flags non numeric entries
      return
      end

