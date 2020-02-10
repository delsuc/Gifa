C This file is a part of the GIFA program
C     This software has been developped by the NMR Group in GIF/Yvette.
C
C     Authors :       M.A.Delsuc
C                     NMR Laboratory,
C                     ICSN - CNRS
C                     91190 GIF/YVETTE   FRANCE
C
C     This software cannot be used unless the user have been fully 
C licensed to do so form the above laboratory.
C     This file cannot be copied, duplicated or used in any other program,
C without written permission from the authors.
C
      subroutine linecorr(array,size,index,szindex,radius,algo)
c IN	: size,index,szindex,radius,algo
c INOUT	: array
c
C    This subroutine will correct the base line of the 1D data-set in
C array[size], with a linear correction (will add a linear baseline to the data
C set) or a Spline correction. The base line to be removed is evaluated from the
C szindex points whose indexes are in the table index[szindex], with averaging
C around each points within a area of +/-radius wide. 
C   Spline correction supposes that index is ordered in ascending order.
C
C size is at least 3
C szindex is at least 1 (if 1, will remove a horizontal base-line)
C szindex cannot be more than 100.
C radius may be zero or more (less than size!)
C algo is 1 for linear, 2 for spline
      implicit none

      integer sztb
      parameter (sztb = 100)
      integer size, szindex, radius, i, index(szindex),j ,k,algo
      real array(size), mean,  la, lb, table(2,sztb),parm(sztb)
      integer mini,maxi,error
      real valspline, zz
C this common is for speed optimization on our Alliant.
C Permits to put write statement outside the loop
      common /splerror/error

      if (size.lt.3 .or. szindex.gt.sztb .or. radius.ge.size
     *    .or. (algo.eq.1 .and. szindex.lt.1)
     *    .or. (algo.eq.2 .and. szindex.lt.3)
     *    .or. algo.lt.1 .or. algo.gt.2) then
        write(*,*) 'Wrong parameters'
        return
      endif

C compute values in table
      mini = index(1)
      maxi = index(szindex)
      do i=1,szindex 
        mean = 0.0
        k = 0
        do j=max(index(i)-radius,1), min(index(i)+radius,size)
           k = k+1
           mean = mean + array(j)
        enddo
        table(1,i) = float(index(i))
        table(2,i) = mean/k
      enddo

      if (szindex.eq.1) then
C do base correction
         mean = -table(2,1)
         do i=1,size 
           array(i) = array(i)+mean
         enddo
      else if (algo.eq.1) then
C compute linear correction
        call linereg(table,szindex,la,lb)
C  end remove it
         do i=1,size 
           array(i) = array(i) - (la*i + lb)
         enddo
      else if (algo.eq.2) then
C compute spline
         error = 0
         call spline(table,szindex,parm)
C  end remove it
         mini = max(1,mini)
         maxi = min(size,maxi)
         zz = valspline(float(mini),table,szindex,parm)
         do i=1,mini
           array(i) = array(i)-zz
         enddo
         zz = valspline(float(maxi),table,szindex,parm)
         do i=maxi,size
           array(i) = array(i)-zz
         enddo
C this is a pseudo comment for our Alliant
cvd$ cncall
         do i=mini+1,maxi-1
          array(i) = array(i)-valspline(float(i),table,szindex,parm)
         enddo
         if (error.ne.0) 
     *       write(*,*) ' Errors during spline computation'
      endif
10    return
      end



      subroutine linereg(table,size,la,lb)
c IN	: table,size
c OUT	: la,lb
c
c computes the linear approx of the tabulated function "table"
c where table(1,i) is x(i) and table(2,i) is y(i)
c returns the coordinates of the line as y = la*x + lb
      implicit none
      integer size,i
      real table(2,size),la,lb,sumx,sumy,sumxx,sumxy,delta,sum

C compute sums
      sumx = 0.0
      sumxx = 0.0
      sumy = 0.0
      sumxy = 0.0
      do i=1,size
        sumx = sumx + table(1,i)
        sumy = sumy + table(2,i)
        sumxx = sumxx + table(1,i) * table(1,i)
        sumxy = sumxy + table(1,i) * table(2,i)
      enddo
      sum = float(size)
      delta = sum*sumxx - sumx*sumx
      lb = (sumxx*sumy - sumx*sumxy) / delta
      la = (sum*sumxy - sumx*sumy) / delta
      return
      end

      subroutine spline(tb,size,parm)
c IN	: tb,size
c OUT	: parm
c
C given the function tabulated in tb(2,size), this subroutine computes
C the parameters for the spline interpolation of this function.
C tb is such that: f(tb(1,i)) = tb(2,i).
C parameters are returned in parm(sztb)
C supposes that "tb" is ordered in ascending order for "tb(1,i)"
c
      Implicit none
      integer size
      real tb(2,size), parm(size)

      integer sztb
      parameter (sztb = 100)
      integer i
      real u(sztb), sig, p,un,qn

      u(1) = 0.0
      parm(1) = 0.0
      do i=2,size-1
         sig = (tb(1,i) - tb(1,i-1)) /
     *         (tb(1,i+1) - tb(1,i-1))
         p = sig*parm(i-1) + 2
         parm(i) = (sig - 1.0)/p
         u(i) = (6.0*((tb(2,i+1)-tb(2,i)) / 
     *                (tb(1,i+1)-tb(1,i))-(tb(2,i)-tb(2,i-1)) /
     *              (tb(1,i)-tb(1,i-1))) /
     *           (tb(1,i+1)-tb(1,i-1)) - sig*u(i-1)) / p
      enddo
      qn = 0.0
      un = 0.0
      parm(size) = 0.0
      do i=size-1,1,-1
          parm(i) = parm(i)*parm(i+1) + u(i)
      enddo
      return
      end

C the recursive key-word is just for our Alliant parrallele machine
C to parallelize the loop in linecorr.
#alliant      recursive function valspline(x,tb,size,parm)
#_alliant      function valspline(x,tb,size,parm)
c IN	: x,tb,size,parm
c
c computes the value in "x" of the spline function defined by "param"
c attached to the function tabulated in "tb".
c param should have been preset by a call to spline
C supposes that "tb" is ordered in ascending order for "tb(1,i)"
      implicit none
      integer size
      real x, tb(2,size), parm(size), valspline
      integer hi,lo,ii,error
      real h,a,b,y
      common /splerror/error
      lo = 1
      hi = size
2     if ( (hi-lo).gt.1) then
        ii=(hi+lo)/2
        if (tb(1,ii).gt.x) then
          hi = ii
        else if (tb(1,ii).lt.x) then
          lo = ii
        else
          goto 1
        endif
        goto 2
      endif
      h = tb(1,hi) - tb(1,lo)
      if (h.eq.0.0) then
          error = 1
      else
          a = (tb(1,hi)-x)/h
          b = (x-tb(1,lo))/h
          y = a*tb(2,lo) + b*tb(2,hi) +
     *         ((a**3-a)*parm(lo)+(b**3-b)*parm(hi))*(h**2)/6.0
          valspline = y
          return
      endif
1     valspline = tb(2,ii)
      return
      end
