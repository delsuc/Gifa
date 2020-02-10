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
C

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      function getmaskno(crdx,crdy)
c
c returns the peak number of the amoeba to which the pixel belongs.
c
c IN	: crdx,crdy
c
      implicit none

#include "sizebase.inc"
#include "peaksize.inc"
#include "ambv.inc"

      integer crdx,crdy,dx,dy,i,check
      integer getmaskno

      getmaskno = 0
      do i = 1,nbpic2d
         dx = abs(int(peak2d(i,1))-crdx)
         dy = abs(int(peak2d(i,4))-crdy)
         if ( ((dx.le.maxamb).or.(dy.le.maxamb)) ) then
c check if the peak i has an amoeba
            check = 0
            call check_amb(i,check)
            if (check.ne.0) then
c check if the pixel belongs to the peak i 
              call check_pix_amb(i,crdx,crdy,check)
c if it is the case, it is finished
              if (check.ne.0) then
                getmaskno = i
                return
              endif
            endif
         endif
      enddo

      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine setpixmask(crdx,crdy,nopk,error)
c
c create the amoeba of peak nopk if it does not exist, and 
c put the pixel (crdx,crdy) into this amoeba.
c IN	: crdx,crdy,nopk
c OUT	: amoeba
c
      implicit none

#include "sizebase.inc"
#include "peaksize.inc"
#include "ambv.inc"

      integer crdx,crdy,nopk,error,check,xx,yy

c if the pixel has the same coordinate than the peak, 
c it is not possible to add it to the amoeba
      if ( (crdx.eq.peak2d(nopk,1)).and.(crdy.eq.peak2d(nopk,4)) ) then
         error = 1
         call gifaouti('***The peak coordinates are automatically added 
     & to the amoeba',nopk)
	 return
      endif
c check if the peak nopk has an amoeba
      check = 0
      call check_amb(nopk,check)
      if (check.eq.0) then
c if peak nopk has no amoeba, create it
         xx = int(peak2d(nopk,1))
         yy = int(peak2d(nopk,4))
         call ini_amb(nopk,xx, yy,error)
	 if (error.ne.0) then
	   call gifaouti('***Error during creating mask of peak',nopk)
	   return
	 endif
      endif
c check that the pixel does not already belongs to the amoeba
      check = 0
      call check_pix_amb(nopk,crdx,crdy,check)
      if (check.ne.0) then
	 call gifaouti('***The pixel already belongs to the 
     & amoeba of ',nopk)
	 return
      endif

c add the pixel to the peak amoeba
      call put_pix_amb(nopk,crdx,crdy,error)
      if (error.ne.0) then
         call gifaouti('***Error during putting a new pixel in the mask 
     & of peak',nopk)
	 return
      endif

      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine rempixmask(crdx,crdy,nopk,error)
c
c remove the pixel (crdx,crdy) from the amoeba of peak nopk
c Be careful: before using this subroutine, you must check that 
c the pixel belongs to the amoeba 
c
c IN	: crdx,crdy,nopk
c OUT	: amoeba
c
      implicit none

#include "sizebase.inc"
#include "peaksize.inc"
#include "ambv.inc"

      integer crdx,crdy,nopk,error,check

c if the pixel has the same coordinate than the peak, 
c it is not possible to remove it from the amoeba
      if ( (crdx.eq.peak2d(nopk,1)).and.(crdx.eq.peak2d(nopk,4)) ) then
         error = 1
         call gifaouti('***Impossible to remove the peak coordinates  
     & from amoeba',nopk)
	 return
      endif
c check if the peak nopk has an amoeba
      check = 0
      call check_amb(nopk,check)
      if (check.eq.0) then
c if the peak nopk has no amoeba, return an error 
         error = 1
         call gifaouti('***No amoeba for the peak',nopk)
	 return
      endif
c if the pixel belongs to the amoeba, remove it 
      call rem_pix_amb(nopk,crdx,crdy,error)

      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine delmask(nopk)
c
c delete the mask of the peak nopk
c
c IN	: nopk
c OUT	: amoeba
c
      implicit none

#include "sizebase.inc"
#include "peaksize.inc"
#include "ambv.inc"

      integer nopk,check

c check if the peak nopk has an amoeba
      check = 0
      call check_amb(nopk,check)
      if (check.eq.0) then
c if peak nopk has no amoeba, error
	call gifaouti('***No existing amoeba for peak ',nopk)
      else
        call del_amb(nopk)
      endif

      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine calcmask(nopk,tab,taille1,taille2,noise,shift,signpic)
c
c calculate the integral and the error from the mask of the peak nopk
c
c IN	: tab,nopk,taille1,taille2,noise,shift,signpic
c OUT	: amoeba
c
      implicit none

#include "sizebase.inc"
#include "peaksize.inc"
#include "ambv.inc"

      integer nopk,total,taille1,taille2,signpic
      integer ipkx,ipky,check
      real sum,tab(taille2,taille1),shift,noise

c initialize the intensity and sum values
      sum = 0.0
      total = 0      
      ipkx = int(peak2d(nopk,1))
      ipky = int(peak2d(nopk,4))
c check if the peak nopk has an amoeba
      check = 0 
      call check_amb(nopk,check)
      if (check.eq.0) then 
        sum = tab(ipkx,ipky)
        total = 1
        goto 100
      endif
      call sum_amb(nopk,total,sum,tab,taille1,taille2)

 100  continue
      peak2d(nopk,3) = sum
      peak2d(nopk,6) = real(total)
      if (peak2d(nopk,3).ne.0.0) then
         peak2d(nopk,3) = peak2d(nopk,3)-(signpic*peak2d(nopk,6)*shift)
         peak2d(nopk,6) = 
     & sqrt(peak2d(nopk,6))*noise*100/abs(peak2d(nopk,3))
      else
         peak2d(nopk,6) = -1000.0
      endif

      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine storemask(name,error)
c
c store all the peak amoeba to a file
c
c IN	: amoeba
c OUT   : error
c
      implicit none

#include "sizebase.inc"
#include "peaksize.inc"
#include "paramfilev.inc"
#include "unitextern.inc"
#include "controlc.inc"
#include "ambv.inc"

      character*256   fichier,name
      integer i,k,nbpixel,crdx,crdy,error,check,ww

      data fichier/'noname'/

      if (name.eq.' ') then
         write(*,20)
         call getstring(fichier)
20       format (' filename: ')
      else
         fichier = name
      endif

      ww = 10
      open(unit=ww,file=fichier,iostat=error,
     *     status='new',form='formatted')
      if (error.ne.0) then
          call gifaout('*** Unable to open the file! ' // fichier)
          return
      endif
      
      do i = 1,nbpic2d
c check if the peak nopk has an amoeba
         check = 0
         call check_amb(i,check)
         if (check.eq.0) then
c no amoeba -> write the peak chemical shifts
            write(ww,*) itopr(peak2d(i,1),si2im,specw2,
     *                          offset2,freq2),
     *      peak2d(i,2),peak2d(i,3),
     *      itopr(peak2d(i,4),si1im,specw1,offset1,freq1),peak2d(i,5)
         else
c store the peak amoeba
            call get_amb(i,liste,nbpixel)
c write the peak chemical shifts and the pixels of amoeba 
            write(ww,*) itopr(peak2d(i,1),si2im,specw2,
     *                          offset2,freq2),
     *      peak2d(i,2),peak2d(i,3),
     *      itopr(peak2d(i,4),si1im,specw1,offset1,freq1),peak2d(i,5), 
     *      nbpixel,(liste(k+1),k=1,2*nbpixel)
c     *      '         ',nbpixel,(liste(k+1),k=1,2*nbpixel)
            if (control.eq.1) goto 40
         endif
      enddo

c 100  format(14(i4,x))
      
 40   continue
      close(10)

      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine readmask(name,tab,taille1,taille2,error)

c read all the peak amoeba from a file
c
c IN	: amoeba
c OUT   : error
c
      implicit none

#include "sizebase.inc"
#include "peaksize.inc"
#include "paramfilev.inc"
#include "controlc.inc"
#include "unitextern.inc"
#include "ambv.inc"

      character*256   fichier,name,line
      integer nbpixel,error,i,xx,szpix,iostat
      integer taille1,taille2,check,temp(14),ww,fin
      real zz1,zz2,zz3,zz4,zz5
      real tab(taille1,taille2)

      data fichier/'noname'/

      if (name.eq.' ') then
         write(*,20)
         call getstring(fichier)
20       format (' filename: ')
      else
         fichier = name
      endif

c du passe, faisons table rase....
      do i = 1,nbpic2d
c check if the peak nopk has an amoeba
         check = 0
         call check_amb(i,check)
         if (check.ne.0) then 
           call del_amb(i)
         endif
      enddo
c clear peak table
      nbpic2d = 0

      open(unit=10,file=fichier,iostat=error,
     *     status='old',form='formatted')
      if (error.ne.0) then
          call gifaout('*** Unable to open the file! ' // fichier)
          return
      endif

      ww = 10
      xx = 0      
 101  continue
       read(ww,100,iostat=error,end=40)line
c       read(line,*,iostat=error)zz1,zz2,zz3,zz4,zz5,'         ',
       read(line,*,iostat=error)zz1,zz2,zz3,zz4,zz5,
     *    nbpixel,(temp(i),i=1,2*min(nbpixel,7))
c read the peak
       zz1 = ptoir(zz1,si2im,specw2,offset2,freq2)
       zz4 = ptoir(zz4,si1im,specw1,offset1,freq1)
       if (zz1.ge.1 .and. zz1.le.si2im .and.
     *            zz4.ge.1 .and. zz4.le.si1im) then
         xx = xx+1
         if (xx.gt.peakmax) then
           call gifaout('*** Too many peaks!')
           error = 1
           return
         endif
         peak2d(xx,1) = zz1
         peak2d(xx,2) = zz2
         peak2d(xx,3) = zz3
         peak2d(xx,4) = zz4
         peak2d(xx,5) = zz5
       else 
         call gifaout('*** Peak outside of the spectrum !')
         error = 1
         return
       endif
       if (nbpixel.gt.0) then
c read amoeba
         szpix = nbpixel
         call copvecti(liste(1),temp,14)
         fin = 14
         szpix = szpix - 7
         do while (szpix.gt.7)
           read(ww,100,iostat=error,end=40)line
           read(line,*,iostat=error)(liste(i),i=1,14)      
           call copvecti(liste(fin+1),temp,14)
           fin = fin + 14
           szpix = szpix - 7
         enddo
         read(ww,100,iostat=error,end=40)line
         read(line,*,iostat=error)(liste(i),i=1,2*szpix)     
         call copvecti(liste(fin+1),temp,2*szpix)
         call put_amb(xx,liste,nbpixel,error)
       endif
      goto 101

 40   continue
 100  format(a80)
      close(10)

      return
      end

