C peakintp.inc
C
C this file contains the peak integration commands
C (PARIS)
      elseif (command.eq.'PEAK') then
         if (dim.eq.1) then
           if (it1d.ne.0) go to 96
           nbpic1d=0
           call stockpoint(column,sizeimage1d,mini,maxi,nbpic1d,
     *                    signpic,zoom,zo1dl,zo1du)
           call gifaouti('Number of peak detected :',nbpic1d)
           do i=1,nbpic1d
              peak1d(i,5) = 0.0 	! mark unknown
              peak1d(i,6) = 0.0
              peak1d(i,7) = 0.0
              peak1d(i,8) = 0.0
              write(peak1d_id(i),'(i12)') i
              call leading(peak1d_id(i))
           enddo 
           porig1d = 1
         elseif (dim.eq.2) then
           if (itype.ne.0) go to 96
c ?????
c           if (sizeimage .gt.smxmax) goto 103
           call message( 'Enter packing radius:  ')
           pkradius=0
           call getint2(pkradius,error)
           if (error.ne.0) goto 93
           call stockpic(plane2d,si1im,si2im,mini,maxi,nbpic2d,
     *                   signpic,zq,zoom,zolf1,zolf2,zour1,zour2)
           if (pkradius.gt.0) call picmaxlink(pkradius,nbpic2d,1)
           call gifaouti('Number of peak detected :',nbpic2d)
           do i=1,nbpic2d
              peak2d(i,6) = 0.0
              peak2d(i,7) = 0.0
              peak2d(i,8) = 0.0
              peak2d(i,9) = 0.0
              peak2d(i,10) = 0.0
              peak2d(i,11) = 0.0
              write(peak2d_id(i),'(i12)') i
              call leading(peak2d_id(i))
           enddo 
           porig2d = 1
         elseif (dim.eq.3) then
           if (it3d.ne.0) go to 96
c           if (sizeimage .gt.smxmax) goto 103
           call message( 'Enter packing radius:  ')
           pkradius=0
           call getint2(pkradius,error)
           if (error.ne.0) goto 93
           nbpic3d = 0
           if (zoom.eq.0) then
             win1 = 1
             win2 = si3d1
           else
             win1 = zo3df1l
             win2 = zo3df1u
           endif
           nbpic3d = 0
           do i = win1,win2
              call stockpic(image(1+(i-1)*si3d2*si3d3),
     *                si3d2,si3d3,mini,maxi,zz,
     *                signpic,zq,zoom,zo3df2l,zo3df3l,zo3df2u,zo3df3u)
              call copypic3d(zz,i,signpic,zq)
           enddo
           if (pkradius.gt.0) call picmaxlink3d(pkradius,nbpic3d,1)
           call gifaouti('Number of peak detected :',nbpic3d)
           porig3d = 1
           nbpic2d = 0
           do i=1,nbpic3d
              write(peak3d_id(i),'(i12)') i
              call leading(peak3d_id(i))
           enddo
         endif

      elseif (command.eq.'PKFILTER') then
c PKFILTER pkradius action distance
c
c search the peak table for peaks less than pkradius apart.
c then applies action of the found peaks
c
c possible actions are :
c    Remove  : remove the small peak and keep only the larger one
c    Link    : keep the small peak but mark it as 'linked' to the larger one
c    Average : remove all peaks and create a new one with coordinates at the barycenter of
c              the found peaks, and intensity equal to the sum
c
c possible distances are :
c   1 : dist = abs(x) + abs(y)  => square
c   2 : dist = sqrt(x^2 + y^2)  => circle
c   0 : dist = x+y              => diamond
c
c remarks
c -  when the pkradius option is used with the PEAK command, it is equivalent
c    to the  PKFILTER pkradius Link 1
c -  if peak A is close to B and B is close to C A B and C will be considered together,
c    even if A is not close to C (and recursively for larger sets of course)
c   
c Applies only to 2D and 3D peak tables
c
c see also : PEAK, PKSYM, PKSELECT
c
c related contexts : $PKRADIUS
c
c RESTE A FAIRE : ../test et debug
         call message( 'Enter packing radius:')
         call getint2(pkradius,error)
         if (error.ne.0) goto 93
         call message( 'Enter action (link - remove - average)')
         st = 'link'
         call getenum2(st,'link remove average',error)
         if (error.ne.0) goto 93
         call uppercase(st,256)
         call message(
     * 'Enter used distance (0 : diamond; 1 : square; 2 : circle)')
         zz = 1
         call getint2(zz,error)
         if (error.ne.0) goto 93
         if (zz.lt.0 .or. zz.gt.2) goto 98

         if (dim.eq.1) then
           goto 124
         elseif (dim.eq.2) then
           if (st .eq. 'LINK') then
             call picmaxlink(pkradius,nbpic2d,zz)
           elseif (st .eq. 'REMOVE') then
             call picmaxrem(pkradius,nbpic2d,zz)
             if (verbose.gt.1) then
               call gifaouti('New number of peaks :',nbpic2d)
             endif
           elseif (st .eq. 'AVERAGE') then
             call picmaxave(pkradius,nbpic2d,zz)
           endif
         elseif (dim.eq.3) then
           if (st .eq. 'LINK') then
             call picmaxlink3d(pkradius,nbpic3d,zz)
           elseif (st .eq. 'REMOVE') then
             call picmaxrem3d(pkradius,nbpic3d,zz)
             if (verbose.gt.1) then
               call gifaouti('New number of peaks :',nbpic3d)
             endif
           elseif (st .eq. 'AVERAGE') then
             call picmaxave3d(pkradius,nbpic3d,zz)
           endif
         endif

      elseif (command.eq.'FIND') then
        call message("Enter name of coding variable")
        call getstring2(stp,error)
        if (error.ne.0) goto 93
        call message("Enter number of coordinates")
        call getint2(xx,error)
        if (error.ne.0) goto 93
	if (xx .lt. 1 .or. xx.gt.3) goto 98
        write (st,*) 'Enter ',xx,' coordinates'
        call message(st)
        call getreal2(zz1,error)
        if (error.ne.0) goto 93
        if (xx.gt.1) call getreal2(zz2,error)
        if (error.ne.0) goto 93
        if (xx.gt.2) call getreal2(zz3,error)
        if (error.ne.0) goto 93
        call hash_find(stp,vcontext,xx,zz1,zz2,zz3,
     *                 found_pk,found_dist,error)
        if (error.ne.0) goto 90

      elseif (command.eq.'SETPEAKID') then
c SETPEAKID peakindex 'Id string'
c
c this commands sets the PEAK_ID field in the current peak table (1D, 2D or 3D)
c the peak_id field is a string with pkidmax (32) char max

        if (dim.eq.1) then
          i = nbpic1d
        elseif (dim.eq.2) then
          i = nbpic2d
        else
          i = nbpic3d
        endif
        if (i.le.0) goto 111
        write (st,*) 'Enter peak index, in the range 1:',i
        call message(st)
        j = 1
        call getint2(j,error)
        if (error.ne.0) goto 93
        if (j.lt.1 .or. j.gt.i) goto 98
        if (dim.eq.1) then
           stp = peak1d_id(j)
        elseif (dim.eq.2) then
           stp = peak2d_id(j)
        else
           stp = peak3d_id(j)
        endif
        call message('Enter peak ID')
        call getstring2(stp,error)
        call trailing(stp,i)
        i = min(i,pkidmax)
        if (error.ne.0) goto 93
        if (dim.eq.1) then
           peak1d_id(j) = stp(1:i)
        elseif (dim.eq.2) then
           peak2d_id(j) = stp(1:i)
        else
           peak3d_id(j) = stp(1:i)
        endif

      elseif (command.eq.'SETPEAK') then
        if (dim.eq.1) then
          i = nbpic1d
        elseif (dim.eq.2) then
          i = nbpic2d
        else
          i = nbpic3d
        endif
        if (i.le.0) goto 111
        write (st,*) 'Enter peak index, in the range 1:',i
        call message(st)
        j = 1
        call getint2(j,error)
        if (error.ne.0) goto 93
        if (j.lt.1 .or. j.gt.i) goto 98
        if (dim.eq.1) then
            call message('Peak amplitude')
            zz1 = peak1d(j,3)
            call getreal2(zz1,error)
            if (error.ne.0) goto 93
            peak1d(j,3) = zz1
            call message('Peak coordinate (in index)')
            zz1 = peak1d(j,1)
            call getreal2(zz1,error)
            if (error.ne.0) goto 93
            if (zz1.lt.1.0 .or. zz1.gt.sizeimage1d) 
     *   call gifaouti('Warning, outside data-set limits, peak :',j)
            peak1d(j,1) = zz1
            call message('Peak width (in index)')
            zz1 = peak1d(j,2)
            call getreal2(zz1,error)
            if (error.ne.0) goto 93
            if (zz1.lt.0.0 ) goto 98
            peak1d(j,2) = zz1
            peak1d(j,4) = 0.0
            peak1d(j,5) = 0.0
            peak1d(j,6) = 0.0
            peak1d(j,7) = 0.0
            peak1d(j,8) = 0.0
        elseif (dim.eq.2) then
            call message('Peak amplitude')
            zz1 = peak2d(j,3)
            call getreal2(zz1,error)
            if (error.ne.0) goto 93
            peak2d(j,3) = zz1
            call message('Peak F1 coordinates (in index)')
            zz1 = peak2d(j,4)
            call getreal2(zz1,error)
            if (error.ne.0) goto 93
            if (zz1.lt.1.0 .or. zz1.gt.si1im)
     *    call gifaouti('Warning, outside data-set limits, peak :',j)
            peak2d(j,4) = zz1
            call message('Peak F1 width (in index)')
            zz1 = peak2d(j,5)
            call getreal2(zz1,error)
            if (error.ne.0) goto 93
            if (zz1.lt.0.0 ) goto 98
            peak2d(j,5) = zz1
            call message('Peak F2 coordinates (in index)')
            zz1 = peak2d(j,1)
            call getreal2(zz1,error)
            if (error.ne.0) goto 93
            if (zz1.lt.1.0 .or. zz1.gt.si2im)
     *    call gifaouti('Warning, outside data-set limits, peak :',j)
            peak2d(j,1) = zz1
            call message('Peak F2 width (in index)')
            zz1 = peak2d(j,2)
            call getreal2(zz1,error)
            if (error.ne.0) goto 93
            if (zz1.lt.0.0 ) goto 98
            peak2d(j,2) = zz1
            peak2d(j,6) = 0.0
            peak2d(j,7) = 0.0
            peak2d(j,8) = 0.0
            peak2d(j,9) = 0.0
            peak2d(j,10) = 0.0
            peak2d(j,11) = 0.0
        else      ! dim 3
            call message('Peak amplitude')
            zz1 = peak3d(j,3)
            call getreal2(zz1,error)
            if (error.ne.0) goto 93
            peak3d(j,3) = zz1
            call message('Peak F1 coordinates (in index)')
            zz1 = peak3d(j,4)
            call getreal2(zz1,error)
            if (error.ne.0) goto 93
            if (zz1.lt.1.0 .or. zz1.gt.si3d1)
     *   call gifaouti('Warning, outside data-set limits, peak :',j)
            peak3d(j,4) = zz1
            call message('Peak F1 width (in index)')
            zz1 = peak3d(j,5)
            call getreal2(zz1,error)
            if (error.ne.0) goto 93
            if (zz1.lt.0.0 ) goto 98
            peak3d(j,5) = zz1
            call message('Peak F2 coordinates (in index)')
            zz1 = peak3d(j,6)
            call getreal2(zz1,error)
            if (error.ne.0) goto 93
            if (zz1.lt.1.0 .or. zz1.gt.si3d2)
     *   call gifaouti('Warning, outside data-set limits, peak :',j)
            peak3d(j,6) = zz1
            call message('Peak F2 width (in index)')
            zz1 = peak3d(j,7)
            call getreal2(zz1,error)
            if (error.ne.0) goto 93
            if (zz1.lt.0.0 ) goto 98
            peak3d(j,7) = zz1
            call message('Peak F3 coordinates (in index)')
            zz1 = peak3d(j,1)
            call getreal2(zz1,error)
            if (error.ne.0) goto 93
            if (zz1.lt.1.0 .or. zz1.gt.si3d3)
     *    call gifaouti('Warning, outside data-set limits, peak :',j)
            peak3d(j,1) = zz1
            call message('Peak F2 width (in index)')
            zz1 = peak3d(j,2)
            call getreal2(zz1,error)
            if (error.ne.0) goto 93
            if (zz1.lt.0.0 ) goto 98
            peak3d(j,2) = zz1
        endif

      elseif (command.eq.'SETPEAK2') then    ! this is the old form.
        if (dim.eq.1) then
          i = nbpic1d
        elseif (dim.eq.2) then
          i = nbpic2d
        else
          i = nbpic3d
        endif
        if (i.le.0) goto 111
        write (st,*) 'Enter peak index, in the range 1:',i
        call message(st)
        j = 1
        call getint2(j,error)
        if (error.ne.0) goto 93
        if (j.lt.1 .or. j.gt.i) goto 98
        if (dim.eq.1) then
            call message('Peak coordinate (in index)')
            zz1 = peak1d(j,1)
            call getreal2(zz1,error)
            if (error.ne.0) goto 93
            if (zz1.lt.1.0 .or. zz1.gt.sizeimage1d) goto 108
            peak1d(j,1) = zz1
            peak1d(j,2) = 3.0
            peak1d(j,3) = column(int(zz1))
            peak1d(j,4) = 0.0
            peak1d(j,5) = 0.0
            peak1d(j,6) = 0.0
            peak1d(j,7) = 0.0
            peak1d(j,8) = 0.0
        elseif (dim.eq.2) then
            call message('Peak coordinates F1 - F2 (in index)')
            zz1 = peak2d(j,4)
            call getreal2(zz1,error)
            if (error.ne.0) goto 93
            if (zz1.lt.1.0 .or. zz1.gt.si1im) goto 108
            zz2 = peak2d(j,1)
            call getreal2(zz2,error)
            if (error.ne.0) goto 93
            if (zz2.lt.1.0 .or. zz2.gt.si2im) goto 108
            peak2d(j,1) = zz2
            peak2d(j,2) = 3.0
            peak2d(j,3) = plane2d(int(zz2)+(int(zz1)-1)*si2im)
            peak2d(j,4) = zz1
            peak2d(j,5) = 3.0
            peak2d(j,6) = 0.0
            peak2d(j,7) = 1.0
            peak2d(j,8) = 0.0
            peak2d(j,9) = 0.0
            peak2d(j,10) = 0.0
            peak2d(j,11) = 0.0
        else
            call message('Peak coordinates F1 - F2 - F3 (in index)')
            zz1 = peak3d(j,4)
            call getreal2(zz1,error)
            if (error.ne.0) goto 93
            if (zz1.lt.1.0 .or. zz1.gt.si3d1) goto 108
            zz2 = peak3d(j,6)
            call getreal2(zz2,error)
            if (error.ne.0) goto 93
            if (zz2.lt.1.0 .or. zz2.gt.si3d2) goto 108
            zz3 = peak3d(j,1)
            call getreal2(zz3,error)
            if (error.ne.0) goto 93
            if (zz3.lt.1.0 .or. zz3.gt.si3d3) goto 108

            peak3d(j,1) = zz3
            peak3d(j,6) = zz2
            peak3d(j,4) = zz1
            peak3d(j,2) = 3.0
            peak3d(j,5) = 3.0
            peak3d(j,7) = 3.0
            peak3d(j,3) = image(int(zz3) + (int(zz2)-1)*si3d3 +
     *                      (int(zz1)-1)*si3d3*si3d2)
        endif

      elseif (command.eq.'POINT->PK') then
        if (nclick.LE.0) goto 91
        if (dim.eq.1) then
          do i=1,nclick
            nbpic1d = nbpic1d+1
            peak1d(nbpic1d,1) = clickt(i,1)
            peak1d(nbpic1d,2) = 3.0
            peak1d(nbpic1d,3) = column(closest(clickt(i,1)))
            peak1d(nbpic1d,4) = 0.0
            peak1d(nbpic1d,5) = 0.0
            peak1d(nbpic1d,6) = 0.0
            peak1d(nbpic1d,7) = 0.0
            peak1d(nbpic1d,8) = 0.0
            write(peak1d_id(nbpic1d),'(i12)') nbpic1d
            call leading(peak1d_id(nbpic1d))
         enddo
        elseif (dim.eq.2) then
          do i=1,nclick
            nbpic2d = nbpic2d+1
            peak2d(nbpic2d,1) = clickt(i,1)
            peak2d(nbpic2d,2) = 3.0
            peak2d(nbpic2d,3) =
     *    plane2d(closest(clickt(i,1))+(closest(clickt(i,2))-1)*si2im)
            peak2d(nbpic2d,4) = clickt(i,2)
            peak2d(nbpic2d,5) = 3.0
            peak2d(nbpic2d,6) = 0.0
            peak2d(nbpic2d,7) = 0.0
            peak2d(nbpic2d,8) = 0.0
            peak2d(nbpic2d,9) = 0.0
            peak2d(nbpic2d,10) = 0.0
            peak2d(nbpic2d,11) = 0.0
            write(peak2d_id(nbpic2d),'(i12)') nbpic2d
            call leading(peak2d_id(nbpic2d))
          enddo
        else
          do i=1,nclick
            nbpic3d = nbpic3d+1
            peak3d(nbpic3d,2) = 3.0
            peak3d(nbpic3d,5) = 3.0
            peak3d(nbpic3d,7) = 3.0
            peak3d(nbpic3d,3) =
     *   plane2d(closest(clickt(i,1))+(closest(clickt(i,2))-1)*si2im)
            if (axis2d.eq.1) then
              peak3d(nbpic3d,1) = clickt(i,1)
              peak3d(nbpic3d,6) = clickt(i,2)
              peak3d(nbpic3D,4) = n2d
            elseif (axis2d.eq.2) then
              peak3d(nbpic3d,1) = clickt(i,1)
              peak3d(nbpic3d,4) = clickt(i,2)
              peak3d(nbpic3D,6) = n2d
            elseif (axis2d.eq.3) then
              peak3d(nbpic3d,6) = clickt(i,1)
              peak3d(nbpic3d,4) = clickt(i,2)
              peak3d(nbpic3D,1) = n2d
            endif
            write(peak3d_id(nbpic3d),'(i12)') nbpic3d
            call leading(peak3d_id(nbpic3d))
          enddo
        endif
        if (debug.ne.0) call gifaouti('Number of added peak : ',nclick)

      elseif (command.eq.'PK->DT') then
        if (dim.ne.1) goto 99
        if (nbpic1d.le.0) goto 111
c        nbpic1d = order
        z = float(sizeimage1d)
        call lpreconstruct1d (sizeimage1d/2,column,peak1d(1,3),
     &          peak1d(1,2),peak1d(1,4),peak1d(1,1),nbpic1d,-z,z)
        it1d = 1
        refresh = 1
        max0 = 0.0
        
      elseif (command.eq.'PKCLEAR') then
         if (dim.eq.1) then
           nbpic1d=0
         elseif (dim.eq.2) then
           nbpic2d=0
         elseif (dim.eq.3) then
           nbpic3d=0
         endif

      elseif (command.eq.'PKRESET') then
         if (dim.eq.1) then
           do i=1,nbpic1d
             xx = int(peak1d(i,1)+0.5)
             if (xx.gt.1 .and. xx.lt.sizeimage1d) then
                peak1d(i,3) = column(xx)
                peak1d(i,6) = 0.0
                peak1d(i,7) = 0.0
                peak1d(i,8) = 0.0
             endif
           enddo
         elseif (dim.eq.2) then
           do i=1,nbpic2d
             xx = int(peak2d(i,1)+0.5)
             yy = int(peak2d(i,4)+0.5)
             if (xx.gt.1 .and. xx.lt.si2im
     *         .and. yy.gt.1 .and. yy.lt.si1im) then
                peak2d(i,3) = plane2d(xx+si2im*(yy-1))
                peak2d(i,6) = 0.0
                peak2d(i,8) = 0.0
                peak2d(i,9) = 0.0
                peak2d(i,10) = 0.0
                peak2d(i,11) = 0.0
             endif
           enddo
         elseif (dim.eq.3) then
           do i=1,nbpic3d
             xx = int(peak3d(i,1)+0.5)
             yy = int(peak3d(i,4)+0.5)
             zz = int(peak3d(i,6)+0.5)
             if (xx.gt.1 .and. xx.lt.si3d3
     *         .and. yy.gt.1 .and. yy.lt.si3d1
     *         .and. zz.gt.1 .and. zz.lt.si3d2) then
                peak3d(i,3) = image(xx + si3d3*(zz-1)
     *            +   si3d3*si3d2*(yy-1))
             endif
           enddo
         endif

      elseif (command.eq.'PKWRITE') then
c store peak table in text format, in ppm
        call message('Enter file name')
        call getstring2(pkname,error)
        ww = 10
        open(ww,file=pkname,form='formatted',
     *            status='new',iostat=error)
        if (error.ne.0) goto 101
        if (dim.eq.1) then
          write(ww,*) nbpic1d
          do i=1,nbpic1d
            write(ww,*) itopr(peak1d(i,1),sizeimage1d,
     *                          sp1d,of1d,freq1d),
     *            peak1d(i,2),peak1d(i,3),peak1d(i,4),peak1d(i,5)

          enddo
        elseif (dim.eq.2) then
          write(ww,*) nbpic2d
          do i=1,nbpic2d
            write(ww,*)
     *      itopr(peak2d(i,1),si2im,specw2,offset2,freq2),
     *      peak2d(i,2),peak2d(i,3),
     *      itopr(peak2d(i,4),si1im,specw1,offset1,freq1),
     *      peak2d(i,5)
C    *       peak2d_id(i)
          enddo
        else
          write(ww,*) nbpic3d
          do i=1,nbpic3d
            write(ww,*) itopr(peak3d(i,1),si3d3,sp3d3,of3d3,freq3d3),
     *      peak3d(i,2),peak3d(i,3),
     *      itopr(peak3d(i,4),si3d1,sp3d1,of3D1,freq3D1),peak3d(i,5),
     *      itopr(peak3d(i,6),si3d2,sp3d2,of3D2,freq3D2),peak3d(i,7)
c     *            peak3d_id(i)
          enddo
        endif
        close(ww)

      elseif (command.eq.'PKREAD') then
c read from pkwrite
        call message('Enter file name')
        call getstring2(pkname,error)
        ww = 10
        open(ww,file=pkname,form='formatted',
     *            status='old',iostat=error)
        if (error.ne.0) goto 101
        call message('Erase peak table ?')
        st = 'yes'
        call getbool2(st,'pkread',error)
        if (error.ne.0) goto 101
        if (dim.eq.1) then
          if (st.eq.'YES') then
             xx = 1
             nbpic1d = 0
          else
             xx = nbpic1d+1
          endif
          read(ww,*,iostat=error) yy
          do i=1,yy
            read(ww,*,iostat=error) zz1,zz2,zz3,zz4,zz5      ! ,st
            if (error.ne.0) goto 93
            zz1 = ptoir(zz1,sizeimage1d,sp1d,of1d,freq1d)
            if (zz1.ge.1 .and. zz1.le.sizeimage1d) then
               if (xx.gt.peakmax) goto 103
               peak1d(xx,1) = zz1
               peak1d(xx,2) = zz2
               peak1d(xx,3) = zz3
               peak1d(xx,4) = zz4
               if (zz5.ge.0.0 .or. zz5.le.2.0) then
                  peak1d(xx,5) = zz5
               else
                  goto 93
               endif
               peak1d(xx,6) = 0.0
               peak1d(xx,7) = 0.0
               peak1d(xx,8) = 0.0
c               peak1d_id(xx) = st
               write(peak1d_id(xx),'(i12)') xx 
               call leading(peak1d_id(xx))
               xx = xx+1
            endif
          enddo
          write(st,*) 'Read in ',xx-nbpic1d-1,' peaks'
          call gifaout(st)
          nbpic1d = xx-1
        elseif (dim.eq.2) then
          if (st.eq.'YES') then
             xx = 1
             nbpic2d = 0
          else
             xx = nbpic2d+1
          endif
          read(ww,*,iostat=error) yy
          do i=1,yy
            read(ww,*,iostat=error) zz1,zz2,zz3,zz4,zz5    ! ,st
            if (error.ne.0) goto 93
            zz1 = ptoir(zz1,si2im,specw2,offset2,freq2)
            zz4 = ptoir(zz4,si1im,specw1,offset1,freq1)
            if (zz1.ge.1 .and. zz1.le.si2im .and.
     *            zz4.ge.1 .and. zz4.le.si1im) then
                if (xx.gt.peakmax) goto 103
                peak2d(xx,1) = zz1
                peak2d(xx,2) = zz2
                peak2d(xx,3) = zz3
                peak2d(xx,4) = zz4
                peak2d(xx,5) = zz5
C                peak2d_id(xx) = st
               write(peak2d_id(xx),'(i12)') xx 
               call leading(peak2d_id(xx))
               xx = xx+1
            endif
          enddo
          write(st,*) 'Read in ',xx-nbpic2d-1,' peaks'
          call gifaout(st)
          nbpic2d = xx-1
        else
          if (st.eq.'YES') then
             xx = 1
             nbpic3d = 0
          else
             xx = nbpic3d+1
          endif
          read(ww,*,iostat=error) yy
          do i=1,yy
            read(ww,*,iostat=error) zz1,zz2,zz3,zz4,zz5,zz6,zz7   ! ,st
            if (error.ne.0) goto 93
            zz1 = ptoir(zz1,si3d3,sp3d3,of3d3,freq3d3)
            zz4 = ptoir(zz4,si3d1,sp3d1,of3d1,freq3d1)
            zz6 = ptoir(zz6,si3d2,sp3d2,of3d2,freq3d2)
            if (zz1.ge.1 .and. zz1.le.si3d3 .and.
     *          zz4.ge.1 .and. zz4.le.si3d1 .and.
     *          zz6.ge.1 .and. zz6.le.si3d2) then
                if (xx.gt.peakmax) goto 103
                peak3d(xx,1) = zz1
                peak3d(xx,2) = zz2
                peak3d(xx,3) = zz3
                peak3d(xx,4) = zz4
                peak3d(xx,5) = zz5
                peak3d(xx,6) = zz6
                peak3d(xx,7) = zz7
C                peak2d_id(xx) = st
               write(peak3d_id(xx),'(i12)') xx 
               call leading(peak3d_id(xx))
               xx = xx+1
            endif
          enddo
          write(st,*) 'Read in ',xx-nbpic3d-1,' peaks'
          call gifaout(st)
          nbpic3d = xx-1
        endif
        close(ww)

      elseif (command.eq.'PKSELECT') then
        if (dim.eq.1) xx = nbpic1d
        if (dim.eq.2) xx = nbpic2d
        if (dim.eq.3) xx = nbpic3d
        call gifaouti('Number of peaks in table : ',xx)
	if (xx.le.0) goto 111
        i = 1
        call message( 'Enter index for each point, enter 0 to end.')
        bool1 = .true.
        zz = 0
        do while (bool1)
          zz = zz+1
          call getint2 (zz,error)
          if (error.ne.0) goto 93
          if (zz.eq.0) bool1 = .false.
          if (zz.gt.xx) goto 98
          if (zz.lt.0) goto 98
          tindex(i) = zz
          i = i + 1
        enddo
        zz = i-2
        call bsort(tindex,zz)
        if (dim.eq.1) then
          nbpic1d = zz
          do i=1,pk1d
            call icopvect(tindex,peak1d(1,i),peak1d(1,i),nbpic1d)
          enddo
          call icopsvect(tindex,peak1d_id(i),peak1d_id(i),nbpic1d)
        elseif (dim.eq.2) then
          nbpic2d = zz
          do i=1,pk2d
            call icopvect(tindex,peak2d(1,I),peak2d(1,i),nbpic2d)
          enddo
          call icopsvect(tindex,peak2d_id(i),peak2d_id(i),nbpic2d)
        elseif (dim.eq.3) then
          nbpic3d = zz
          do i=1,pk3d
            call icopvect(tindex,peak3d(1,I),peak3d(1,i),nbpic3d)
          enddo
          call icopsvect(tindex,peak3d_id(i),peak3d_id(i),nbpic3d)
        endif
        call gifaouti('New number of peaks : ',zz)

      elseif (command.eq.'PKCLEAN') then
        z = leastsq
        call message('Remove the peaks who have amplitude less than ?')
        call getreal2(z,error)
        zz = 0
        if (dim.eq.1) then
          do yy = 1,nbpic1d
            if (peak1d(yy,3).gt.z) then
                zz = zz + 1
                tindex(zz) = yy
            endif
          enddo
          call bsort(tindex,zz)
          nbpic1d = zz
          do i=1,pk1d
            call icopvect(tindex,peak1d(1,i),peak1d(1,i),nbpic1d)
          enddo
          call icopsvect(tindex,peak1d_id(i),peak1d_id(i),nbpic1d)
        elseif (dim.eq.2) then
          do yy = 1,nbpic2d
            if (peak2d(yy,3).gt.z) then
                zz = zz + 1
                tindex(zz) = yy
            endif
          enddo
          call bsort(tindex,zz)
          nbpic2d = zz
          do i=1,pk2d
            call icopvect(tindex,peak2d(1,i),peak2d(1,i),nbpic2d)
          enddo
          call icopsvect(tindex,peak2d_id(i),peak2d_id(i),nbpic2d)
        elseif (dim.eq.3) then
          do yy = 1,nbpic3d
            if (peak3d(yy,3).gt.z) then
                zz = zz + 1
                tindex(zz) = yy
            endif
          enddo
          call bsort(tindex,zz)
          nbpic3d = zz
          do i=1,pk3d
            call icopvect(tindex,peak3d(1,i),peak3d(1,i),nbpic3d)
          enddo
          call icopsvect(tindex,peak3d_id(i),peak3d_id(i),nbpic3d)
        endif
        call gifaouti('New number of peaks : ',zz)

      elseif (command.eq.'MINIMAX') then
         call message( 'Enter minimum value of peak:  ')
         call getreal2(mini,error)
         call message( 'Enter maximum value of peak:  ')
         call getreal2(maxi,error)

      elseif (command.eq.'ZERO_QU') then
         call message( 'Normal integration (0),
     *   Absolute value integration (1):  ')
	 zz = zq
         call getint2(zz,error)
         if (zz.ne.0 .and. zz.ne.1) goto 98
         zq = zz

      elseif (command.eq.'SIGN_PEAK') then
         call message( 'Find positive peaks or negative peaks? (1/-1)')
	 zz = signpic
         call getint2(zz,error)
         if (zz.ne.-1 .and. zz.ne.1) goto 98
	 signpic = zz

      elseif (command.eq.'INT1D') then
        if (dim.ne.1) goto 99
C fist compute max0 if absent (may happen in macro)
        if (max0.eq.0.0)
     *     call mxavect(zz2,max0,xx,yy,column,sizeimage1d)
        zz1 = max0
        call intvect(column,sizeimage1d,it1d)
        call MXAVECT(zz2,zz3,xx,yy,column,sizeimage1d)
        call mltvect(column,column,zz1/zz3,sizeimage1d)
C this sequence should leave max0 unchanged
        refresh = 1

      elseif (command.eq.'INTEG') then
         if (dim.eq.3) goto 124
         if (sizeimage.gt.2*smxmax) goto 103
         call message( 'Enter values of factor, slope, and threshold:')
         call getreal2(factor,error)
         call getreal2(slope,error)
         call getreal2(thresh,error)
         if (factor.le.0.0 .or. error.ne.0) goto 98
         if (dim.eq.1) then
            if (it1d.ne.0) go to 96
            call integ1(column,factor,slope,thresh,
     & sizeimage1d,zq,signpic,shift,noise)
            porig1d = 2
         elseif (dim.eq.2) then
            if (itype.ne.0) go to 96
 	    zz = 3
            call message( 'Enter value of maximum amoeba radius ')
            call getint2(zz,error)
            if (zz.gt. (szindex/16) .or. error.ne.0) goto 98
            z = 0.0
            call prpvect(work2,z,sizeimage)
	    call mkmask (plane2d,work2,si2im,si1im,
     & verbose,tindex,factor,slope,thresh,zz,signpic,zq)
            call mskintg(plane2d,work2,si1im,si2im,
     * noise,shift,signpic)
            porig2d = 2
         endif

c      elseif (command.eq.'SUPERP') then
c         if (dim.ne.2) goto 97
c         z = 0.0
c 	 call prpvect(datab,z,si1dat*si2dat)
c 	 call superp(plane2d,datab)

      elseif (command.eq.'MSKINTEG') then
         if (dim.ne.2) goto 97
         if (itype.ne.0) go to 96
         call mskintg(plane2d,work2,si1im,si2im,
     *       noise,shift,signpic)

      elseif (command.eq.'MSKMODIF') then
         if (dim.ne.2) goto 97
         if (itype.ne.0) go to 96
	 call message( 'Enter the F1 pixel coordinate')
         call getint2(yy,error)
	 if ((yy.le.0).or.(yy.gt.si1im).or.(error.ne.0)) goto 98
	 call message( 'Enter the F2 pixel coordinate')
         call getint2(xx,error)
	 if ((xx.le.0).or.(xx.gt.si2im).or.(error.ne.0)) goto 98
	 call message( 'Enter the amoeba value')
         call getint2(zz,error)
	 if ((zz.lt.0).or.(zz.gt.nbpic2d).or.(error.ne.0)) goto 98
         work2(xx+((yy-1)*si2im)) = zz

      elseif (command.eq.'MSKSEARCH') then
         if (dim.ne.2) goto 97
         if (itype.ne.0) go to 96
	 call message( 'Enter the peak number you want to search for')
         call getint2(zz,error)
         call searchamb(zz,work2,si1im,si2im)

      elseif (command.eq.'MSKCONC') then
         if (dim.ne.2) goto 97
         if (itype.ne.0) go to 96
         xx = 1
	 call message( 'Concatenate amoeba of peak :')
         call getint2(xx,error)
	 if ((xx.lt.0).or.(xx.gt.nbpic2d).or.(error.ne.0)) goto 98
         yy = xx+1
	 call message( 'With amoeba of peak :')
         call getint2(yy,error)
	 if ((yy.lt.0).or.(yy.gt.nbpic2d).or.(error.ne.0)) goto 98
         call replam(work2,si1im,si2im,xx,yy)

       elseif (command.eq.'MSKZERO') then
         if (dim.ne.2) goto 97
         if (itype.ne.0) go to 96
         xx = 1
         call prpvect(work2,0.0,si1im*si2im)

      elseif (command.eq.'INTEG-OLD') then
         if  (dim.eq.1) then
            if (nbpic1d.eq.0) goto 111
         elseif  (dim.eq.2) then
            if (nbpic2d.eq.0) goto 111
c            call message ('Do you want to keep the amoebas ? ')
c            call getbool2(st,'integ',error)
c            if (st.eq.'YES' .and. sizeimage.gt.2*smxmax) goto 103
            st = 'yes'
            if (sizeimage.gt.2*smxmax) goto 103
         elseif (dim.eq.3) then
            goto 124
         endif
         call message( 'Enter values of factor, slope, and threshold:')
         call getreal2(factor,error)
         call getreal2(slope,error)
         call getreal2(thresh,error)
         if (factor.le.0.0 .or. slope.lt.0.0 .or. error.ne.0) go to 98
         if (dim.eq.1) then
           if (it1d.ne.0) go to 96
           call gifaout ('Peak coordinates in '//unitx)
           write(st,'(2x,A6,A10,A15,A12,A6)')
     *       'index','coord','volume','left/right extent','error'
           call gifaout(st)
           do zz=1,nbpic1d
               call intpic1d(column,zz,volume,i,factor,slope,thresh,
     *                   sizeimage1d,limg,limd,signpic,zq)
               if (signpic.eq.-1) then
                 volume= volume+i*shift
               else
                 volume=volume-i*shift
               endif
               peak1d(zz,2) = max(1.0,float(limd-limg)/2.0)
               peak1d(zz,3) = volume/peak1d(zz,2)
               incert=sqrt(float(i))*noise
               call currunitr(zz4,peak1d(zz,1),1)
               if (volume.ne.0.0) then
                 write(st,46) zz,zz4,volume,
     *             limg-int(peak1d(zz,1)),limd-int(peak1d(zz,1)),
     *             100.0*incert/volume
               else
                 write(st,46) zz,zz4,volume,
     *             limg-int(peak1d(zz,1)),limd-int(peak1d(zz,1)),
     *             0.0
46    format(I6,F10.3,G15.6,i6,i6,' +/-',f6.1,'%')
               endif
               call gifaout(st)
           enddo
           porig1d = 2
        elseif (dim.eq.2) then
           if (itype.ne.0) go to 96
           if (st.eq.'yes') call prpvect(work2,0.0,sizeimage)
           do zz=1,nbpic2d
            peak2d(zz,3)=0.0
           enddo
           do zz=1,nbpic2d
c            if (st.eq.'yes') then
             call intpic(plane2d,work2,zz,volume,i,factor,slope,thresh,
     *                si1im,si2im,ligb,ligh,limg,limd,
     *                pkradius,signpic,zq)
c            else
c             call intpic(plane2d,-1.0,zz,volume,i,factor,slope,thresh,
c     *                si1im,si2im,ligb,ligh,limg,limd,
c     *                 comp,signpic,zq)
c             endif
              volume=volume-i*shift
              incert=sqrt(float(i))*noise
              peak2d(zz,3) = volume
              if (peak2d(zz,5).lt.0.0) then
                  i = abs(peak2d(zz,5))
                  peak2d(i,3) = peak2d(i,3)+volume
              endif
           enddo
           call gifaout ('Peak coordinates in '//unitx)
           write(st,'(2x,A6,2A10,A15,4x,A6)')
     *       'index','F1 coord','F2 coord','volume','error'
           call gifaout(st)
           do zz=1,nbpic2d
               call currunitr(zz1,peak2d(zz,4),1)
               call currunitr(zz2,peak2d(zz,1),2)
               volume =  peak2d(zz,3)
               if (volume.ne.0.0) then
                  write(st,44) zz,zz1,zz2,
     *                         volume,100.0*incert/volume
               else
                  write(st,44) zz,zz1,zz2,
     *                         volume,0.0
44    format(I6,2f10.3,G15.6,' +/- ',f6.1,'%')
               endif
               call gifaout(st)
           enddo
           porig2d = 2
         endif

      elseif (command.eq.'MSKINTEG-OLD') then
         if (dim.ne.2) goto 97
         do i=1,nbpic2d
           tindex(i)=0
           peak2d(i,3)=0.0
         enddo
         do i=1,si1im
           do j=1,si2im
              if (work2(j+(i-1)*si2im).ne.0.0) then
                zz = int(work2(j+(i-1)*si2im))
                peak2d(zz,3) = peak2d(zz,3) + plane2d(j+(i-1)*si2im)
                tindex(zz) = tindex(zz)+1
              endif
           enddo
         enddo
         do zz=1,nbpic2d
               if (peak2d(zz,5).lt.0.0) then
                  i = abs(peak2d(zz,5))
                  peak2d(i,3) = peak2d(i,3)+volume
               endif
         enddo
         write(st,'(2x,A6,2A10,A15,4x,A6)')
     *       'index','F1 coord','F2 coord','volume','error'
         call gifaout(st)
         do zz=1,nbpic2d
               call currunitr(zz1,peak2d(zz,4),1)
               call currunitr(zz2,peak2d(zz,1),2)
               volume = peak2d(zz,3)
               if (volume.ne.0.0) then
                  write(st,44) zz,zz1,zz2,
     *                         volume,100.0*incert/volume
               else
                  write(st,44) zz,zz1,zz2,
     *                         volume,0.0
               endif
               call gifaout(st)
         enddo
         porig2d = 2

      elseif (command.eq.'PKPATTERN') then
         call message
     * ('Enter pattern for peak display (+ x * s d o b) :')
         call getenum2(peakpat,'+ x * s d o b',error)
         if (error.ne.0) goto 93
         call lowercase(peakpat,1)
         
      elseif (command.eq.'SHOWPEAK') then
         if (dim.eq.1) xx = nbpic1d
         if (dim.eq.2) xx = nbpic2d
         if (dim.eq.3) xx = nbpic3d
         call message('Which peak to show :')
         call getint2(zz,error)
         if (error.ne.0) goto 93
         if (zz.lt.1 .or. zz.gt.xx) goto 98
c en 1D
         if (dim.eq.1) then
           if (vd_id(1).eq.0) goto 127
           call showpeak1d(zz,vd_id(1))
c en 2D
         elseif (dim.eq.2) then
           if (nbpic2d.le.0) goto 111
           if (vd_id(2).eq.0 .and. vd_id(3).eq.0) goto 127
           if (vd_id(2).ne.0) call showpeak2d(zz,vd_id(2))
           if (vd_id(3).ne.0) call showpeak2d(zz,vd_id(3))
         else
C in 3D  
           if (nbpic3d.le.0) goto 111
           if (disp3d.eq.1) then
              call showpeak3d(zz,vd_id(10))
           endif				! (if disp3d .ne. 1)
           if (vd_id(2).ne.0) call showpeak3d2d(zz,vd_id(2))
           if (vd_id(3).ne.0) call showpeak3d2d(zz,vd_id(3))
         endif		! on dim


      elseif (command.eq.'SHOWPEAKS') then
C in 1D
         if (dim.eq.1) then
           if (nbpic1d.le.0) goto 111
           if (vd_id(1).eq.0) goto 127
           do i=1,nbpic1d
             call showpeak1d(i,vd_id(1))
           enddo
C in 2D  
         elseif (dim.eq.2) then
           if (nbpic2d.le.0) goto 111
           if (vd_id(2).eq.0 .and. vd_id(3).eq.0) goto 127
           if (vd_id(2).ne.0) then
             do i=1,nbpic2d
               call showpeak2d(i,vd_id(2))
             enddo
           endif
           if (vd_id(3).ne.0) then
             do i=1,nbpic2d
               call showpeak2d(i,vd_id(3))
             enddo
           endif
C in 3D  
         else
           if (nbpic3d.le.0) goto 111
           if (disp3d.eq.1) then
             do i=1,nbpic3d
                  call showpeak3d(i,vd_id(10))
             enddo
           endif			! (if disp3d .ne. 1)
           if (vd_id(2).ne.0) then
               do i=1,nbpic3d
                  call showpeak3d2d(i,vd_id(2))
               enddo
            endif
           if (vd_id(3).ne.0) then
               do i=1,nbpic3d
                  call showpeak3d2d(i,vd_id(3))
               enddo
            endif
         endif		! on dim
          
      elseif (command.eq.'PLOTPEAKS') then
        call message('size of the cross to plot (in cm) :')
        zz3 = 0.1
        call getreal2(zz3,error)
        if (error.ne.0) goto 93
        zz3 = abs(zz3/2.0)
        st = '*PLOTTER'
        call getout2(st,error)
        if (error.ne.0) goto 93
C       No screen support yet...
        if (st(1:3).eq.'*PR') goto 106
        if (st(1:2).eq.'*S') goto 106
        pl_id = 99
        call plopen(pl_id,st,error)
        if (error.ne.0) goto 101
        if (dim.eq.1) then
          if (nbpic1d.eq.0) goto 111
          do i=1,nbpic1d
             if (peak1d(i,2).ge.0.0) then
               zz1 = cy + 0.5*mod(i,3)
               if (zoom.eq.0) then
                 zz2 = cx*(peak1d(i,1)-0.5)/sizeimage1d
               else
                 zz2 = cx*(peak1d(i,1)-zo1dl+0.5)/(zo1du-zo1dl+1)
               endif
               if (zz2.ge.0.0 .and. zz2.le.cx)
     *               call plotcross2(pl_id,zz2,zz1,zz3,i)
             endif
          enddo
        elseif (dim.eq.2) then
          if (nbpic2d.eq.0) goto 111
           do i=1,nbpic2d
             if (peak2d(i,5).ge.0.0) then
               if (zoom.eq.0) then
                  zz1 = cy*(peak2d(i,4)-0.5)/si1im
                  zz2 = cx*(peak2d(i,1)-0.5)/si2im
               else
                  zz2 = cx*(peak2d(i,1)-zolf2+0.5) / (zour2-zolf2+1)
                  zz1 = cy*(peak2d(i,4)-zolf1+0.5) / (zour1-zolf1+1)
               endif
               if (zz1.ge.0.0 .and. zz1.le.cy .and.
     *             zz2.ge.0.0 .and. zz2.le.cx)
     *             call plotcross2(pl_id,zz2,zz1,zz3,i)
            endif
          enddo
        else
           do i=1,nbpic3d
             if (peak3d(i,5).ge.0.0) then
              if (axis2d.eq.1 .and.
     *                (abs(n2d-peak3d(i,4)).lt.0.5)) then
                if (zoom.eq.0) then
                  zz1 = (peak3d(i,6)-0.5)/si1im
                  zz2 = (peak3d(i,1)-0.5)/si2im
                else
                  zz2 = (peak3d(i,1)-zolf2+0.5) / (zour2-zolf2+1)
                  zz1 = (peak3d(i,6)-zolf1+0.5) / (zour1-zolf1+1)
                endif
              elseif (axis2d.eq.2 .and. 
     *                (abs(n2d-peak3d(i,6)).lt.0.5)) then
                if (zoom.eq.0) then
                  zz1 = (peak3d(i,4)-0.5)/si1im
                  zz2 = (peak3d(i,1)-0.5)/si2im
                else
                  zz2 = (peak3d(i,1)-zolf2+0.5) / (zour2-zolf2+1)
                  zz1 = (peak3d(i,4)-zolf1+0.5) / (zour1-zolf1+1)
                endif
              elseif (axis2d.eq.3 .and.
     *                (abs(n2d-peak3d(i,1)).lt.0.5)) then
                if (zoom.eq.0) then
                  zz1 = (peak3d(i,4)-0.5)/si1im
                  zz2 = (peak3d(i,6)-0.5)/si2im
                else
                  zz2 = (peak3d(i,6)-zolf2+0.5) / (zour2-zolf2+1)
                  zz1 = (peak3d(i,4)-zolf1+0.5) / (zour1-zolf1+1)
                endif
              endif
              zz2 = cx*zz2
              zz1 = cy*zz1
              if (zz1.ge.0.0 .and. zz1.le.cy .and.
     *             zz2.ge.0.0 .and. zz2.le.cx)
     *             call plotcross2(pl_id,zz2,zz1,zz3,i)


             endif
           enddo
        endif
        call plclose(pl_id)

      elseif (command.eq.'MAXINBOX') then
         if (dim.eq.1) then
            call maxinbox1d(column,sizeimage1d,maxinbox,error)
         elseif (dim.eq.2) then
            call maxinbox2d(plane2d,si1im,si2im,maxinbox,error)
         else
            goto 124
         endif
         if (error.ne.0) goto 169
         incert=noise
         write(st,*) 'maxinbox=',maxinbox,'+/-',incert
         call gifaout(st)

      elseif (command.eq.'SOMREC' .or. command.eq.'SUMREC') then
         if (dim.eq.1) then
            call intrec1d(column,sizeimage1d,volume,i,error)
         elseif (dim.eq.2) then
            call intrec2d(plane2d,si1im,si2im,volume,i,error)
         else
            goto 124
         endif
         if (error.ne.0) goto 169
         if (signpic.eq.-1) then
           volume = -volume+i*shift
         else
           volume = volume-i*shift
         endif
         incert=sqrt(float(i))*noise
         write(st,*) 'volume=',volume,'+/-',incert
         call gifaout(st)

      elseif (command.eq.'PKSYM') then
         if (dim.ne.2) goto 97
	 call message('Remove (0) or add (1) non symmetrical values?')
	 call getint2(zz,error)
	 if (error.ne.0) goto 93
	 if ((zz.ne.0).and.(zz.ne.1)) goto 98
	 call message('Hertz tolerance ?')
         z=1.0
         call getreal2(z,error)
         if (error.ne.0) goto 93
         if (z.lt.0) goto 98
	 if (zz.eq.0) call peaksym_rem (tindex,szindex,error,z)
	 if (zz.eq.1) call peaksym_add (error,z)
	 if (error.eq.1) goto 103

      elseif (command.eq.'PKPROJ') then
         if (dim.ne.2) goto 97
	 call message('Projection along f1 or f2?')
         zz=2
         call getdim(zz,error)
         if (error.ne.0) goto 93
         if ((zz.ne.1).and.(zz.ne.2)) goto 98
         call message( 'Choose Mean or Skyline')
         st='Skyline'
         call getenum2(st,'mean skyline',error)
         if (error.ne.0) goto 93
         call uppercase(st,256)
         if ((st.ne.'MEAN').and.(st.ne.'SKYLINE')) goto 98
         call message('Hertz tolerance ?')
         z=1.0
         call getreal2(z,error)
         if (error.ne.0) goto 93
         if (z.lt.0) goto 98
	 call peakproj (error,zz,st(1:1),z)
	 if (error.eq.1) goto 103
c         call t2dto1d (mod(zz,2)+1)

      elseif (command.eq.'PKRM') then
        if (dim.eq.1) xx = nbpic1d
        if (dim.eq.2) xx = nbpic2d
        if (dim.eq.3) xx = nbpic3d
        write (st,*) 'Number of peaks in table : ',xx
        call message(st)
        i = 1
        call message( 'Enter index for each point, enter 0 to end.')
        bool1 = .true.
        zz = 0
        do while (bool1)
          zz = zz+1
          call getint2 (zz,error)
          if (error.ne.0) goto 98
          if (zz.eq.0) bool1 = .false.
          if (zz.gt.xx) goto 98
          tindex(i) = zz
          i = i + 1
        enddo
        zz = i-2
c        write(*,*)'zz',zz
        call bsort(tindex,zz)
        if (dim.eq.1) then
          do i=1,pk1d
            call cicopvect(tindex,peak1d(1,i),peak1d(1,i),nbpic1d,zz)
          enddo
          call cicopsvect(tindex,peak1d_id(i),peak1d_id(i),nbpic1d,zz)
          nbpic1d = nbpic1d - zz
          write(st,*) 'New number of peaks : ',nbpic1d
        elseif (dim.eq.2) then
          do i=1,pk2d
            call cicopvect(tindex,peak2d(1,i),peak2d(1,i),nbpic2d,zz)
          enddo
          call cicopsvect(tindex,peak2d_id(i),peak2d_id(i),nbpic2d,zz)
          nbpic2d = nbpic2d - zz
          write(st,*) 'New number of peaks : ',nbpic2d
        elseif (dim.eq.3) then
          do i=1,pk3d
            call cicopvect(tindex,peak3d(1,i),peak3d(1,i),nbpic3d,zz)
          enddo
          call cicopsvect(tindex,peak3d_id(i),peak3d_id(i),nbpic3d,zz)
          nbpic3d = nbpic3d - zz
          write(st,*) 'New number of peaks : ',nbpic3d
        endif
        call message(st)
c
c Nouvelles commandes de manipulation d'amibes: en cours de debuggage
c   
c      elseif (command.eq.'AMBPUT') then
c        if (dim.ne.2) call gifaout("Command available in 2D only!")
c	call message("F2 coordinate of pixel?")
c  	call getint2(xx)
c 	if (xx.gt.si2im) goto 108
c	call message("F1 coordinate of pixel?")
c  	call getint2(yy)
c 	if (yy.gt.si1im) goto 108
c        z = getmaskno(xx,yy)
c	call gifaoutr("Peak number :",z)
c
c      elseif (command.eq.'AMBPUT') then
c        if (dim.ne.2) call gifaout("Command available in 2D only!")
c	call message("F2 coordinate of pixel?")
c  	call getint2(xx)
c 	if (xx.gt.si2im) goto 108
c	call message("F1 coordinate of pixel?")
c  	call getint2(yy)
c 	if (yy.gt.si1im) goto 108
c	call message("Peak number ?")
c  	call getint2(zz)
c 	if (zz.gt.nbpic2d) goto 147
c        call setpixmask(xx,yy,zz,error)
c	if (error.ne.0) goto 90
c
c
c      elseif (command.eq.'AMBREM') then
c        if (dim.ne.2) call gifaout("Command available in 2D only!")
c	call message("F2 coordinate of pixel?")
c  	call getint2(xx)
c 	if (xx.gt.si2im) goto 108
c	call message("F1 coordinate of pixel?")
c  	call getint2(yy)
c 	if (yy.gt.si1im) goto 108
c	call message("Peak number ?")
c  	call getint2(zz)
c 	if (zz.gt.nbpic2d) goto 147
c        call rempixmask(xx,yy,zz,error)
c	if (error.ne.0) goto 90
c	 
c      elseif (command.eq.'AMBDEL') then
c	call message("Peak number ?")
c  	call getint2(zz)
c 	if (zz.gt.nbpic2d) goto 147
c        call delmask(zz)
c
c      elseif (command.eq.'AMBINT') then
c	call message("Peak number ?")
c  	call getint2(zz)
c 	if (zz.gt.nbpic2d) goto 147
c        call calcmask(zz,plane2d,si1im,si2im,noise,shift,signpic)
c
c      elseif (command.eq.'AMBSTO') then
c        call message( 'Enter file-name')
c        call getstring2(name,error)
c        call storemask(amoeba,liste,name,error)
c	if (error.ne.0) goto 90
c
c      elseif (command.eq.'AMBREAD') then
c        call message( 'Enter file-name')
c        call getstring2(name,error)
c        call readmask(amoeba,liste,name,error)
c	if (error.ne.0) goto 90
	























