      elseif (command.eq.'DIAGONALIZE') then
         if (dim.ne.2) goto 97
         if (si1im.ne.si2im) goto 129
         if (itype.ne.0) goto 96
         if (2*si1im*si2im.gt.smxmax) goto 103
         call copvect(work2,plane2d,si1im*si2im)
         call sdcopvect(plane2d,work2,si1im*si2im)
         call rs(si2im,si1im,plane2d,work1d,1,plane2d,work2,work3,error)
         call dscopvect(column,work1d,si2im)	! eigen values
         call revvect(column,si2im)
         sp1d = specw2
         of1d = offset2                   
         sizeimage1d = si2im
         it1d = 0
         call dscopvect(plane2d,plane2d,si1im*si2im) ! eigen vectors
         do i=1,si1im/2
            call copvect(work1d,plane2d((i-1)*si2im+1), si2im)
            call copvect(plane2d((i-1)*si2im+1),
     *                   plane2d((si1im-i)*si2im+1), si2im)
            call copvect(plane2d((si1im-i)*si2im+1), work1d, si2im)
         enddo
         max0 = 0.0
         refresh = 1

      elseif (command.eq.'ROTATION') then
         if (dim.ne.2) goto 97
         if (itype.ne.0) goto 96
         call rotation(plane2d,si1im,si2im,itermax,
     *                 miniter,tolerance,verbose)
         max0 = 0.0
         refresh = 1

      elseif (command.eq.'CLEAN_WAT') then
         if (dim.ne.2) goto 97
         if (si1im.ne.si2im) goto 129
         if (itype.ne.0) goto 96
         call message('Enter the region to be removed')
         dim = 1
         zz = sizeimage1d
         sizeimage1d = si2im
         call getrect(win1,win3,win2,win4,error)
         dim = 2
         sizeimage1d = zz
         call moindo(plane2d,si1im,win1,win2)
         max0 = 0.0
         refresh = 1

      elseif (command.eq.'BACKPROD') then
         if (dim.ne.2) goto 97
         if (itype.ne.0) goto 96
         if (si2im**2 .gt.smxmax) goto 103
         call copvect(work3,plane2d,sizeimage)
         call backprod(plane2d,work3,si1im,si2im)
         si1im = si2im
         sizeimage = si2im**2
         max0 = 0.0
         refresh = 1

      elseif (command.eq.'SQRT_E') then
         if (dim.ne.2) goto 97
         if (itype.ne.0) goto 96
         if (si1im .ne.sizeimage1d) goto 117
         do i=1,si1im
            z = sqrt(abs(column(i)))
            if (debug.ne.0) call gifaoutr('eig : ', z)
            call mltvect(plane2d((i-1)*si2im+1),plane2d((i-1)*si2im+1),
     *                    z,si2im)
         enddo
         max0 = 0.0
         refresh = 1

      elseif (command.eq.'LEFTPROD') then
C left multiplies the 1D buffer with the matrix held in the DATA buffer
         if ( emptydata ) goto 95
         if (dim .ne. 1) goto 99
         if (itype.ne.0) goto 96
         if (dimdat.ne.2) goto 126
         if (sizeimage1d .ne. si2dat) goto 117
         call copvect(work1d,column,sizeimage1d)
         call leftprod(column,datab,work1d,si1dat,sizeimage1d)
         sizeimage1d = si1dat
         max0 = 0.0
         refresh = 1



