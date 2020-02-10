
      elseif (command.eq.'BLOCBASE') then
         call getint2(blocbase,error)
         if (blocbase.lt.0 .or. blocbase.gt.4096 ) then
            blocbase = 1024
            goto 98
         endif

      elseif (command.eq.'DEGRE') then
         call getint2(degre,error)
         if (degre.lt.0 .or. degre.gt.degremax ) then
            degre = degremax
            goto 98
         endif         

      elseif (command.eq.'BLCITER') then
         call getint2(blciter,error)
         if (blciter.lt.1 .or. blciter.gt.blcitermax ) then
            blciter = 1
            goto 98
         endif         

      elseif (command.eq.'BLCU') then
         call getreal2(ldbu,error)

      elseif (command.eq.'BLCV') then
         call getreal2(ldbv,error)

      elseif (command.eq.'BLCW') then
         call getreal2(ldbw,error)

      elseif (command.eq.'SDB') then
         call getreal2(sdb,error)

      elseif (command.eq.'SDS') then
         call getreal2(sds,error)

      elseif (command.eq.'SCB') then
         call getreal2(scb,error)

      elseif (command.eq.'SCS') then
         call getreal2(scs,error)

      elseif (command.eq.'LEVELSEGM2') then
         call getreal2(seuilsegm2,error)
         if (seuilsegm2.lt.0) then
            seuilsegm2 = 0
            goto 98
         endif

      elseif (command.eq.'LEVELHYSTE') then
         call getreal2(seuilhyste,error)
         if (seuilhyste.lt.0) then
            seuilhyste = 0
            goto 98
         endif

      elseif (command.eq.'MORPHOB') then
         call getreal2(morphob,error)
         if (morphob.lt.0) then
            morphob = 0
            goto 98
         endif

      elseif (command.eq.'MORPHOS') then
         call getreal2(morphos,error)
         if (morphos.lt.0) then
            morphos = 0
            goto 98
         endif

      elseif (command.eq.'WINMA') then
         call getreal2(fenma,error)
         if (fenma.lt.0) then
            fenma = 1
            goto 98
         endif

      elseif (command.eq.'WINMA2') then
         call getreal2(fenma2,error)
         if (fenma2.lt.0) then
            fenma2 = 1
            goto 98
         endif

      elseif (command.eq.'WINLIN') then
         call getreal2(fenlin,error)
         if (fenlin.lt.0) then
            fenlin = 1
            goto 98
         endif

      elseif (command.eq.'ITERMA2') then
         call getint2(iterma2,error)
         if (iterma2.lt.0) then
            iterma2 = 1
            goto 98
         endif

      elseif (command.eq.'WINSEGM2') then
         call getreal2(fensegm2,error)
         if (fensegm2.lt.0) then
            fensegm2 = 1
            goto 98
         endif

      elseif (command.eq.'DCITER') then
         call getint2(nueeiter,error)
         if (nueeiter.lt.0) then
            nueeiter = 1
            goto 98
         endif

      elseif (command.eq.'DCALGO') then
         call message( '  +1: With value')
         call message( ' +10: With 1st derivative')
         call message( '+100: With 2sd derivatives')
         call getint2(nueealgo,error)
         if (nueealgo.ne.1 .and. nueealgo.ne.10 .and.
     *nueealgo.ne.11 .and. nueealgo.ne.100 .and.
     *nueealgo.ne.101 .and. nueealgo.ne.110 .and.
     *nueealgo.ne.111) then
            nueealgo = 2
            goto 98
         endif

      elseif (command.eq.'DCDISTANCE') then
         call message( '   0: Norm 1')
         call message( '   1: Euclidian distance')
         call getint2(nueedistance,error)
         if (nueedistance.ne.0 .and. nueedistance.ne.1) then
            nueedistance = 0
            goto 98
         endif

      elseif (command.eq.'DCFACTOR') then
         call getreal2(nueefactor,error)
         if (nueefactor.lt.0) then
            nueefactor = 1
            goto 98
         endif

      elseif (command.eq.'BCORRP') then
         call message( '   --- Unit control --- (BLOCBASE)')
         call getint2(blocbase,error)
         if (blocbase.lt.0 .or. blocbase.gt.4096 .or. error.ne.0) then
            blocbase = 1024
            goto 98
         endif                  
         call message( '   --- Iterations control ---')
         call message( '   Number maximum of iterations ? (BLCITER)')
         call getint2(blciter,error)
         if (blciter.lt.1 .or. blciter.gt.blcitermax
     *       .or.error.ne.0) then
            blciter = 1
            goto 98
         endif
         call message( '   Level ? (BLCW)')
         call getreal2(ldbw,error)	
         if (ldbw.lt.0 .or.error.ne.0) then
            ldbw = 0
            goto 98
         endif 
         call message( '   --- Initial smooth of data --- (SMOOTH1)')
         call message( '   0: Without')
         call message( '  +1: Moving Average')
         call message( ' +10: Hysterisis')
         call getint2(lisse,error)
         if (lisse.ne.0 .and. lisse.ne.1 .and. 
     *       lisse.ne.10 .and. lisse.ne.11 .or.error.ne.0) then
            lisse = 0
            goto 98
         endif
         if (lisse - (lisse/10)*10 .eq. 1) then
            call message( '   Number of points ? (WINMA)')
            call getreal2(fenma,error)
            if (fenma.lt.1 .or.error.ne.0) then
               fenma = 1
               goto 98
            endif
         endif
         if (lisse/10 .eq. 1) then
            call message( '   Level ? (LEVELHYSTE)')
            call getreal2(seuilhyste,error)
            if (seuilhyste.lt.0 .or.error.ne.0) then
               seuilhyste = 0
               goto 98
            endif 
         endif
         call message( '   --- First segmentation --- (SEGM1)')
         call message( '   0: Without')
         call message( '   1: Standard Deviation alone')
         call message( '   2: Standard Deviation with 1st derivative')
         call message( '   3: Threshold with 1st & 2sd derivatives')
         call message( '   4: Nuees Dynamiques') 
         call message( ' +10: Morphological mathematics filtering')
         call getint2(segm1,error)
         if ((segm1.lt.0 .or. segm1.gt.4) .and.
     *(segm1.lt.11 .or. segm1.gt.14) .or.error.ne.0) then
            segm1 = 1
            goto 98
         endif
         if (segm1.eq.1 .or. segm1.eq.2 .or. 
     *segm1.eq.11 .or. segm1.eq.12) then
            call message( '   BLCU ?')
            call getreal2(ldbu,error)
            if (ldbu.lt.0 .or.error.ne.0) then
               ldbu = 4.0
               goto 98
            endif
            call message( '   BLCV ?')
            call getreal2(ldbv,error)
            if (ldbv.lt.0 .or.error.ne.0) then
               ldbv = 2.0
               goto 98
            endif
         endif
         if (segm1.eq.3 .or. segm1.eq.13) then
            call message( '   SDB ?')
            call getreal2(sdb,error)
            call message( '   SCB ?')
            call getreal2(scb,error)
            call message( '   SDS ?')
            call getreal2(sds,error)
            call message( '   SCS ?')
            call getreal2(scs,error)
            if (error.ne.0) goto 98
         endif
         if (segm1.eq.4 .or. segm1.eq.14) then
            call message( '   Number of iterations ? (DCITER)')
            call getint2(nueeiter,error)
            if (nueeiter.lt.0 .or.error.ne.0) then
               nueeiter = 1
               goto 98
            endif
            call message( 
     *'   Dynamic clusters algorithm : (DCALGO)')
            call message( '     +1: With value')
            call message( '    +10: With 1st derivative')
            call message( '   +100: With 2sd derivatives')
            call getint2(nueealgo,error)
               if (nueealgo.ne.1 .and. nueealgo.ne.10 .and.
     *              nueealgo.ne.11 .and. nueealgo.ne.100 .and.
     *              nueealgo.ne.101 .and. nueealgo.ne.110 .and.
     *              nueealgo.ne.111 .or.error.ne.0) then
               nueealgo = 2
               goto 98
            endif
            call message( 
     *'   Dynamic clusters distance : (DCDISTANCE)')
            call message( 
     *'      0: Norm 1')
            call message( '      1: Euclidian distance')
            call getint2(nueedistance,error)
            if (nueedistance.ne.0 .and. nueedistance.ne.1
     *          .or.error.ne.0) then
               nueedistance = 0
               goto 98
            endif
            call message( '   Dynamic clusters factor : (DCFACTOR)')
            call getreal2(nueefactor,error)
            if (nueefactor.lt.0 .or.error.ne.0) then
               nueefactor = 1
               goto 98
            endif
         endif
         if ((segm1/10).eq.1) then
            call message(
     *'   Number of points for baseline (MORPHOB)')  
            call getreal2(morphob,error)
            if (morphob.lt.0 .or.error.ne.0) then
               morphob = 0
               goto 98
            endif
            call message('   Number of points for signal (MORPHOS)')
            call getreal2(morphos,error)
            if (morphos.lt.0 .or.error.ne.0) then
               morphos = 0
               goto 98
            endif
         endif
         call message('   --- Second segmentation --- (SEGM2)')
         call message( '   0: Without')
         call message( '   1: Interactif')
         call message( '   2: Automatic')
         call getint2(segm2,error)
         if (segm2.lt.0 .or. segm2.gt.2  .or.error.ne.0) then
            segm2 = 0
            goto 98
         endif
         if (segm2.eq.1) then
            call message('   Do not forget to define the window !')
         endif
         if (segm2.eq.2) then
            call message('   Level ? (LEVELSEGM2)')
            call getreal2(seuilsegm2,error)
            if (seuilsegm2.lt.0 .or.error.ne.0) then
               seuilsegm2 = 0
               goto 98
            endif
            call message('   Number of points ? (WINSEGM2)') 
            call getreal2(fensegm2,error)
            if (fensegm2.lt.1 .or.error.ne.0) then
               seuilsegm2 = 1
               goto 98
            endif
         endif
         call message('   --- Baseline estimation --- (APPROX)')
         call message( '   0: Moving average')
         call message( '   1: Legendre')
c         call message( '   2: Tchebytchev')
c         call message( '   3: Fourrier')
         call message( 
     *' +00: Signal to zero')
         call message( 
     *' +10: Linear interpolation of signal')
          call message( '+100: Elastic effect')
         call getint2(approx,error)
c         if ((approx.lt.0 .or. approx.gt.3) .and.
         if (approx.ne.0 .and. approx.ne.1 
     *.and. approx.ne.11 .and. approx.ne.10 
     *.and. approx.ne.100 .and. approx.ne.110 .or.error.ne.0) then
            approx = 1
            goto 98
         endif
         if (approx/10 - (approx/100*10) .eq.1) then
            call message('   Number of points ? (WINLIN)')
            call getreal2(fenlin,error)
            if (fenlin.lt.1 .or.error.ne.0) then
               fenlin = 1
               goto 98
            endif       
         endif
         if (approx.eq.0 .or. approx.eq.10) then
            call message('   Number of points ? (WINMA2)')
            call getreal2(fenma2,error)
            if (fenma2.lt.1 .or.error.ne.0) then
               fenma2 = 1
               goto 98
            endif
            call message('   Number of iterations ? (ITERMA2)')
            call getint2(iterma2,error)
            if (iterma2.lt.1 .or.error.ne.0) then
               iterma2 = 1
               goto 98
            endif
         else
            call message('   Degree of polynomial ? (DEGRE)')
            call getint2(degre,error)
            if (degre.lt.0 .or. degre.gt.degremax  .or.error.ne.0) then
               degre = degremax
               goto 98
            endif         
         endif

      elseif (command.eq.'BCORRP0') then
#include "linecor2i.inc"

      elseif (command.eq.'BCORRP1') then
         blocbase = 1024
         lisse = 11
         fenma = 2
         seuilhyste = 1e-2
         blciter = 1
         segm1 = 14
         nueealgo = 10
         nueeiter = 30
         nueefactor = 1e4
         morphob = 1
         morphos = 0
         segm2 = 0
         approx = 10 
         fenlin = 10
         fenma2 = 5
         iterma2 = 2

      elseif (command.eq.'BCORRP?') then     
         call gifaout( ' --- Unit control --- (BLOCBASE)')
         call gifaouti('Number of points for calculation of windows : ',
     * blocbase)
         call gifaout( '   --- Iterations control ---')
         call gifaouti('   Number maximum of iterations (BLCITER) : ',
     *                  blciter) 
         call gifaoutr('   Level : (BLCW) ',ldbw)
         call gifaout( '   --- Initial smooth of data --- (SMOOTH1)')
         if (lisse.eq.0) then
            call gifaout( '   0: Without')
         elseif (lisse - (lisse/10)*10 .eq. 1) then
            call gifaout( '  +1: Moving Average')
            call gifaoutr( '   Number of points : (WINMA) ', fenma)
         endif
         if (lisse/10 .eq. 1) then
            call gifaout( ' +10: Hysterisis')
            call gifaoutr('   Level : (LEVELHYSTE) ', seuilhyste)
         endif
         call gifaout( '   --- First segmentation --- (SEGM1)')
         if (segm1.eq.0) then
            call gifaout( '   0: Without')
         elseif (segm1.eq.1) then
            call gifaout( '   1: Standard Deviation alone')
         elseif (segm1.eq.11) then
            call gifaout( 
     *'  11: Standard Deviation & Morphological mathematics filtering ')
         elseif (segm1.eq.2) then
            call gifaout( 
     *'   2: Standard Deviation with 1st derivative')
         elseif (segm1.eq.12) then
            call gifaout( 
     *'  12: Standard Deviation with 1st derivative & 
     * Morphological mathematics filtering')
         elseif (segm1.eq.3) then
            call gifaout( '   3: Threshold with 1st & 2sd derivatives')
         elseif (segm1.eq.13) then
            call gifaout( '  13: Threshold with 1st & 2sd derivatives &
     * Morphological mathematics filtering')
         elseif (segm1.eq.4) then
            call gifaout('   4: Dynamic clusters')
         elseif (segm1.eq.14) then
            call gifaout('  14: Dynamic clusters &
     * Morphological mathematics filtering')
         endif
         if (segm1.eq.1 .or. segm1.eq.2 .or. 
     *segm1.eq.11 .or. segm1.eq.12) then
           call gifaoutr('   BLCU : ', ldbu)
           call gifaoutr('   BLCV : ', ldbv)
         endif
         if (segm1.eq.3) then
           call gifaoutr('   SDB : ', sdb)
           call gifaoutr('   SCB : ', scb)
           call gifaoutr('   SDS : ', sds)
           call gifaoutr('   SCS : ', scs)
         endif
         if (segm1.eq.4 .or. segm1.eq.14) then
            call gifaout ('    Dynamic clusters algorithm (DCALGO) :')
            if (nueealgo.eq.1) write(mes,*) '      1: With value'
            if (nueealgo.eq.10) 
     *        write(mes,*) '   10: With 1st derivative'
            if (nueealgo.eq.11) write(mes,*) 
     *'     11: With value and 1st derivative'
            if (nueealgo.eq.100) write(mes,*) 
     *'   100: With 2sd derivative'
            if (nueealgo.eq.101) write(mes,*) 
     *'   101: With value and 1st derivative'
            if (nueealgo.eq.110) write(mes,*) 
     *'   110: With value and 2sd derivative'
            if (nueealgo.eq.111) write(mes,*) 
     *'   111: With value and 1st and 2sd derivative'
            call gifaout(mes)
            call gifaout('      Distance (DCDISTANCE) : ')
            if (nueedistance.eq.0) call gifaout('         0: Norm 1')
            if (nueedistance.eq.1)
     *                call gifaout('         1: Euclidian distance') 
            call gifaouti('      Number of iterations (DCITER) : ',
     *                           nueeiter)
            call gifaoutr('      Factor (DCFACTOR) : ', nueefactor)
         endif
         if ((segm1/10).eq.1) then
           call gifaout('      Morphological filtering (SEGM1) : ')
           call gifaoutr('         Number of points for baseline
     * (MORPHOB) : ',morphob)
           call gifaoutr('         Number of points for signal
     * (MORPHOS) : ',morphos)
         endif
         call gifaout('   --- Second segmentation --- (SEGM2) ')
         if (segm2.eq.0) then
            call gifaout( '   0: Without')
         elseif (segm2.eq.1) then
            call gifaout( '   1: Interactif')
         elseif (segm2.eq.2) then
            call gifaout( '   2: Automatic')
            call gifaoutr('   Level : ', seuilsegm2)
         endif
         call gifaout('   --- Baseline estimation --- (APPROX) ')
         if ((approx-(approx/10)*10).eq.0) then 
            call gifaout( '   0: Moving average')
         else
            call gifaout( '   1: Legendre')
            call gifaouti('   Degree of polynomial (DEGRE) : ', degre) 
         endif
         if ((approx/10 - (approx/100)*10) .eq. 0) then
            call gifaout(' +00: Signal to zero')
         else
            call gifaout(' +10: Linear interpolation of signal')
         endif
         if ((approx/100) .eq. 1) then
            call gifaout( '+100: Elastic effect')
         endif
         if (approx/10 - (approx/100)*10 .eq.1) then
            call gifaoutr('   Number of points (WINLIN) : ',fenlin)
         endif
         if (approx.eq.0 .or. approx.eq.10 .or.
     *       approx.eq.100 .or. approx.eq.110) then
            call gifaoutr('   Number of points (WINMA2) : ', fenma2)
            call gifaouti('   Number of iterations (ITERMA2) : ',
     *                        iterma2)
          endif

      elseif (command.eq.'SMOOTH1') then
         call message( '   0: Without')
         call message( '  +1: Moving Average')
         call message( ' +10: Hysterisis')
         call getint2(lisse,error)
         if (lisse.ne.0 .and. lisse.ne.1 
     *.and. lisse.ne.10 .and. lisse.ne.11 .or. error.ne.0) then
            lisse = 0
            goto 98
         endif

      elseif (command.eq.'SEGM1') then
         call message( '   0: Without')
         call message( '   1: Standard Deviation alone')
         call message( '   2: Standard Deviation with 1st derivative')
         call message( '   3: Threshold with 1st & 2sd derivatives')
         call message( '   4: Nuees Dynamiques')
         call message( ' +10: Morphological mathematics filtering')
         call getint2(segm1,error)
         if ((segm1.lt.0 .or. segm1.gt.4) .and.
     *       (segm1.lt.11 .or.segm1.gt.14) .or. error.ne.0) then
            segm1 = 1
            goto 98
         endif

      elseif (command.eq.'SEGM2') then
         call message( '   0: Without')
         call message( '   1: Interactif')
         call message( '   2: Automatic')
         call getint2(segm2,error)
         if (segm2.lt.0 .or. segm2.gt.2  .or. error.ne.0) then
            segm2 = 0
            goto 98
         endif

      elseif (command.eq.'APPROX') then
         call message( '   0: Moving average')
         call message( '   1: Legendre')
c         call message( '   2: Tchebytchev')
c         call message( '   3: Fourrier')
         call message(' +00: Signal to zero')
         call message(' +10: Linear interpolation of signal')
         call message( '+100: Elastic effect')
         call getint2(approx,error)
c         if ((approx.lt.0 .or. approx.gt.3) .and.
         if (approx.ne.0 .and. approx.ne.1 .and.
     *       approx.ne.10 .and. approx.ne.100 
     * .and. approx.ne.110 .and. approx.ne.11  .or. error.ne.0) then
            approx = 1
            goto 98
         endif


      elseif (command.eq.'BCORR') then   ! Base line correction
c        if (itype.ne.0) goto 96
        call message( 'Enter algorithm:  (1) linear (2) spline' //
     * ' (3) automatic : ')
        win4 = 1
        call getint2(win4,error)
        if (win4.le.0 .or. win4.gt.3 .or. error.ne.0) goto 98

c first let's do polynomial
        if (win4.eq.3) then
           if (dim.eq.1) then
             if (checkdim(1,it1d,dim)) goto 96
             yy = 2
           elseif (dim.eq.2) then
             call message( 'In F1 or F2 ? ' )
             yy = 1
             call getdim(yy,error)
             if ((yy.ne.1).and.(yy.ne.2)) goto 98
             if (checkdim(yy,itype,dim)) goto 96
           else
             call message( 'In F1, F2 of F3 ? ' )
             yy = 1
             call getdim(yy,error)
             if ((yy.ne.1).and.(yy.ne.2).and.(yy.ne.4)) goto 98
             if (checkdim(yy,it3d,dim)) goto 96
           endif
           if (error.ne.0) goto 98
           if (dim.eq.1) then
             if (sizeimage1d .gt. bcmax) goto 103
             call linecor2(debug,verbose,yy)  
           elseif (dim.eq.2) then
             if (yy.eq.1) then
               if (si1im.gt.bcmax) goto 103
             else
               if (si2im.gt.bcmax) goto 103
             endif
             call linecor2(debug,verbose,yy)
           else
             if (yy.eq.1) then
               if (si3d1.gt.bcmax) goto 103
             elseif (yy.eq.2) then
               if (si3d2.gt.bcmax) goto 103
             else
               if (si3d3.gt.bcmax) goto 103
             endif
             call linecor2(debug,verbose,yy)
           endif
           max0=0.0
           refresh=1

c now let's do linear or spline
        else
        call message( 'Enter radius for each point')
        zz = 1
        call getint2(zz,error)
        if (zz.lt.0) goto 98
        if (dim.eq.1) then
          if (zz.gt.sizeimage1d) goto 98
          win1 = sizeimage1d
          yy = 2
          if (checkdim(1,it1d,dim)) goto 96
        elseif (dim.eq.2) then
          call message( 'in F1 or F2 ? ' )
          yy = 1
          call getdim(yy,error)
          if (yy.eq.1) then
             win1 = si1im
          elseif (yy.eq.2) then
             win1 = si2im
          else
             goto 98
          endif
          if (checkdim(yy,itype,dim)) goto 96
        elseif (dim.eq.3) then
          call message( 'in F1, F2, or F3 ?' )
          yy = 1
          call getdim(yy,error)
          if (yy.eq.1) then
             win1 = si3d1
          else if (yy.eq.2) then
             win1 = si3d2
          else if (yy.eq.4) then
             win1 = si3d3
          else
             goto 98
          endif
          if (checkdim(yy,it3d,dim)) goto 96
        endif
C   Enter index points
        i = 1              
61        call message( 'Enter index for each point, enter 0 to end.')
            if (nclick.ge.i) then
               if (yy.eq.1) then
                  xx = closest(clickt(i,2))
               else
                  xx = closest(clickt(i,1))
               endif
            else
               xx = 0
            endif
            call getcoord(xx,yy,error)
            if (xx.eq.0) goto 62
            if (xx.lt.0) goto 108
            if (xx.gt.win1) goto 108
            tindex(i) = xx
            i = i+1
            if (i.gt.szindex) goto 105
            goto 61
62      if ((i.le.1.and.win4.eq.1) .or.
     *      (i.le.3.and.win4.eq.2)) goto 98
        i = i-1
        call bsort(tindex,i)
        if (verbose.ne.0) then
           write(*,*) (tindex(j),j=1,i)
           write(journal,*) (tindex(j),j=1,i)
        endif
        if (dim.eq.1) then
          call linecorr(column,sizeimage1d,tindex,i,zz,win4)
        else if (dim.eq.2) then
          if (yy.eq.1) then
            if (si1im.gt.sizemax) goto 103
            do j=1,si2im
              call getcol(work1d,plane2d,j,si1im,si2im)
              call linecorr(work1d,si1im,tindex,i,zz,win4)
              call putcol(work1d,plane2d,j,si1im,si2im)
            enddo
          else
            do j=1,si1im
               call linecorr(plane2d(si2im*(j-1)+1),
     *                       si2im,tindex,i,zz,win4)
            enddo           
          endif
        elseif (dim.eq.3) then
          if (yy.eq.1) then
            if (si3d1.gt.sizemax) goto 103
            do j=1,si3d2*si3d3
              call getcol(work1d,image,j,si3d1,si3d2*si3d3)
              call linecorr(work1d,si3d1,tindex,i,zz,win4)
              call putcol(work1d,image,j,si3d1,si3d2*si3d3)
            enddo
          elseif (yy.eq.2) then
            if (si3d2.gt.sizemax) goto 103
            do k=1,si3d1
              do j=1,si3d3
                call getcol(work1d,image((k-1)*si3d2*si3d3+1),
     *                                           j,si3d2,si3d3)
                call linecorr(work1d,si3d2,tindex,i,zz,win4)
                call putcol(work1d,image((k-1)*si3d2*si3d3+1),
     *                                           j,si3d2,si3d3)
              enddo
            enddo
          else  if (yy.eq.3) then
            do j=1,si3d1*si3d2
               call linecorr(image(si3d3*(j-1)+1),
     *                       si3d3,tindex,i,zz,win4)
            enddo           
          endif
        endif
        max0=0.0
        refresh = 1
      endif


