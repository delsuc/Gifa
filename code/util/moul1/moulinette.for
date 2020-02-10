C program to code and decode GIFA "readh writeh" files in the standard
C portable compact file format "reads writes" of the 2.5 version
C This portable compact file format is an ASCII storage, with no loss
C of space compared to binary format. This file format is machine independent
C and can even be E-Mailed, if you take care not to send too large files.
C********************************************************
       program  moulinette


      implicit none

      include 'sizepb.inc'
      include 'paramfile.inc'
C      include 'controlc.inc'
      include 'sizeparam.inc'

      integer      datadim,code,noise,error
      character*80 namein,nameout

      datadim=1
      code=1
      namein=' '
      nameout=' '

10    write(*,20)
      call getint(code)
20    format(' Do you want to compact(1) or decompact(2) data ?')
      if ((code.ne.1).and.(code.ne.2))  goto 10 

30    write(*,40)
      call getint(datadim)
40    format(' Enter the dimension of your data, 1D(1) or 2D(2):')

      if (code.eq.1) then
        write(*,*) ' name of data to compact: '
        call getstring(namein)
        write(*,*) ' name of compacted data: '
        call getstring(nameout)
       if (datadim.eq.1) then
        call lhare(image,sizeimage,namein,noise,error)
        if (error.eq.1) goto 60
        call writetc(image,1,1,sizeimage,1,max0,nameout)
        if (error.eq.1) goto 60
       elseif (datadim.eq.2) then
        call lhare2d(image,si1im,si2im,namein,error,noise)
        if (error.eq.1) goto 60   
        call writetc(image,1,si1im,si2im,2,max0,nameout)
        if (error.eq.1) goto 60
       else 
        goto 30
       endif
      else
        write(*,*) ' name of data to decompact: '
        call getstring(namein)
        write(*,*) ' name of decompacted data: '
        call getstring(nameout)
        call readtc(image,namein,si1im,si2im,si3im,dim,max0,error)
        if (error.eq.1) goto 60
        write(*,*) 'dim = ',dim
       if (dim.eq.1) then
        sizeimage=si3im
        call whare2d(image,1,sizeimage,nameout)   
        if (error.eq.1) goto 60
       elseif (dim.eq.2) then
        call whare2d(image,si2im,si3im,nameout) 
        if (error.eq.1) goto 60
       else
        goto 50
       endif
      endif
      write(*,*) ' successful completion'
      stop
      
50    write(*,*) ' data are not the right kind'
      stop      
      
60    write(*,*) ' error during the process'
      stop
      end
