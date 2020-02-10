c       Programme tape          Version 1 20/12/1989    Remi Le Goas
c					 1/10/91	M.A.Delsuc  
c
c       converts a Bruker Fid file with parameters, which was got on the
c       unix machine through tape, into a GIFA compatible data file
c
c	To read a bruker tape on a unix machine, type :
c	dd if=/dev/name_of_tape_device of=name_of_temp_file ibs=1536 \
c	   conv=noerror 
c
        program         tape
        implicit  none 
        common /b/ buffer(1536)
c 
        character*32    fname1,fname2
        character*13    ch
c
        byte            buffer,buf(6)
c
        integer       n,nn,random_read,d0,dt,bruktime,ti,error    
        integer       i1,lub,luf,maxsize,nrec,k,temp,in,date,rflg 
        integer       convert,fsize,dw,texp,indx,i,quaflg
	integer	      crandom_read,lengthname,ll     
c
        real            sw1,sw2,f(128*1024),convertreal,o1,o2,sfreq    
c
        external        convert,convertreal,bruktime
c
 1      format(a)
        luf = 2
        maxsize = 128*1024
c
c 1. INPUT OF THE FILENAMES :
c
       write(6,2)
2      format('> BRUKER file name --> ',$)
       read(*,1) fname1
       call trailing(fname1,lengthname)
c
       write(6,3)
3      format('> GIFA   file name --> ',$)
       read(*,1) fname2

c
c 2. OPEN CORRESPONDING FILES, WHICH ARE UNFORMATTED :
c
        call copen(lub,fname1,lengthname,error)
	if (error.eq.-1) then
		write(*,*) 'error in opening Input file '
		goto 999
	endif
        open (luf,file=fname2,status='new',form='unformatted',err=990)
c
c 3. READ THE BRUKER -1 BLOCK THAT CONTAINS THE ACQUISITION PARAMETERS
c
        nn=0
	ll = 1536
   10   rflg = crandom_read(lub,nn,ll,buffer)
        if (rflg.ne.0) then
         if (rflg.eq.-1) then
           goto 100
         else
           write (6,15)
   15      format (' Erreur de lecture du fichier')
           goto 999
         endif
        endif
c
C recognize the magic header of bruker tape files
        if (buffer(1).eq.71.and.buffer(2).eq.-124.and.buffer(3).eq.-11) 
     +  then
          nrec = 0
          n = 0
          call unpack(buffer(4),ch(1:4))
          call unpack(buffer(7),ch(5:8))
          ch(9:9)='.'
          call unpack(buffer(10),ch(10:13))
          if (ch.eq.' CFGFILE.SYST') then   
            write (6,*) ch  
            nn=nn+1536
            goto 10
          endif 
          if (ch.eq.' ENDFILE.SYST') then
            write (6,192)
  192       format(' End-of-file found.')
            goto 999
          endif
c  
c       b. get the values of some parameters ( filesize, dw,...) using
c       "convert" which converts a bruker integer word in a Alliant
c        integer*4
c 
c   filesize in bytes 
          call lecture(0,120,buf)
          fsize   = convert(buf)
c   dwell time 
          call lecture(0,132,buf)
          dw      = convert(buf)
c   count of experiments done 
          call lecture(0,168,buf)
          texp    = convert(buf)
c   spectral width 
          call lecture(1,159,buf)
          sw2 = convertreal(buf)
c   observation frequency 
          call lecture(1,171,buf)
          o1 = convertreal(buf)
c   decoupling frequency
          call lecture(1,177,buf)
          o2 = convertreal(buf)
c   spectrometer frequency
          call lecture(1,231,buf)
          sfreq = convertreal(buf)
c   quadrature flag
          call lecture(0,237,buf)
          quaflg = convert(buf)
c   temperature
          call lecture(0,255,buf)
          temp = convert(buf)
c   increment for D0
          call lecture(0,282,buf)
          ti = convert(buf)
          in = bruktime(ti)
          sw1 = 1/(2*1.e-6*in)
c   date
          call lecture(0,318,buf)
          date = convert(buf)
c   first delay
          call lecture(0,327,buf)
          dt = convert(buf)
          d0 = bruktime(dt)
c 
          if (fsize.gt.maxsize) then
            write (6,191) fsize
 191        format (' File size of ',i8,'K is too big!')
            goto 999
          endif
c 
          write (6,65) ch,fsize
          write (6,61) sfreq 
          write (6,63) o1,o2
          write (6,64) quaflg 
          write (6,62) texp
          write (6,60) dw,sw2 
          write (6,66) in,sw1
          write (6,67) temp
          write (6,68) date
          write (6,69) d0
  60      format (' Dwell time = ',i6,' usec ',' Spectral width = ', 
     +  f10.4,' Hz')
  61      format (' Spectrometer frequency = ',f10.4,' MHz')
  62      format (' Total count of experiments done = ',i8)
  63      format (' Observation frequency = ',f10.4,' Hz',/,' Decoupling
     + frequency = ',f10.4,' Hz')  
  64      format (' Quadrature flag = ',i6)
  65     format (//,' Filename = ',a13,8x,' Size = ',i6,' Bruker words')
  66      format (' Increment = ',i6,' usec ',' Spectral width = ',f10.4,
     +' Hz')
  67      format (' Temperature = ',i6,' K')
  68      format (' Date = ',i10)
  69      format (' First delay = ',i8,' usec',/)
c
          nn=nn+1536
          goto 10
c
       endif
c
c   convert Bruker data into GIFA data 
c 
       do i = 1,512
          indx=(i-1)*3
          call lecture(0,indx,buf)
          i1 = convert(buf)
          n=n+1
          f(n)=float(i1)
c   write a record (GIFA assumes size is # of complex points)
          if (n.ge.fsize) then
            nrec=nrec+1
            write (luf) fsize/2,(f(k),k=1,fsize)
            n=0
          endif
       enddo 
       nn=nn+1536
       goto 10
c
 100   write (luf) sw1,0,sw2,0,0
       close(lub)
       close(luf)
       write (6,110) nrec
 110   format (' File closed; ',i6,' records ')
       goto 999
c
 990   write(*,*) 'error while opening output file'
       goto 999
c
 999   stop
       end
 
 
 
c       Convert.for             Version 1 20/12/1989    Remi Le Goas  
c
C       Convert is an integer function that convert a Bruker word
C       in an integer. A Bruker word is made of 3 bytes - machin(1),(2) et 
C       (3) and we simply add a fourth byte behind to build an Alliant 
C       integer*4 (which is different from a VAX!).
C
        integer*4 function convert(machin)
        integer*4 int_dum,k  
        byte byt_dum(4),machin(3)
        equivalence(int_dum,byt_dum)
c
        byt_dum(4) = 0
        do k=1,3
          byt_dum(k) = machin(k)
        enddo 
c
        convert = int_dum/256
c
        return
        end
 
 
c
c       convertreal.for         version 1 10/2/1989     Bruno Kieffer
c                                                       Patrice Koehl
c
c       This function converts a real double precision bruker word (6 bytes)
c       in an real*4 Alliant (or VAX) word
c
        function convertreal(buffer)
c
        byte            buffer(6)
        integer*4       int_dum
        integer         ifl(6)
        real*8          xman,yman,zman,temp,expo
c
        do i=1,6
                ifl(i)=buffer(i)
                if(ifl(i).lt.0) ifl(i) = ifl(i) + 256
        enddo   
        xman=ifl(1)/128.+ifl(2)/32768.+ifl(3)/8388608.
        itemp=iand(ifl(5),31)
        temp=float(itemp)
        yman=temp/268435456.
        temp=float(ifl(6))
        xman=xman+yman+temp/68719476736.
        if(xman.gt.1.0) xman=xman-2.0
        expo=8.*float(ifl(4))+float(ifl(5).and.224)/32.
        if(expo.gt.1023.) expo=expo-2048.
c
        convertreal=sngl(xman*(2**expo))
c
        return
        end
   
   
c  
c       subroutine unpack.for
c
c       unpack converts Bruker characters coded with 4 bits to 8-bit
c       ASCII  characters.
c 
        subroutine unpack (b,ch)
        byte b(3),bc(4),bi(4)
        integer*4 ih1,ih2,ih3
        character*4 c,ch
        equivalence (bi,ih1)
        equivalence (bc,c)  
        bi(1)=0
        do i=2,4
         bi(i)=b(i-1)
        enddo   
        ih2=ih1
        do i=1,4
         ih1=ih1/64
         ih3=ih1*64
         bc(5-i)=ih2-ih3
         ih2=ih2/64
         if (bc(5-i).eq.0) bc(5-i)=32
         if (bc(5-i).lt.31) bc(5-i)=bc(5-i)+64
        enddo 
        ch=c
        return
        end     
 
 
c
c     subroutine lecture.for 
c
c     lecture reads bytes from a buffer array of Bruker data at a 
c     precise index (3 bytes for a Bruker integer, 6 for a Bruker real).
c 
      subroutine lecture(l,index,buf)
c  
      common /b/ buffer(1536)
      integer l,index
      byte    buf(6),buffer
c
      do k=1,3
        buf(k) = buffer(index+k)
      enddo
      if (l.eq.1) then
        do k=4,6
          buf(k) = buffer(index+k)
        enddo
      endif
c
      return
      end 
  
 
c     function bruktime.for
c
c      bruktime converts time coded in binary Bruker
c      format to an integer.
c
      function bruktime(t)
c
      integer*4 t,exp,mant
      
      exp=ibits(t,0,3)
      mant=ibits(t,3,11)
      bruktime=mant*(10**exp)/10
c
      return
      end
 
c************************************************************
C  Removes trailing blanks at the end of string st of length l
C
c************************************************************
      subroutine trailing(st,l)
      integer l,i
      character*(*) st
      if (st.eq.' ') then
         i = 1
      else
         I = Len(St)
10       If (St(I:I).Eq.' ') Then
            I = I-1
            If (I.Eq.1) Goto 20
            Goto 10
         endif
      endif
20    l = i
      return
      end

      
