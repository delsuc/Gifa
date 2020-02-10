c       Programme transdata      Version 1 20/12/1989    Remi Le Goas
c					   1/11/91	M.A.Delsuc  
c
c       converts a Bruker Fid file with parameters, which was got on the
c       Machine through network, into a GIFA compatible data file
c
        program         transdata 
        implicit  none 
c 
        character*80    fname1,fname2,fname3
c
        byte            buffer(4*1024),buf(8)
        common /b/ buffer
c
        integer   n,k,nn,d0,dt,bruktime,ti,random_read,    
     * i1,maxsize,nrec,temp,in,date,rflg,lub,error,ll,crandom_read,
     * convert,fsize,dw,texp,indx,i,quaflg,lengthname
c
        real            sw1,sw2,f(128*1024),convertreal,o1,o2,sfreq    
c
        external        convert,convertreal,bruktime
c
        maxsize = 128*1024
c
c 1. INPUT OF THE FILENAMES :
c
       write(*,2)
2      format ('> BRUKER .SER+ file name --> ',$)
       read(*,1) fname1
       call trailing(fname1,lengthname)
c
       write(*,3)
3      format ('> GIFA         file name --> ',$)
       read(*,1) fname2
       write(*,4)
4      format ('> Parameter    file name --> ',$)
       read(*,1) fname3
 1      format(a)

c
c 2. OPEN CORRESPONDING FILES, WHICH ARE UNFORMATTED :
c
        call copen(lub,fname1,lengthname,error)
	if (error.eq.-1) then
		write(*,*) 'error in opening Input file '
		goto 999
	endif
        open (2,file=fname2,status='new',form='unformatted',err=990)
        open (10,file=fname3,status='new',form='formatted',err=990)
c
c 3. READ THE BRUKER -1 BLOCK THAT CONTAINS THE ACQUISITION PARAMETERS
c
        nn=0
	ll = 1024
   10   rflg = crandom_read(lub,nn,ll,buffer)
        if (rflg.ne.0) then
         if (rflg.eq.-1) then
           goto 100
	else
           goto 991
         endif
        endif
c
c       b. get the values of some parameters ( filesize, dw,...) using
c       "convert" which converts a bruker X32 integer word in a Alliant
c        integer*4
c 
c   filesize in bytes 
          call lecture(0,160,buf)
          fsize   = convert(buf)
c   dwell time 
          call lecture(0,176,buf)
          dw      = convert(buf)
c   count of experiments done 
          call lecture(0,224,buf)
          texp    = convert(buf)
c   spectral width 
          call lecture(1,212,buf)
          sw2 = convertreal(buf)
c   observation frequency 
          call lecture(1,228,buf)
          o1 = convertreal(buf)
c   decoupling frequency
          call lecture(1,236,buf)
          o2 = convertreal(buf)
c   spectrometer frequency
          call lecture(1,308,buf)
          sfreq = convertreal(buf)
c   quadrature flag
          call lecture(0,316,buf)
          quaflg = convert(buf)
c   temperature
          call lecture(0,340,buf)
          temp = convert(buf)
c   increment for D0
          call lecture(0,376,buf)
          ti = convert(buf)
          in = bruktime(ti)
c   in=0 if it is a 1d-experiment
          if (in.ne.0) then
            sw1 = 1/(2*1.e-6*in)
          else
            write (*,'(a)') ' You are processing a 1D-experiment '
          endif
c   date
          call lecture(0,424,buf)
          date = convert(buf)
c   first delay
          call lecture(0,436,buf)
          dt = convert(buf)
          d0 = bruktime(dt)
c 
          if (fsize.gt.maxsize) then
            write (*,191) fsize
 191        format (' File size of ',i8,' is too big!')
            goto 999
          endif
c
C show results on screen and on a file.
          write (*,65) fsize
          write (*,61) sfreq 
          write (*,63) o1,o2
          write (*,64) quaflg 
          if (in.ne.0) then
           write (*,62) texp
          endif
          write (*,60) dw,sw2 
          if (in.ne.0) then
           write (*,66) in,sw1
          endif
          write (*,67) temp
          write (*,68) date
          write (*,69) d0
c
	  write (10,*) 'Parameters for the file : ',fname2
	  write(10,*)
          write (10,65) fsize
          write (10,61) sfreq 
          write (10,63) o1,o2
          write (10,64) quaflg 
          if (in.ne.0) then
           write (10,62) texp
          endif
          write (10,60) dw,sw2 
          if (in.ne.0) then
           write (10,66) in,sw1
          endif
          write (10,67) temp
          write (10,68) date
          write (10,69) d0

  60      format (' Dwell time = ',i6,' usec ',' Spectral width = ', 
     +  f10.4,' Hz')
  61      format (' Spectrometer frequency = ',f10.4,' MHz')
  62      format (' Total count of experiments done = ',i8)
  63      format (' Observation frequency = ',f10.4,' Hz',/,' Decoupling
     + frequency = ',f10.4,' Hz')  
  64      format (' Quadrature flag = ',i6)
  65      format (/,' Size = ',i6,' words')
  66      format (' Increment = ',i6,' usec ',' Spectral width = ',f10.4,
     +' Hz')
  67      format (' Temperature = ',i6,' K')
  68      format (' Date = ',i10)
  69      format (' First delay = ',i8,' usec',/)
c

        nn=nn+1024
80      rflg = crandom_read(lub,nn,fsize*4,buffer)
        if (rflg.ne.0) then
         if (rflg.eq.-1) then
           goto 100
         else
           goto 991
         endif
        endif

c
c   convert Bruker data into GIFA data 
c 
         do i = 1,fsize 
          indx=(i-1)*4
          call lecture(0,indx,buf)
          i1 = convert(buf)
          f(i)=float(i1)
         enddo 
         nrec=nrec+1
c   write a record (GIFA assumes size is # of complex points)
         write (2) fsize/2,(f(k),k=1,fsize)
         nn=nn+fsize*4 
         goto 80 

 100     write (2) sw1,0,sw2,0,0
         close(1)
         close(2)
         write (*,110) nrec
	 write(10,110) nrec
 110     format (' File closed; ',i6,' records ')
         goto 999
c    
 990   write(*,*) 'error while opening output file'
       goto 999
c
 991   write(*,*) 'error while reading the file'
       goto 999
c
 999   close(10)
       call exit
       end
 
 
 
c       Convert.for             Version 1 20/12/1989    Remi Le Goas  
c
C       Convert is an integer function that convert a Bruker X32 word
C       in an integer. A Bruker X32 word is made of 4 bytes - machin(1),(2) 
C       (3)et(4) and we simply select the first 3 bytes,swap them and add 
c       a fourth byte to build an Alliant integer*4(which is different 
c       from a Vax!).
C
        integer*4 function convert(machin)
        integer*4 int_dum,k  
        byte byt_dum(4),machin(4)
        equivalence(int_dum,byt_dum)
c
        do k=1,3
          byt_dum(k) = machin(4-k)
        enddo 
        byt_dum(4) = 0
c
        convert = int_dum/256
c
        return
        end
 
 
c
c       convertreal.for         version 1 10/2/1989     Bruno Kieffer
c                                                       Patrice Koehl
c
c       This function converts a real double precision bruker X32 word 
c       (8 bytes),in an real*4 Alliant (or VAX) word
c
        real*4 function convertreal(buffer)
c
        byte            buffer(8)
        integer*4       int_dum,itemp,i
        integer         ifl(6)
        real*8          xman,yman,zman,temp,expo
c
        do i=1,3
                ifl(i)=buffer(4-i)
                if(ifl(i).lt.0) ifl(i) = ifl(i) + 256
        enddo   
        do i=4,6
                ifl(i)=buffer(11-i)
                if (ifl(i).lt.0) ifl(i)=ifl(i)+256
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
        integer*4 ih1,ih2,ih3,i
        character*4 c,ch
        equivalence (bi,ih1)
        equivalence (bc,c)  
        bi(1)=0
        do i=2,4
         bi(i)=b(5-i)
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
c     precise index (4 bytes for a Bruker X32 integer, 8 for a Bruker X32
c     real).
c 
      subroutine lecture(l,index,buf)
c  
      common /b/ buffer(4*1024)
      integer l,index,k
      byte    buf(8),buffer
c
      do k=1,4
        buf(k) = buffer(index+k)
      enddo
      if (l.eq.1) then
        do k=5,8
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
      integer*4 function bruktime(t)
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
 
      
