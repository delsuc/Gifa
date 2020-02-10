c       Programme ethernet      Version 2 10-mar-90     Bruno Kieffer
C                                                       Patrice Koehl
C                                         modified    M.A.Delsuc
c
c       converts a Bruker Fid file with parameters, which was got on the
c       Vax through ethernet, into a GIFA compatible data file
c
        parameter       (lub=1,luf=2)
        parameter       (nmax = 3*66*1024)
c
cinclude "paramfilev.inc"
c
        character*32    fname1,fname2
c
        byte            bruker(nmax),buffer(nmax)
        byte            buffer1,buffer2,buffer3,xbuffer(6)
c
        integer*2       nn
        integer*4       si,filesize,convert,fsize
        integer*4       brukerdata,ftnmrdata,blocksize
c
        real*4          FTNMR(nmax/3),sf0
        real*4          convertreal
c
c
        external convert, convertreal
c
1       format(a)
c
        brukerdata      = 768
        ftnmrdata       = 256
        blocksize       = 1344
c
        type *,'  Ethernet  V 2.0'
        type *,' '
c
c 1. INPUT OF THE FILENAMES :
c
        write(6,'(''$> nom du fichier bruker a transformer --> '')')
        accept 1,fname1
 
        write(6,'(''$> nom du fichier GIFA                --> '')')
        accept 1,fname2
c
c 2. OPEN CORRESPONDING FILE, WHICH ARE UNFORMATTED :
c
        open(unit= lub,file=fname1,status='old',form='unformatted',
     1    recordtype='variable',readonly)
c
        open(unit=luf,file=fname2,status='new',form='unformatted')
c
c 3. READ THE BRUKER -1 BLOCK THAT CONTAINS THE ACQUISITION PARAMETERS
c
        read(lub)nn,(buffer(n),n=1,nn)
c
c       a. Eliminate the 130 first bytes that correspond to software
c       parameters
c
        do n=1,nn-130
                buffer(n)=buffer(n+130)
        enddo
c
c       b. get the values of some parameters ( filesize, si,...) using
c       "convert" which converts a bruker integer word in a Vax integer * 4
c
        buffer1 = buffer(16)
        buffer2 = buffer(17)
        buffer3 = buffer(18)
        fsize   = convert(buffer1,buffer2,buffer3)
C
        buffer1 = buffer(121)
        buffer2 = buffer(122)
        buffer3 = buffer(123)
        si      = convert(buffer1,buffer2,buffer3)
c
        type *,' '
        if (fsize.gt.65536) then
                type *,' processing a 2D '
        else
                type *,' processing a 1D'
        endif
c
        filesize = ftnmrdata + SI
        nfid =fsize/si
        nbltot = 0
c
c       c. assign the converted paramters to the effective GIFA data buffer :
c
c
        do i = 1,6
                xbuffer(i) = buffer( 3*53 + i)
        enddo
        specw2 = convertreal(xbuffer)
c
        do i = 1,6
                xbuffer(i) = buffer( 3*77 + i)
        enddo
        sf0 = convertreal(xbuffer)

        do i = 1,6
                xbuffer(i) = buffer( 3*57 + i)
        enddo
        o1      = convertreal(xbuffer)

        do i = 1,6
                xbuffer(i) = buffer( 3*59 + i)
        enddo
        o2      = convertreal(xbuffer)

        type *,' '
        type *,' Bruker SI              : ',SI
        type *,' Spectral width         : ',specw2
        type *,' Spectrometer frequency : ',sf0
        type *,' O1                     : ',o1
        type *,' O2                     : ',o2
        type *,' Settimg itype to  0  '
        specw1 = specw2
        offset1 = 0.0
        offset2 = 0.0
        itype = 0
c
c 4. READ FIRST FID AND PARAMETERS
c
c       a.Bruker files are totally unformated, so it is necessary to rewind the
c       bruker file :
c
        rewind(lub)
c
c       b. Define the number of blocks to read according to si
c
        nrecord = 3 * si
        nbl = nrecord/blocksize - 1
        ire=mod(nrecord,blocksize)
        if(ire.ne.0) nbl = nbl + 1
c
c       c.Read the first record ( correspond to record -1 of Bruker ) which
c       contains the acquisition parameters :
c
        read(lub)nn,(buffer(n),n=1,nn)
        do n=1,brukerdata
                buffer(n)= buffer(n+130)
        enddo
c
c       d. Store in FTNMR the 256 FTNMR parameters :
c
        do n=1,ftnmrdata
                nl = 3*(n-1) + 1
                ftnmr(n)=convert(buffer(nl),buffer(nl+1),buffer(nl+2))
        enddo
c
c       e. Read the second record which, again, contain a software header
C       and the beginning of the Fid ....
C
        read(lub) nn,(buffer(n),n=1,nn)
        do n=1,nn-130
                buffer(n)=buffer(n+130)
        enddo
        nb2 = nn - 130
c
c       f. .....Read the end of the Fid ...
C
        nadd = nb2
        do i = 1,nbl
                read(lub) nn,(buffer(nadd + j),j = 1,nn)
                nadd = nadd + nn
        enddo
c
c       g. ....and store it in FTNMR
c
        do i = 1,si
           il = 3*(i-1) + 1
           ftnmr(256+i)=float(convert(buffer(il),buffer(il+1),buffer(il+2)))
        enddo
c
c
        write(luf) si/2,(FTNMR(n),n=ftnmrdata + 1,filesize)
c
        type *,' '
        type *,' -------------------------------- '
        type *,' parameters + FID  1  completed'
c
        nbltot = nbl + 2
        if (nfid.eq.1) goto 1001
c
c 5. READ ALL FIDS FOR A 2D EXPERIMENT
c
c       a. first find how many data have already been read
c
        nfill   = nadd - 3*si
c
c       b. Start the loop over the nfid-1 remaining fid
c
        do nf=1, (nfid-1)
c
c               i.copy the end of the last block at the beginning of the buffer
c
                do j = 1,nfill
                        buffer(j) = buffer(3*si+j)
                enddo
c
c               ii. determine the number of blocks to read to complete the fid
c
                nbl = (3*si - nfill)/blocksize
                ire = mod(3*si - nfill,blocksize)
                if(ire.ne.0) nbl = nbl + 1
                nbltot = nbltot + nbl
c
c               iii. complete corresponding buffer
c
                nadd = nfill
                do i = 1,nbl
                        read(lub) nn,(buffer(nadd+j),j=1,nn)
                        nadd = nadd + nn
                enddo
c
c               iv. evaluates new remain for next fid
c
                nfill = nadd - 3*si
c
c               v. fill ftnmr buffer and write on FTNMR file
C
                do i = 1,si
                        il = 3*(i-1) + 1
                        ftnmr(i)=float(convert(buffer(il),buffer(il+1),
     1                  buffer(il+2)))
                enddo
                write(luf) SI/2,(ftnmr(n),n=1,SI)
c
                if  (mod(nf,16).eq.0) type *,nf ,' Fid completed '
c
        enddo
c

1001    call writeparam(luf)
        close(lub)
        close(luf)
c
        type *,nbltot,' blocks read '
c
        stop
        end
 
 
 
c       Convert.for             Version 1 10/2/1989     Bruno Kieffer
c                                                       Patrice Koehl
c
C       Convert is an integer function that convert a Bruker word
C       in an integer ( a Bruker word is made of 3 bytes - Buffer1, Buffer2
C       and Buffer3 and we simply add a fourth byte in front to build a Vax
C       integer * 4 integer )
C
        integer*4 function convert(buffer1,buffer2,buffer3)
c
        integer*4 int_dum,comp2,i
c
        byte byt_dum(4),buffer1,buffer2,buffer3
c
        equivalence(int_dum,byt_dum)
c
        byt_dum(4) = 0
        byt_dum(3) = buffer1
        byt_dum(2) = buffer2
        byt_dum(1) = buffer3
c
        if (int_dum.lt.8388607) goto 500
c
        comp2=0
        int_dum=int_dum-1
        do 400 i=0,23
        if (int_dum.ge.2**(23-i)) then
        int_dum=int_dum-2**(23-i)
        else
        comp2=comp2+2**(23-i)
        endif
 400    continue
        int_dum=-comp2
c
 500    convert = int_dum
c
        return
        end
 
 
 
c       convertreal.for         version 1 10/2/1989     Bruno Kieffer
c                                                       Patrice Koehl
c
c       This function converts a real double precision bruker word (6 bytes)
c       in an real * 4 Vax word
c
        function convertreal(buffer)
c
        byte            buffer(6)
c
        integer*4       int_dum
        integer         ifl(6)
c
        real*8          xman,yman,zman,temp,expo
c
        index = 3*(n-1) + 1
c
        do 11 i=1,6
                ifl(i)=buffer(i)
                if(ifl(i).lt.0) ifl(i) = ifl(i) + 256
11      continue
c
        xman=ifl(1)/128.+ifl(2)/32768.+ifl(3)/8388608.
c
        itemp=ifl(5).and.31
        temp=float(itemp)
c
        yman=temp/268435456.
        temp=float(ifl(6))
c
        xman=xman+yman+temp/68719476736.
        if(xman.gt.1.0) xman=xman-2.0
c
        expo=8.*float(ifl(4))+float(ifl(5).and.224)/32.
        if(expo.gt.1023.) expo=expo-2048.
c
        convertreal=sngl(xman*(2**expo))
c
        return
        end
