C ps300p.inc
C holds the commands for the ps300-dependent functions
C
#vms
#vms      elseif (command.eq.'PSINIT') then
#vmsC reinitializes the ps300 processor: all graphical objects are

#vmsC destroyed
#vms         if ( disp .eq. 0 ) then
#vms            write(*,*) 'Are you working on a PS?(Y/N)'
#vms            call getstring2(st,error)
#vms            if (st.ne.'Y') goto 125
#vms         endif
#vms         call pattch( modify,vaxerrhan )
#vms         call pinit(vaxerrhan )
#vms         call pdtach( vaxerrhan )
#vms         disp = 0
#vms
#vms      elseif (command.eq.'PSAXIS') then
#vmsC ps300
#vmsC syntax: axis n(1,2,3). shows the data along the chosen axis
#vms        if (disp.ne.1) goto 119
#vms        call message('which axis? (f1,f2,f3)')
#vms        zz = 3
#vms        call getdim(zz,error)
#vms           if (zz .eq. 4) then
#vms                 zz = 3
#vms           else if (zz.eq.3)then
#vms              zz = 4
#vms           endif
#vms        if (zz.lt.1 .or. zz.gt.3) goto 98
#vms        call pattch ( modify,vaxerrhan )
#vms        call showalong( zz )
#vms        call pdtach ( vaxerrhan )
#vms
#vms      elseif (command.eq.'PSCLIP') then
#vmsC ps300
#vmsC makes the z-clipping active
#vms        if (disp.ne.1) goto 119
#vms        call pattch ( modify,vaxerrhan )
#vms        call setclipon( fobjects )
#vms        call pdtach ( vaxerrhan )
#vms
#vms      elseif (command.eq.'PSDECLIP') then
#vmsC ps300
#vmsC makes the z-clipping inactive
#vms        if (disp.ne.1) goto 119
#vms        call pattch ( modify,vaxerrhan )
#vms        call setclipoff( fobjects )
#vms        call pdtach ( vaxerrhan )
#vms
#vms
#vms      elseif (command.eq.'PSSTEREO') then
#vmsC ps300
#vmsC shows a stereo view of the data
#vms        if (disp.ne.1) goto 119
#vms        call pattch( vaxerrhan )
#vmsC        call stereo( fobjects )
#vms        call pdtach( vaxerrhan )
#vms
Ccvms      else if (command.eq.'PSDISP') then
Ccvms              if ( disp .eq. 0 ) then
Ccvms                 write(*,*) 'Are you working on a PS?(Y/N)'
Ccvms                 call getstring2(st,error)
Ccvms                 if (st.ne.'Y') goto 125
Ccvms                 call pattch ( modify,vaxerrhan )
Ccvms                 call ppurge ( vaxerrhan )
Ccvms                 call pinit( vaxerrhan )
Ccvms                 call objects( name,nuage,iobjects,fobjects,colour )
Ccvms                 call aides3d( iobjects )
Ccvms                 call tree3d( iobjects,fobjects )
Ccvms                 call network3d( fobjects )
Ccvms                 call pdtach ( vaxerrhan )
Ccvms                 disp = 1
Ccvms              endif
Ccvms              call pattch ( modify,vaxerrhan )
Ccvms              call initlevels( nuage )
Ccvms              call coloursetup( level,colour,nuage )
Ccvms              if (max0.eq.0.0) then
Ccvms                 write(*,*) 'Looking for the Maximum intensity'
Ccvms                 call mnxvect(mini,maxi,zz,i,image,sizeimage3d)
Ccvms                 if (maxi.eq.0.0) maxi = 1.0
Ccvms                 max0 = maxi
Ccvms              endif
Ccvms              call fill3d( image,si3d1,si3d2,si3d3,nuage,
Ccvms     *                     sign,max0,level,scale,it3d )
Ccvms              call ppurge( vaxerrhan )
Ccvms              call pdtach ( vaxerrhan )
Ccvms
Ccvms
C****************************************************************

