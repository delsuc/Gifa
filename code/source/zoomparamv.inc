C zoomparamv.inc
C
C this file holds the variables for zoom and point

        integer clickmax		! size of the click table
        parameter (clickmax=100)
	real clickt(clickmax,2)         ! click table

	integer  nclick,     ! number of clicks in the table
     *    clickbutton,       ! last clicked mouse button
     *    zoom,        ! 1 if in zoom mode
     *    zo1dl,zo1du,    ! zoom coordinates in 1D
     *    zolf2,zolf1,zour2,zour1,    ! zoom coordinates in 2D
     *    zo3df1l,zo3df1u,zo3df2l,zo3df2u,zo3df3l,zo3df3u !zoom coordinates in 3D
C selection cadre coordinates
        integer selectx1,selecty1,selectx2,selecty2

        character*8 unitx,unity
	
        common/zoomparam/nclick,clickt,
     *        zoom,zo1dl,zo1du,zolf2,zolf1,zour2,zour1,
     *        zo3df1l,zo3df1u,zo3df2l,zo3df2u,zo3df3l,zo3df3u,
     *        clickbutton,unitx,unity,
     *        selectx1,selecty1,selectx2,selecty2

