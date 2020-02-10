C This file is a part of the RMN3D program
C     This software has been developped by the NMR Group in GIF/Yvette.
C
C     Authors :       M.A.Delsuc, M.Robin
C                     NMR Laboratory,
C                     ICSN - CNRS
C                     91190 GIF/YVETTE   FRANCE
C
C     This software cannot be used unless the users have been fully 
C licensed to do so from the above laboratory.
C     This file cannot be copied, duplicated or used in any other program,
C without written permission from the authors.
C

C**************************************************************************
       subroutine putplane(plane,cube,
     *                     size1,size2,size3,index) 
c IN	: plane,size1,size2,size3,index
c INOUT	: cube
c
c puts a plane F1 (F2-F3) at "index" into a cube(size1,size2,size3)
c
       implicit none

       integer size1,size2,size3,index
       real plane(size3*size2),cube(size3*size2*size1)
                            
       call copvect(cube((index-1)*size3*size2+1),
     *                              plane,size3*size2)
       return
       end

C**************************************************************************
       subroutine putplane2(plane,cube,
     *                     size1,size2,size3,index) 
c IN	: plane,size1,size2,size3,index
c INOUT	: cube
c
c puts a plane F2 (F1-F3) at "index" into a cube(size1,size2,size3)
c

       implicit none

       integer size1,size2,size3,index,i
       real plane(size3*size1),cube(size3*size2*size1)
                            
       do i=1,size1
          call copvect(cube((index-1)*size3+(i-1)*size3*size2+1),
     *                                 plane((i-1)*size3+1),size3)
       enddo
       return
       end

C**************************************************************************
       subroutine putplane3(plane,cube,
     *                     size1,size2,size3,index) 
c IN	: plane,size1,size2,size3,index
c INOUT	: cube
c
c puts a plane F3 (F1-F2) at "index" into a cube(size1,size2,size3)
c

       implicit none

       integer size1,size2,size3,index,i
       real plane(size2*size1),cube(size3*size2*size1)
   
       do i=1,size1
          call putcol(plane((i-1)*size2+1),
     *                cube((i-1)*size2*size3+1),index,size2,size3)
       enddo
       return
       end
