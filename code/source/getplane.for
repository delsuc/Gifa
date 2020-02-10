C This file is a part of the RMN3D program
C     This software has been developped by the NMR Group in GIF/Yvette.
C
C     Authors :       M.A.Delsuc, V.Stoven, M.Robin
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
       subroutine getplane(plane,cube,
     *                     size1,size2,size3,index)
c IN	: cube,size1,size2,size3,index
c OUT	: plane
c
c extracts the plan "index" orthogonal to F1 dim from "cube"
c

       implicit none

       integer size1,size2,size3,index
       real plane(size3*size2),cube(size3*size2*size1)
                            
       call copvect(plane,
     *          cube((index-1)*size3*size2+1),size3*size2)
       return
       end

C**************************************************************************
       subroutine getplane2(plane,cube,
     *                     size1,size2,size3,index) 
c IN	: cube,size1,size2,size3,index
c OUT	: plane
c
c extracts the plan "index" orthogonal to F2 dim from "cube"


       implicit none

       integer size1,size2,size3,index,i
       real plane(size3*size1),cube(size3*size2*size1)
                            
       do i=1,size1
          call copvect(plane((i-1)*size3+1),
     *          cube((index-1)*size3+(i-1)*size3*size2+1),size3)
       enddo
       return
       end

C**************************************************************************
       subroutine getplane3(plane,cube,
     *                     size1,size2,size3,index) 
c IN	: cube,size1,size2,size3,index
c OUT	: plane
c
c extracts the plan "index" orthogonal to F3 dim from "cube"
c

       implicit none

       integer size1,size2,size3,index,i
       real plane(size2*size1),cube(size3*size2*size1)
   
       do i=1,size1
          call getcol(plane((i-1)*size2+1),
     *           cube((i-1)*size2*size3+1),index,size2,size3)
       enddo
       return
       end
