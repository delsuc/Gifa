        subroutine moindo(spectre,n,min,max)

        implicit none
        integer n,min,max,i,j
        real spectre(n)

c routine faite pour la symetrisation d'une bande d'eau

      
        do i=1,n
         do j=min,max
          spectre(i,j) = spectre (j,i)
         enddo
        enddo  

       return 
