c Modif Terez 18/08/97
c Amoeba table containing the adresses of the chained list of amoeba pixels
c for independent handling of each peak amoeba
      integer maxamb
      parameter (maxamb = 64) ! maximum size of an amoeba=4*maxamb*maxamb
      integer liste(4*maxamb*maxamb) 
c      equivalence(liste(1),  work3(1))

      common/amb/liste

