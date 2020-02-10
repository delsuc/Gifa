transdata : transdata.o cio_lib.o  
	f77 -o transdata -g transdata.o cio_lib.o

transdata.o : transdata.f

cio_lib.o : cio_lib.c

clean :
	rm -f *.o transdata *~
