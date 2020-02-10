nmrlink : nmrlink.o cio_lib.o
	f77 -o nmrlink nmrlink.o cio_lib.o

nmrlink.o : nmrlink.f

cio_lib.o : cio_lib.c

clean : 
	rm -f *.o nmrlink *~
