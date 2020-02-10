tape : tape.o cio_lib.o
	f77 -o tape -g tape.o cio_lib.o

tape.o : tape.f
	f77 -c -g tape.f

cio_lib.o : cio_lib.c
	cc -c -g cio_lib.c
