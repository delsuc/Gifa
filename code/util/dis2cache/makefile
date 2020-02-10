#Makefile for Gifa utilities

# macros for the C routines
CC = gcc
# gcc under Linux
#COPT = -O2 -m486 -ffast-math
# gcc on SPARCstation 10
#COPT = -O2 -msupersparc -ffast-math

# for SUN, SGI, IBM
#CFLAGS = -DUNDERSCORE
# for Linux/f2c
CFLAGS = -DLINUX -DF2C
# for SPARC Solaris f77
#CFLAGS = -DSUN -DSOLARIS -DUNDERSCORE
# for SPARC Solaris f2c
#CFLAGS = -DSUN -DSOLARIS -DF2C

# for linking
L = $(CC)
#flags for Linux
#LDFLAGS = -s -ffast-math -lm
LDFLAGS = -lm

all	: dis2cach disinfo

# link
dis2cach : dis2cach.o conv.o cache_mad.o util.o reversebyte.o
	$L -o dis2cach dis2cach.o conv.o cache_mad.o util.o reversebyte.o $(LDFLAGS)
disinfo	 : disinfo.o conv.o
	$L -o disinfo disinfo.o conv.o $(LDFLAGS)

#then code itself
dis2cach.o  : dis2cach.c nmrpar.h conv.h cache_mad.h util.h
	$(CC) -c  $(CFLAGS) $(COPT) dis2cach.c
disinfo.o   : disinfo.c nmrpar.h conv.h
	$(CC) -c  $(CFLAGS) $(COPT) disinfo.c
conv.o	    : conv.c nmrpar.h conv.h
	$(CC) -c  $(CFLAGS) $(COPT) conv.c
cache_mad.o : cache_mad.c cache_mad.h util.h
	$(CC) -c  $(CFLAGS) $(COPT) cache_mad.c
util.o	    : util.c util.h
	$(CC) -c  $(CFLAGS) $(COPT) util.c
reversebyte.o	: reversebyte.c
	$(CC) -c  $(CFLAGS) $(COPT) reversebyte.c
clean :
	rm -f *.o dis2cach disinfo *~ #*#
