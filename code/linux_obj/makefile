#Makefile for GIFA
# this one is for Linux with f2c
#uses perl scripts fpp for fortran preprocessing 
SHELL = /bin/sh

# first remove all definitions, and add ours
.SUFFIXES: 

.SUFFIXES: .o .c .for .f72

#sources are in the directory VPATH, typically set by callers

###################################################################
#START of the user-modifiable part
#macro for compilation

#set DEBUG to Y if you want to compile with debugger support
# typically set by callers
#DEBUG = Y

#CC and FORTRAN are the respective compilers
CC = gcc
FORTRAN = ./fortran

#CFLAGS and FFLAGS are the compilation options (optimisation not included)
#-Wno-implicit-function-declaration 
#CFLAGS = -DF2C -DLINUX -DNOREADLINE -Dis_big_endian -I/usr/X11R6/include -I/usr/include/gdbm_compat -I. -I/home/gifa/Gifa/code/source -mieee-fp
CFLAGS = -DF2C -DLINUX  -Dis_big_endian -I/usr/X11R6/include -I/usr/include/gdbm_compat -I. -I/home/gifa/Gifa/code/source -mieee-fp
FFLAGS =

#COPT and FOPT are optimisation switches used if not DEBUG
COPT = -O2 -fomit-frame-pointer
#COPT = -O2 -mpentium
FOPT = -O

#FPP and FPPFLAGS are for the fortran preprocessor (here home-written in perl)
FPP = $(VPATH)/fpp
FPPFLAGS = -linux  -Kf2c 

# for libraries
LIBRARIAN = @true
# (true is the command that does nothing)
ARADD = -rus
ARCREATE = -cqs

# for linking
#L = gcc -static
L = gcc 
#LDFLAG = -L/usr/X11R6/lib /usr/lib/libf2c.a /usr/X11R6/lib/libXm.a -lXt -lX11 -lgdbm_compat -lm
#LDFLAG = -L/usr/local/lib -Xlinker -Bstatic -lf2c -lXm -lgdbm_compat -Xlinker -Bdynamic -lXt -lX11 -lXext -lXft -lfontconfig -lpng -ljpeg  -lSM -lICE -lm 
LDFLAG = -L/usr/local/lib -Xlinker -lf2c -lXm -lgdbm_compat -Xlinker -lXt -lX11 -lXext  -lSM -lICE -lm -lreadline
#LDFLAG = -L/usr/X11R6/lib /usr/lib/libf2c.a /usr/local/lib/libXm.a -lXt -lX11 -lXext -lgdbm_compat -lm -lreadline 

# END of the user-modifiable part
###################################################################



#then rules
.c.o:
	@echo $<
	@if [ "$(DEBUG)" = "Y" ]; then \
		$(CC) $(CFLAGS) -DDEBUG -g -c $< ; \
	else \
		$(CC) $(CFLAGS) $(COPT) -c $< ; \
	fi

.for.o:
	@echo $<
	@if [ "$(DEBUG)" = "Y" ]; then \
		(cd $(VPATH); $(FPP) $(FPPFLAGS) -D $*) ; \
		mv $(VPATH)/$*.f .; \
		$(FORTRAN) $(FFLAGS) $*.f; \
	else \
		(cd $(VPATH);$(FPP) $(FPPFLAGS) $*) ; \
		mv $(VPATH)/$*.f .; \
		$(FORTRAN) $(FFLAGS) $(FOPT) $*.f; \
		rm -f $*.f; \
	fi

.f72.o:
	@echo $<
	@if [ "$(DEBUG)" = "Y" ]; then \
		(cd $(VPATH); $(FPP) $(FPPFLAGS) -D $*) ; \
		mv $(VPATH)/$*.f .; \
		$(FORTRAN72) $(FFLAGS) $*.f; \
	else \
		(cd $(VPATH);$(FPP) $(FPPFLAGS) $*) ; \
		mv $(VPATH)/$*.f .; \
		$(FORTRAN) $(FFLAGS) $(FOPT) $*.f; \
		rm -f $*.f; \
	fi


# then dependence list

include	$(VPATH)/depend_list
