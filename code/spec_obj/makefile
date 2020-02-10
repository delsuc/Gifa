#Makefile for GIFA
# this one is for f2c / SPEC1/BRUKER
#uses perl scripts fpp for fortran preprocessing 
SHELL = /bin/sh

# first remove all definitions, and add ours
.SUFFIXES: 

.SUFFIXES: .o .c .for

#sources are in the directory VPATH, typically set by callers
VPATH = /d1/people/melanie/gifav4/source/test_make

###################################################################
#START of the user-modifiable part
#macro for compilation

#set DEBUG to Y if you want to compile with debugger support
# typically set by callers
#DEBUG = Y

#CC and FORTRAN are the respective compilers
CC = gcc
FORTRAN = fortran

#CFLAGS and FFLAGS are the compilation options (optimisation not included)
CFLAGS = -DF2C -DSPEC1 -DSYSV -DMips
FFLAGS = 

#COPT and FOPT are optimisation switches used if not DEBUG
COPT = -O
FOPT = -O

#FPP and FPPFLAGS are for the fortran preprocessor (here home-written in perl)
FPP = perl $(VPATH)/fpp 
FPPFLAGS = -f2c -spec1

# for linking
L = cc -systype bsd43
LDFLAG = -L/usr/X11R5/lib -L/usr/local/lib -L/usr/lib -lI77 -lF77 -lXm -lXt -lX11 -lm -lbsd

# END of the user-modifiable part
###################################################################


#then rules
.c.o:
	@echo $<
	@if [ "$(DEBUG)" = "Y" ]; then \
		$(CC) $(CFLAGS) -g -c $(VPATH)/$< ; \
	else \
		$(CC) $(CFLAGS) $(COPT) -c $(VPATH)/$< ; \
	fi
	

.for.o:
	@echo $<
	@if [ "$(DEBUG)" = "Y" -o "$*" = "execute" ]; then \
		(cd $(VPATH); $(FPP) $(FPPFLAGS) -D $*) ; \
		mv $(VPATH)/$*.f .; \
		$(FORTRAN) $(FFLAGS) $*.f; \
	else \
		(cd $(VPATH);$(FPP) $(FPPFLAGS) $*) ; \
		mv $(VPATH)/$*.f .; \
		$(FORTRAN) $(FFLAGS) $(FOPT) $*.f; \
		rm -f $*.f; \
	fi



# then dependence list

include	../depend_list
