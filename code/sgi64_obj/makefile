#Makefile for GIFA
# this one is for SGI
#uses perl scripts fpp for fortran preprocessing 
SHELL = /bin/sh

# first remove all definitions, and add ours
.SUFFIXES: 

.SUFFIXES: .o .c .for

#sources are in the directory VPATH, typically set by callers
#VPATH = /d1/people/melanie/gifav4/source/test_make

###################################################################
#START of the user-modifiable part
#macro for compilation

#set DEBUG to Y if you want to compile with debugger support
# typically set by callers
#DEBUG = Y

#CC and FORTRAN are the respective compilers
CC = cc
FORTRAN = f77 

#CFLAGS and FFLAGS are the compilation options (optimisation not included)
CFLAGS = -n32 -mips4 -DSGI64 -DUNDERSCORE -DNOREADLINE -I/usr/local/lib
FFLAGS = -n32 -mips4

#COPT and FOPT are optimisation switches used if not DEBUG
COPT = -O 
FOPT = -O 

#FPP and FPPFLAGS are for the fortran preprocessor (here home-written in perl)
#FPP = /usr/bsd/rsh comte cd $(VPATH) ";" ./fpp 
FPP = ./fpp
#FPPFLAGS = -sgi -Kreadline
FPPFLAGS = -sgi

# for linking
L = f77 
# Modif jluc 4/3/96 
LDFLAG = -g -lXm -lXt -lX11 -lPW
#LDFLAG = -g -lmalloc -lXm -lXt -lX11 -lPW /usr/local/lib/libreadline.a -lcurses

LIBRARIAN = true

# END of the user-modifiable part
###################################################################


#then rules
.c.o:
	@echo $<
	@if [ "$(DEBUG)" = "Y" ]; then \
		$(CC) $(CFLAGS) -DDEBUG -g -c $(VPATH)/$< ; \
	else \
		$(CC) $(CFLAGS) $(COPT) -c $(VPATH)/$< ; \
	fi
	

.for.o:
	@echo $<
	@if [ "$(DEBUG)" = "Y" ] ;  then \
		(cd $(VPATH); $(FPP) $(FPPFLAGS) -D $*) ; \
		mv $(VPATH)/$*.f .; \
		case $* in \
		  "execute")  $(FORTRAN) $(FFLAGS) -Nn24000 -g -c $*.f ;; \
		  *) $(FORTRAN) $(FFLAGS) -g -c $*.f ; \
		esac; \
	else \
		(cd $(VPATH); $(FPP) $(FPPFLAGS) $*) ; \
		mv $(VPATH)/$*.f .; \
		case $* in \
		  "execute")  $(FORTRAN) $(FFLAGS) -Nn24000 -c $*.f ;; \
		  "variable")  $(FORTRAN) $(FFLAGS) $(FOPT) -Olimit 1100 -c $*.f ;; \
		  *) $(FORTRAN) $(FFLAGS) $(FOPT) -c $*.f;; \
		esac; \
		rm -f $*.f ; \
	fi



# then dependence list

include	/usr/local/gifa/source/depend_list
