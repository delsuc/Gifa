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
CC = cc -no-cpp-precomp -fno-strict-aliasing
FORTRAN = ./fortran

#CFLAGS and FFLAGS are the compilation options (optimisation not included)
#CFLAGS = -DF2C -DDARWIN -DNOREADLINE -Dis_big_endian -I/usr/X11R6/include
#CFLAGS = -DF2C -DDARWIN -DNOREADLINE -I/usr/X11R6/include -I/sw/include
CFLAGS = -Wlarger-than-4000 -DF2C -DDARWIN -DNOREADLINE -I/usr/X11R6/include -I/usr/local/include
FFLAGS = 

#COPT and FOPT are optimisation switches used if not DEBUG
COPT = -O3 -fomit-frame-pointer
FOPT = -O


#FPP and FPPFLAGS are for the fortran preprocessor (here home-written in perl)
FPP = $(VPATH)/fpp
FPPFLAGS = -Darwin -Kf2c

# for libraries
LIBRARIAN = ranlib

# for linking
L = cc -g 
# version statique
#LDFLAG = -L/usr/X11R6/lib -L/sw/lib /usr/local/lib/libf2c.a -lXm /usr/X11R6/lib/libXt.a /usr/X11R6/lib/libX11.a -lX11 -lXext -lXp -lICE -lSM  -lm 
# version dynamique
#-Xlinker -multiply_defined -Xlinker warning 
LDFLAG = -flat_namespace -Xlinker -bind_at_load -faltivec -L/usr/X11R6/lib  -L/usr/local/lib  /usr/local/lib/libf2c_G3.a -lXm -lXt -lX11 -lXext -lICE -lSM -lm 

# END of the plateform adapted part
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
