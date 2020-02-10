#if defined(MACHTEN) || defined(SPEC1)
#  define BSD
#else
#  define SYS5
#endif

#include <math.h>
#include <stdio.h>
#include <sys/types.h>
#include <time.h>
#include <sys/times.h>
#include <limits.h>
#include <unistd.h>
#include <string.h>
#include "util.h"

/**************************************************************/
/*
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     cconvert_string(st2,st1,l)                                         C
C     Function to convert a C string into a fortran string.              C
C                                                                        C
C     st1 = string to convert. (C)                                       C
C     st2 = converted string.  (fortran)                                 C
C     l = length of st.                                                  C
C                                                                        C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
*/
void cconvert_string(st2,st1,l)

#ifdef VMS
/*  ATTENTION   A REECRIRE */
char st2[];
struct dsc$descriptor_s *st1;
int l;
{
   strncpy(st2,st1->dsc$a_pointer,l);
   st2[l] = '\0';
}
#else
char *st1,*st2;
int *l;
{
  *l = strlen(st1);
  strcpy(st2,st1);
}
#endif
/**************************************************************/

/*
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     convert_string(st2,st1,l)                                          C
C     Function to convert a fortran string into a C string.              C
C                                                                        C
C     st1 = string to convert. (Fortran)                                 C
C     st2 = converted string.  (C)                                       C
C     l = length of st.                                                  C
C                                                                        C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
*/

void convert_string(st2,st1,l)

#ifdef VMS
char st2[];
struct dsc$descriptor_s *st1;
int l;
{
   strncpy(st2,st1->dsc$a_pointer,l);
   st2[l] = '\0';
}
#else
char *st1,*st2;
int l;
{
  strncpy(st2,st1,l);
  st2[l] = '\0';
}
#endif

/*
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     adjust_cstring(st,l)                                               C
C   Function to delete end blank char end put /0 at the end of C string. C
C                                                                        C
C     st = string to adjust. (C)                                         C
C     l = length of st.                                                  C
C                                                                        C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
*/

/**************************************************************/
adjust_cstring(st,l) 
char *st;
int l;
{
	int i;
	i = l-1;
	while(st[i] == ' ')i--;
	st[i+1] = '\0';	
}

/**************************************************************/

  static int  nprog,iprog;

void INPROG(i)
int *i;

/*
set of command to show the progress of a long run.
M.A.Delsuc


 will write :
in progress 0%....25%....50%....75%....100%
 
 one dot each 1/20th of the run

initialisation : initinprog(n)
where   int *n   is the total numer of count to go

run :            inprog(i)
where   int *i   is the current index.

inprog(i) can be called as you wish :
     very sparsely or very often, it does not matter.
*/
{
  while ( ((*i*20/nprog) >= iprog ) & (iprog<20) )
  {
    iprog++; 
    switch (iprog)
    { case (5) :
        printf("25%%");  break;
      case(10) :
        printf("50%%");  break;
      case(15) :
        printf("75%%");  break;
      case(20) :
        printf("100%%\n");  break;
      default :
        printf(".");
      }
    fflush(stdout);
  }
}

/**************************************************************/

void INITINPROG(n)

int *n;
/*
  initialises for INPROG
*/

{

   nprog = *n;
   iprog = 0;
   printf("In Progress : 0%%");
   fflush(stdout);
 }


#include <unistd.h>
#include <signal.h>
#include <sys/time.h>

/**************************************************************/
void do_nothing()
{ }

extern  void WIN_REFRESH();

/**************************************************************/
void CWAITASEC(sec)
float *sec;

/* cwaitasec(sec) will wait take the process to sleep for a time
sec (in second) Called from fortran -waitasec-
*/

#ifdef SYS5
{ int    micro;
  struct itimerval tnew,old;
  struct sigaction snew;

  sigaction(SIGALRM, NULL, &snew);
  
  snew.sa_handler = *do_nothing;

  sigaction(SIGALRM, &snew, NULL);

  micro = 1000000*(*sec);

  tnew.it_value.tv_sec = 0;
  tnew.it_value.tv_usec = micro;
  tnew.it_interval.tv_sec = 0;
  tnew.it_interval.tv_usec = micro;  /* sometimes pause is launched too late */

  setitimer (ITIMER_REAL, &tnew, &old);
  pause();
  
/* disable TIMER */
  tnew.it_value.tv_usec = 0;
  tnew.it_interval.tv_usec = 0; 
  setitimer (ITIMER_REAL, &tnew, &old);

}
#else
 { }
#endif



/**************************************************************/
void GIFAPRINT(st,l)

char *st;
int  *l;

{ char stl[256];

   convert_string(stl,st,*l);
   printf("%s\n",stl);
   fflush(stdout);

}

/**************************************************************/
void SHOWPROMPT(l)

int  *l;

{
  if (*l == 0) printf("Gifa>");
  if (*l == 1) printf("\\Gifa>");
   fflush(stdout);
}

/**************************************************************/
void GIFAFLUSH(unit)

FILE    **unit;

{
  fflush(*unit);
}


/**************************************************************/
/* f2c does not know of chdir */
#ifdef F2C
int chdir_(path,lp)
char *path;
int  *lp;

{ char stl[256];

   convert_string(stl,path,*lp);
   return(chdir(stl));
}

double dtime_ (float *time) /* f2c does not know dtime */
{
  static float utime=0, stime=0;
  struct tms buffer;

  if (times(&buffer) == -1) return -1;
  time[0] = (float)buffer.tms_utime/CLK_TCK-utime;
  time[1] = (float)buffer.tms_stime/CLK_TCK-stime;
  utime = (float)buffer.tms_utime/CLK_TCK;
  stime = (float)buffer.tms_stime/CLK_TCK;
  return time[0]+time[1];
}
#endif

/**************************************************************/
void GIFA_EXIT(code)

int *code;
{
#ifdef SUN
#ifndef F2C
#  include <sys/ieeefp.h>
          char mode[32], out[32], in[32];
          ieee_flags("clearall",mode, in, &out);
#endif
#endif

  exit(*code);
}
