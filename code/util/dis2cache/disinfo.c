/************************************
 * Information about DISNMR spectra *
 ************************************/
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <time.h>
#include <math.h>

#include "nmrpar.h"
#include "conv.h"

#define DEBUG 0

int main (int argc, char **argv)
{
  int		fd;
  int		tmp;
  DISPAR	dispar;
  char		filename[256], oprname[256], comment[256];
  time_t	time;

  if (argc < 2)
  {
    printf ("Usage: %s <filename>\n", argv[0]);
    exit (0);
  }

  fd = open (argv[1], O_RDONLY);
  if (fd == -1)
  {
    printf ("Cannot open file: %s\n", argv[1]);
    exit (0);
  }

#if DEBUG
  printf ("DISPAR size = %d bytes\n", sizeof(dispar));
#endif
  
  if (read (fd, &dispar, sizeof(dispar)) != sizeof(dispar))
  {
    printf ("Cannot read parameter block of %s\n", argv[1]);
    close (fd);
    exit (0);
  }

  /****************************
   * Printing information ... *
   ****************************/

  conv_filename (filename, &dispar.U1.FDTINFO[3]);
  printf ("NAME:    %s\n", filename);
  tmp = conv_integer (&dispar.U1.FDTINFO[15]);
  if (tmp < 0) tmp *= -256;
  tmp /= 1024;
  printf ("SIZE:    %d K\n", tmp);
  conv_sixbit (oprname, &dispar.U1.FDTINFO[36]);
  printf ("OPR:     %s\n", oprname);
  conv_ascii (comment, dispar.U1.COMMTARY, sizeof(dispar.U1.COMMTARY));
  printf ("TI:      %s\n", comment);
  conv_filename (filename, (char *)&dispar.U1.AQREC.SAUNM1);
  printf ("AU PROG: %s\n",    filename);
  printf ("SI:      %d K\n",  conv_integer (&dispar.U1.AQREC.ASIZE)/1024);
  printf ("NE DONE: %d\n",    conv_integer (&dispar.U1.AQREC.TEXP));
  printf ("NS DONE: %d\n",    conv_integer (&dispar.U1.AQREC.SWPCOM));
  printf ("SF0:     %g Hz\n", conv_real    (&dispar.U1.AQREC.SFREQ0));
  printf ("SF:      %g Hz\n", conv_real    (&dispar.U1.AQREC.SFREQ));
  printf ("SW:      %g Hz\n", conv_real    (&dispar.U1.AQREC.SPWIDTH));
  time = conv_time (&dispar.U1.AQREC.ACQDATE, &dispar.U1.AQREC.ACQTIME);
  printf ("DATE:    %s\n", ctime (&time));
  
  close (fd);
  exit (0);
}
/*************************
 * E N D   O F   F I L E *
 *************************/
