/*****************************************************************
 * Converting DISNMR 1D FID or 2D .SER to the native GIFA format *
 *****************************************************************/
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
#include "cache_mad.h"
#include "util.h"

#define DEBUG 0

extern void INPROG	(int *);
extern void INITINPROG	(int *);

int main (int argc, char **argv)
{
  int		i, j, fd, len, mode, err, dim, si1, si2, si3, ne, nd0,
  		globex, type, itype, xlow, xup, ylow, yup;
  double	sw1, sw2, of1, of2, sf0, sf1, sf2, in, maxy;
  char		str[256];
  DISPAR	dispar;
  INTEGER	*ibuf;
  float		*fbuf;
  DATASET	*data;

  if (argc < 3)
  {
    printf ("Usage: %s <DISNMR file> <GIFA file>\n", argv[0]);
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

  /************************
   * Initialize cache ... *
   ************************/
  CH_INITIALISE ();

  len  = strlen (argv[2]);
  mode = CF_WRITE;
  err  = 0;
  CH_CREATE (&data, argv[2], &len, &mode, &err);
  if (err)
  {
    printf ("Cannot create %s\n", argv[2]);
    CH_CLOSE (&data, &err);
    close (fd);
    exit (0);
  }

  /************************
   * Query parameters ... *
   ************************/
  conv_sixbit (str, &dispar.U1.FDTINFO[9]);		/* extension	*/
  if (strcmp (str, "SER")) dim = 1;
  else dim = 2;
  si1 = conv_integer (&dispar.U1.AQREC.ASIZE);		/* SI		*/
  if (dim == 2)						/* 2D .SER	*/
  {
    si2 = si1;
    si1 = conv_integer (&dispar.U1.AQREC.TEXP);		/* exps done	*/
    ne  = conv_integer (&dispar.U1.AQREC.CEXP);		/* NE		*/
    printf ("Number of experiments (NE): %d\n", ne);
    printf ("Number of experiments done: %d\n", si1);
    printf ("Number of experiments to convert [%d]: ", si1);
    str[0] = '\0';
    gets (str);
    sscanf (str, "%d", &si1);
  }
  else si2 = 1;						/* 1D FID	 */
  si3 = 1;						/* 3D impossible */

#if DEBUG
  printf ("Calling CH_SETUP (%d, %d, %d, %d)\n", dim, si1, si2, si3);
#endif
  
  CH_SETUP (&data, &dim, &si1, &si2, &si3, &err);
  if (err)
  {
    printf ("Cannot setup parameters for %s\n", argv[2]);
    CH_CLOSE (&data, &err);
    close (fd);
    exit (0);
  }

  type = H_DOUBLE;
  sw2  = sw1 = conv_real (&dispar.U1.AQREC.SPWIDTH);	/* SW		*/
  len  = strlen ("Specw1");
  if (dim == 2)
  {
    in   = conv_delay (&dispar.U1.AQREC.PD0INC);	/* IN = 1/2/ND0/SW1 */
    nd0  = 1;
    printf ("Number of incrementable delays (ND0) [%d]: ", nd0);
    str[0] = '\0';
    gets (str);
    sscanf (str, "%d", &nd0);
    sw1 = 1.0/nd0/in;					/* 2 * DISNMR's SW1 */
    CH_PUTPARAM_TYPE (&data, &sw2, "Specw2", &len, &type, &err);
  }
  CH_PUTPARAM_TYPE (&data, &sw1, "Specw1", &len, &type, &err);

#if DEBUG
  printf ("Specw1 = %g Specw2 = %g\n", sw1, sw2);
#endif
  
  of2 = of1 = conv_real (&dispar.U1.AQREC.OFFSET);	/* O1-SR-1/2/SW	*/
  len = strlen ("Offset1");
  if (dim == 2)
  {
    printf ("Spectrum offset along the F1 axis: [%g]: ", of1);
    str[0] = '\0';
    gets (str);
    sscanf (str, "%lg", &of1);
    CH_PUTPARAM_TYPE (&data, &of2, "Offset2", &len, &type, &err);
  }
  CH_PUTPARAM_TYPE (&data, &of1, "Offset1", &len, &type, &err);

#if DEBUG
  printf ("Offset1 = %g Offset2 = %g\n", of1, of2);
#endif
  
  sf0 = conv_real (&dispar.U1.AQREC.SFREQ0);		/* SF0		*/
  len = strlen ("Frequency");
  CH_PUTPARAM_TYPE (&data, &sf0, "Frequency", &len, &type, &err);

  sf2 = sf1 = conv_real (&dispar.U1.AQREC.SFREQ);	/* SF		*/
  len = strlen ("Freq1");
  if (dim == 2)
  {
    printf ("Basic spectrometer frequency: %g MHz\n", sf0);
    printf ("Spectrometer frequency along the F1 axis: [%g]: ", sf1);
    str[0] = '\0';
    gets (str);
    sscanf (str, "%lg", &sf1);
    CH_PUTPARAM_TYPE (&data, &sf2, "Freq2", &len, &type, &err);
  }
  CH_PUTPARAM_TYPE (&data, &sf1, "Freq1", &len, &type, &err);

#if DEBUG
  printf ("Frequency = %g Freq1 = %g Freq2 = %g\n", sf0, sf1, sf2);
#endif

  maxy = 0.0;						/* not yet known */
  len  = strlen ("Absmax");
  CH_PUTPARAM_TYPE (&data, &maxy, "Absmax", &len, &type, &err);

  type  = H_INT;
  itype = 0;						/* perhaps SEQ	 */
  len   = strlen ("Type");
  CH_PUTPARAM_TYPE (&data, &itype, "Type", &len, &type, &err);

#if DEBUG
  printf ("Type = %d\n", itype);
#endif

  if (err)
  {
    printf ("Cannot setup parameters for %s\n", argv[2]);
    CH_CLOSE (&data, &err);
    close (fd);
    exit (0);
  }

  /********************
   * Convert data ... *
   ********************/
  globex = conv_integer (&dispar.U1.AQREC.GLOBEX) - 23;	/* NC is important */

  switch (dim)
  {
  case 1:						/* 1D FID */
    ibuf = malloc (si1*sizeof(*ibuf));
    fbuf = malloc (si1*sizeof(*fbuf));
    if (ibuf == NULL || fbuf == NULL)
    {
      printf ("No memory\n");
      CH_CLOSE (&data, &err);
      close (fd);
      exit (0);
    }
    xlow = 1;
    xup  = si1;
    mode = CB_WRITE;
    if (read (fd, ibuf, si1*sizeof(*ibuf)) != si1*sizeof(*ibuf))
    {
      printf ("Cannot read %s\n", argv[1]);
      CH_CLOSE (&data, &err);
      close (fd);
      exit (0);
    }
    for (j=0; j<si1; j++)
      fbuf[j] = (float) ldexp ((double)conv_integer(&ibuf[j]), globex);
    CH_AC1DAREA (fbuf, &data, &xlow, &xup, &mode, &err);
    break;
  case 2:						/* 2D .SER */
    INITINPROG (&si1);
    ibuf = malloc (si2*sizeof(*ibuf));
    fbuf = malloc (si2*sizeof(*fbuf));
    if (ibuf == NULL || fbuf == NULL)
    {
      printf ("No memory\n");
      CH_CLOSE (&data, &err);
      close (fd);
      exit (0);
    }
    xlow = 1;
    xup  = si2;
    mode = CB_WRITE;
    for (i=0; i<si1; i++)				/* for each row */
    {
      if (read (fd, ibuf, si2*sizeof(*ibuf)) != si2*sizeof(*ibuf))
      {
	printf ("Cannot read block %d of %s\n", i+1, argv[1]);
	CH_CLOSE (&data, &err);
	close (fd);
	exit (0);
      }
      for (j=0; j<si2; j++)
	fbuf[j] = (float) ldexp ((double)conv_integer(&ibuf[j]), globex);
      ylow = yup = i+1;
      CH_AC2DAREA (fbuf, &data, &ylow, &xlow, &yup, &xup, &mode, &err);
      INPROG (&yup);
    }
  }
  
  CH_CLOSE (&data, &err);				/* done */
  close (fd);
  exit (0);
}

/************************************************
 * To resolve extern reference from cache_mad.c *
 ************************************************/
void GIFAOUTC (char *str)
{
  printf ("%s\n", str);
}
/*************************
 * E N D   O F   F I L E *
 *************************/
