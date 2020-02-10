#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>

void main (int argc, char **argv)
{
  FILE *in;
  char str[256], fromname[256], toname[256],
  	*chr, *header, chr0, chr1, chr2, chr3;
  int headersize=0, from, to, size, i;
  
  if (argc < 2)
  {
    printf ("Usage: %s <from> [<to>]\n", argv[0]);
    exit (0);
  }

  strcpy (fromname, argv[1]);
  if (argc == 2)
  {
    strcpy (toname, fromname);
    strcat (toname, "~");
  }
  else strcpy (toname, argv[2]);

  in = fopen (fromname, "r");
  if (in == NULL)
  {
    printf ("Cannot open file: %s\n", fromname);
    exit (0);
  }

  for (;;)
  {
    if (fgets (str, 256, in) == NULL)
    {
      printf ("Cannot find HeaderSize\n");
      fclose (in);
      exit (0);
    }
    if (str[0] == '0')
    {
      printf ("Cannot find HeaderSize\n");
      fclose (in);
      exit (0);
    }
    if (strncmp (str, "HeaderSize", 10)) continue;
    chr = strchr (str, '=');
    if (chr == NULL) continue;
    if (sscanf (chr+1, "%d", &headersize) == 1) break;
  }

  fclose (in);
  if (headersize == 1 || headersize == 2) headersize *= 16384;
  if (headersize == 0)
  {
    printf ("Cannot find HeaderSize\n");
    exit (0);
  }

  header = malloc (headersize);
  if (header == NULL)
  {
    printf ("No memory\n");
    exit (0);
  }
  from = open (fromname, O_RDONLY);
  if (from == -1)
  {
    printf ("Cannot open file: %s\n", fromname);
    exit (0);
  }
  if (read (from, header, headersize) != headersize)
  {
    printf ("Cannot read header\n");
    close (from);
    exit (0);
  }
  to = creat (toname, 0664);
  if (to == -1)
  {
    printf ("Cannot create file: %s\n", toname);
    close (from);
    exit (0);
  }
  if (write (to, header, headersize) != headersize)
  {
    printf ("Cannot write header\n");
    close (to);
    close (from);
    exit (0);
  }

  if (headersize < 4096)
  {
    headersize = 4096;
    header = realloc (header, headersize);
    if (header == NULL)
    {
      printf ("No memory\n");
      close (to);
      close (from);
      exit (0);
    }
  }
  for (;;)
  {
    size = read (from, header, headersize);
    if (size <= 0) break;
    chr = header;
    for (i=0; i<size/4; i++)
    {
      chr0 = *chr;
      chr1 = *(chr+1);
      chr2 = *(chr+2);
      chr3 = *(chr+3);
      *chr++ = chr3;
      *chr++ = chr2;
      *chr++ = chr1;
      *chr++ = chr0;
    }
    if (write (to, header, size) != size)
    {
      printf ("Cannot write\n");
      break;
    }
  }

  close (to);
  close (from);
  if (argc == 2)
  {
    unlink (fromname);
    link (toname, fromname);
    unlink (toname);
  }
  exit (0);
}
