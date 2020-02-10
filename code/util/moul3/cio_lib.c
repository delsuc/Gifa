/*


*/
/* #include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
 #include <unitstd.h> 
#include <fcntl.h>
#include <errno.h> */
#include <stdio.h>

#ifdef UNDERSCORE
#  define COPEN copen_
#  define CRANDOM_READ crandom_read_
#else
#  define COPEN copen
#  define CRANDOM_READ crandom_read
#endif

void COPEN(filedes,name,namelength,error)
FILE	**filedes;
int	*error,*namelength;
char	*name;
{ char	lname[80],type[1];
  int	rr;

	strncpy(lname,name,*namelength);
	lname[*namelength] = '\0';
	printf ("%s \n",lname);
	strcpy(type,"r");
	*filedes = fopen(lname, type);
	if (*filedes == NULL) *error = -1;

}

int CRANDOM_READ(filedes,offset,size,buffer)
FILE	**filedes;
int	*offset,*size;
char	*buffer;
{
int rr,err;
	err = fseek(*filedes,*offset,0);
	if (err != -1)
	{	err = fread(buffer,sizeof(*buffer),*size,*filedes);
	}
	else	err=0;

	if (err == 0)
	{	rr = -1;		/* means end-of-file */
	}
	else	rr = 0;		/* means no-error */

	return(rr);
}
