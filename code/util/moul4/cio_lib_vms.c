/*


*/
/* #include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
 #include <unitstd.h> 
#include <fcntl.h>
#include <errno.h> */
#include <stdio.h>
#include <descrip.h>

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

char *st2;
struct dsc$descriptor_s *st1;
int l;
{
   strncpy(st2,st1->dsc$a_pointer,l);
   st2[l] = '\0';
}

void copen_(filedes,name,namelength,error)
FILE	**filedes;
int	*error,*namelength;
char	*name;
{ char	lname[80];
  int	rr;

        convert_string(lname,name,*namelength);
	printf ("%s \n",lname);
	*filedes = fopen(lname, "r");
	if (*filedes == NULL) *error = -1;
        else *error = 0;

}

int crandom_read_(filedes,offset,size,buffer)
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

