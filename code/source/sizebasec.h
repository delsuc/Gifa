/* This contains the size of the static main buffer

This files actually duplicate the information which is found
in sizebase.inc used on the Fortran side

*/

#define SMXMAX 2048*1024
#define SIZEMAX SMXMAX/16
#define SMXTOT 8*SMXMAX
