/***************************************
 * ASPECT to UNIX data convert library *
 ***************************************/
#ifndef CONV_H
#define CONV_H

/************************************************************
 * Convert 24-bit ASPECT integer into 32-bit native integer *
 ************************************************************/
extern	int	conv_integer	(void *);

/********************************************************
 * Convert 48-bit ASPECT real into 64-bit native double *
 ********************************************************/
extern	double	conv_real	(void *);

/************************************************************
 * Convert 24-bit ASPECT duration into 64-bit native double *
 ************************************************************/
extern	double	conv_delay	(void *);

/************************************************
 * Convert 4 ASPECT sixbits into 3 native ASCII *
 ************************************************/
extern	void	conv_sixbit	(char * /*dest*/, char * /*src*/);

/************************************************************
 * Convert ASPECT mark parity ASCII into space parity ASCII *
 ************************************************************/
extern	void	conv_ascii	(char * /*dest*/, char * /*src*/, int /*len*/);

/********************************************************
 * Convert 3-word ASPECT filename into intuitive format *
 ********************************************************/
extern	void	conv_filename	(char * /*dest*/, char * /*src*/);

/*******************************************************************
 * Convert 24-bit BCD ASPECT date and time into UNIX native time_t *
 *******************************************************************/
extern	time_t	conv_time	(void * /*ddmmyy*/, void * /*hhmmss*/);

#endif /* CONV_H */
/*************************
 * E N D   O F   F I L E *
 *************************/
