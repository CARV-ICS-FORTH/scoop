#ifndef _PBPI_ERROR_H
#define _PBPI_ERROR_H

//debug: print debug information
//requires: none
//effects:	print message to stdout
void debug(const char *fmt, ...);

//fatal: report unrecoverable error
//requires:	none
//effects:	print error message to stderr and abort the program
void fatal(const char *fmt, ...);

//error: report unrecoverable error
//requires:	none
//effects:	print error message to stderr and abort the program
void error(const char *fmt, ...);

//message: print a message to stdout
//requires: none
//effects: print a message to stdout
void message(const char *fmt, ...);

//warning: print a warning message to stderr
//requires: none
//effects: print a warning message to stderr
void warning(const char *fmt, ...);

//bug: print a bug message to stderr
//requires: none
//effects: print a bug message to stderr and abort the program
void bug(const char *fmt, ...);

//void log2file(FILE *fpLog, const char *fmt, ...);


#endif

