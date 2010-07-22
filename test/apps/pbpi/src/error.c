/** This software is part of the dissertation "High Performance, Bayesian-based Phylogenetic Inference Framework" 
 *  by Xizhou Feng and is still under development. 
 *  Copyright: 
 *	Xizhou Feng
 *      Kirk Cameron
 *      Virginia Tech IP
 *  Contact:
 *	Xizhou Feng
 *	Department of Computer Science
 *	Virgnia Tech, Blacksburg, VA 24060 
 *	Email: fengx@cs.vt.edu
 *	Phone: (540)231-9245
 */

#include "pbpi.h"
#include <stdarg.h>
#include "error.h"

static char errbuf[1024];

//debug: print debug information
//requires: none
//effects:	print message to stdout
void debug(const char *fmt, ...)
{
#ifdef DEBUG
	va_list	ap;
	va_start(ap, fmt);
	vsprintf(errbuf, fmt, ap);
	fprintf(stdout, "DEBUG: %s\n", errbuf); 
	fflush(stderr);
	va_end(ap);
#else
	;
#endif
}

//fatal: report unrecoverable error
//requires:	none
//effects:	print error message to stderr and abort the program
void fatal(const char *fmt, ...)
{
	va_list	ap;
	va_start(ap, fmt);
	vsprintf(errbuf, fmt, ap);
	fprintf(stderr, "FATAL: %s\n", errbuf); 
	fflush(stderr);
	va_end(ap);
	exit(0);
}

//error: report unrecoverable error
//requires:	none
//effects:	print error message to stderr and abort the program
void error(const char *fmt, ...)
{
	va_list	ap;
	va_start(ap, fmt);
	vsprintf(errbuf, fmt, ap);
	fprintf(stderr, "ERROR: %s\n", errbuf); 
	fflush(stderr);
	va_end(ap);
	exit(0);
}

//message: print a message to stdout
//requires: none
//effects: print a message to stdout
void message(const char *fmt, ...)
{
	va_list	ap;
	va_start(ap, fmt);
	vsprintf(errbuf, fmt, ap);
	fprintf(stdout, "MSG: %s\n", errbuf); 
	fflush(stdout);
	va_end(ap);
}

//warning: print a warning message to stderr
//requires: none
//effects: print a warning message to stderr
void warning(const char *fmt, ...)
{
	va_list	ap;
	va_start(ap, fmt);
	vsprintf(errbuf, fmt, ap);
	fprintf(stdout, "MSG: %s\n", errbuf); 
	fflush(stdout);
	va_end(ap);
}

//bug: print a bug message to stderr
//requires: none
//effects: print a bug message to stderr and abort the program
void bug(const char *fmt, ...)
{
	va_list	ap;
	va_start(ap, fmt);
	vsprintf(errbuf, fmt, ap);
	fprintf(stderr, "BUG: %s\n", errbuf); 
	fflush(stderr);
	va_end(ap);
	exit(0);
}


//log: print a log message to logfile
//requires: none
//effects: print a log message to logfile
//void log2file(FILE *fpLog, const char *fmt, ...)
//{
//	va_list	ap;
//	va_start(ap, fmt);
//	vsprintf(errbuf, fmt, ap);
//	fprintf(fpLog, "LOG: %s\n", errbuf); 
//	fflush(fpLog);
//	va_end(ap);
//}


