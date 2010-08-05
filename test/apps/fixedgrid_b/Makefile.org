#include $(TAU_MAKEFILE)

CC = $(TAU_COMPILER) gcc
CFLAGS = -Wall -O5

LD = $(TAU_COMPILER) gcc
LDFLAGS = -lm -Wall

MKDEP = makedepend

RM = /bin/rm -f

PROG = fixedgrid

CHEM   = chem
UTIL   = util
CONFIG = config

SRCS = fixedgrid.c \
       discretize.c \
       chemistry.c \
       transport.c \
       $(CHEM)/saprc99_Integrator.c \
       $(CHEM)/saprc99_Function.c \
       $(CHEM)/saprc99_Initialize.c \
       $(CHEM)/saprc99_Jacobian.c \
       $(CHEM)/saprc99_LinearAlgebra.c \
       $(CHEM)/saprc99_Rates.c \
       $(CHEM)/saprc99_Monitor.c \
       $(CHEM)/saprc99_JacobianSP.c \
       $(UTIL)/fileio.c \
       $(UTIL)/timer.c

OBJS = fixedgrid.o \
       discretize.o \
       chemistry.o \
       transport.o \
       $(CHEM)/saprc99_Integrator.o \
       $(CHEM)/saprc99_Function.o \
       $(CHEM)/saprc99_Initialize.o \
       $(CHEM)/saprc99_Jacobian.o \
       $(CHEM)/saprc99_LinearAlgebra.o \
       $(CHEM)/saprc99_Rates.o \
       $(CHEM)/saprc99_Monitor.o \
       $(CHEM)/saprc99_JacobianSP.o \
       $(UTIL)/fileio.o \
       $(UTIL)/timer.o

INCLUDES = -I. \
	   -I$(CHEM) \
	   -I$(UTIL) \
	   -I$(CONFIG)

PROG = fixedgrid

all: $(PROG)

$(PROG): $(OBJS)
	$(LD) $(LDFLAGS) $(OBJS) -o $(PROG)

%.o: %.c
	$(CC) $(CFLAGS) $(INCLUDES) -c $< -o $@

tidy:
	$(RM) $(OBJS) *~ Output/*

clean: 
	$(RM) $(PROG) $(OBJS)

depend:
	$(RM) .depend
	$(MKDEP) $(INCLUDES) -f- -- $(CFLAGS) -- $(SRCS) > .depend

#include .depend

