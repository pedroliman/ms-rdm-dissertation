CC = gcc
MPICC = mpicc
CFLAGS = -O3
LDFLAGS = 
LIBS = -lm
UNAME_S = $(shell uname -s)

ifneq (, $(findstring SunOS, $(UNAME_S)))
	LIBS += -lnsl -lsocket -lresolv
endif

compile:
	$(CC) $(CFLAGS) $(LDFLAGS) -o dtlz2_serial.exe dtlz2_serial.c borg.c mt19937ar.c $(LIBS)
	$(CC) $(CFLAGS) $(LDFLAGS) -o dtlz2_advanced.exe dtlz2_advanced.c borg.c mt19937ar.c $(LIBS)
	$(CC) $(CFLAGS) $(LDFLAGS) -o borg.exe frontend.c borg.c mt19937ar.c $(LIBS)
.PHONY: compile

