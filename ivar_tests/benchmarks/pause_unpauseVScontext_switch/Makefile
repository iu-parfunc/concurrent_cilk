BIN=pause_unpause.exe 
OUT=bin
SCRIPTS="../../scripts"
SCRIPT_PREFIX="../benchmarks/pause_unpauseVScontext_switch"
include  ../../Makefile.common

all: pthread_context_switch

pthread_context_switch: pthread_context_switch.c
	gcc -march=native -O3 -D_XOPEN_SOURCE=600 -D_GNU_SOURCE -std=c99  -W -Wall -lrt -lpthread pthread_context_switch.c -o pthread_context_switch.exe
