CC = gcc
CFLAGS = -g -Wall -O3 -DNDEBUG -I. -I../peg-vm

SRCS = parser.c compiler.c pegcc.c ast.c
OBJS = $(SRCS:.c=.o)

PEGCC_OBJS = pegcc.o compiler.o parser.o ast.o

all: pegcc

pegcc: $(PEGCC_OBJS)
	$(CC) $(CFLAGS) -o $@ $(PEGCC_OBJS)

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f $(OBJS) pegcc

.PHONY: all clean
