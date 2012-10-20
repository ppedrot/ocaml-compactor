CC=gcc
CFLAGS=-W -Wall -pedantic
LDFLAGS=
SRC=buffer.c partition.c hopcroft.c main.c
OBJ=$(patsubst %.c,%.o,$(SRC))
EXEC=main
INCLUDE = .

all: $(OBJ)
	$(CC) $(CFLAGS) $(LDFLAGS) $(OBJ) -o $(EXEC)

clean:
	rm -rf *.o
