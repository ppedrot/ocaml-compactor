CC=c99
CFLAGS=-W -Wextra -Wall -pedantic -fPIC -I"/usr/lib/ocaml"
LDFLAGS=
SRC=represent.c buffer.c partition.c hopcroft.c main.c
OBJ=$(patsubst %.c,%.o,$(SRC))
EXEC=main
OCAMLMKLIB=ocamlmklib

# %.o: %.c
# 	ocamlc -c $<


all: $(OBJ)
# 	$(CC) $(CFLAGS) $(LDFLAGS) $(OBJ) -o $(EXEC)
	$(OCAMLMKLIB) 

clean:
	rm -rf *.[oa]
	rm -rf *.so
