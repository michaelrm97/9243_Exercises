CC			= gcc
CFLAGS		= -Wall -Werror -std=c99

OBJ			= rstart.o

%.o : %.cpp
	$(CC) -c -o $@ $< $(CFLAGS)

all: rstart

rstart: $(OBJ)
	$(CC) -o $@ $^ $(CFLAGS)

.PHONY: clean

clean:
	rm -rf *.o rstart
