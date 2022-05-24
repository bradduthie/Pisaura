CXX    = gcc
CFLAGS = -fopenmp -ansi -Wall -pedantic -std=c99

RunInbr: nuptial_rep.o nuptials.o rand_sampling.o as183.o
	$(CC) $(CFLAGS) -o nuptial_rep nuptial_rep.o nuptials.o rand_sampling.o 
	as183.o -lm

nuptials.o: nuptials.c
	$(CC) $(CFLAGS) -c nuptials.c

nuptials.o: nuptials.c
	$(CC) $(CFLAGS) -c nuptials.c

rand_sampling.o: rand_sampling.c
	$(CC) $(CFLAGS) -c rand_sampling.c

as183.o: as183.c
	$(CC) $(CFLAGS) -c as183.c

clean:
	rm -rf *.o *.txt daj
	find . -type f -name out\* -exec rm {} \;
