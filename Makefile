FC = ifort
CC = clang
FFLAGS  = -c -debug all -free -module Modules
CFLAGS  = -c -g
LDFLAGS =
SOURCES = Searches/Search.f   \
          Structures/List.f Structures/Queue.f Structures/Stack.f Structures/ListIterator.f \
          Sorts/Sort.f       \
		  Units/Report.f     \
          $(wildcard **/*.c) \
          $(wildcard **/*.f) \
          Algorithms.f
OBJECTS = $(patsubst %.f, Objects/%.o, $(patsubst %.c, Objects/%_c.o, $(SOURCES)) )
EXECUTABLE = algorithms

all: $(SOURCES) $(EXECUTABLE)

$(EXECUTABLE): $(OBJECTS)
	$(FC) $(LDFLAGS) $(OBJECTS) -o Objects/$@

Objects/%.o: %.f
	@mkdir -p Modules
	@mkdir -p $$(dirname $@)
	$(FC) $(FFLAGS) -c $< -o $@

Objects/%_c.o: %.c
	@mkdir -p $$(dirname $@)
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	@rm -rf Modules Objects $(EXECUTABLE)
