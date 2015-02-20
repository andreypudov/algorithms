FC = ifort
CC = clang
FFLAGS  = -c -debug all -free -module Modules
CFLAGS  = -c -g
LDFLAGS =

INTERFACES = Searches/Search.f \
             Structures/List.f Structures/Queue.f Structures/Stack.f Structures/ListIterator.f \
             Sorts/Sort.f   \
		     Units/Report.f
EXCLUDES = $(patsubst %, ! -path './%', Algorithms.f Examples/*) \
           $(patsubst %, ! -path './%', $(INTERFACES))
SOURCES  = $(INTERFACES) \
		   $(shell find . -name '*.c' $(EXCLUDES)) \
		   $(shell find . -name '*.f' $(EXCLUDES)) \
           $(shell find Examples -name '*.f' ! -name 'Example.f') Examples/Example.f \
           Algorithms.f
#          $(wildcard **/*.c)
OBJECTS  = $(patsubst %.f, Objects/%.o, $(patsubst %.c, Objects/%_c.o, $(SOURCES)))
EXECUTABLE = algorithms

all: $(SOURCES) $(EXECUTABLE)

$(EXECUTABLE): $(OBJECTS)
	$(FC) $(LDFLAGS) $(OBJECTS) -o Objects/$@

Objects/%.o: %.f
	@mkdir -p Modules
	@mkdir -p $(dir $@)
	$(FC) $(FFLAGS) -c $< -o $@

Objects/%_c.o: %.c
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	@rm -rf Modules Objects $(EXECUTABLE)
