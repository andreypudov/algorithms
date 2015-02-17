FC = ifort
FFLAGS  = -c -debug all -free -module Modules -c
LDFLAGS =
SOURCES = Searches/Search.f \
          Structures/List.f Structures/Queue.f Structures/Stack.f Structures/ListIterator.f \
          Sorts/Sort.f \
          $(wildcard **/*.f) \
          Algorithms.f
OBJECTS = $(patsubst %.f, Objects/%.o, $(SOURCES))
EXECUTABLE = algorithms

all: $(SOURCES) $(EXECUTABLE)

$(EXECUTABLE): $(OBJECTS)
	$(FC) $(LDFLAGS) $(OBJECTS) -o Objects/$@

Objects/%.o: %.f
	@mkdir -p Modules
	@mkdir -p $$(dirname $@)
	$(FC) $(FFLAGS) -c $< -o $@

clean:
	@rm -rf Modules $(OBJECTS) Objects $(EXECUTABLE)
