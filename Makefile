FC = ifort
FFLAGS = -c -debug all -free
LDFLAGS =
SOURCES = Searches/Search.f \
          Structures/List.f Structures/Queue.f Structures/Stack.f Structures/ListIterator.f \
          Sorts/Sort.f \
          $(wildcard **/*.f) \
          Algorithms.f
OBJECTS = $(SOURCES:.f=.o)
EXECUTABLE = algorithms

all: $(SOURCES) $(EXECUTABLE)

$(EXECUTABLE): $(OBJECTS)
	$(FC) $(LDFLAGS) $(OBJECTS) -o $@

.f.o:
	$(FC) $(FFLAGS) $< -o $@

clean:
	rm -rf *.mod $(OBJECTS) $(EXECUTABLE)
