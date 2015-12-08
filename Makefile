FC = ifort
CC = clang
FFLAGS  = -c -free -module Modules -reentrancy threaded -openmp -fast # -fast # -debug all
CFLAGS  = -c -pthread -openmp -g
LDFLAGS = -openmp

INTERFACES = Arrays/Arrays.f \
			 Units/Asserts.f Units/Report.f \
			 Examples/RubiksCube/Common.f Examples/RubiksCube/Cube.f Examples/RubiksCube/Rotator.f \
			 Examples/RubiksCube/Search.f Examples/RubiksCube/RubiksCube.f \
			 Features/Inheritance/Shape.f \
			 Randoms/Random.f \
	         Searches/Search.f \
             Structures/List.f Structures/Queue.f Structures/Stack.f Structures/ListIterator.f \
             Sorts/Sort.f   \
			 Graphs/Vertex.f Graphs/Graph.f \
			 Structures/ArrayStack.f Structures/ArrayQueue.f \
		     Units/Parameters.f Units/Report.f
INCLUDES = $(foreach d, $(shell find . -name '*.h'), -I$d)
EXCLUDES = $(patsubst %, ! -path './%', Algorithms.f Examples/* Exercises/* Features/* Problems/*) \
           $(patsubst %, ! -path './%', $(INTERFACES))
SOURCES  = $(INTERFACES) \
		   $(shell find . -name '*.c' $(EXCLUDES) | sort) \
		   $(shell find . -name '*.f' $(EXCLUDES) | sort) \
           $(shell find Examples  -name '*.f' ! -name 'Example.f')   Examples/Example.f \
		   $(shell find Exercises -name '*.f' ! -name 'Exercises.f') Exercises/Exercises.f \
		   $(shell find Features  -name '*.f' ! -name 'Feature.f')   Features/Feature.f \
		   $(shell find Problems  -name '*.f' ! -name 'Problems.f')  Problems/Problems.f \
           Algorithms.f
#          $(wildcard **/*.c)
OBJECTS  = $(patsubst %.f, Objects/%.o, $(patsubst %.c, Objects/%_c.o, $(SOURCES)))
EXECUTABLE = algorithms

all: $(SOURCES) $(EXECUTABLE)

$(EXECUTABLE): $(OBJECTS)
	@echo 'Linking to $@...'
	@$(FC) $(LDFLAGS) $(OBJECTS) -o Objects/$@

Objects/%.o: %.f
	@echo 'Compiling $@...'
	@mkdir -p Modules
	@mkdir -p $(dir $@)
	@$(FC) $(FFLAGS) -c $< -o $@

Objects/%_c.o: %.c
	@echo 'Compiling $@...'
	@mkdir -p $(dir $@)
	@$(CC) $(CFLAGS) $(INCLUDES) -c $< -o $@

clean:
	@echo "Cleaning..."
	@rm -rf Modules Objects $(EXECUTABLE)
