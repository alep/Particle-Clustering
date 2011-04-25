FC=gfortran
DEBUG=-pg -g
FLAGS=-Wall -pedantic $(DEBUG)
srcdir=.

# Note: that the order matters, the most dependent object at the top
objects = \
$(srcdir)/circularqueue.o \
$(srcdir)/particles.o \
$(srcdir)/cluster.o

all: $(objects)
	$(FC) $(FLAGS) -o cluster $(objects)

$(srcdir)/%.o: $(srcdir)/%.f90
	$(FC) $(FLAGS) -c $< -o $@

test: $(srcdir)/circularqueue.o $(srcdir)/test_queue.f90
	$(FC) $(FLAGS) -o test test_queue.f90 $<

clean:
	rm -rf *.mod *.o cluster test
