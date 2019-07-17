FC=gfortran -c
FL=gfortran
FFLAGS=-fbacktrace -Og -fcheck=all -g
test: test_argparser.o argparser.o
	$(FL) $(FFLAGS) $^ -o $@
test_argparser.o: test_argparser.f08 argparser.o
	$(FC) $(FFLAGS) $<
argparser.o: argparser.f08
	$(FC) $(FFLAGS) $<
.PHONY: clean
clean:
	rm -vf *.o *.mod test
