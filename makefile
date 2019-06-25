test: test_argparser.o argparser.o
	gfortran $^ -o $@
test_argparser.o: test_argparser.f08 argparser.o
	gfortran -c $<
argparser.o: argparser.f08
	gfortran -c $<
clean:
	rm -f *.o *.mod test
