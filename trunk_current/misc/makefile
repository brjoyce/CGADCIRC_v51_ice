FC = ifort
FLAGS = -g -O3 -assume byterecl -132 -i-dynamic -assume buffered_io

.PHONY.: clean
	rm *.o hfcheck

hfcheck: checkhotstartfile.o
	$(FC) $(FLAGS) -o $@ $^

%.o: %.f90
	$(FC) $(FLAGS) -o $@ -c $<

checkhotstartfile.o :  checkhotstartfile.f90