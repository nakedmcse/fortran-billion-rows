all: billionrows

billionrows: billionrows.f90
	gfortran -O3 -o billionrows billionrows.f90 -fopenmp

clean:
	rm -f billionrows
