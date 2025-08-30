all: billionrows

billionrows: billionrows.f90
	gfortran -O3 -o billionrows billionrows.f90 -fopenmp

billionrowsnonmp: billionrows.f90
	gfortran -O3 -o billionrowssingle billionrows.f90

clean:
	rm -f billionrows
