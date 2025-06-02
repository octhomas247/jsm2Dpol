# JSMF90 Makefile

FC=gfortran -O5 -ffree-line-length-none -ffree-form

all: jsm
	echo "Building all: jsm"

jsm: 
	$(FC) jsm2Dpol.f90 jsm2Dpolcom.f90 jsm2Dpol_sub.f90 -o af90.j 

clean:
	rm ./af90.j *.o *.mod
