all:
	ifort -DSTANDALONE bgc.f90 -fpp

green:
	ifort -DSTANDALONE -DGREEN bgc.f90 -fpp