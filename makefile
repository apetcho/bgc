all:
	ifort -DSTANDALONE -DRST bgc.f90 -fpp

first:
	ifort -DSTANDALONE bgc.f90 -fpp

green:
	ifort -DSTANDALONE -DRST -DGREEN bgc.f90 -fpp