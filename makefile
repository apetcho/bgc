all:
	ifort -fpp -DSTANDALONE -DRST bgc.f90

first:
	ifort -fpp -DSTANDALONE bgc.f90

green:
	ifort -fpp -DSTANDALONE -DRST -DGREEN bgc.f90

pce:
	ifort -fpp -DSTANDALONE -DRST -DPCE bgc.f90
