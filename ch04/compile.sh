gfortran -c mod_initial.f90
gfortran -c mod_diff.f90
gfortran -c tsunami.f90
gfortran *.o -o tsunami
rm *.o *.mod
