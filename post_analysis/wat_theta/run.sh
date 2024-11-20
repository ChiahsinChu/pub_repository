gfortran watorient.f90 -o watorien.x
./watorien.x < input.watori
rm watorien.x

#awk '{i++; s=s+($2+$5)/2; print s/i }' dipole.out > wat-num.dat
#awk '{i++; s=s+($3+$6)/2; print s/i }' dipole.out > wat-ori.dat
a=$(awk '{s=s+$2}'END'{print s}' angle.out)
awk '{print $1, $2/"'$a'"}' angle.out > angle.dat
rm *.out