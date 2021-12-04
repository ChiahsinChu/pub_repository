#!/bin/bash --login

# grep Fermi level from output data
# a.u. (hartree) to eV
grep 'Fermi energy' ../../output* > tmp.dat
flag=$(grep 'SCF run terminated' ../../output* | awk '{print $2}')
if [ $flag ]; then
    sed -i '$d' tmp.dat 
fi

awk '{print $3*27.2113838565563}' tmp.dat > fermi.dat
awk '{i++; s=s+$1; print s/i}' fermi.dat > fermi_ave.dat
rm tmp.dat

