#!/bin/bash --login

i=6000
while [ $i -le 23000 ]
do

grep 'Fermi Ener' ../../out.$i > fermi-$i.dat


cat fermi-$i.dat >> fermi.dat

i=$((i+50))


done

awk '{i++; s=s+$5; print s/i}' fermi.dat > ave.dat

