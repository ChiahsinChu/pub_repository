#!/bin/bash --login

gfortran extract.f90 -o extract.x
gfortran aver.f90 -o aver.x

i=20000
while [ $i -le 40000 ]
do

cp ../../pot-$i.cube input.cube 
./extract.x
mv output.cube har-$i.dat

cp har-$i.dat input.dat
./aver.x
cp output.dat out-$i.dat

cat out-$i.dat >> out

i=$((i+50))


done

awk '{i++; s=s+$1; print s/i}' out > ave.dat 

