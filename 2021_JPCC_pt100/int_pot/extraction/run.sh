#!/bin/bash --login

gfortran watorient.f90 -o watorien.x

i=0
while [ $i -le 1000 ]
do

cp ../../cshe/extraction/coords.$i ./TRAJECTORY
./watorien.x < input.watori
mv top.xyz ../top/coords.$i
mv bottom.xyz ../bottom/coords.$i

i=$((i+200))

done

rm  watorien.x
