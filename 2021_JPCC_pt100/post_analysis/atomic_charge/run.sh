#!/bin/bash --login

gfortran watorient.f90 -o watorien.x

i=0
while [ $i -le 1000 ]
do
cp ../out.$i ./output.out

cp ../coord/coord.$i ./TRAJECTORY 

FN=$(wc -l TRAJECTORY)

echo $FN > index.dat

FN=$((FN+2))

for file in output.out; do
    grep -A $FN "Mulliken Population Analysis" $file > mulliken.dat
    grep "STEP NUMBER" $file > step_raw
    awk '{print $4}' step_raw > step
    rm step_raw
done

for file in output.out; do
    grep -A $FN "Hirshfeld Charges" $file > hirshfeld.dat
    grep "STEP NUMBER" $file > step_raw
    awk '{print $4}' step_raw > step
    rm step_raw
done

./watorien.x < input.watori

cp 1.out out-1-$i.dat
cp 2.out out-2-$i.dat 
cp 3.out out-3-$i.dat 
cp 4.out out-4-$i.dat 
cp 5.out out-5-$i.dat

cat out-1-$i.dat >> out-1.dat
cat out-2-$i.dat >> out-2.dat
cat out-3-$i.dat >> out-3.dat
cat out-4-$i.dat >> out-4.dat
cat out-5-$i.dat >> out-5.dat

i=$((i+100))


done

# 'mulliken O ','mulliken H ','mulliken total ','hirshfeld O ','hirshfeld H ','hirshfeld total'
awk '{i++; j=j+$1;k=k+$2;l=l+$3;m=m+$4;n=n+$5;o=o+$6;print j/i"  "k/i"  "l/i"  "m/i"  "n/i"  "o/i}' out-1.dat > bridge_oh.dat
awk '{i++; j=j+$1;k=k+$2;l=l+$3;m=m+$4;n=n+$5;o=o+$6;print j/i"  "k/i"  "l/i"  "m/i"  "n/i"  "o/i}' out-2.dat > top_oh.dat
awk '{i++; j=j+$1;k=k+$2;l=l+$3;m=m+$4;n=n+$5;o=o+$6;print j/i"  "k/i"  "l/i"  "m/i"  "n/i"  "o/i}' out-3.dat > wata.dat
awk '{i++; j=j+$1;k=k+$2;l=l+$3;m=m+$4;print j/i"  "k/i"  "l/i"  "m/i}' out-4.dat > pt.dat
awk '{i++; j=j+$1;k=k+$2;l=l+$3;m=m+$4;print j/i"  "k/i"  "l/i"  "m/i}' out-5.dat > total.dat

rm watorien.x 