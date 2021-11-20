gfortran extract.f90 -o extract.x

i=25300
while [ $i -le 34350  ]
do

echo $i > fort.11
#echo $i

./extract.x
cp fort.13  coords.$i

i=$((i+50))

done

