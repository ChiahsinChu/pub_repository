gfortran extract.f90 -o extract.x

i=0
while [ $i -le 1000 ]
do
echo $i > fort.11
#echo $i
./extract.x
cp fort.13  coords.$i
i=$((i+50))
done
rm fort.11 fort.13 extract.x
