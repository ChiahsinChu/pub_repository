
i=20000

while [ $i -le 40000  ]

do

cp ../pot-$i.cube input.cube

./cubecruncher.x -i input.cube -o output.cube -1d_profile 3 0.1

awk '{print $2*27.2114}' profile_3.dat > ./pot-$i-01.dat

awk '{if (NR==50) {temp1=$1} if (NR==330){temp2=$1;print temp1-temp2}}' ./pot-$i-01.dat > tmp.dat

cat tmp.dat >> dipole.dat

i=$((i+200))

done

awk '{i++;s=s+$1;print s/i}' dipole.dat > ave.dat

