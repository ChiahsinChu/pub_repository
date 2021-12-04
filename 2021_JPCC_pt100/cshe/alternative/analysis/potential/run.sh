#!/bin/bash --login

PROJECT="pt"

gfortran aver.f90 -o aver.x

i=1
while [ $i -le 20001 ]
do  
    # hartree potential in one dimension
    cd ./cubecruncher
    cp ../../../$PROJECT-v_hartree-1_$i.cube ./input.cube
    ./cubecruncher.x -i input.cube -o output.cube -1d_profile 3 0.1
    awk '{print $2*27.2113838565563}' profile_3.dat > ../har-$i.dat
    cd ../
    # average the hartree potential of waterbox
    # change the parameters in aver.f90 accrodingly
    cp har-$i.dat input.dat
    ./aver.x
    mv output.dat out-$i.dat
    i=$((i+50))
done

cat out-*.dat > hartree.dat
awk '{i++; s=s+$1; print s/i}' hartree.dat > hartree_ave.dat 

rm aver.x
