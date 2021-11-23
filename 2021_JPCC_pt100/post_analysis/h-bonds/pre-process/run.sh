gfortran wrap.f90 -o wrap.x
./wrap.x
rm wrap.x

gfortran periodic.f90 -o periodic.x
./periodic.x
mv fort.12 out.xyz
