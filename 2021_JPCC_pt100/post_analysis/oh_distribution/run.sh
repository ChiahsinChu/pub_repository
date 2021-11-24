gfortran pos.f90 -o watorien.x
./watorien.x < input.watori
rm watorien.x
gfortran ori.f90 -o watorien.x
./watorien.x < input.watori
rm watorien.x 