gfortran watorient.f90 -o watorien.x
./watorien.x < input.watori
rm watorien.x

nbin=$(awk '{if (NR==2) {print $5}}' surf.out)
nbin=$((nbin/2))

split  -d -l $nbin dipole.out  dip-
tac dip-01 > dip-02
pr -m -t -e30 -w 300 dip-00 dip-02 > dip-03
awk '{i++; print 0.2*i-0.1, s=($3+$8)/2}' dip-03 > o.dat
awk '{i++; print 0.2*i-0.1, s=($4+$9)/2}' dip-03 > h.dat
awk '{print s=($5+$10)/2}' dip-03 > ori.dat
pr -m -t -e30 o.dat h.dat ori.dat > stru.dat
awk '{print $1, $2*$5}' stru.dat > inter.dat

rm dip* stru.dat ion.out

awk '{if ($1<=2.7) {i++;s=s+$2; print (s/i)*(11.246*11.246*2.7*6.02/180)/16}}' o.dat > tmp.dat
echo "water A coverage" >> wat_cov.dat
tail -n 1 tmp.dat >> wat_cov.dat
awk '{if ($1<=4.5) {i++;s=s+$2; print (s/i)*(11.246*11.246*4.5*6.02/180)/16}}' o.dat > tmp.dat
echo "water B coverage" >> wat_cov.dat
tail -n 1 tmp.dat >> wat_cov.dat
awk '{if ($1>2.7 && $1<=4.5) {i++;s=s+$2;print s}}' inter.dat > tmp.dat
echo "water B dipole" >> watb_dip.dat
tail -n 1 tmp.dat >> watb_dip.dat