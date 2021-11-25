program estraggo
character*80 pippo
character*8 name
real*8 x,y,z,zmin,zmax
REAL,DIMENSION(3,597)::coor
CHARACTER*4,DIMENSION(597)::element

natoms=387
npt=96
nwat=291
nion=0
nframes=1001
read(11,*) nstep

OPEN (12,FILE='TRAJECTORY')
do i=1,nframes
   read(12,*) l
   read(12,99) a,k
   if (k.eq.nstep) then
      do j=1,natoms
         read(12,*)  element(j),(coor(ix,j),ix=1,3)
      end do
      do j=1,natoms
         write(13,'(a4,3 f10.5)') element(j),(coor(ix,j),ix=1,3)
      enddo
      goto 120
   endif
   do j=1,natoms
      read(12,100)pippo
   enddo
enddo

99      format(a4,i20)
100     format(a80)
120     continue

CLOSE(12)

stop
end


