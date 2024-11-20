      program estraggo
        IMPLICIT NONE
!        character*80 pippo
!        character*4 name
!        real*8 x,y,z
        REAL,DIMENSION(3,999)::coor
        CHARACTER*4,DIMENSION(999)::element
        integer:: i,j,k,natoms,nframes,nwat,nion
        REAL::a,b

        natoms=295
        nframes = 51960
        nwat=291
        nion=4
        a=11.246
        b=11.246

        OPEN (11,FILE='wrap.xyz')
!        OPEN(12,FILE="coord.xyz")
        do j=1,nframes
           read(11,*) 
           read(11,*) 
        do i=1,natoms
           read(11,*)  element(i),coor(1,i),coor(2,i),coor(3,i)
        end do

!        OPEN(12,FILE="coord.xyz")  
              write(12,*)   9*natoms
              write(12,*)   'wrap'
              do i=1,nwat-2,3
                 write(12,*)  element(i),coor(1,i),coor(2,i),coor(3,i) 
                 write(12,*)  element(i+1),coor(1,i+1),coor(2,i+1),coor(3,i+1)
                 write(12,*)  element(i+2),coor(1,i+2),coor(2,i+2),coor(3,i+2)
              enddo
              do i=nwat+1,natoms
                  write(12,*) element(i),coor(1,i),coor(2,i),coor(3,i)
              enddo

              do i=1,nwat-2,3
                 write(12,*)  element(i),coor(1,i)-a,coor(2,i),coor(3,i) 
                 write(12,*)  element(i+1),coor(1,i+1)-a,coor(2,i+1),coor(3,i+1)
                 write(12,*)  element(i+2),coor(1,i+2)-a,coor(2,i+2),coor(3,i+2)
              enddo
              do i=nwat+1,natoms
                  write(12,*) element(i),coor(1,i)-a,coor(2,i),coor(3,i)
              enddo

              do i=1,nwat-2,3
                 write(12,*)  element(i),coor(1,i)+a,coor(2,i),coor(3,i)
                 write(12,*)  element(i+1),coor(1,i+1)+a,coor(2,i+1),coor(3,i+1)
                 write(12,*)  element(i+2),coor(1,i+2)+a,coor(2,i+2),coor(3,i+2)
              enddo
              do i=nwat+1,natoms
                  write(12,*) element(i),coor(1,i)+a,coor(2,i),coor(3,i)
              enddo


              do i=1,nwat-2,3
                 write(12,*)  element(i),coor(1,i)+a,coor(2,i)+b,coor(3,i)
                 write(12,*)  element(i+1),coor(1,i+1)+a,coor(2,i+1)+b,coor(3,i+1)
                 write(12,*)  element(i+2),coor(1,i+2)+a,coor(2,i+2)+b,coor(3,i+2)
              enddo
              do i=nwat+1,natoms
                  write(12,*) element(i),coor(1,i)+a,coor(2,i)+b,coor(3,i)
              enddo


              do i=1,nwat-2,3
                 write(12,*)  element(i),coor(1,i)-a,coor(2,i)+b,coor(3,i)
                 write(12,*)  element(i+1),coor(1,i+1)-a,coor(2,i+1)+b,coor(3,i+1)
                 write(12,*)  element(i+2),coor(1,i+2)-a,coor(2,i+2)+b,coor(3,i+2)
              enddo
              do i=nwat+1,natoms
                  write(12,*) element(i),coor(1,i)-a,coor(2,i)+b,coor(3,i)
              enddo


              do i=1,nwat-2,3 
                 write(12,*)  element(i),coor(1,i)+a,coor(2,i)-b,coor(3,i)
                 write(12,*)  element(i+1),coor(1,i+1)+a,coor(2,i+1)-b,coor(3,i+1)
                 write(12,*)  element(i+2),coor(1,i+2)+a,coor(2,i+2)-b,coor(3,i+2)
              enddo
              do i=nwat+1,natoms
                  write(12,*) element(i),coor(1,i)+a,coor(2,i)-b,coor(3,i)
              enddo


              do i=1,nwat-2,3  
                 write(12,*)  element(i),coor(1,i)-a,coor(2,i)-b,coor(3,i)
                 write(12,*)  element(i+1),coor(1,i+1)-a,coor(2,i+1)-b,coor(3,i+1)
                 write(12,*)  element(i+2),coor(1,i+2)-a,coor(2,i+2)-b,coor(3,i+2)
              enddo
              do i=nwat+1,natoms
                  write(12,*) element(i),coor(1,i)-a,coor(2,i)-b,coor(3,i)
              enddo


              do i=1,nwat-2,3
                   write(12,*)  element(i),coor(1,i),coor(2,i)-b,coor(3,i)
                   write(12,*)  element(i+1),coor(1,i+1),coor(2,i+1)-b,coor(3,i+1)
                   write(12,*)  element(i+2),coor(1,i+2),coor(2,i+2)-b,coor(3,i+2)
              enddo
              do i=nwat+1,natoms
                  write(12,*) element(i),coor(1,i),coor(2,i)-b,coor(3,i)
              enddo


              do i=1,nwat-2,3
                   write(12,*)  element(i),coor(1,i),coor(2,i)+b,coor(3,i)
                   write(12,*)  element(i+1),coor(1,i+1),coor(2,i+1)+b,coor(3,i+1)
                   write(12,*)  element(i+2),coor(1,i+2),coor(2,i+2)+b,coor(3,i+2)
              enddo
              do i=nwat+1,natoms
                  write(12,*) element(i),coor(1,i),coor(2,i)+b,coor(3,i)
              enddo
             

!              do i=2,nwat-1,3
!                 write(12,*)  element(i),coor(1,i),coor(2,i),coor(3,i)
!                 write(12,*)  element(i),coor(1,i)-11.246,coor(2,i),coor(3,i)
!                 write(12,*)  element(i),coor(1,i)+11.246,coor(2,i),coor(3,i)
!                 write(12,*)  element(i),coor(1,i)-16.869,coor(2,i)+9.739,coor(3,i)
!                 write(12,*)  element(i),coor(1,i)-5.623,coor(2,i)+9.739,coor(3,i)
!                 write(12,*)  element(i),coor(1,i)+5.623,coor(2,i)+9.739,coor(3,i)
!                 write(12,*)  element(i),coor(1,i)-5.623,coor(2,i)-9.739,coor(3,i)
!                 write(12,*)  element(i),coor(1,i)+16.869,coor(2,i)-9.739,coor(3,i)
!                 write(12,*)  element(i),coor(1,i)+5.623,coor(2,i)-9.739,coor(3,i)
!              enddo

!              do i=3,nwat,3
!                 write(12,*)  element(i),coor(1,i),coor(2,i),coor(3,i)
!                 write(12,*)  element(i),coor(1,i)-11.246,coor(2,i),coor(3,i)
!                 write(12,*)  element(i),coor(1,i)+11.246,coor(2,i),coor(3,i)
!                 write(12,*)  element(i),coor(1,i)-16.869,coor(2,i)+9.739,coor(3,i)
!                 write(12,*)  element(i),coor(1,i)-5.623,coor(2,i)+9.739,coor(3,i)
!                 write(12,*)  element(i),coor(1,i)+5.623,coor(2,i)+9.739,coor(3,i)
!                 write(12,*)  element(i),coor(1,i)-5.623,coor(2,i)-9.739,coor(3,i)
!                 write(12,*)  element(i),coor(1,i)+16.869,coor(2,i)-9.739,coor(3,i)
!                 write(12,*)  element(i),coor(1,i)+5.623,coor(2,i)-9.739,coor(3,i)
!              enddo
 
!             do i=1,nion
!                 write(12,*)  element(i+207),coor(1,i+207),coor(2,i+207),coor(3,i+207) 
!              enddo
!          CLOSE(12)
        enddo

!99      format(a4,i9)
!100     format(a80)
!120     continue
        close(11)
       END program

