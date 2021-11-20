      program estraggo
        character*80 pippo
        character*8 name
        real*8 x,y,z,zmin,zmax
        REAL,DIMENSION(3,597)::coor
        CHARACTER*4,DIMENSION(597)::element

        natoms=421
        npt=96
        nwat=291
        nion=38
        nframes=19251
        read(11,*) nstep

        OPEN (12,FILE='TRAJECTORY')
        do i=1,nframes
           read(12,*) l
           read(12,99) a,k
!           zmin = 0
!           zmax = 0
           if (k.eq.nstep) then
              do j=1,natoms
                 read(12,*)  element(j),(coor(ix,j),ix=1,3)
              end do
!              do j=1,16
!                 zmin = zmin + coor(3,nwat+48+j)/16            
!              enddo
!              do j=1,16
!                 zmax = zmax + coor(3,nwat+j)/16 
!              enddo
!             print *, zmin, zmax
       
!            DO j=1,nwat/3
!               IF ( coor(3,3*j-2) < (zmin+2.8) .OR. coor(3,3*j-2) > (zmax-2.8) ) THEN
!               write(13,'(a4,3 f10.5)') 'OA',(coor(ix,3*j-2),ix=1,3)
!               write(13,'(a4,3 f10.5)') 'HA',(coor(ix,3*j-1),ix=1,3)
!               write(13,'(a4,3 f10.5)') 'HA',(coor(ix,3*j),ix=1,3)
!               END IF
!            END DO

!            DO j=1,nwat/3
!               IF (  coor(3,3*j-2) < (zmin+4.0) ) THEN
!               write(13,'(a4,3 f10.5)') 'OB',(coor(ix,3*j-2),ix=1,3)
!               write(13,'(a4,3 f10.5)') 'HB',(coor(ix,3*j-1),ix=1,3)
!               write(13,'(a4,3 f10.5)') 'HB',(coor(ix,3*j),ix=1,3)
!               END IF
!            END DO
            
!            DO j=1,nwat/3
!               IF (  coor(3,3*j-2) > (zmax-4.0) ) THEN
!               write(13,'(a4,3 f10.5)') 'OB',(coor(ix,3*j-2),ix=1,3)
!               write(13,'(a4,3 f10.5)') 'HB',(coor(ix,3*j-1),ix=1,3)
!               write(13,'(a4,3 f10.5)') 'HB',(coor(ix,3*j),ix=1,3)
!               END IF
!            END DO

!            DO j=1,nwat/3
!               IF ( coor(3,3*j-2) > 0  ) THEN
!               write(13,'(a4,3 f10.5)') 'OC',(coor(ix,3*j-2),ix=1,3)
!               write(13,'(a4,3 f10.5)') 'HC',(coor(ix,3*j-1),ix=1,3)
!               write(13,'(a4,3 f10.5)') 'HC',(coor(ix,3*j),ix=1,3)
!               END IF
!            END DO

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


