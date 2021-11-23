      PROGRAM watorient
      IMPLICIT NONE
      INTEGER::ioerr
      INTEGER::i,j,k,m,n1,n2,n3,nat,nion1,nion2,nwat,noh,nhb_wat,nhb_oh,nmetal
      INTEGER::ibin,nbin,cbin
      INTEGER,ALLOCATABLE::iwat(:),ioh(:)
      INTEGER::nframe,nframe_max

      REAL(KIND=KIND(0.0D0))::d,d1,d2,d_0,avzmin,avzmax,dphi
      REAL(KIND=KIND(0.0D0))::box(3),pt_down(2,16),pt_up(2,16),bridge_down(2,32),&
      bridge_up(2,32),dipole(3)
      REAL(KIND=KIND(0.0D0)),ALLOCATABLE::coor(:,:),oho(:,:),g1(:),g2(:),g3(:)

      CHARACTER::input*20,dummy*20,element*5
      LOGICAL::watori,ionpos


!initialisation

      ioerr = 0
      nframe = 20000
      nmetal=96
      avzmin = 5.176
      avzmax = 30.770
      nbin=30
      dphi=0.1
      nat = 0
      nwat = 0
      noh= 0
      d = 0
      d_0 = 0
      n1 = 0
      n2 = 0
      n3 = 0



!read input file from stdin

      readinput: DO
         READ(5,*,IOSTAT=ioerr) input

         IF ( ioerr < 0 ) THEN
            EXIT readinput
!        ELSEIF (ioerr > 0) THEN
!          STOP "error in reading input file"
         END IF

         WRITE(6,*) input,ioerr

         IF ( INDEX(input,"BOX") /= 0 ) THEN
            READ(5,*) (box(i),i=1,3)
!            WRITE(*,*) (box(i),i=1,3)
         ELSEIF ( INDEX(input,"NAT") /= 0 ) THEN
            READ(5,*) nat
!            WRITE(*,*) nat
         ELSEIF ( INDEX(input,"WATER") /= 0 ) THEN
            READ(5,*) nwat
            ALLOCATE(iwat(nwat))
            READ(5,*) (iwat(i),i=1,nwat)
!            WRITE(*,*) (iwat(i),i=1,nwat)
         ELSEIF (INDEX(input,"OH") /= 0 ) THEN
            READ(5,*) noh
            ALLOCATE(ioh(noh))
            READ(5,*) (ioh(i),i=1,noh)
         ELSE
            STOP "wrong input"
         END IF
      END DO readinput


!read coordinates from TRAJECTORY file in xyz format



      ALLOCATE(oho(3,noh))
      ALLOCATE(coor(3,nat))
      ALLOCATE(g1(nbin))
      ALLOCATE(g2(nbin))
      ALLOCATE(g3(nbin))



      OPEN(11,FILE="TRAJECTORY",STATUS='OLD',IOSTAT=ioerr)
      OPEN(12,FILE="total_pos.dat")
      OPEN(13,FILE="bridge_pos.dat")
      OPEN(14,FILE="top_pos.dat")


      DO i=1,nframe

          READ(11,*,IOSTAT=ioerr) dummy
          IF ( ioerr < 0 ) THEN
              EXIT
          END IF
          READ(11,*) dummy

          DO j=1,nat
              READ(11,*) element,coor(1,j),coor(2,j),coor(3,j)
          ENDDO

          DO j=1,noh
              oho(1,j)=coor(1,nat-2*noh+2*j-1)
              oho(2,j)=coor(2,nat-2*noh+2*j-1)
              oho(3,j)=coor(3,nat-2*noh+2*j-1)
          END DO

          ! top site coordinates
          DO j=1,16
              pt_down(1,j)=coor(1,3*nwat+j)
              pt_down(2,j)=coor(2,3*nwat+j)
              pt_up(1,j)=coor(1,3*nwat+80+j)
              pt_up(2,j)=coor(2,3*nwat+80+j)
          END DO

          !bridge site coordinates
          DO j=1,4

              bridge_down(1,4*j-3)=(pt_down(1,4*j-3)+pt_down(1,4*j-2))/2
              bridge_down(2,4*j-3)=(pt_down(2,4*j-3)+pt_down(2,4*j-2))/2
              bridge_down(1,4*j-2)=(pt_down(1,4*j-2)+pt_down(1,4*j-1))/2
              bridge_down(2,4*j-2)=(pt_down(2,4*j-2)+pt_down(2,4*j-1))/2
              bridge_down(1,4*j-1)=(pt_down(1,4*j-1)+pt_down(1,4*j))/2
              bridge_down(2,4*j-1)=(pt_down(2,4*j-1)+pt_down(2,4*j))/2
              bridge_down(1,4*j)=(pt_down(1,4*j)+pt_down(1,4*j-3)-box(1))/2
              bridge_down(2,4*j)=(pt_down(2,4*j)+pt_down(2,4*j-3))/2

              bridge_down(1,16+4*j-3)=(pt_down(1,j)+pt_down(1,j+4))/2
              bridge_down(2,16+4*j-3)=(pt_down(2,j)+pt_down(2,j+4))/2
              bridge_down(1,16+4*j-2)=(pt_down(1,j+4)+pt_down(1,j+8))/2
              bridge_down(2,16+4*j-2)=(pt_down(2,j+4)+pt_down(2,j+8))/2
              bridge_down(1,16+4*j-1)=(pt_down(1,j+8)+pt_down(1,j+12))/2
              bridge_down(2,16+4*j-1)=(pt_down(2,j+8)+pt_down(2,j+12))/2
              bridge_down(1,16+4*j)=(pt_down(1,j+12)+pt_down(1,j))/2
              bridge_down(2,16+4*j)=(pt_down(2,j+12)+pt_down(2,j)-box(2))/2

              bridge_up(1,4*j-3)=(pt_up(1,4*j-3)+pt_up(1,4*j-2))/2
              bridge_up(2,4*j-3)=(pt_up(2,4*j-3)+pt_up(2,4*j-2))/2
              bridge_up(1,4*j-2)=(pt_up(1,4*j-2)+pt_up(1,4*j-1))/2
              bridge_up(2,4*j-2)=(pt_up(2,4*j-2)+pt_up(2,4*j-1))/2
              bridge_up(1,4*j-1)=(pt_up(1,4*j-1)+pt_up(1,4*j))/2
              bridge_up(2,4*j-1)=(pt_up(2,4*j-1)+pt_up(2,4*j))/2
              bridge_up(1,4*j)=(pt_up(1,4*j)+pt_up(1,4*j-3)-box(1))/2
              bridge_up(2,4*j)=(pt_up(2,4*j)+pt_up(2,4*j-3))/2

              bridge_up(1,16+4*j-3)=(pt_up(1,j)+pt_up(1,j+4))/2
              bridge_up(2,16+4*j-3)=(pt_up(2,j)+pt_up(2,j+4))/2
              bridge_up(1,16+4*j-2)=(pt_up(1,j+4)+pt_up(1,j+8))/2
              bridge_up(2,16+4*j-2)=(pt_up(2,j+4)+pt_up(2,j+8))/2
              bridge_up(1,16+4*j-1)=(pt_up(1,j+8)+pt_up(1,j+12))/2
              bridge_up(2,16+4*j-1)=(pt_up(2,j+8)+pt_up(2,j+12))/2
              bridge_up(1,16+4*j)=(pt_up(1,j+12)+pt_up(1,j))/2
              bridge_up(2,16+4*j)=(pt_up(2,j+12)+pt_up(2,j)-box(2))/2

          END DO

          DO j=1,noh
              d1=4
              d2=4
              ! down
              IF (oho(3,j)<box(3)/2) THEN
                  ! top site
                  DO k=1,16
                      IF ((pt_down(1,k)-oho(1,j)) > box(1)/2) THEN
                          oho(1,j)=oho(1,j)+box(1)*(FLOOR((pt_down(1,k)-oho(1,j)-box(1)/2)/box(1))+1)
                      ELSEIF ( (pt_down(1,k)-oho(1,j)) < -box(1)/2 ) THEN
                          oho(1,j)=oho(1,j)+box(1)*(FLOOR((pt_down(1,k)-oho(1,j)+box(1)/2)/box(1)))
                      END IF
                      IF ((pt_down(2,k)-oho(2,j)) > box(2)/2) THEN
                          oho(2,j)=oho(2,j)+box(2)*(FLOOR((pt_down(2,k)-oho(2,j)-box(2)/2)/box(2))+1)
                      ELSEIF ( (pt_down(2,k)-oho(2,j)) < -box(2)/2 ) THEN
                          oho(2,j)=oho(2,j)+box(2)*(FLOOR((pt_down(2,k)-oho(2,j)+box(2)/2)/box(2)))
                      END IF
                      d=sqrt((pt_down(1,k)-oho(1,j))**2+(pt_down(2,k)-oho(2,j))**2)
                      IF (d<d1) THEN
                          d1=d
                      END IF
                  END DO !k

                  ! bridge site
                  DO k=1,32
                      IF ((bridge_down(1,k)-oho(1,j)) > box(1)/2) THEN
                          oho(1,j)=oho(1,j)+box(1)*(FLOOR((bridge_down(1,k)-oho(1,j)-box(1)/2)/box(1))+1)
                      ELSEIF ( (bridge_down(1,k)-oho(1,j)) < -box(1)/2 ) THEN
                          oho(1,j)=oho(1,j)+box(1)*(FLOOR((bridge_down(1,k)-oho(1,j)+box(1)/2)/box(1)))
                      END IF
                      IF ((bridge_down(2,k)-oho(2,j)) > box(2)/2) THEN
                          oho(2,j)=oho(2,j)+box(2)*(FLOOR((bridge_down(2,k)-oho(2,j)-box(2)/2)/box(2))+1)
                      ELSEIF ( (bridge_down(2,k)-oho(2,j)) < -box(2)/2 ) THEN
                          oho(2,j)=oho(2,j)+box(2)*(FLOOR((bridge_down(2,k)-oho(2,j)+box(2)/2)/box(2)))
                      END IF

                      d=sqrt((bridge_down(1,k)-oho(1,j))**2+(bridge_down(2,k)-oho(2,j))**2)
                      IF (d<d2) THEN
                          d2=d
                      END IF
                  END DO !k

                  n1=n1+1
                  d_0=oho(3,j)-avzmin
                  DO k=1,nbin
                      IF ( d_0 >= (k-1)*dphi .AND. d_0 < k*dphi ) THEN
                          g1(k)=g1(k)+1
                      END IF
                  END DO !k

                  IF (d1>d2) THEN !bridge site
                      n2=n2+1
                      DO k=1,nbin
                          IF ( d_0 >= (k-1)*dphi .AND. d_0 < k*dphi ) THEN
                              g2(k)=g2(k)+1
                          END IF
                      END DO !k
                  ELSE !top site
                      n3=n3+1
                      DO k=1,nbin
                          IF ( d_0 >= (k-1)*dphi .AND. d_0 < k*dphi ) THEN
                              g3(k)=g3(k)+1
                          END IF
                      END DO !k
                  END IF

              END IF
              ! end down

              ! up
              IF (oho(3,j)>box(3)/2) THEN
                  ! top site
                  DO k=1,16
                      IF ((pt_up(1,k)-oho(1,j)) > box(1)/2) THEN
                          oho(1,j)=oho(1,j)+box(1)*(FLOOR((pt_up(1,k)-oho(1,j)-box(1)/2)/box(1))+1)
                      ELSEIF ( (pt_up(1,k)-oho(1,j)) < -box(1)/2 ) THEN
                          oho(1,j)=oho(1,j)+box(1)*(FLOOR((pt_up(1,k)-oho(1,j)+box(1)/2)/box(1)))
                      END IF
                      IF ((pt_up(2,k)-oho(2,j)) > box(2)/2) THEN
                          oho(2,j)=oho(2,j)+box(2)*(FLOOR((pt_up(2,k)-oho(2,j)-box(2)/2)/box(2))+1)
                      ELSEIF ( (pt_up(2,k)-oho(2,j)) < -box(2)/2 ) THEN
                          oho(2,j)=oho(2,j)+box(2)*(FLOOR((pt_up(2,k)-oho(2,j)+box(2)/2)/box(2)))
                      END IF
                      d=sqrt((pt_up(1,k)-oho(1,j))**2+(pt_up(2,k)-oho(2,j))**2)
                      IF (d<d1) THEN
                          d1=d
                      END IF
                  END DO !k

                  ! bridge site
                  DO k=1,32
                      IF ((bridge_up(1,k)-oho(1,j)) > box(1)/2) THEN
                          oho(1,j)=oho(1,j)+box(1)*(FLOOR((bridge_up(1,k)-oho(1,j)-box(1)/2)/box(1))+1)
                      ELSEIF ( (bridge_up(1,k)-oho(1,j)) < -box(1)/2 ) THEN
                          oho(1,j)=oho(1,j)+box(1)*(FLOOR((bridge_up(1,k)-oho(1,j)+box(1)/2)/box(1)))
                      END IF
                      IF ((bridge_up(2,k)-oho(2,j)) > box(2)/2) THEN
                          oho(2,j)=oho(2,j)+box(2)*(FLOOR((bridge_up(2,k)-oho(2,j)-box(2)/2)/box(2))+1)
                      ELSEIF ( (bridge_up(2,k)-oho(2,j)) < -box(2)/2 ) THEN
                          oho(2,j)=oho(2,j)+box(2)*(FLOOR((bridge_up(2,k)-oho(2,j)+box(2)/2)/box(2)))
                      END IF
                      d=sqrt((bridge_up(1,k)-oho(1,j))**2+(bridge_up(2,k)-oho(2,j))**2)
                      IF (d<d2) THEN
                          d2=d

                      END IF
                  END DO !k

                  n1=n1+1
                  d_0=avzmax-oho(3,j)
                  DO k=1,nbin
                      IF ( d_0 >= (k-1)*dphi .AND. d_0 < k*dphi ) THEN
                          g1(k)=g1(k)+1
                      END IF
                  END DO !k

                  IF (d1>d2) THEN !bridge site
                      n2=n2+1
                      DO k=1,nbin
                          IF ( d_0 >= (k-1)*dphi .AND. d_0 < k*dphi ) THEN
                              g2(k)=g2(k)+1
                          END IF
                      END DO !k
                  ELSE !top site
                      n3=n3+1
                      DO k=1,nbin
                          IF ( d_0 >= (k-1)*dphi .AND. d_0 < k*dphi ) THEN
                              g3(k)=g3(k)+1
                          END IF
                      END DO !k
                  END IF

              END IF
              ! end up
!              WRITE(12,*) d1,d2

          END DO !j

      END DO !i

      DO i=1,nbin
          WRITE(12,*) i*dphi-dphi/2,DBLE(g1(i))/DBLE(n1)
          WRITE(13,*) i*dphi-dphi/2,DBLE(g2(i))/DBLE(n1)
          WRITE(14,*) i*dphi-dphi/2,DBLE(g3(i))/DBLE(n1)
      END DO


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!reading the whole trajectory is finished
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      CLOSE(11)
      CLOSE(12)
      CLOSE(13)
      CLOSE(14)

   CONTAINS

      FUNCTION vector(point1,point2,cell)
           IMPLICIT NONE
           INTEGER::i
           REAL(KIND=KIND(0.0D0))::vector(3),point1(3),point2(3),cell(3)
           vector=point1-point2
           DO i=1,3
              vector(i)=vector(i)-cell(i)*ANINT(vector(i)/cell(i))
           END DO
       END FUNCTION vector

    END PROGRAM watorient

