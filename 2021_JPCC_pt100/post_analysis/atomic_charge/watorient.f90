      PROGRAM watorient
      IMPLICIT NONE
      INTEGER::ioerr
      INTEGER::i,j,k,m,n1,n2,n3,ix,nat,nion1,nion2,nwat,noh,nhb_wat,nhb_oh,nmetal
      INTEGER::ibin,nbin,cbin,dphi
      INTEGER,ALLOCATABLE::iwat(:),ioh(:)
      INTEGER::nframe
      INTEGER::temp1

      REAL(KIND=KIND(0.0D0))::d,d1,d2,angle,cangle,avzmin,avzmax
      REAL(KIND=KIND(0.0D0))::mc1_1,mc1_2,mc2_1,mc2_2,mc3_1,mc3_2,hc1_1,hc1_2,&
      hc2_1,hc2_2,hc3_1,hc3_2,mc4_1,mc4_2,hc4_1,hc4_2
      REAL::temp2,temp3,temp4
      REAL(KIND=KIND(0.0D0))::box(3),pt_down(2,16),pt_up(2,16),bridge_down(2,32),&
      bridge_up(2,32),dipole(3)
      REAL(KIND=KIND(0.0D0)),ALLOCATABLE::coor(:,:),oho(:,:),wato(:,:),charge_m(:),charge_h(:)

      CHARACTER::input*20,dummy*20,element*5
      LOGICAL::watori,ionpos


!initialisation

      ioerr = 0
      nframe = 1
      nmetal=48
      nbin=36
      dphi=5
      avzmin = 0
      avzmax = 0
      nat = 0
      nwat = 0
      noh= 0
      d = 0
      n1 = 0
      n2 = 0
      n3 = 0
      mc1_1 = 0
      mc1_2 = 0
      mc2_1 = 0
      mc2_2 = 0
      mc3_1 = 0
      mc3_2 = 0
      hc1_1 = 0
      hc1_2 = 0
      hc2_1 = 0
      hc2_2 = 0
      hc3_1 = 0
      hc3_2 = 0
      mc4_1 = 0
      mc4_2 = 0
      hc4_1 = 0
      hc4_2 = 0

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
!         ELSEIF ( INDEX(input,"NAT") /= 0 ) THEN
!            READ(5,*) nat
!            WRITE(*,*) nat
!         ELSEIF ( INDEX(input,"WATER") /= 0 ) THEN
!            READ(5,*) nwat
!            ALLOCATE(iwat(nwat))
!            READ(5,*) (iwat(i),i=1,nwat)
!            WRITE(*,*) (iwat(i),i=1,nwat)
         ELSEIF (INDEX(input,"OH") /= 0 ) THEN
            READ(5,*) noh
            ALLOCATE(ioh(noh))
!            READ(5,*) (ioh(i),i=1,noh)
         ELSE
            STOP "wrong input"
         END IF
      END DO readinput


!read coordinates from TRAJECTORY file in xyz format
      
      OPEN(20,FILE="index.dat")
      READ(20,*) nat

      nwat=(nat-2*noh-48)/3


      ALLOCATE(oho(3,noh))
!      ALLOCATE(wato(3,nwat))
      ALLOCATE(coor(3,nat))

      ALLOCATE(charge_m(nat))
      ALLOCATE(charge_h(nat))

      DO i=1,noh
          ioh(i)=nat-2*noh+2*i-1
      END DO

      OPEN(11,FILE="TRAJECTORY",STATUS='OLD',IOSTAT=ioerr)
      OPEN(12,FILE="mulliken.dat",STATUS='OLD',IOSTAT=ioerr)
      OPEN(13,FILE="hirshfeld.dat",STATUS='OLD',IOSTAT=ioerr)


!================ read the charge data ================

      DO i = 1,2
          read(12,*) dummy
          read(13,*) dummy
      END DO
      DO i = 1,nat
          read(12,*) temp1,element,temp2,temp3,charge_m(i)
          read(13,*) temp1,element,temp2,temp3,temp4,charge_h(i)
      END DO

!================ read the charge data ================

      DO i=1,nframe

!          READ(11,*,IOSTAT=ioerr) dummy
!          READ(11,*) dummy

          DO j=1,nat
              READ(11,*) element,coor(1,j),coor(2,j),coor(3,j)
          ENDDO

          DO j=1,noh
              oho(1,j)=coor(1,nat-2*noh+2*j-1)
              oho(2,j)=coor(2,nat-2*noh+2*j-1)
              oho(3,j)=coor(3,nat-2*noh+2*j-1)
          END DO

!          DO j=1,nwat
!              wato(1,j)=coor(1,3*j-2)
!              wato(2,j)=coor(2,3*j-2)
!              wato(3,j)=coor(3,3*j-2)
!          END DO

          DO j=1,16
              avzmin=avzmin+coor(3,3*nwat+j)
              avzmax=avzmax+coor(3,3*nwat+80+j)
          END DO
          avzmin=avzmin/16
          avzmax=avzmax/16
!          WRITE(*,*) avzmin,avzmax

          ! top site coordinates
          DO j=1,16
              pt_down(1,j)=coor(1,3*nwat+j)
              pt_down(2,j)=coor(2,3*nwat+j)
!              pt_up(1,j)=coor(1,3*nwat+80+j)
!              pt_up(2,j)=coor(2,3*nwat+80+j)
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

!              bridge_up(1,4*j-3)=(pt_up(1,4*j-3)+pt_up(1,4*j-2))/2
!              bridge_up(2,4*j-3)=(pt_up(2,4*j-3)+pt_up(2,4*j-2))/2
!              bridge_up(1,4*j-2)=(pt_up(1,4*j-2)+pt_up(1,4*j-1))/2
!              bridge_up(2,4*j-2)=(pt_up(2,4*j-2)+pt_up(2,4*j-1))/2
!              bridge_up(1,4*j-1)=(pt_up(1,4*j-1)+pt_up(1,4*j))/2
!              bridge_up(2,4*j-1)=(pt_up(2,4*j-1)+pt_up(2,4*j))/2
!              bridge_up(1,4*j)=(pt_up(1,4*j)+pt_up(1,4*j-3)-box(1))/2
!              bridge_up(2,4*j)=(pt_up(2,4*j)+pt_up(2,4*j-3))/2

!              bridge_up(1,16+4*j-3)=(pt_up(1,j)+pt_up(1,j+4))/2
!              bridge_up(2,16+4*j-3)=(pt_up(2,j)+pt_up(2,j+4))/2
!              bridge_up(1,16+4*j-2)=(pt_up(1,j+4)+pt_up(1,j+8))/2
!              bridge_up(2,16+4*j-2)=(pt_up(2,j+4)+pt_up(2,j+8))/2
!              bridge_up(1,16+4*j-1)=(pt_up(1,j+8)+pt_up(1,j+12))/2
!              bridge_up(2,16+4*j-1)=(pt_up(2,j+8)+pt_up(2,j+12))/2
!              bridge_up(1,16+4*j)=(pt_up(1,j+12)+pt_up(1,j))/2
!              bridge_up(2,16+4*j)=(pt_up(2,j+12)+pt_up(2,j)-box(2))/2

          END DO

          DO j=1,noh
              d1=4
              d2=4
              ! down
!              IF (oho(3,j)<box(3)/2) THEN
                  ! distance to top site
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

                  ! distance to bridge site
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

                  IF (d1>d2) THEN !bridge site
                      n1=n1+1
                      mc1_1=mc1_1+charge_m(ioh(j))
                      mc1_2=mc1_2+charge_m(ioh(j)+1)
                      hc1_1=hc1_1+charge_h(ioh(j))
                      hc1_2=hc1_2+charge_h(ioh(j)+1)
                  ELSE !top site
                      n2=n2+1
                      mc2_1=mc2_1+charge_m(ioh(j))
                      mc2_2=mc2_2+charge_m(ioh(j)+1)
                      hc2_1=hc2_1+charge_h(ioh(j))
                      hc2_2=hc2_2+charge_h(ioh(j)+1)
                  END IF

!              END IF
              ! end down

              ! up
!              IF (oho(3,j)>box(3)/2) THEN
!                  ! top site
!                  DO k=1,16
!                      IF ((pt_up(1,k)-oho(1,j)) > box(1)/2) THEN
!                          oho(1,j)=oho(1,j)+box(1)*(FLOOR((pt_up(1,k)-oho(1,j)-box(1)/2)/box(1))+1)
!                      ELSEIF ( (pt_up(1,k)-oho(1,j)) < -box(1)/2 ) THEN
!                          oho(1,j)=oho(1,j)+box(1)*(FLOOR((pt_up(1,k)-oho(1,j)+box(1)/2)/box(1)))
!                      END IF
!                      IF ((pt_up(2,k)-oho(2,j)) > box(2)/2) THEN
!                          oho(2,j)=oho(2,j)+box(2)*(FLOOR((pt_up(2,k)-oho(2,j)-box(2)/2)/box(2))+1)
!                      ELSEIF ( (pt_up(2,k)-oho(2,j)) < -box(2)/2 ) THEN
!                          oho(2,j)=oho(2,j)+box(2)*(FLOOR((pt_up(2,k)-oho(2,j)+box(2)/2)/box(2)))
!                      END IF
!                      d=sqrt((pt_up(1,k)-oho(1,j))**2+(pt_up(2,k)-oho(2,j))**2)
!                      IF (d<d1) THEN
!                          d1=d
!                      END IF
!                  END DO !k

                  ! bridge site
!                  DO k=1,32
!                      IF ((bridge_up(1,k)-oho(1,j)) > box(1)/2) THEN
!                          oho(1,j)=oho(1,j)+box(1)*(FLOOR((bridge_up(1,k)-oho(1,j)-box(1)/2)/box(1))+1)
!                      ELSEIF ( (bridge_up(1,k)-oho(1,j)) < -box(1)/2 ) THEN
!                          oho(1,j)=oho(1,j)+box(1)*(FLOOR((bridge_up(1,k)-oho(1,j)+box(1)/2)/box(1)))
!                      END IF
!                      IF ((bridge_up(2,k)-oho(2,j)) > box(2)/2) THEN
!                          oho(2,j)=oho(2,j)+box(2)*(FLOOR((bridge_up(2,k)-oho(2,j)-box(2)/2)/box(2))+1)
!                      ELSEIF ( (bridge_up(2,k)-oho(2,j)) < -box(2)/2 ) THEN
!                          oho(2,j)=oho(2,j)+box(2)*(FLOOR((bridge_up(2,k)-oho(2,j)+box(2)/2)/box(2)))
!                      END IF
!                      d=sqrt((bridge_up(1,k)-oho(1,j))**2+(bridge_up(2,k)-oho(2,j))**2)
!                      IF (d<d2) THEN
!                          d2=d

!                      END IF
!                  END DO !k

!                  IF (d1>d2) THEN !bridge site
!                      n1=n1+1
!                      mc1_1=mc1_1+charge_m(ioh(j))
!                      mc1_2=mc1_2+charge_m(ioh(j)+1)
!                      hc1_1=hc1_1+charge_h(ioh(j))
!                      hc1_2=hc1_2+charge_h(ioh(j)+1)
!                  ELSE !top site
!                      n2=n2+1
!                      mc2_1=mc2_1+charge_m(ioh(j))
!                      mc2_2=mc2_2+charge_m(ioh(j)+1)
!                      hc2_1=hc2_1+charge_h(ioh(j))
!                      hc2_2=hc2_2+charge_h(ioh(j)+1)
!                  END IF

!              END IF
              ! end up

          END DO !j

!          DO j=1,nwat
!              IF ( (wato(3,j)<(avzmin+2.7)) .OR. (wato(3,j)>=(avzmax-2.7)) ) THEN
!                  n3=n3+1
!                  mc3_1=mc3_1+charge_m(iwat(j))
!                  mc3_2=mc3_2+(charge_m(iwat(j)+1)+charge_m(iwat(j)+2))/2
!                  hc3_1=hc3_1+charge_h(iwat(j))
!                  hc3_2=hc3_2+(charge_h(iwat(j)+1)+charge_h(iwat(j)+2))/2
!              END IF
!
 !         END DO

      END DO !i

      DO i=1,16
          mc4_1=mc4_1+charge_m(nwat*3+i)
!          mc4_1=mc4_1+charge_m(nwat*3+80+i)
          hc4_1=hc4_1+charge_h(nwat*3+i)
 !         hc4_1=hc4_1+charge_h(nwat*3+80+i)
      END DO
      mc4_1=mc4_1/16
      hc4_1=hc4_1/16

      DO i=1,nmetal
          mc4_2=mc4_2+charge_m(nwat*3+i)
          hc4_2=hc4_2+charge_h(nwat*3+i)
      END DO
      mc4_2=mc4_2/nmetal
      hc4_2=hc4_2/nmetal


      OPEN(14,FILE="1.out")
      OPEN(15,FILE="2.out")
      OPEN(16,FILE="3.out")
      OPEN(17,FILE="4.out")
      OPEN(18,FILE="5.out")

!      WRITE(14,*) 'bridge-site OH'
!      WRITE(14,*) 'mulliken O ','mulliken H ','mulliken total ','hirshfeld O ','hirshfeld H ','hirshfeld total'
      WRITE(14,*) DBLE(mc1_1)/DBLE(n1),DBLE(mc1_2)/DBLE(n1),(DBLE(mc1_1)+DBLE(mc1_2))/DBLE(n1),&
      DBLE(hc1_1)/DBLE(n1),DBLE(hc1_2)/DBLE(n1),(DBLE(hc1_1)+DBLE(hc1_2))/DBLE(n1)
      WRITE(15,*) DBLE(mc2_1)/DBLE(n2),DBLE(mc2_2)/DBLE(n2),(DBLE(mc2_1)+DBLE(mc2_2))/DBLE(n2),&
      DBLE(hc2_1)/DBLE(n2),DBLE(hc2_2)/DBLE(n2),(DBLE(hc2_1)+DBLE(hc2_2))/DBLE(n2)
!      WRITE(16,*) DBLE(mc3_1)/DBLE(n3),DBLE(mc3_2)/DBLE(n3),(DBLE(mc3_1)+2*DBLE(mc3_2))/DBLE(n3),&
!      DBLE(hc3_1)/DBLE(n3),DBLE(hc3_2)/DBLE(n3),(DBLE(hc3_1)+2*DBLE(hc3_2))/DBLE(n3)
      WRITE(17,*) mc4_1,hc4_1,mc4_2,hc4_2
      WRITE(18,*) DBLE(mc1_1)+DBLE(mc1_2)+DBLE(mc2_1)+DBLE(mc2_2),&
      (DBLE(mc1_1)+DBLE(mc1_2)+DBLE(mc2_1)+DBLE(mc2_2))/(DBLE(n1)+DBLE(n2)),&
      DBLE(hc1_1)+DBLE(hc1_2)+DBLE(hc2_1)+DBLE(hc2_2),&
      (DBLE(hc1_1)+DBLE(hc1_2)+DBLE(hc2_1)+DBLE(hc2_2))/(DBLE(n1)+DBLE(n2))

!      WRITE(14,*) 'top-site OH'
!      WRITE(14,*) 'mulliken O ','mulliken H ','mulliken total ','hirshfeld O ','hirshfeld H ','hirshfeld total'

!      WRITE(14,*) 'wata'
!      WRITE(14,*) 'mulliken O ','mulliken H ','mulliken total ','hirshfeld O ','hirshfeld H ','hirshfeld total'

!      WRITE(14,*) 'surface Pt'
!      WRITE(14,*) 'mulliken ','hirshfeld'




!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!reading the whole trajectory is finished
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      CLOSE(11)
      CLOSE(12)
      CLOSE(13)
      CLOSE(14)
      CLOSE(15)
      CLOSE(16)
      CLOSE(17)
      CLOSE(18)

    END PROGRAM watorient

