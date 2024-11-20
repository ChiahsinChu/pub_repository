      PROGRAM watorient
      IMPLICIT NONE
      INTEGER::ioerr
      INTEGER::i,j,k,m,n,ix,nat,nion1,nion2,nwat,noh,nhb_wat,nhb_oh
      INTEGER::ibin,nbin,cbin
      INTEGER,ALLOCATABLE::iwat(:),ioh(:)
      INTEGER::nframe,nframe_max
      INTEGER::g,h1,h2,h3,h4,h5,h6,h7,h8

      REAL(KIND=KIND(0.0D0))::zmin,zmax,z,dz,avzmin,avzmax,avz,d,&
                              costheta1,costheta2
      REAL(KIND=KIND(0.0D0))::box(3),dipole1(3),dipole2(3),dipole3(3)
      REAL(KIND=KIND(0.0D0)),ALLOCATABLE:: wato(:,:),wath1(:,:),wath2(:,:),&
      coor(:,:),oho(:,:),ohh(:,:)

      CHARACTER::input*20,element*5,dummy*20
      LOGICAL::watori,ionpos

!**************************************************************************************************
!nion1,nion2,nwat,nsurf1,nsurf2               number of ions,waters,surface atoms
!iion1,nion2,iwat,isurf1,isurf2               index of ions,waters,surface atoms
!ion1,ion2,wato,wath1,wath2,surf1,surf2       coordinates of ions,waters,surface atoms
!zmin,zmax,z                                  surface bounds along z axis
!avzmin,avzmax,avz                            averaging along MD trajectory
!ibin,nbin,dz                                 index,total number and width of bin
!g,costheta,sintheta                          histogram and dipole orientation parameter
!zion,z2ion,avzion,avz2ion,varzion            z dimension,averaged and variance of ion position,
!watori,ionpos                                logic variables controlling which data are computed
!**************************************************************************************************

!initialisation

      ioerr = 0
      nframe = 36476
      avzmin =  5.366
      avzmax =  30.768      
      nat = 0
      nwat = 0
      noh= 0
      costheta1=0.00
      costheta2=0.00

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
!            WRITE(*,*) (ioh(i),i=1,noh)
!         ELSEIF ( INDEX(input,"DZ") /= 0 ) THEN
!            READ(5,*) dz
         ELSE
            STOP "wrong input"
         END IF
      END DO readinput


!read coordinates from TRAJECTORY file in xyz format


      ALLOCATE(wato(3,nwat))
      ALLOCATE(wath1(3,nwat))
      ALLOCATE(wath2(3,nwat))
      ALLOCATE(oho(3,noh))
      ALLOCATE(ohh(3,noh))
      ALLOCATE(coor(3,nat))

      g = 0
      h1 = 0
      h2 = 0
      h3 = 0
      h4 = 0
      h5 = 0
      h6 = 0
      h7 = 0
      h8 = 0

      OPEN(11,FILE="TRAJECTORY",STATUS='OLD',IOSTAT=ioerr)
      OPEN(12,FILE="hbond.out")
      WRITE(12,*) "time","  ","wata-wata","  ","wata-watb","  ","wata-watc","  ","wata-oh",&
      "  ","wata_total","  ","oh-wata","  ","oh-watb","  ","oh-watc","  ","oh-oh","  ","oh_total"

      DO m=1,nframe
          READ(11,*,IOSTAT=ioerr) dummy
          IF ( ioerr < 0 ) THEN
              EXIT
          END IF
          READ(11,*) dummy

          DO j=1,nat
              READ(11,*) element,coor(1,j),coor(2,j),coor(3,j)
          ENDDO

!assign water coordinates, assume the format of water following O H H
          DO j=1,nwat
              wath1(1,j)=coor(1,iwat(j)+2)
              wath1(2,j)=coor(2,iwat(j)+2)
              wath1(3,j)=coor(3,iwat(j)+2)
              wath2(1,j)=coor(1,iwat(j)+1)
              wath2(2,j)=coor(2,iwat(j)+1)
              wath2(3,j)=coor(3,iwat(j)+1)
              wato(1,j)=coor(1,iwat(j))
              wato(2,j)=coor(2,iwat(j))
              wato(3,j)=coor(3,iwat(j))
          END DO

          DO j=1,noh
              oho(1,j)=coor(1,ioh(j))
              oho(2,j)=coor(2,ioh(j))
              oho(3,j)=coor(3,ioh(j))
              ohh(1,j)=coor(1,ioh(j)+1)
              ohh(2,j)=coor(2,ioh(j)+1)
              ohh(3,j)=coor(3,ioh(j)+1)
          ENDDO

!        PRINT *, wath1(2,100)
!calculate water orientation parameters, costheta and sintheta
          DO k=1,nwat
              IF ( wato(3,k) <= (avzmin+2.7) .OR. wato(3,k) >= (avzmax-2.7) ) THEN
                  g=g+1
                  DO i=1,9
                      DO j=1,nwat
                          d = SQRT((wato(1,k)-coor(1,(i-1)*(3*nwat+2*noh)+iwat(j)))**2+&
                              (wato(2,k)-coor(2,(i-1)*(3*nwat+2*noh)+iwat(j)))**2+&
                              (wato(3,k)-coor(3,(i-1)*(3*nwat+2*noh)+iwat(j)))**2)
                          IF (d > 1 .AND. d < 3.5) Then
                              dipole1=vector(wath1(:,k),wato(:,k),box)
                              dipole2=vector(wath2(:,k),wato(:,k),box)
                              dipole3=vector(coor(:,(i-1)*(3*nwat+2*noh)+iwat(j)),wato(:,k),box)
                              costheta1=(dipole1(1)*dipole3(1)+dipole1(2)*dipole3(2)+dipole1(3)*dipole3(3))&
                                        /sqrt(dipole1(1)**2+dipole1(2)**2+dipole1(3)**2)&
                                        /sqrt(dipole3(1)**2+dipole3(2)**2+dipole3(3)**2)
                              costheta2=(dipole3(1)*dipole2(1)+dipole3(2)*dipole2(2)+dipole3(3)*dipole2(3))&
                                        /sqrt(dipole2(1)**2+dipole2(2)**2+dipole2(3)**2)&
                                        /sqrt(dipole3(1)**2+dipole3(2)**2+dipole3(3)**2)
                              IF (costheta1 > 0.82 .OR. costheta2 > 0.82) THEN
                                  IF (coor(3,(i-1)*(3*nwat+2*noh)+iwat(j))<= (avzmin+2.7) &
                                     .OR. coor(3,(i-1)*(3*nwat+2*noh)+iwat(j)) > (avzmax-2.7)) THEN
                                      h1=h1+1
                                  END IF
                                  IF ((( coor(3,(i-1)*(3*nwat+2*noh)+iwat(j)) > (avzmin+2.7)) &
                                     .AND. ( coor(3,(i-1)*(3*nwat+2*noh)+iwat(j)) <= (avzmin+4.3))) .OR. &
                                     ((coor(3,(i-1)*(3*nwat+2*noh)+iwat(j)) <= (avzmax-2.7) ) .AND. &
                                     (coor(3,(i-1)*(3*nwat+2*noh)+iwat(j)) > (avzmax-4.3))))  THEN
                                      h2=h2+1
                                  END IF
                                  IF (coor(3,(i-1)*(3*nwat+2*noh)+iwat(j))> (avzmin+4.3) &
                                     .AND. coor(3,(i-1)*(3*nwat+2*noh)+iwat(j)) <= (avzmax-4.3)) THEN
                                      h3=h3+1
                                  END IF
                              END IF
                          END IF
                      END DO
                      DO j=1,noh
                          d = SQRT((wato(1,k)-coor(1,(i-1)*(3*nwat+2*noh)+ioh(j)))**2+&
                              (wato(2,k)-coor(2,(i-1)*(3*nwat+2*noh)+ioh(j)))**2+&
                              (wato(3,k)-coor(3,(i-1)*(3*nwat+2*noh)+ioh(j)))**2)
                          IF (d > 1 .AND. d < 3.5) Then
                              dipole1=vector(wath1(:,k),wato(:,k),box)
                              dipole2=vector(wath2(:,k),wato(:,k),box)
                              dipole3=vector(coor(:,(i-1)*(3*nwat+2*noh)+ioh(j)),wato(:,k),box)
                              costheta1=(dipole1(1)*dipole3(1)+dipole1(2)*dipole3(2)+dipole1(3)*dipole3(3))&
                                        /sqrt(dipole1(1)**2+dipole1(2)**2+dipole1(3)**2)&
                                        /sqrt(dipole3(1)**2+dipole3(2)**2+dipole3(3)**2)
                              costheta2=(dipole3(1)*dipole2(1)+dipole3(2)*dipole2(2)+dipole3(3)*dipole2(3))&
                                        /sqrt(dipole2(1)**2+dipole2(2)**2+dipole2(3)**2)&
                                        /sqrt(dipole3(1)**2+dipole3(2)**2+dipole3(3)**2)
                              IF (costheta1 > 0.82 .OR. costheta2 > 0.82) THEN
                                  h4=h4+1
                              END IF
                          END IF
                      END DO
                  END DO
              END IF
          END DO

          DO k=1,noh
              DO i=1,9
                  DO j=1,nwat
                      d = SQRT((oho(1,k)-coor(1,(i-1)*(3*nwat+2*noh)+iwat(j)))**2+&
                          (oho(2,k)-coor(2,(i-1)*(3*nwat+2*noh)+iwat(j)))**2+&
                          (oho(3,k)-coor(3,(i-1)*(3*nwat+2*noh)+iwat(j)))**2)
                      IF (d > 1 .AND. d < 3.5) Then
                          dipole1=vector(ohh(:,k),oho(:,k),box)
                          dipole2=vector(coor(:,(i-1)*(3*nwat+2*noh)+iwat(j)),oho(:,k),box)
                          costheta1=(dipole1(1)*dipole2(1)+dipole1(2)*dipole2(2)+dipole1(3)*dipole2(3))&
                                    /sqrt(dipole1(1)**2+dipole1(2)**2+dipole1(3)**2)&
                                    /sqrt(dipole2(1)**2+dipole2(2)**2+dipole2(3)**2)
                          IF (costheta1 > 0.82) THEN
                              IF (coor(3,(i-1)*(3*nwat+2*noh)+iwat(j))<= (avzmin+2.7) &
                                 .OR. coor(3,(i-1)*(3*nwat+2*noh)+iwat(j)) > (avzmax-2.7)) THEN
                                      h5=h5+1
                              END IF
                              IF ((( coor(3,(i-1)*(3*nwat+2*noh)+iwat(j)) > (avzmin+2.7)) &
                                     .AND. ( coor(3,(i-1)*(3*nwat+2*noh)+iwat(j)) <= (avzmin+4.3))) .OR. &
                                     ((coor(3,(i-1)*(3*nwat+2*noh)+iwat(j)) <= (avzmax-2.7) ) .AND. &
                                     (coor(3,(i-1)*(3*nwat+2*noh)+iwat(j)) > (avzmax-4.3))))  THEN
                                      h6=h6+1
                              END IF
                              IF (coor(3,(i-1)*(3*nwat+2*noh)+iwat(j))> (avzmin+4.3) &
                                 .AND. coor(3,(i-1)*(3*nwat+2*noh)+iwat(j)) <= (avzmax-4.3)) THEN
                                  h7=h7+1
                              END IF
                          END IF
                      END IF
                  END DO
                  DO j=1,noh
                      d = SQRT((oho(1,k)-coor(1,(i-1)*(3*nwat+2*noh)+ioh(j)))**2+&
                          (oho(2,k)-coor(2,(i-1)*(3*nwat+2*noh)+ioh(j)))**2+&
                          (oho(3,k)-coor(3,(i-1)*(3*nwat+2*noh)+ioh(j)))**2)
                      IF (d > 1 .AND. d < 3.5) Then
                          dipole1=vector(ohh(:,k),oho(:,k),box)
                          dipole2=vector(coor(:,(i-1)*(3*nwat+2*noh)+ioh(j)),oho(:,k),box)
                          costheta1=(dipole1(1)*dipole2(1)+dipole1(2)*dipole2(2)+dipole1(3)*dipole2(3))&
                                    /sqrt(dipole1(1)**2+dipole1(2)**2+dipole1(3)**2)&
                                    /sqrt(dipole2(1)**2+dipole2(2)**2+dipole2(3)**2)
                          IF (costheta1 > 0.82) THEN
                              h8=h8+1
                          END IF
                      END IF
                  END DO
              END DO
          END DO
          WRITE(12,*) m*0.5/1000,DBLE(h1)/DBLE(g),DBLE(h2)/DBLE(g),DBLE(h3)/DBLE(g),DBLE(h4)/DBLE(g),&
          DBLE(h1+h2+h3+h4)/DBLE(g),DBLE(h5)/DBLE(noh*m),DBLE(h6)/DBLE(noh*m),DBLE(h7)/DBLE(noh*m),&
          DBLE(h8)/DBLE(noh*m),DBLE(h5+h6+h7+h8)/DBLE(noh*m)
      END DO

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!reading the whole trajectory is finished
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      CLOSE(11)
      CLOSE(12)

!write ion position
!      IF ( ionpos ) THEN
!         avzion1=avzion1/DBLE(nframe)/DBLE(nion1)
!         avzion2=avzion2/DBLE(nframe)/DBLE(nion2)
!         avz2ion1=avz2ion1/DBLE(nframe)/DBLE(nion1)
!         avz2ion2=avz2ion2/DBLE(nframe)/DBLE(nion2)
!         avzion1=avzion1/DBLE(nframe)
!         avzion2=avzion2/DBLE(nframe)
!         avz2ion1=avzion1/DBLE(nframe)
!         avz2ion2=avzion2/DBLE(nframe)
!         varzion1=avz2ion1-avzion1**2
!         varzion2=avz2ion2-avzion2**2

!         OPEN(13,FILE="ion.out")
!         WRITE(13,*) ' avzion1 ',' varzion1 ', ' avzion2 ',' varzion2 '
!         WRITE(13,*) avzion1, varzion1, avzion2, varzion2
!         CLOSE(13)
!      END IF


      CONTAINS
!subrountine to fold up coordinates in a pbc cell
!        SUBROUTINE fold(pos,cell)
!           IMPLICIT NONE
!           INTEGER::i
!           REAL::pos(3),cell(3)
!           DO i=1,3
!              pos(i)=pos(i)-cell(i)*ANINT(pos(i)/cell(i))
!           END DO
!        END SUBROUTINE fold

!function to calculate a vector in pbc condition
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
