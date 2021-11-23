      PROGRAM watorient 
      IMPLICIT NONE 
      INTEGER::ioerr
      INTEGER::i,j,k,ix,nat,nion1,nion2,nwat,nsurf1,nsurf2,dphi 
      INTEGER::ibin,nbin,cbin
      INTEGER,ALLOCATABLE::iion1(:),iion2(:),iwat(:),isurf1(:),isurf2(:)
      INTEGER::nframe,nframe_max
      INTEGER,ALLOCATABLE::g(:),h(:)

      REAL(KIND=KIND(0.0D0))::z,dz,avzmin,avzmax,avz,avzion1,&
      avzion2,avz2ion1,avz2ion2,varzion1,varzion2,angle,cangle
      REAL(KIND=KIND(0.0D0))::box(3),dipole(3)
      REAL(KIND=KIND(0.0D0)),ALLOCATABLE::ion1(:,:),ion2(:,:),&
      wato(:,:),wath1(:,:),wath2(:,:),&
      surf1(:,:),surf2(:,:),coor(:,:),costhetamin(:),costhetamax(:),&
      zmin(:),zmax(:),gmin(:),gmax(:)

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
      nframe = 20000
      nframe_max = 100000
      nsurf1 = 16
      nsurf2 = 16
      nat = 387
      nwat = 97
      nbin=36
      dphi=5

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
         ELSEIF ( INDEX(input,"NAT") /= 0 ) THEN
            READ(5,*) nat 
!         ELSEIF ( INDEX(input,"ION1") /= 0 ) THEN
!            READ(5,*) nion1
!            ALLOCATE(iion1(nion1))
!            ALLOCATE(zion1(nion1))
!            ALLOCATE(z2ion1(nion1))
!            READ(5,*) (iion1(i),i=1,nion1) 
!         ELSEIF ( INDEX(input,"ION2") /= 0 ) THEN
!            READ(5,*) nion2
!            ALLOCATE(iion2(nion2))
!            ALLOCATE(zion2(nion2))
!            ALLOCATE(z2ion2(nion2))
!            READ(5,*) (iion2(i),i=1,nion2) 
         ELSEIF ( INDEX(input,"WATER") /= 0 ) THEN
            READ(5,*) nwat
            ALLOCATE(iwat(nwat))
            READ(5,*) (iwat(i),i=1,nwat) 
         ELSEIF ( INDEX(input,"SURF1") /= 0 ) THEN
            READ(5,*) nsurf1
            ALLOCATE(isurf1(nsurf1))
            READ(5,*) (isurf1(i),i=1,nsurf1) 
         ELSEIF ( INDEX(input,"SURF2") /= 0 ) THEN
            READ(5,*) nsurf2
            ALLOCATE(isurf2(nsurf2))
            READ(5,*) (isurf2(i),i=1,nsurf2) 
!         ELSEIF ( INDEX(input,"DZ") /= 0 ) THEN
!            READ(5,*) dz
!         ELSEIF ( INDEX(input,"WATORI") /= 0 ) THEN
!            watori = .TRUE.
!         ELSEIF ( INDEX(input,"IONPOS") /= 0 ) THEN
!            ionpos = .TRUE.
         ELSE
            STOP "wrong input"
         END IF
      END DO readinput 
!      PRINT *, nat, nion, nwat, nsurf1, nsurf2
!      PRINT *, (isurf1(i),i=1,nsurf1), (isurf2(i),i=1,nsurf2) 
      PRINT *, (iwat(i),i=1,nwat) 
      
!read coordinates from TRAJECTORY file in xyz format     

!      ALLOCATE(ion1(3,nion1))
!      ALLOCATE(ion2(3,nion2))
      ALLOCATE(wato(3,nwat))
      ALLOCATE(wath1(3,nwat))
      ALLOCATE(wath2(3,nwat))
      ALLOCATE(coor(3,nat))
      ALLOCATE(surf1(3,nsurf1))
      ALLOCATE(surf2(3,nsurf2))

      OPEN(11,FILE="TRAJECTORY")

!      PRINT *, ioerr

      ALLOCATE(gmin(nframe))
      ALLOCATE(gmax(nframe))
      ALLOCATE(costhetamin(nframe))
      ALLOCATE(costhetamax(nframe))
      ALLOCATE(zmin(nframe))
      ALLOCATE(zmax(nframe))
      ALLOCATE(h(nbin))
!      g = 0
      DO i=1,nbin  
       h(i) = 0
      end do
!      costheta = 0

!      REWIND(11) 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!read through the whole trajectory and do analyses
!water orientation parameter, ion position,
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      histogram: DO i=1,nframe
         READ(11,*,IOSTAT=ioerr) dummy

         IF ( ioerr < 0 ) THEN
            EXIT histogram     
         END IF

!         nframe=nframe+1

         READ(11,*) dummy

         DO j=1,nat
            READ(11,*) element,(coor(ix,j),ix=1,3)
!fold up coordinates
!           CALL fold(coor(:,j),box)
         ENDDO

         DO j=1,nsurf1
            surf1(1,j)=coor(1,isurf1(j))
            surf1(2,j)=coor(2,isurf1(j))
            surf1(3,j)=coor(3,isurf1(j))
!            PRINT *, surf1(:,j)
         END DO

         DO j=1,nsurf2
            surf2(1,j)=coor(1,isurf2(j))
            surf2(2,j)=coor(2,isurf2(j))
            surf2(3,j)=coor(3,isurf2(j))
!            PRINT *, surf2(:,j)
         END DO

!fold up surf2(3,1) with referene to surf1(3,1),then
!fold up surfact atoms using the first surface atom as a reference
!assume water volume is larger than oxide. BE CAREFUL! Otherwise, comment out the next line
!         surf2(3,1)=surf2(3,1)-box(3)*ANINT((surf2(3,1)-surf1(3,1))/box(3))
!         DO j=2,nsurf1
!            surf1(3,j)=surf1(3,j)-box(3)*ANINT((surf1(3,j)-surf1(3,1))/box(3))
!         END DO
!         DO j=2,nsurf2
!            surf2(3,j)=surf2(3,j)-box(3)*ANINT((surf2(3,j)-surf2(3,1))/box(3))
!         END DO

!         zmin = 0.0
!         zmax = 0.0

         DO j=1,nsurf1
            zmin(i)=zmin(i)+surf1(3,j)/DBLE(nsurf1)
         END DO
         DO j=1,nsurf2
            zmax(i)=zmax(i)+surf2(3,j)/DBLE(nsurf2)
         END DO

!assume water volume is larger than oxide
!         zmax(i)=zmax(i)+box(3)


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

!          g = 0
!          h = 0
!          costheta = 0
!calculate water orientation parameters, costheta and sintheta
            DO k=1,nwat
               IF ( wato(3,k) < (zmin(i)+2.7) ) THEN
                  gmin(i)=gmin(i)+1  
                  dipole=vector(wath1(:,k),wato(:,k),box)   
                  cangle=dipole(3)/sqrt((dipole(1)**2+dipole(2)**2+dipole(3)**2))
                  angle=acos(cangle)*180/3.1416
!                print *, angle
                  DO j=1,nbin
                   IF ( angle >= (j-1)*dphi .AND. angle < j*dphi ) THEN
                     h(j)=h(j)+1
                   end if
                  end do

                  dipole=vector(wath2(:,k),wato(:,k),box)
                  cangle=dipole(3)/sqrt((dipole(1)**2+dipole(2)**2+dipole(3)**2))
                  angle=acos(cangle)*180/3.1416
!                print *, angle
                  DO j=1,nbin
                   IF ( angle >= (j-1)*dphi .AND. angle < j*dphi ) THEN
                     h(j)=h(j)+1
                   end if
                  end do

               END IF
            END DO

            DO k=1,nwat
               IF ( wato(3,k) > (zmax(i)-2.7)  ) THEN
                  gmax(i)=gmax(i)+1

                  dipole=vector(wath1(:,k),wato(:,k),box) 
                  cangle=-dipole(3)/sqrt((dipole(1)**2+dipole(2)**2+dipole(3)**2))
                angle=acos(cangle)*180/3.1416
!                print *, angle
                  DO j=1,nbin
                   IF ( angle >= (j-1)*dphi .AND. angle < j*dphi ) THEN
                     h(j)=h(j)+1
                   end if
                  end do

                  dipole=vector(wath1(:,k),wato(:,k),box)
                  cangle=-dipole(3)/sqrt((dipole(1)**2+dipole(2)**2+dipole(3)**2))
                angle=acos(cangle)*180/3.1416
!                print *, angle
                  DO j=1,nbin
                   IF ( angle >= (j-1)*dphi .AND. angle < j*dphi ) THEN
                     h(j)=h(j)+1
                   end if
                  end do

               END IF

           END DO



      END DO histogram 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!reading the whole trajectory is finished
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      CLOSE(11)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!write calculated data into files
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!write water orientation parameters 
      OPEN(12,FILE="dipole.out")
      WRITE(12,*) 'zmin(i)', 'gmin(i)',  &
                  'zmax(i)', 'gmax(i)' 
      DO j=1,nframe
!      costhetamin(j)=costhetamin(j)/DBLE(gmin(j))
!      costhetamax(j)=costhetamax(j)/DBLE(gmax(j))    

        WRITE(12,*) zmin(j), gmin(j),  zmax(j), gmax(j)
      END DO
      
     CLOSE(12)

      OPEN(13,FILE="angle.out")
!      WRITE(13,*) 'angle', ' h'
      DO j=1,nbin
        WRITE(13,*) j*5-2.5, h(j)
      END DO

     CLOSE(13)

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
