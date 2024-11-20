      PROGRAM watorient 
      IMPLICIT NONE 
      INTEGER::ioerr
      INTEGER::i,j,k,ix,nat,nion1,nion2,nwat,nsurf1,nsurf2  
      INTEGER::ibin,nbin,cbin
      INTEGER,ALLOCATABLE::iion1(:),iion2(:),iwat(:),isurf1(:),isurf2(:)
      INTEGER::nframe,nframe_max
      INTEGER,ALLOCATABLE::g(:),h(:)

      REAL(KIND=KIND(0.0D0))::zmin,zmax,z,dz,avzmin,avzmax,avz,avzion1,&
      avzion2,avz2ion1,avz2ion2,varzion1,varzion2
      REAL(KIND=KIND(0.0D0))::box(3),dipole(3)
      REAL(KIND=KIND(0.0D0)),ALLOCATABLE::ion1(:,:),ion2(:,:),&
      wato(:,:),wath1(:,:),wath2(:,:),&
      surf1(:,:),surf2(:,:),coor(:,:),costheta(:),sintheta(:),gr(:),&
      zion1(:),z2ion1(:),zion2(:),z2ion2(:),tcostheta(:),hr(:)

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
      nframe = 0
      nframe_max = 100000
      avzmin = 0.0
      avzmax = 0.0
      avzion1 = 0.0
      avzion2 = 0.0
      avz2ion1 = 0.0
      avz2ion2 = 0.0
      nion1 = 0
      nion2 = 0
      nsurf1 = 0
      nsurf2 = 0
      nat = 0
      nwat = 0
      watori = .FALSE.
      ionpos = .FALSE.

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
         ELSEIF ( INDEX(input,"ION1") /= 0 ) THEN
            READ(5,*) nion1
            ALLOCATE(iion1(nion1))
            ALLOCATE(zion1(nion1))
            ALLOCATE(z2ion1(nion1))
            READ(5,*) (iion1(i),i=1,nion1) 
         ELSEIF ( INDEX(input,"ION2") /= 0 ) THEN
            READ(5,*) nion2
            ALLOCATE(iion2(nion2))
            ALLOCATE(zion2(nion2))
            ALLOCATE(z2ion2(nion2))
            READ(5,*) (iion2(i),i=1,nion2) 
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
         ELSEIF ( INDEX(input,"DZ") /= 0 ) THEN
            READ(5,*) dz
         ELSEIF ( INDEX(input,"WATORI") /= 0 ) THEN
            watori = .TRUE.
         ELSEIF ( INDEX(input,"IONPOS") /= 0 ) THEN
            ionpos = .TRUE.
         ELSE
            STOP "wrong input"
         END IF
      END DO readinput 
!      PRINT *, nat, nion, nwat, nsurf1, nsurf2
!      PRINT *, (isurf1(i),i=1,nsurf1), (isurf2(i),i=1,nsurf2) 
!      PRINT *, (iwat(i),i=1,nwat) 
      
!read coordinates from TRAJECTORY file in xyz format     

      ALLOCATE(ion1(3,nion1))
      ALLOCATE(ion2(3,nion2))
      ALLOCATE(wato(3,nwat))
      ALLOCATE(wath1(3,nwat))
      ALLOCATE(wath2(3,nwat))
      ALLOCATE(coor(3,nat))
      ALLOCATE(surf1(3,nsurf1))
      ALLOCATE(surf2(3,nsurf2))

      OPEN(11,FILE="TRAJECTORY",STATUS='OLD',IOSTAT=ioerr)

!      PRINT *, ioerr
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!calculate average distance between two surfaces during MD trajectory!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      ave_distance:DO i=1,nframe_max
         READ(11,*,IOSTAT=ioerr) dummy
!         PRINT *, ioerr,dummy

         IF ( ioerr < 0 ) THEN
            EXIT ave_distance 
         END IF

         nframe=nframe+1
!         PRINT *, nframe

         READ(11,*) dummy
!         PRINT *, dummy

         DO j=1,nat
            READ(11,*) element,(coor(ix,j),ix=1,3)
!            PRINT *, coor(:,j)
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
         surf2(3,1)=surf2(3,1)-box(3)*ANINT((surf2(3,1)-surf1(3,1))/box(3))
         DO j=2,nsurf1
            surf1(3,j)=surf1(3,j)-box(3)*ANINT((surf1(3,j)-surf1(3,1))/box(3))
         END DO
         DO j=2,nsurf2
            surf2(3,j)=surf2(3,j)-box(3)*ANINT((surf2(3,j)-surf2(3,1))/box(3))
         END DO

         zmin = 0 
         zmax = 0

         DO j=1,nsurf1
            zmin=zmin+surf1(3,j)/DBLE(nsurf1)
         END DO
         DO j=1,nsurf2
            zmax=zmax+surf2(3,j)/DBLE(nsurf2)
         END DO

!assume water volume is larger than oxide
         zmax=zmax+box(3)
        
         avzmin=avzmin+zmin
         avzmax=avzmax+zmax

      END DO ave_distance

      avzmin=avzmin/DBLE(nframe)
      avzmax=avzmax/DBLE(nframe)
      avz=avzmax-avzmin

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                  averaging distance finished                     !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!determine nbin
      cbin=INT(avz/dz/2.0)
      nbin=2*cbin
      PRINT *, nframe,avzmin,avzmax,avz,nbin

      ALLOCATE(g(nbin))
      ALLOCATE(gr(nbin))
      ALLOCATE(costheta(nbin))
      ALLOCATE(h(nbin))
      ALLOCATE(hr(nbin))
      g = 0
      h = 0
      costheta = 0

      REWIND(11) 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!read through the whole trajectory and do analyses
!water orientation parameter, ion position,
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      histogram: DO i=1,nframe_max
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
         END DO

         DO j=1,nsurf2
            surf2(1,j)=coor(1,isurf2(j))
            surf2(2,j)=coor(2,isurf2(j))
            surf2(3,j)=coor(3,isurf2(j))
         END DO

!fold up surf2(3,1) with referene to surf1(3,1),then
!fold up surfact atoms using the first surface atom as a reference
!assume water volume is larger than oxide.BE CAREFUL! Otherwise, comment out the next line
         surf2(3,1)=surf2(3,1)-box(3)*ANINT((surf2(3,1)-surf1(3,1))/box(3))
         DO j=2,nsurf1
            surf1(3,j)=surf1(3,j)-box(3)*ANINT((surf1(3,j)-surf1(3,1))/box(3))
         END DO       
         DO j=2,nsurf2
            surf2(3,j)=surf2(3,j)-box(3)*ANINT((surf2(3,j)-surf2(3,1))/box(3))
         END DO       
       
         zmin = 0.0
         zmax = 0.0

         DO j=1,nsurf1 
            zmin=zmin+surf1(3,j)/DBLE(nsurf1)
         END DO
         DO j=1,nsurf2 
            zmax=zmax+surf2(3,j)/DBLE(nsurf2)
         END DO
          
!assume water volume is larger than oxide
         zmax=zmax+box(3)
         z=zmax-zmin

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

!fold water between zmin and zmax
         DO j=1,nwat
            wath1(3,j)=wath1(3,j)-box(3)*ANINT((wath1(3,j)-(zmin+zmax)/2.0)/box(3))
            wath2(3,j)=wath2(3,j)-box(3)*ANINT((wath2(3,j)-(zmin+zmax)/2.0)/box(3))
            wato(3,j)=wato(3,j)-box(3)*ANINT((wato(3,j)-(zmin+zmax)/2.0)/box(3))
         END DO

!calculate water orientation parameters, costheta and sintheta
         IF ( watori ) THEN
         DO j=1,cbin
            DO k=1,nwat
               IF ( wato(3,k) >= zmin+(j-1)*dz .AND. wato(3,k) < zmin+j*dz ) THEN
                  g(j)=g(j)+1  
                  dipole=vector(wath1(:,k),wato(:,k),box) &
                     +vector(wath2(:,k),wato(:,k),box)  
                  costheta(j)=costheta(j) &
                     +dipole(3)/sqrt((dipole(1)**2+dipole(2)**2+dipole(3)**2)) 
               END IF
               IF ( wath1(3,k) >= zmin+(j-1)*dz .AND. wath1(3,k) < zmin+j*dz ) THEN
                  h(j)=h(j)+1
               END IF
               IF ( wath2(3,k) >= zmin+(j-1)*dz .AND. wath2(3,k) < zmin+j*dz ) THEN
                  h(j)=h(j)+1
               END IF

               IF ( wato(3,k) > zmax-j*dz .AND. wato(3,k) <= zmax-(j-1)*dz ) THEN
                  g(nbin-j+1)=g(nbin-j+1)+1
                  dipole=vector(wath1(:,k),wato(:,k),box) &
                     +vector(wath2(:,k),wato(:,k),box)
                  costheta(nbin-j+1)=costheta(nbin-j+1) &
                     -dipole(3)/sqrt((dipole(1)**2+dipole(2)**2+dipole(3)**2)) 
               END IF
               IF ( wath1(3,k) > zmax-j*dz .AND. wath1(3,k) <= zmax-(j-1)*dz ) THEN
                  h(nbin-j+1)=h(nbin-j+1)+1
               END IF
               IF ( wath2(3,k) > zmax-j*dz .AND. wath2(3,k) <= zmax-(j-1)*dz ) THEN
                  h(nbin-j+1)=h(nbin-j+1)+1
               END IF

!               IF ( wato(3,k) <= zmax-j*dz .AND. wato(3,k) >= zmin+j*dz ) THEN 
!                   g(cbin+1)=g(cbin+1)+1 
!                   dipole=vector(wath1(:,k),wato(:,k),box) &
!                     +vector(wath2(:,k),wato(:,k),box)+wato(:,k)
!                   costheta(cbin+1)=costheta(cbin+1) &
!                      +dipole(3)/sqrt((dipole(1)**2+dipole(2)**2+diople(3)**2)) 
!               END IF
            END DO
         END DO
         END IF


!calculate average ion position
         IF ( ionpos ) THEN
         IF ( nion1 /= 0 ) THEN
!assign ion coordinates
            DO j=1,nion1
              ion1(1,j)=coor(1,iion1(j))
               ion1(2,j)=coor(2,iion1(j))
               ion1(3,j)=coor(3,iion1(j))
!fold ions between zmin and zmax
!               ion1(3,j)=ion1(3,j)-box(3)*ANINT((ion1(3,j)-(zmin+zmax)/2.0)/box(3))
               zion1(j)=ion1(3,j)
!               z2ion1(j)=ion1(3,j)**2
            END DO
         avzion1=avzion1+SUM(zion1)
!         avz2ion1=avz2ion1+SUM(z2ion1)
!         avzion1=avzion1+SUM(zion1)/DBLE(nion1)
!         avz2ion1=avz2ion1+SUM(zion1)/DBLE(nion1)
         END IF

         IF ( nion2 /= 0 ) THEN
!assign ion coordinates
            DO j=1,nion2
               ion2(1,j)=coor(1,iion2(j))
               ion2(2,j)=coor(2,iion2(j))
               ion2(3,j)=coor(3,iion2(j))
!fold ions between zmin and zmax
!               ion2(3,j)=ion2(3,j)-box(3)*ANINT((ion2(3,j)-(zmin+zmax)/2.0)/box(3))
               zion2(j)=ion2(3,j)
!               z2ion2(j)=ion2(3,j)**2
            END DO
         avzion2=avzion2+SUM(zion2)
!         avz2ion2=avz2ion2+SUM(z2ion2)
!         avzion2=avzion2+SUM(zion2)/DBLE(nion2)
!         avz2ion2=avz2ion2+SUM(zion2)/DBLE(nion2)
         END IF
!         PRINT *, avzion1, avz2ion1, avzion2, avz2ion2
         END IF

      END DO histogram 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!reading the whole trajectory is finished
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      CLOSE(11)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!write calculated data into files
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      OPEN(14,FILE="surf.out")
      WRITE(14,*) ' nframe ',' avzmin ',' avzmax ',' avz ',' nbin '
      WRITE(14,*) nframe,avzmin,avzmax,avz,nbin
      CLOSE(14)

!write water orientation parameters 
      IF ( watori ) THEN 
      PRINT *, g 
      OPEN(12,FILE="dipole.out")
!      WRITE(12,*) ' index ', ' z ', ' g(i) ', 'h(i)', ' costheta '
      DO j=1,cbin
         costheta(j)=costheta(j)/DBLE(g(j))
!normalized to the density of bulk water
         gr(j)=DBLE(g(j))/DBLE(nframe)/box(1)/box(2)/dz/32.0*9.86**3
         hr(j)=DBLE(h(j))/DBLE(nframe)/box(1)/box(2)/dz/32.0*9.86**3/2
         WRITE(12,*) j, avzmin+dz*(DBLE(j)-0.5), gr(j), hr(j), costheta(j)
      END DO
      DO j=cbin+1,nbin
         costheta(j)=costheta(j)/DBLE(g(j))
!normalized to the density of bulk water
         gr(j)=DBLE(g(j))/DBLE(nframe)/box(1)/box(2)/dz/32.0*9.86**3
         hr(j)=DBLE(h(j))/DBLE(nframe)/box(1)/box(2)/dz/32.0*9.86**3/2
         WRITE(12,*) j, avzmax-dz*(DBLE(nbin)-DBLE(j)+0.5), gr(j), hr(j), costheta(j)
      END DO
      CLOSE(12)
      END IF

!write ion position
      IF ( ionpos ) THEN
         avzion1=avzion1/DBLE(nframe)/DBLE(nion1)
         avzion2=avzion2/DBLE(nframe)/DBLE(nion2)
!         avz2ion1=avz2ion1/DBLE(nframe)/DBLE(nion1)
!         avz2ion2=avz2ion2/DBLE(nframe)/DBLE(nion2)
!         avzion1=avzion1/DBLE(nframe)
!         avzion2=avzion2/DBLE(nframe)
!         avz2ion1=avzion1/DBLE(nframe)
!         avz2ion2=avzion2/DBLE(nframe)
!         varzion1=avz2ion1-avzion1**2
!         varzion2=avz2ion2-avzion2**2
        
         OPEN(13,FILE="ion.out")
         WRITE(13,*) ' avzion1 ', ' avzion2 '
         WRITE(13,*) avzion1,  avzion2
         CLOSE(13) 
      END IF


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
