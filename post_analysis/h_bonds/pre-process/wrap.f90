      PROGRAM watorient 
      IMPLICIT NONE 
      INTEGER::ioerr
      INTEGER::i,j,k,ix,l,m,n,nat,nwat,nmetal,nperi1,nperi2,nion
      INTEGER,ALLOCATABLE::iion1(:),iion2(:),iwat(:),isurf1(:),isurf2(:)
      INTEGER::nframe

      REAL::a,b
      REAL(KIND=KIND(0.0D0)),ALLOCATABLE::ion1(:,:),ion2(:,:),&
      wato(:,:),wath1(:,:),wath2(:,:),coor(:,:)

      CHARACTER::input*20,dummy*20
      CHARACTER*4,DIMENSION(999)::element
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
      nat = 391
      nmetal = 96
      nwat = 97
      nion= 4
      a = 11.246 
      b = 11.246
!read input file from stdin      

!      readinput: DO 
!         READ(5,*,IOSTAT=ioerr) input

!         IF ( ioerr < 0 ) THEN
!            EXIT readinput
!        ELSEIF (ioerr > 0) THEN
!          STOP "error in reading input file" 
!         END IF

!         WRITE(6,*) input,ioerr 

!         IF ( INDEX(input,"BOX") /= 0 ) THEN
!            READ(5,*) (box(i),i=1,3)
!         IF ( INDEX(input,"NAT") /= 0 ) THEN
!            READ(5,*) nat 
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
!         ELSEIF ( INDEX(input,"WATER") /= 0 ) THEN
!            READ(5,*) nwat
!            ALLOCATE(iwat(nwat))
!            READ(5,*) (iwat(i),i=1,nwat) 
!         ELSEIF ( INDEX(input,"SURF1") /= 0 ) THEN
!            READ(5,*) nsurf1
!            ALLOCATE(isurf1(nsurf1))
!            READ(5,*) (isurf1(i),i=1,nsurf1) 
!         ELSEIF ( INDEX(input,"SURF2") /= 0 ) THEN
!            READ(5,*) nsurf2
!            ALLOCATE(isurf2(nsurf2))
!            READ(5,*) (isurf2(i),i=1,nsurf2) 
!         ELSE
!            STOP "wrong input"
!         END IF
!      END DO readinput 
!      PRINT *, nat, nion, nwat, nsurf1, nsurf2
!      PRINT *, (isurf1(i),i=1,nsurf1), (isurf2(i),i=1,nsurf2) 
!      PRINT *, (iwat(i),i=1,nwat) 
      
!read coordinates from TRAJECTORY file in xyz format     

!      ALLOCATE(ion1(3,nion1))
!      ALLOCATE(ion2(3,nion2))
      ALLOCATE(wato(3,nwat))
      ALLOCATE(wath1(3,nwat))
      ALLOCATE(wath2(3,nwat))
      ALLOCATE(coor(3,nat))
!      ALLOCATE(surf1(3,nsurf1))
!      ALLOCATE(surf2(3,nsurf2))

      OPEN(11,FILE="TRAJECTORY")


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!read through the whole trajectory and do analyses
!water orientation parameter, ion position,
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      DO i=1,nframe
         READ(11,*) dummy
         READ(11,*) dummy
         write(*,*) i
         DO j=1,nat
            READ(11,*) element(j), coor(1,j), coor(2,j), coor(3,j)
         end do
         
         DO m=1,3*nwat-2,3
            wato(1,(m+2)/3)=coor(1,m)
            wato(2,(m+2)/3)=coor(2,m)
            wato(3,(m+2)/3)=coor(3,m) 
         end do

         DO n=2,3*nwat-1,3
            wath1(1,(n+1)/3)=coor(1,n)
            wath1(2,(n+1)/3)=coor(2,n)
            wath1(3,(n+1)/3)=coor(3,n)
         end do

         DO l=3,3*nwat,3
            wath2(1,(l)/3)=coor(1,l)
            wath2(2,(l)/3)=coor(2,l)
            wath2(3,(l)/3)=coor(3,l)
         end do

          OPEN(12,FILE="wrap.xyz")   
           write(12,*) 3*nwat+nion
           write(12,*) 'i =' 
          
!          do k=1,nmetal
!            coor(1,k) = coor(1,k) + FLOOR(coor(2,k)/b) * a/2
!            coor(2,k) = coor(2,k) - FLOOR(coor(2,k)/b) * b
!            coor(1,k) = coor(1,k) - a * FLOOR((1.732*coor(1,k)+coor(2,k))/(1.732*a))
!            write(12,*) element(k), coor(1,k), coor(2,k), coor(3,k)
!          enddo

          do j=1,nwat
            nperi1 =  FLOOR(wato(2,j)/b)
            wato(2,j) = wato(2,j) - nperi1 * b
            nperi2 =  FLOOR(wato(1,j)/a)
            wato(1,j) = wato(1,j) - nperi2*a

            wath1(1,j) = wath1(1,j) - nperi2 * a
            wath1(2,j) = wath1(2,j) - nperi1 *b

            wath2(1,j) = wath2(1,j) - nperi2 * a
            wath2(2,j) = wath2(2,j) - nperi1 *b

            write(12,*) element(3*j-2), wato(1,j), wato(2,j), wato(3,j)
            write(12,*) element(3*j-1), wath1(1,j), wath1(2,j),wath1(3,j)
            write(12,*) element(3*j), wath2(1,j), wath2(2,j),wath2(3,j)
          enddo

          do j=3*nwat+nmetal+1,nat
              write(12,*) element(j), coor(1,j), coor(2,j), coor(3,j)
          enddo

!          do k=1+nmetal+nwat*3,nat
!            coor(1,k) = coor(1,k) + FLOOR(coor(2,k)/b) * a/2
!            coor(2,k) = coor(2,k) - FLOOR(coor(2,k)/b) * b
!            coor(1,k) = coor(1,k) - a * FLOOR((1.732*coor(1,k)+coor(2,k))/(1.732*a))
!            write(12,*) element(k), coor(1,k), coor(2,k), coor(3,k)
!          enddo

        end do

          CLOSE(11)
          CLOSE(12)
 

!         DO j=1,nsurf1
!            surf1(1,j)=coor(1,isurf1(j))
!            surf1(2,j)=coor(2,isurf1(j))
!            surf1(3,j)=coor(3,isurf1(j))
!         END DO

!         DO j=1,nsurf2
!            surf2(1,j)=coor(1,isurf2(j))
!            surf2(2,j)=coor(2,isurf2(j))
!            surf2(3,j)=coor(3,isurf2(j))
!         END DO

!fold up surf2(3,1) with referene to surf1(3,1),then
!fold up surfact atoms using the first surface atom as a reference
!assume water volume is larger than oxide.BE CAREFUL! Otherwise, comment out the next line
!         surf2(3,1)=surf2(3,1)-box(3)*ANINT((surf2(3,1)-surf1(3,1))/box(3))
!         DO j=2,nsurf1
!            surf1(3,j)=surf1(3,j)-box(3)*ANINT((surf1(3,j)-surf1(3,1))/box(3))
!         END DO       
!         DO j=2,nsurf2
!            surf2(3,j)=surf2(3,j)-box(3)*ANINT((surf2(3,j)-surf2(3,1))/box(3))
!         END DO       
       
!        zmin = 0.0
!         zmax = 0.0

!        DO j=1,nsurf1 
!            zmin=zmin+surf1(3,j)/DBLE(nsurf1)
!         END DO
!         DO j=1,nsurf2 
!            zmax=zmax+surf2(3,j)/DBLE(nsurf2)
!         END DO
          


!fold water between zmin and zmax
!         DO j=1,nwat
!            wath1(3,j)=wath1(3,j)-box(3)*ANINT((wath1(3,j)-(zmin+zmax)/2.0)/box(3))
!            wath2(3,j)=wath2(3,j)-box(3)*ANINT((wath2(3,j)-(zmin+zmax)/2.0)/box(3))
!            wato(3,j)=wato(3,j)-box(3)*ANINT((wato(3,j)-(zmin+zmax)/2.0)/


  
      END PROGRAM watorient 
