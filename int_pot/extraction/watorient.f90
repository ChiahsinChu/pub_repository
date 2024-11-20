PROGRAM watorient
   IMPLICIT NONE
   INTEGER::ioerr
   INTEGER::i,j,k,ix,nat,nwat
   INTEGER::ibin,nbin,cbin
   INTEGER,ALLOCATABLE::iwat(:)
   REAL(KIND=KIND(0.0D0))::box(3),dipole(3),avzmin,avzmax
   REAL(KIND=KIND(0.0D0)),ALLOCATABLE::wato(:,:),wath1(:,:),wath2(:,:),&
   coord(:,:)
   CHARACTER*4,DIMENSION(999)::element
   CHARACTER::input*20,dummy*20
   LOGICAL::watori,ionpos

   !initialisation
   ioerr = 0
   nat = 0
   nwat = 0
   watori = .FALSE.
   ionpos = .FALSE.
   avzmin=5.112
   avzmax=30.826

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
      ELSEIF ( INDEX(input,"WATER") /= 0 ) THEN
         READ(5,*) nwat
         ALLOCATE(iwat(nwat))
         READ(5,*) (iwat(i),i=1,nwat)
      ELSE
         STOP "wrong input"
      END IF
   END DO readinput

   !read coordinates from TRAJECTORY file in xyz format
   ALLOCATE(wato(3,nwat))
   ALLOCATE(wath1(3,nwat))
   ALLOCATE(wath2(3,nwat))
   ALLOCATE(coord(3,nat))

   OPEN(11,FILE='TRAJECTORY',STATUS='OLD',IOSTAT=ioerr)
   OPEN(12,FILE='bottom.xyz')
   OPEN(13,FILE='top.xyz')
!   READ(11,*,IOSTAT=ioerr) dummy
!   READ(11,*) dummy

   DO j=1,nat
      READ(11,*) element(j),(coord(ix,j),ix=1,3)
   ENDDO
   ! extract water A 
   DO j=1,nwat*3,3
      IF (coord(3,j)<avzmin+2.7) THEN
         WRITE (12,*) element(j),coord(1,j),coord(2,j),coord(3,j)
         WRITE (12,*) element(j+1),coord(1,j+1),coord(2,j+1),coord(3,j+1)
         WRITE (12,*) element(j+2),coord(1,j+2),coord(2,j+2),coord(3,j+2)
      END IF
      IF (coord(3,j)>avzmax-2.7) THEN
         WRITE (13,*) element(j),coord(1,j),coord(2,j),coord(3,j)
         WRITE (13,*) element(j+1),coord(1,j+1),coord(2,j+1),coord(3,j+1)
         WRITE (13,*) element(j+2),coord(1,j+2),coord(2,j+2),coord(3,j+2)
      END IF 
   END DO
   ! extract Pt and OH
   DO j=3*nwat+1,nat
      IF  (coord(3,j)<box(3)/2) THEN
         WRITE (12,*) element(j),coord(1,j),coord(2,j),coord(3,j)
      END IF
      IF  (coord(3,j)>box(3)/2) THEN
         WRITE (13,*) element(j),coord(1,j),coord(2,j),coord(3,j)
      END IF
   END DO

   CLOSE(11)
   CLOSE(12)
   CLOSE(13)
END PROGRAM watorient
