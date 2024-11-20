PROGRAM extract
   IMPLICIT NONE
   INTEGER,PARAMETER :: x=17, y=17, z=450, skip=391
   INTEGER::i, j, k
   REAL,DIMENSION(x,y,z)::potential

   OPEN (10,FILE='input.cube')
   DO i=1,skip
      READ (10,*)
   END DO
! skip the head line (nat+6)
   DO i=1,x
      DO j=1,y
         DO k=0,z/6-1
            READ (10,*) potential(i,j,6*k+1),potential(i,j,6*k+2),potential(i,j,6*k+3), &
                              potential(i,j,6*k+4),potential(i,j,6*k+5),potential(i,j,6*k+6)
         END DO
   !      READ (10,*) potential(i,j,z-2),potential(i,j,z-1),potential(i,j,z)
   !      open when z cannot be devided completely
      END DO
   END DO
   CLOSE (10)
   OPEN (11,FILE='output.cube')
   DO i=1,z
      WRITE (11,*) 27.2114*average(potential(1:x,1:y,i:i),x,y,1)
   END DO
   CLOSE (11)

   CONTAINS
      FUNCTION average(array,x1,y1,z1)
         IMPLICIT NONE
         REAL::average,summation
         INTEGER,INTENT(IN)::x1,y1,z1
         INTEGER::i,j,k
         REAL,DIMENSION(x1,y1,z1),INTENT(IN)::array
         summation=0
         DO i=1,x1
            DO j=1,y1
               DO k=1,z1
                  summation=summation+array(i,j,k)
               END DO
            END DO
         END DO
         average=summation/x1/y1/z1
      END FUNCTION average

END PROGRAM extract
