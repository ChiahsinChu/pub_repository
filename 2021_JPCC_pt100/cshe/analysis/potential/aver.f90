      PROGRAM extract
        IMPLICIT NONE
        INTEGER,PARAMETER ::  a=124, skip1=163, skip2=163
        INTEGER::i, j, k
        
        REAL,DIMENSION(a)::energy



        OPEN (12,FILE='input.dat')
        DO i=1,skip1
           READ (12,*)
        END DO
        DO i=1,a
           READ (12,*) energy(i)
        END DO
        Do i=1,skip2
           READ (12,*)
        END DO

        CLOSE (12)

        OPEN (13,FILE='output.dat')
        WRITE (13,*) reference(energy(1:a),a)
        CLOSE (13)


      CONTAINS

        FUNCTION reference(array,z1)
          IMPLICIT NONE
          REAL::reference,summation
          INTEGER,INTENT(IN):: z1
          INTEGER::i
          REAL,DIMENSION(z1),INTENT(IN)::array
          summation=0
          DO i=1,z1
                   summation=summation+array(i)
          END DO
          reference=summation/z1
        END FUNCTION reference

      END PROGRAM extract
