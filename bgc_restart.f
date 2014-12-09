!
!  rstart from rst.csv
!
      OPEN(10,file='rst.csv')
      READ(10,'()')
      DO i=Istr,Iend
        DO k=1,Nbed
          READ(10,*) (bpw(i,j,k,itrc),itrc=1,NBGCPW),                         &
     &               (bsm(i,j,k,itrc),itrc=1,NBGCSM)
        END DO
      END DO
      CLOSE(10)
