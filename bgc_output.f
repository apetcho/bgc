!
!------------------------------------------------------------------------------
!  standard out
!------------------------------------------------------------------------------
!
        i=1
        IF (mod(dtBgc*Iter,5.0d0*86400.0d0).eq.0.0d0) THEN
          cff=dtBgc*Iter/86400.0d0
          WRITE(*,'(i7,9a8)') int(cff),'days  DO','NO3','MnO2','FeOOH','SO4',  &
     &                          'Mn','Fe','NH4','PO4'
          WRITE(*,'(f6.2,a,9a8)') tsm(i,1),'C',                         &
     &                        ('mmol/m3',iter2=1,2),                    &
     &                        ('nmol/g', iter2=1,2),                    &
     &                        ('mmol/m3',iter2=1,5)!,                    &
     !&                        'nmol/g'
          WRITE(*,1000) 'depth',                                        &
     &                  pw(i,0,iwO2_),                                  &
     &                  pw(i,0,iwNO3),                                  &
     &                  bgcsm(i,j,1,iMnOA)+bgcsm(i,j,1,iMnOB),          &
     &                  bgcsm(i,j,1,iFeOA)+bgcsm(i,j,1,iFeOB)+bgcsm(i,j,1,iFeOP), &
     &                  pw(i,0,iwSO4),                                  &
     &                  pw(i,0,iwMn_),                                  &
     &                  pw(i,0,iwFe_),                                  &
     &                  pw(i,0,iwNH4),                                  &
     &                  pw(i,0,iwPO4)
!
          WRITE(*,*) ('-',iter2=1,78)
!
          DO k=1,49 !Nbed
            WRITE (*,1001) depth(i,k),                                  &
     &                  pw(i,k,iwO2_),                                  &
     &                  pw(i,k,iwNO3),                                  &
     &                  sm(i,k,iMnOA)+sm(i,k,iMnOB),                    &
     &                  sm(i,k,iFeOA)+sm(i,k,iFeOB)+sm(i,k,iFeOP),      &
     &                  pw(i,k,iwSO4),                                  &
     &                  pw(i,k,iwMn_),                                  &
     &                  pw(i,k,iwFe_),                                  &
     &                  pw(i,k,iwNH4),                                  &
     &                  pw(i,k,iwPO4)
          END DO
        END IF
!
1000    FORMAT (a7,  f8.3,f8.3,f8.0,f8.0,f8.0,f8.3,f8.3,f8.2,f8.3)
1001    FORMAT (f7.2,f8.3,f8.3,f8.0,f8.0,f8.0,f8.3,f8.3,f8.2,f8.3)
!
!------------------------------------------------------------------------------
!  pw & sm output to csv
!------------------------------------------------------------------------------
!
        IF (Iter.eq.0) THEN
          OPEN (10,file='bgc_out.csv')
          WRITE (10,1010) 'time','z','temp',                            &
     &      'DO','NH4','NO3','PO4','SO4','H2S','Mn','Fe','CH4',         &
     &      'POMf','POMs','POMn','FeOOHA','FeOOHB','FeOOHP',            &
     &      'MnO2A','MnO2B','S0','FeS','FeS2'
        END IF
!
        IF (mod(dtBgc*Iter,30.0d0*86400.0d0).eq.0.0d0) THEN
          t=dtBgc*Iter/86400.0d0
          DO k=1,Nbed
            WRITE (10,1011) t,depth(i,k),tsm(i,k),                      &
     &                      (pw(i,k,itrc),itrc=1,NBGCPW),               &
     &                      (sm(i,k,itrc),itrc=1,NBGCSM)
          END DO
        END IF

1010    FORMAT (23(a,','))
1011    FORMAT (23(f16.8,','))
!
!------------------------------------------------------------------------------
!  flux output to csv
!------------------------------------------------------------------------------
!
        IF (Iter.eq.0) THEN
          OPEN (20,file='out_flux.csv')
          WRITE (20,1020) 'time','temp',                                &
     &      'DO','NH4','NO3','PO4','SO4','H2S','Mn','Fe',               &
     &      'POMf','POMs','POMn','FeOOHA','FeOOHB','FeOOHP',            &
     &      'MnO2A','MnO2B','S0','FeS','FeS2'
        END IF
!
        IF (mod(dtBgc*Iter,86400.0d0).eq.0.0d0.and.Iter.ne.0) THEN
          t=dtBgc*Iter/86400.0d0
          cff=day2s/100.0d0
          WRITE (20,1021) t,tsm(i,1),                                   &
     &                    pwflux(i,0,iwO2_)*32.0d0*cff,                 &
     &                    pwflux(i,0,iwNH4)*14.0d0*cff,                 &
     &                    pwflux(i,0,iwNO3)*14.0d0*cff,                 &
     &                    pwflux(i,0,iwPO4)*31.0d0*cff,                 &
     &                    pwflux(i,0,iwSO4)*32.0d0*cff,                 &
     &                    pwflux(i,0,iwH2S)*32.0d0*cff,                 &
     &                    pwflux(i,0,iwMn_)*cff,                        &
     &                    pwflux(i,0,iwFe_)*cff,                        &
     &                    (smflux(i,0,itrc)*cff,itrc=1,NBGCSM)
        END IF

1020    FORMAT (2(a,','), 6(a,'(mg/m2/day),'), 13(a,'(mmol/m2/day),'))
1021    FORMAT (21(f16.8,','))
