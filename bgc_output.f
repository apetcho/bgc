!
!------------------------------------------------------------------------------
!  standard out
!------------------------------------------------------------------------------
!
        i=1
        IF (mod(dtBgc*Iter,86400.0d0).eq.0.0d0) THEN
          cff=dtBgc*Iter/86400.0d0
          WRITE(*,'(i7,9a8)') int(cff),'daysDOMf','DOMs','MnO2','FeOOH',&
     &                        'SO4','Mn','Fe','NH4','PO4'
          WRITE(*,'(f6.2,a,9a8)') tsm(i,1),'C',                         &
     &                        ('mmol/m3',iter2=1,2),                    &
     &                        ('nmol/g', iter2=1,2),                    &
     &                        ('mmol/m3',iter2=1,5)
          WRITE(*,1000) 'depth',                                        &
     &                  pw(i,0,iwDOMf),                                 &
     &                  pw(i,0,iwDOMs),                                 &
     &                  bsm(i,j,1,iMnOA)+bsm(i,j,1,iMnOB),          &
     &                  bsm(i,j,1,iFeOA)+bsm(i,j,1,iFeOB)+bsm(i,j,1,iFeOP), &
     &                  pw(i,0,iwSO4),                                  &
     &                  pw(i,0,iwMn_),                                  &
     &                  pw(i,0,iwFe_),                                  &
     &                  pw(i,0,iwNH4),                                  &
     &                  pw(i,0,iwPO4)
!
          WRITE(*,*) ('-',iter2=1,78)
!
          DO k=1,Nbed
            WRITE (*,1001) depth(i,k),                                  &
     &                  pw(i,k,iwDOMf),                                 &
     &                  pw(i,k,iwDOMs),                                 &
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
1000    FORMAT (a7,  f8.2,f8.2,f8.0,f8.0,f8.0,f8.3,f8.3,f8.2,f8.3)
1001    FORMAT (f7.2,f8.2,f8.2,f8.0,f8.0,f8.0,f8.3,f8.3,f8.2,f8.3)
!
!------------------------------------------------------------------------------
!  pw & sm output to csv (uM = mmol/m3)
!------------------------------------------------------------------------------
!
        IF (Iter.eq.0) THEN
          OPEN (10,file='out.csv')
          WRITE (10,1010) 'time','z','temp',                            &
     &      'DO','NH4','NO3','PO4','SO4','H2S','Mn','Fe','CH4',         &
     &      'DOMf','DOMs','POMf','POMs','POMn','FeOOHA','FeOOHB',       &
     &      'FeOOHP','MnO2A','MnO2B','S0','FeS','FeS2'
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

1010    FORMAT (25(a,','))
1011    FORMAT (25(f16.8,','))
!
!-----------------------------------------------------------------------
!  flux output to csv (nmol/cm2/s -> mmol/m2/day)
!-----------------------------------------------------------------------
!
        IF (Iter.eq.0) THEN
          OPEN (20,file='out_flux.csv')
          WRITE (20,1020) 'time','temp',                                &
     &      'DO','NH4','NO3','PO4','SO4','H2S','Mn','Fe','CH4',         &
     &      'DOMf','DOMs','POMf','POMs','POMn','FeOOHA','FeOOHB',       &
     &      'FeOOHP','MnO2A','MnO2B','S0','FeS','FeS2'
        END IF
!
        IF (mod(dtBgc*Iter,86400.0d0).eq.0.0d0.and.Iter.ne.0) THEN
          t=dtBgc*Iter/86400.0d0
          cff=(m2cm*m2cm*day2s)/1.0d6
          WRITE (20,1021) t,tsm(i,1),                                   &
     &                    (pwflux(i,0,itrc)*cff,itrc=1,NBGCPW),         &
     &                    (smflux(i,0,itrc)*cff,itrc=1,NBGCSM)
        END IF

1020    FORMAT (2(a,','), 22(a,','))
1021    FORMAT (24(f16.8,','))


!------------------------------------------------------------------------------
!  circulations output to csv (nmol/cm2/s -> mmol/m2/day)
!------------------------------------------------------------------------------
!
        IF (Iter.eq.0) THEN
          OPEN (30,file='out_circulations.csv')
          WRITE (30,1030) 'time','temp',                                &
     &                    (itrc,itrc=1,50)
        END IF
!
        IF (mod(dtBgc*Iter,86400.0d0).eq.0.0d0.and.Iter.ne.0) THEN
          t=dtBgc*Iter/86400.0d0
          cff=(m2cm*m2cm*day2s)/1.0d6
          WRITE (30,1031) t,tsm(i,1),                                   &
     &                    (F(i,itrc)*cff,itrc=1,50)
        END IF

1030    FORMAT (2(a,','), 50('F',i02,','))
1031    FORMAT (52(f16.8,','))