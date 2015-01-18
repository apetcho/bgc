!
!-----------------------------------------------------------------------
!  standard out
!-----------------------------------------------------------------------
!
        IF (mod(dtBgc*Iter,30.0d0*86400.0d0).eq.0.0d0) THEN
          cff=dtBgc*Iter/86400.0d0
!
          WRITE(*,'(i7,9a8)') int(cff),'daysDOMf','DOMs','MnO2','FeOOH',&
     &                        'SO4','Mn','Fe','NH4','PO4'
          WRITE(*,'(f6.2,a,9a8)') tsm(i,1),'C',                         &
     &                        ('mmol/m3',iter2=1,2),                    &
     &                        ('nmol/g', iter2=1,2),                    &
     &                        ('mmol/m3',iter2=1,5)
!
          DO i=Istr,Iend
            WRITE(*,1000) 'point',i,                                    &
     &                  pw(i,0,iwDOMf),                                 &
     &                  pw(i,0,iwDOMs),                                 &
     &                  0.0d0,                                          &
     &                  0.0d0,                                          &
     &                  pw(i,0,iwSO4),                                  &
     &                  pw(i,0,iwMn_),                                  &
     &                  pw(i,0,iwFe_),                                  &
     &                  pw(i,0,iwNH4),                                  &
     &                  pw(i,0,iwPO4)
            WRITE(*,*) ('-',iter2=1,78)
!
            DO k=1,Nbed,4
              WRITE (*,1001) depth(i,k),                                &
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
            WRITE(*,*) ('=',iter2=1,78)
!
          END DO
        END IF
!
1000    FORMAT (a6,i1,f8.2,f8.2,f8.0,f8.0,f8.0,f8.3,f8.3,f8.2,f8.3)
1001    FORMAT (f7.2, f8.2,f8.2,f8.0,f8.0,f8.0,f8.3,f8.3,f8.2,f8.3)
!
!=======================================================================
!
        DO i=Istr,Iend
!
!-----------------------------------------------------------------------
!  pw & sm output to csv (uM = mmol/m3)
!-----------------------------------------------------------------------
!
        IF (Iter.eq.0) THEN
          WRITE(filename,'(a,"/out",i1,".csv")') trim(outdir),i
          OPEN (10+i,file=filename)
          WRITE (10+i,1010) 'time','depth','temp',                      &
     &      'O2','NH4','NO3','PO4','SO4','H2S','Mn','Fe','CH4',         &
     &      'DOMf','DOMs','POMf','POMs','POMn','FeOOHA','FeOOHB',       &
     &      'FeOOHP','MnO2A','MnO2B','S0','FeS','FeS2'
        END IF
!
        IF (mod(dtBgc*Iter,30.0d0*86400.0d0).eq.0.0d0) THEN
          t=dtBgc*Iter/86400.0d0
          DO k=1,Nbed
            WRITE (10+i,1011) t,depth(i,k),tsm(i,k),                    &
     &                        (poro(i,k)*pw(i,k,itrc),itrc=1,NBGCPW),   &
     &                        (sm(i,k,itrc),itrc=1,NBGCSM)
          END DO
        END IF
!
1010    FORMAT (25(a,','))
1011    FORMAT (25(f16.8,','))
!
!  last year output to csv (uM = mmol/m3)
!
        IF (Iter.eq.0) THEN
          WRITE(filename,'(a,"/last",i1,".csv")') trim(outdir),i
          OPEN (20+i,file=filename)
          WRITE (20+i,1010) 'time','depth','temp',                      &
     &      'O2','NH4','NO3','PO4','SO4','H2S','Mn','Fe','CH4',         &
     &      'DOMf','DOMs','POMf','POMs','POMn','FeOOHA','FeOOHB',       &
     &      'FeOOHP','MnO2A','MnO2B','S0','FeS','FeS2'
        END IF
!
        IF ((mod(dtBgc*Iter,1.0d0*86400.0d0).eq.0.0d0) .and.            &
     &      (dtBgc*Iter.ge.(ndays-360.0d0)*86400.0d0)) THEN
          t=dtBgc*Iter/86400.0d0-(ndays-360.0d0)
          DO k=1,Nbed
            WRITE (20+i,1011) t,depth(i,k),tsm(i,k),                    &
     &                        (poro(i,k)*pw(i,k,itrc),itrc=1,NBGCPW),   &
     &                        (sm(i,k,itrc),itrc=1,NBGCSM)
          END DO
        END IF
!
!-----------------------------------------------------------------------
!  output restart csv file (uM = mmol/m3)
!-----------------------------------------------------------------------
!
        IF (mod(dtBgc*Iter,360.0d0*86400.0d0).eq.0.0d0) THEN
          WRITE(filename,'(a,"/rst",i1,".csv")') trim(outdir),i
          OPEN (30+i,file=filename)
          WRITE (30+i,1012)                                             &
     &      'O2','NH4','NO3','PO4','SO4','H2S','Mn','Fe','CH4',         &
     &      'DOMf','DOMs','POMf','POMs','POMn','FeOOHA','FeOOHB',       &
     &      'FeOOHP','MnO2A','MnO2B','S0','FeS','FeS2'
          t=dtBgc*Iter/86400.0d0
          DO k=1,Nbed
            WRITE (30+i,1013) (pw(i,k,itrc),itrc=1,NBGCPW),             &
     &                        (sm(i,k,itrc),itrc=1,NBGCSM)
          END DO
          CLOSE(30+i)
        END IF
!
1012    FORMAT (22(a,','))
1013    FORMAT (22(f16.8,','))
!
!-----------------------------------------------------------------------
!  flux output to csv (nmol/cm2/s -> mmol/m2/day)
!-----------------------------------------------------------------------
!
        IF (Iter.eq.0) THEN
          WRITE(filename,'(a,"/out_flux",i1,".csv")') trim(outdir),i
          OPEN (40+i,file=filename)
          WRITE (40+i,1020) 'time','temp',                              &
     &      'O2','NH4','NO3','PO4','SO4','H2S','Mn','Fe','CH4',         &
     &      'DOMf','DOMs','POMf','POMs','POMn','FeOOHA','FeOOHB',       &
     &      'FeOOHP','MnO2A','MnO2B','S0','FeS','FeS2',                 &
     &      'bPOMf','bPOMs','bPOMn','bFeOOHA','bFeOOHB',                &
     &      'bFeOOHP','bMnO2A','bMnO2B','bS0','bFeS','bFeS2'
        END IF
!
        IF (mod(dtBgc*Iter,86400.0d0).eq.0.0d0.and.Iter.ne.0) THEN
          t=dtBgc*Iter/86400.0d0
          cff=(m2cm*m2cm*day2s)/1.0d6
          WRITE (40+i,1021) t,tsm(i,1),                                 &
     &                    (pwflux(i,0,itrc)*cff,itrc=1,NBGCPW),         &
     &                    (smflux(i,0,itrc)*cff,itrc=1,NBGCSM),         &
     &                    (smflux(i,Nbed,itrc)*cff,itrc=1,NBGCSM)
        END IF
!
1020    FORMAT (2(a,','), 33(a,','))
1021    FORMAT (35(f16.8,','))
!
!-----------------------------------------------------------------------
!  circulations output to csv (nmol/cm2/s -> mmol/m2/day)
!-----------------------------------------------------------------------
!
        IF (Iter.eq.0) THEN
          WRITE(filename,'(a,"/out_circulations",i1,".csv")')           &
     &      trim(outdir),i
          OPEN (50+i,file=filename)
          WRITE (50+i,1030) 'time','temp',(itrc,itrc=1,50)
        END IF
!
        IF (mod(dtBgc*Iter,86400.0d0).eq.0.0d0.and.Iter.ne.0) THEN
          t=dtBgc*Iter/86400.0d0
          cff=(m2cm*m2cm*day2s)/1.0d6
          WRITE (50+i,1031) t,tsm(i,1),                                 &
     &                    (F(i,itrc)*cff,itrc=1,50)
        END IF
!
        END DO
!
1030    FORMAT (2(a,','), 50('F',i02,','))
1031    FORMAT (52(f16.8,','))
