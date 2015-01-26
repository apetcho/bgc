!
!------------------------------------------------------------------------
!  Biogeochemical model variables and parameters.
!------------------------------------------------------------------------
!
      integer :: BgcIter
      real(8) :: DBL
      real(8) :: dens
      real(8) :: bwMn
      real(8) :: bwFe
      real(8) :: bwCH4
      real(8) :: bwNO3(IminS:ImaxS)
      real(8) :: bwNH4(IminS:ImaxS)
      real(8) :: bwPO4(IminS:ImaxS)
      real(8) :: bwH2S(IminS:ImaxS)
      real(8) :: bwDOMf(IminS:ImaxS)
      real(8) :: bwDOMs(IminS:ImaxS)
      real(8) :: DO20(IminS:ImaxS)
      real(8) :: facDO(IminS:ImaxS)
      real(8) :: POM20(IminS:ImaxS)
      real(8) :: facPOM(IminS:ImaxS)
      real(8) :: a_poro(IminS:ImaxS)
      real(8) :: b_poro(IminS:ImaxS)
      real(8) :: c_poro(IminS:ImaxS)
      real(8) :: a_wsm(IminS:ImaxS)
      real(8) :: b_wsm(IminS:ImaxS)
      real(8) :: c_wsm(IminS:ImaxS)
      real(8) :: D0O2
      real(8) :: aO2
      real(8) :: bO2
      real(8) :: D0NO3
      real(8) :: aNO3
      real(8) :: D0H2S
      real(8) :: aH2S
      real(8) :: bH2S
      real(8) :: D0SO4
      real(8) :: aSO4
      real(8) :: D0NH4
      real(8) :: aNH4
      real(8) :: D0Mn
      real(8) :: aMn
      real(8) :: D0Fe
      real(8) :: aFe
      real(8) :: D0PO4
      real(8) :: aPO4
      real(8) :: D0DOMf
      real(8) :: aDOMf
      real(8) :: D0DOMs
      real(8) :: aDOMs
      real(8) :: Q10p
      real(8) :: Q10s
      real(8) :: z_DBw
      real(8) :: u_DBw
      real(8) :: a_DBw
      real(8) :: a_DBs
      real(8) :: a_irr
      real(8) :: b_irr
      real(8) :: c_irr
      real(8) :: d_irr
      real(8) :: e_irr
      real(8) :: f_irr
      real(8) :: KdNH4
      real(8) :: KdNO3
      real(8) :: KdMn
      real(8) :: KdFe
      real(8) :: KdPO4
      real(8) :: ratio_n
      real(8) :: ratio_f
      real(8) :: ratio_FA
      real(8) :: ratio_MA
      real(8) :: ratio_CN
      real(8) :: ratio_CP
      real(8) :: ratio_DOMf
      real(8) :: FMnO2
      real(8) :: FFeOOH
      real(8) :: KO2
      real(8) :: KNO3
      real(8) :: KMnO2
      real(8) :: KFeOOH
      real(8) :: KPOMf
      real(8) :: KPOMs
      real(8) :: KDOMf
      real(8) :: KDOMs
      real(8) :: K06
      real(8) :: K07
      real(8) :: K08
      real(8) :: K09
      real(8) :: K10
      real(8) :: K11
      real(8) :: K12
      real(8) :: K13
      real(8) :: K14
      real(8) :: K15
      real(8) :: K16
      real(8) :: K17
      real(8) :: K18
      real(8) :: K19
      real(8) :: K20
      real(8) :: K21
      real(8) :: H2Sstop
!
!  namelists
!
      namelist /params/ outdir, inifile,                                &
                        BgcIter, DBL, dens, H2Sstop,                    &
                        bwSO4, bwMn, bwFe, bwCH4,                       &
                        bwNO3, bwNH4, bwPO4, bwH2S, bwDOMf, bwDOMs,     &
                        DO20, facDO, POM20, facPOM,                     &
                        a_poro, b_poro, c_poro,                         &
                        a_wsm, b_wsm, c_wsm,                            &
                        D0O2, aO2, bO2, D0NO3, aNO3, D0H2S, aH2S, bH2S, &
                        D0SO4, aSO4, D0NH4, aNH4, D0Mn, aMn, D0Fe, aFe, &
                        D0PO4, aPO4, D0DOMf, aDOMf, D0DOMs, aDOMs,      &
                        Q10p, Q10s,                                     &
                        z_DBw, u_DBw, a_DBw, a_DBs,                     &
                        a_irr, b_irr, c_irr, d_irr, e_irr, f_irr,       &
                        KdNH4, KdNO3, KdMn, KdFe, KdPO4,                &
                        ratio_n, ratio_f, ratio_FA, ratio_MA,           &
                        ratio_DOMf, ratio_CN, ratio_CP,                 &
                        FMnO2, FFeOOH,                                  &
                        KO2, KNO3, KMnO2, KFeOOH,                       &
                        KPOMf, KPOMs, KDOMf, KDOMs,                     &
                        K06, K07, K08, K09, K10, K11, K12, K13, K14,    &
                        K15, K16, K17, K18, K19, K20, K21
!
!  read namelist
!
      read(5,nml=params)
      
      write(*,2000) 'outdir =', trim(outdir)
      write(*,2000) 'inifile(1) =', trim(inifile(1))
      write(*,2000) 'inifile(1) =', trim(inifile(2))
      write(*,2003) 'BgcIter =', BgcIter
      write(*,2001) 'DBL =', DBL
      write(*,2001) 'dens =', dens
      write(*,2001) 'bwMn =', bwMn
      write(*,2001) 'bwFe =', bwFe
      write(*,2001) 'bwCH4 =', bwCH4
      write(*,2002) 'bwNO3 =', bwNO3(IminS:ImaxS)
      write(*,2002) 'bwNH4 =', bwNH4(IminS:ImaxS)
      write(*,2002) 'bwPO4 =', bwPO4(IminS:ImaxS)
      write(*,2002) 'bwH2S =', bwH2S(IminS:ImaxS)
      write(*,2002) 'bwDOMf =', bwDOMf(IminS:ImaxS)
      write(*,2002) 'bwDOMs =', bwDOMs(IminS:ImaxS)
      write(*,2002) 'DO20 =', DO20(IminS:ImaxS)
      write(*,2002) 'facDO =', facDO(IminS:ImaxS)
      write(*,2002) 'POM20 =', POM20(IminS:ImaxS)
      write(*,2002) 'facPOM =', facPOM(IminS:ImaxS)
      write(*,2002) 'a_poro =', a_poro(IminS:ImaxS)
      write(*,2002) 'b_poro =', b_poro(IminS:ImaxS)
      write(*,2002) 'c_poro =', c_poro(IminS:ImaxS)
      write(*,2002) 'a_wsm =', a_wsm(IminS:ImaxS)
      write(*,2002) 'b_wsm =', b_wsm(IminS:ImaxS)
      write(*,2002) 'c_wsm =', c_wsm(IminS:ImaxS)
      write(*,2001) 'D0O2 =', D0O2
      write(*,2001) 'aO2 =', aO2
      write(*,2001) 'bO2 =', bO2
      write(*,2001) 'D0NO3 =', D0NO3
      write(*,2001) 'aNO3 =', aNO3
      write(*,2001) 'D0H2S =', D0H2S
      write(*,2001) 'aH2S =', aH2S
      write(*,2001) 'bH2S =', bH2S
      write(*,2001) 'D0SO4 =', D0SO4
      write(*,2001) 'aSO4 =', aSO4
      write(*,2001) 'D0NH4 =', D0NH4
      write(*,2001) 'aNH4 =', aNH4
      write(*,2001) 'D0Mn =', D0Mn
      write(*,2001) 'aMn =', aMn
      write(*,2001) 'D0Fe =', D0Fe
      write(*,2001) 'aFe =', aFe
      write(*,2001) 'D0PO4 =', D0PO4
      write(*,2001) 'aPO4 =', aPO4
      write(*,2001) 'D0DOMf =', D0DOMf
      write(*,2001) 'aDOMf =', aDOMf
      write(*,2001) 'D0DOMs =', D0DOMs
      write(*,2001) 'aDOMs =', aDOMs
      write(*,2001) 'Q10p =', Q10p
      write(*,2001) 'Q10s =', Q10s
      write(*,2001) 'z_DBw =', z_DBw
      write(*,2001) 'u_DBw =', u_DBw
      write(*,2001) 'a_DBw =', a_DBw
      write(*,2001) 'a_DBs =', a_DBs
      write(*,2001) 'a_irr =', a_irr
      write(*,2001) 'b_irr =', b_irr
      write(*,2001) 'c_irr =', c_irr
      write(*,2001) 'd_irr =', d_irr
      write(*,2001) 'e_irr =', e_irr
      write(*,2001) 'f_irr =', f_irr
      write(*,2001) 'KdNH4 =', KdNH4
      write(*,2001) 'KdNO3 =', KdNO3
      write(*,2001) 'KdMn =', KdMn
      write(*,2001) 'KdFe =', KdFe
      write(*,2001) 'KdPO4 =', KdPO4
      write(*,2001) 'ratio_n =', ratio_n
      write(*,2001) 'ratio_f =', ratio_f
      write(*,2001) 'ratio_FA =', ratio_FA
      write(*,2001) 'ratio_MA =', ratio_MA
      write(*,2001) 'ratio_CN =', ratio_CN
      write(*,2001) 'ratio_CP =', ratio_CP
      write(*,2001) 'ratio_DOMf =', ratio_DOMf
      write(*,2001) 'FMnO2 =', FMnO2
      write(*,2001) 'FFeOOH =', FFeOOH
      write(*,2001) 'KO2 =', KO2
      write(*,2001) 'KNO3 =', KNO3
      write(*,2001) 'KMnO2 =', KMnO2
      write(*,2001) 'KFeOOH =', KFeOOH
      write(*,2001) 'KPOMf =', KPOMf
      write(*,2001) 'KPOMs =', KPOMs
      write(*,2001) 'KDOMf =', KDOMf
      write(*,2001) 'KDOMs =', KDOMs
      write(*,2001) 'K06 =', K06
      write(*,2001) 'K07 =', K07
      write(*,2001) 'K08 =', K08
      write(*,2001) 'K09 =', K09
      write(*,2001) 'K10 =', K10
      write(*,2001) 'K11 =', K11
      write(*,2001) 'K12 =', K12
      write(*,2001) 'K13 =', K13
      write(*,2001) 'K14 =', K14
      write(*,2001) 'K15 =', K15
      write(*,2001) 'K16 =', K16
      write(*,2001) 'K17 =', K17
      write(*,2001) 'K18 =', K18
      write(*,2001) 'K19 =', K19
      write(*,2001) 'K20 =', K20
      write(*,2001) 'K21 =', K21
      write(*,2001) 'H2Sstop =', H2Sstop

2000  format(a13,a)
2001  format(a13,f16.8)
2002  format(a13,2f16.8)
2003  format(a13,i16)

      !write(6,*) ''
      !write(6,*) 'Check your upper parameters & ENTER if it is OK.'
      !read(6,'()')
