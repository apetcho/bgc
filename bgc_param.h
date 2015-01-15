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
      
      write(*,*) outdir
      write(*,*) inifile
      write(*,*) BgcIter
      write(*,*) DBL
      write(*,*) dens
      write(*,*) bwMn
      write(*,*) bwFe
      write(*,*) bwCH4
      write(*,*) bwNO3(IminS:ImaxS)
      write(*,*) bwNH4(IminS:ImaxS)
      write(*,*) bwPO4(IminS:ImaxS)
      write(*,*) bwH2S(IminS:ImaxS)
      write(*,*) bwDOMf(IminS:ImaxS)
      write(*,*) bwDOMs(IminS:ImaxS)
      write(*,*) DO20(IminS:ImaxS)
      write(*,*) facDO(IminS:ImaxS)
      write(*,*) POM20(IminS:ImaxS)
      write(*,*) facPOM(IminS:ImaxS)
      write(*,*) a_poro(IminS:ImaxS)
      write(*,*) b_poro(IminS:ImaxS)
      write(*,*) c_poro(IminS:ImaxS)
      write(*,*) a_wsm(IminS:ImaxS)
      write(*,*) b_wsm(IminS:ImaxS)
      write(*,*) c_wsm(IminS:ImaxS)
      write(*,*) D0O2
      write(*,*) aO2
      write(*,*) bO2
      write(*,*) D0NO3
      write(*,*) aNO3
      write(*,*) D0H2S
      write(*,*) aH2S
      write(*,*) bH2S
      write(*,*) D0SO4
      write(*,*) aSO4
      write(*,*) D0NH4
      write(*,*) aNH4
      write(*,*) D0Mn
      write(*,*) aMn
      write(*,*) D0Fe
      write(*,*) aFe
      write(*,*) D0PO4
      write(*,*) aPO4
      write(*,*) D0DOMf
      write(*,*) aDOMf
      write(*,*) D0DOMs
      write(*,*) aDOMs
      write(*,*) Q10p
      write(*,*) Q10s
      write(*,*) z_DBw
      write(*,*) u_DBw
      write(*,*) a_DBw
      write(*,*) a_DBs
      write(*,*) a_irr
      write(*,*) b_irr
      write(*,*) c_irr
      write(*,*) d_irr
      write(*,*) e_irr
      write(*,*) f_irr
      write(*,*) KdNH4
      write(*,*) KdNO3
      write(*,*) KdMn
      write(*,*) KdFe
      write(*,*) KdPO4
      write(*,*) ratio_n
      write(*,*) ratio_f
      write(*,*) ratio_FA
      write(*,*) ratio_MA
      write(*,*) ratio_CN
      write(*,*) ratio_CP
      write(*,*) ratio_DOMf
      write(*,*) FMnO2
      write(*,*) FFeOOH
      write(*,*) KO2
      write(*,*) KNO3
      write(*,*) KMnO2
      write(*,*) KFeOOH
      write(*,*) KPOMf
      write(*,*) KPOMs
      write(*,*) KDOMf
      write(*,*) KDOMs
      write(*,*) K06
      write(*,*) K07
      write(*,*) K08
      write(*,*) K09
      write(*,*) K10
      write(*,*) K11
      write(*,*) K12
      write(*,*) K13
      write(*,*) K14
      write(*,*) K15
      write(*,*) K16
      write(*,*) K17
      write(*,*) K18
      write(*,*) K19
      write(*,*) K20
      write(*,*) K21
      write(*,*) H2Sstop

