!
!------------------------------------------------------------------------
!  Biogeochemical model variables and parameters.
!------------------------------------------------------------------------
!
!  layer (cm)
!
      real(8), parameter :: c_dz = 1.0d0 ! not use
!
!  Number of iterations
!
      integer, parameter :: BgcIter = 1
      real(8), parameter :: ndays   = 150.0d0*360.0d0
!
!------------------------------------------------------------------------
!  (F) Fossing et al.(2004) 
!  (B) Berg et al.(2003) 
!  (A) Angara et al.(2009)
!  (W) Wijsman et al.(2002)
!------------------------------------------------------------------------
!
      real(8), parameter :: bwSO4 = 27300.0d0 ! (F)(A)
      real(8), parameter :: bwMn  = 0.0d0     ! (F)(A)
      real(8), parameter :: bwFe  = 0.0d0     ! (F)(A)
      real(8), parameter :: bwCH4 = 0.0d0     ! (F)(A)
      real(8), parameter :: bwDOMf= 300.0d0   !    (A)
      real(8), parameter :: bwDOMs= 0.0d0     !    (A)
!
!  Porosity (nondimensional)
!
      real(8), parameter :: a_poro = 0.69d0  ! (F)0.763 (A)0.69
      real(8), parameter :: b_poro = 0.21d0  ! (F)0.086 (A)0.21
      real(8), parameter :: c_poro = 0.088d0  ! (F)0.216 (A)0.088
!
!  Density (g cm-3)
!
      real(8), parameter :: dens = 2.5d0 ! (F)2.04 (B)2.41 (A)2.5
!
!  Burial rate (cm year-1)
!
     !real(8), parameter :: c_wsm = 0.22d0 ! (F)0.064 (B)0.12
      real(8), parameter :: a_wsm = 0.22d0 ! (A)0.22
      real(8), parameter :: b_wsm = 0.26d0 ! (A)0.26
      real(8), parameter :: c_wsm = 0.3d0  ! (A)0.3
!
!  difusivity i free water (cm2 s-1)
!
      real(8), parameter :: D0O2  = 11.7d0    ! (F)(A)
      real(8), parameter :: aO2   = 0.344d0   ! (F)(A)
      real(8), parameter :: bO2   = 0.00505d0 ! (F)(A)
      real(8), parameter :: D0NO3 = 9.72d0    ! (F)(A)
      real(8), parameter :: aNO3  = 0.365d0   ! (F)(A)
      real(8), parameter :: D0H2S = 8.74d0    ! (F)(A)
      real(8), parameter :: aH2S  = 0.264d0   ! (F)(A)
      real(8), parameter :: bH2S  = 0.004d0   ! (F)(A)
      real(8), parameter :: D0SO4 = 4.96d0    ! (F)(A)
      real(8), parameter :: aSO4  = 0.226d0   ! (F)(A)
      real(8), parameter :: D0NH4 = 9.76d0    ! (F)(A)
      real(8), parameter :: aNH4  = 0.398d0   ! (F)(A)
      real(8), parameter :: D0Mn  = 3.04d0    ! (F)(A)
      real(8), parameter :: aMn   = 0.153d0   ! (F)(A)
      real(8), parameter :: D0Fe  = 3.36d0    ! (F)(A)
      real(8), parameter :: aFe   = 0.148d0   ! (F)(A)
      real(8), parameter :: D0PO4 = 9.76d0    ! (F?)(A)
      real(8), parameter :: aPO4  = 0.398d0   ! (F?)(A)
      real(8), parameter :: D0DOM = 9.76d0    !    (A)*
      real(8), parameter :: aDOM  = 0.398d0   !    (A)*
!
!  Q10 (not to use)
!
      real(8), parameter :: Q10p = 3.8d0 ! (F)(A)
      real(8), parameter :: Q10s = 2.0d0 ! (F)(A)
!
!  Biodiffusivity of solutes (cm2 s-1)
!
      real(8), parameter :: z_DBw = 11.8d0
      real(8), parameter :: u_DBw = 3.51d-6
      real(8), parameter :: a_DBw = -0.378d0
!
!  Biodiffusivity of solids
!
      real(8), parameter :: a_DBs = 9.3d0 ! (F)(A)9.3
!
!  Bioirrigation parameter
!
      real(8), parameter :: a_irr = 0.885d0 ! (F)0.885
      real(8), parameter :: b_irr = 0.054d0 ! (F)0.054
      real(8), parameter :: c_irr = 2.53d0  ! (F)2.53
      real(8), parameter :: d_irr = 0.352d0 ! (F)0.352
      real(8), parameter :: e_irr = 6.0d0   ! (A)6
      real(8), parameter :: f_irr = 0.05d0  ! (A)0.05
!
!  Kadsorption constants (cm3 g-1)
!
      real(8), parameter :: KdNH4 = 1.5d0   ! (F)2.2 (A)1.5
      real(8), parameter :: KdNO3 = 5.4d0   !        (A)5.4
      real(8), parameter :: KdMn  = 13.0d0  ! (F)(A)13
      real(8), parameter :: KdFe  = 500.0d0 ! (F)(A)500
     !real(8), parameter :: KdPO4 = 2.0d0   ! (F)(A)2
!
!  Diffusive boundary layer (cm)
!
      real(8), parameter :: DBL = 0.03d0
!
!  Ratios
!
      real(8), parameter :: ratio_n  = 0.2d0  ! FOMn/FOMtotal   (F)0.08 (A)0.2
      real(8), parameter :: ratio_f  = 0.4d0  ! FOMf/FOMtotal   (F)0.42 (A)0.4
      real(8), parameter :: ratio_FA = 0.5d0  ! FFeOOHA/FFeOOHB         (A)0.5
      real(8), parameter :: ratio_MA = 0.5d0  ! FMnO2A/FMnO2B           (A)0.5
      real(8), parameter :: ratio_CN = 8.0d0  ! molC/molN of OM (F)10   (A)8
      real(8), parameter :: ratio_CP = 70.0d0 ! molC/molP of OM (F)80   (A)70
      real(8), parameter :: ratio_DOMf = 0.75d0 ! molFast/molTotal      (A)0.75 !Nchange
!
!  External fluxes ((F)(W)nmol/cm2/s, (A)mmol/m2/day)
!
      real(8), parameter :: FMnO2  = 2.0d-2  ! (F)3.5d-6  (W)1.2d-6 (A)2.0d-2
      real(8), parameter :: FFeOOH = 1.0d0   ! (F)2.05d-4 (W)3.2d-6 (A)1.8
      real(8), parameter :: FPOM   = 22.5d0  ! (F)1.0d-3  (W)1.0d-4 (A)22.5
!
!  Limiting concentrations (uM, nmol g-1)
!
      real(8), parameter :: KO2    = 20.0d0     ! (F)(A)20
      real(8), parameter :: KNO3   = 5.0d0      ! (F)(A)5
      real(8), parameter :: KMnO2  = 50000.0d0  ! (F)(A)5.0d4 
      real(8), parameter :: KFeOOH = 100000.0d0 ! (F)(A)1.0d5
!
!  Rate constants (uM = mmol m-3 = nmol cm-3)
!
      real(8), parameter :: KDOMf = 1.0d-3 ! s-1                  (A)1.0d-3
      real(8), parameter :: KDOMs = 5.0d-9 ! s-1                  (A)5.0d-9
      real(8), parameter :: KPOMf = 2.5d-6  ! s-1      (F)9.6d-6  (A)2.5d-6  (B)2.4d-6 (W)8.7d-7
      real(8), parameter :: KPOMs = 1.2d-10 ! s-1      (F)1.2d-8  (A)1.2d-10 (B)3.0d-9 (W)3.5d-8
      real(8), parameter :: K06  = 2.5d-7  ! uM-1 s-1  (F)2.5d-6  (B)(A)2.5d-7
      real(8), parameter :: K07  = 5.0d-14 ! s-1       (F)5.0d-11 (A)5.0d-14
      real(8), parameter :: K08  = 1.7d-9  ! uM-1 s-1  (F)1.7d-8  (A)1.7d-9
      real(8), parameter :: K09  = 1.5d-5  ! uM-1 s-1  (F)(A)1.5d-5  
      real(8), parameter :: K10  = 2.0d-7  ! uM-1 s-1  (F)2.0d-8  (A)2.0d-7
      real(8), parameter :: K25  = 2.0d-7  ! uM-1 s-1  (F)2.0d-8  (A)2.0d-7
      real(8), parameter :: K11  = 5.0d-4  ! uM-1 s-1  (F)(A)5.0d-4  
      real(8), parameter :: K12  = 3.0d-9  ! uM-1 s-1  (F)(A)3.0d-9
      real(8), parameter :: K13  = 3.75d-5 ! uM-1 s-1  (F)7.5d-7  (A)3.75d-5
      real(8), parameter :: K14  = 3.0d-10 ! cm3/nmols (F)3.0d-12 (A)3.0d-10
      real(8), parameter :: K15  = 7.5d-12 ! s-1       (F)2.5d-11 (A)7.5d-12
      real(8), parameter :: K16  = 5.0d-5  ! uM-1 s-1  (F)(A)5.0d-5
      real(8), parameter :: K17  = 6.0d-7  ! uM-1 s-1  (F)(A)6.0d-7
      real(8), parameter :: K18  = 3.0d-10 ! uM-1 s-1  (F)1.6d-8  (B)(A)3.0d-10
      real(8), parameter :: K19  = 7.0d-7  ! s-1       (F)(A)7.0d-7
      real(8), parameter :: K20  = 1.3d-9  ! s-1       (F)(A)1.3d-9
      real(8), parameter :: K21  = 9.0d-10 ! s-1       (F)(A)9.0d-10
     !real(8), parameter :: K22  = 3.2d-4  ! uM-1 s-1  (W)3.2d-4
     !real(8), parameter :: K23  = 3.2d-10 ! uM-1 s-1  (W)3.2d-10
!
!  Inhibiting concentration (uM)
!
      real(8), parameter :: H2Sstop = 10.0d0 ! (F)(A)10
!
!------------------------------------------------------------------------
!  Wijsman et al., 2002.
!------------------------------------------------------------------------
!
!  Half-saturation conc. inhabitation (uM)
!
      real(8), parameter :: KinO2dn = 10.0d0
      real(8), parameter :: KinO2   = 8.0d0
      real(8), parameter :: KinNO3  = 10.0d0
      real(8), parameter :: KinMnO2 = 5000.0d0  ! umol dm-3 = nmol g-1
      real(8), parameter :: KinFeOH = 1.25d4    ! umol dm-3 = nmol g-1
      real(8), parameter :: KinSO4  = 1000.0d0
!
!  Half-saturation conc. limitation (uM)
!
      real(8), parameter :: KsO2   = 3.1d0
      real(8), parameter :: Ksnit  = 1.0d0
      real(8), parameter :: KsNO3  = 5.0d0    ! 30.0d0
      real(8), parameter :: KsMnO2 = 5000.0d0 ! 5000.0d0
      real(8), parameter :: KsFeOH = 1.25d4
      real(8), parameter :: KsSO4  = 1620.0d0
      real(8), parameter :: KsFeS  = 6.31d3
!
!  Methan
!
      real(8), parameter :: smu  = 1.002d-3*1000.0d0/100.0d0
      real(8), parameter :: Vb1  = 37.7d0
      real(8), parameter :: smph = 7.5d0
