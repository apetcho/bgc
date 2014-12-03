!
!  argument
!
      integer, parameter :: LBi   = 1
      integer, parameter :: UBi   = 1
      integer, parameter :: LBj   = 1
      integer, parameter :: UBj   = 1
      integer, parameter :: UBk   = 20
      integer, parameter :: UBt   = 17
      integer, parameter :: IminS = 1
      integer, parameter :: ImaxS = 1
      integer, parameter :: Istr  = 1
      integer, parameter :: Iend  = 1
      integer, parameter :: j     = 1
      real(8), parameter :: dtdays = 30.0d0/86400.0d0
!
!  USE mod_
!
      integer, parameter :: N    = 20
      integer, parameter :: NBT  = 17
      integer, parameter :: Nbed = 60
!
!  USE mod_biology
!
      integer, parameter :: wLDet = 1.0d0      ! m/day
      integer, parameter :: wSDet = 0.1d0      ! m/day
!
!  Set biological tracer identification indices.
!
      integer, parameter :: iNO3_ = 1          ! Nitrate concentration
      integer, parameter :: iNH4_ = 2          ! Ammonium concentration
      integer, parameter :: iChlo = 3          ! Chlorophyll concentration
      integer, parameter :: iPhyt = 4          ! Phytoplankton concentration
      integer, parameter :: iZoop = 5          ! Zooplankton concentration
      integer, parameter :: iLDeN = 6          ! Large detritus N-concentration
      integer, parameter :: iSDeN = 7          ! Small detritus N-concentration
      integer, parameter :: iLDeC = 8          ! Large detritus C-concentration
      integer, parameter :: iSDeC = 9          ! Small detritus C-concentration
      integer, parameter :: iTIC_ = 10         ! Total inorganic carbon
      integer, parameter :: iTAlk = 11         ! Total alkalinity
      integer, parameter :: iOxyg = 12         ! Dissolved oxygen concentration
      integer, parameter :: iPO4_ = 13         ! Phosphorus concentration        !*!
      integer, parameter :: iLDeP = 14         ! Large detritus P-concentration  !*!
      integer, parameter :: iSDeP = 15         ! Small detritus P-concentration  !*!
      integer, parameter :: iCOD_ = 16         ! COD O-concentration             !*!
      integer, parameter :: iH2S_ = 17         ! H2S O-concentration             !*!
!
!  Set bgc tracer identification indices.
!
      integer, parameter :: NBGCPW = 11        ! Bgc variables !Nchange9-11!
      integer, parameter :: NBGCSM = 11        ! Bgc variables
      integer, parameter :: NBGCF = 9          ! Bgc variables
!
      integer, parameter :: iwO2_ = 1          ! O2 in pore water 
      integer, parameter :: iwNH4 = 2          ! NH4 in pore water 
      integer, parameter :: iwNO3 = 3          ! NO3 in pore water 
      integer, parameter :: iwPO4 = 4          ! PO4 in pore water 
      integer, parameter :: iwSO4 = 5          ! SO4 in pore water 
      integer, parameter :: iwH2S = 6          ! H2S in pore water 
      integer, parameter :: iwMn_ = 7          ! Mn in pore water 
      integer, parameter :: iwFe_ = 8          ! Fe in pore water 
      integer, parameter :: iwCH4 = 9          ! CH4 in pore water
      integer, parameter :: iwDOMf = 10        ! DOMfast in pore water !Nchange
      integer, parameter :: iwDOMs = 11        ! DOMslow in pore water !Nchange
!
      integer, parameter :: iPOMf = 1          ! Perticle Organic Mater fast
      integer, parameter :: iPOMs = 2          ! Perticle Organic Mater slow
      integer, parameter :: iPOMn = 3          ! Perticle Organic Mater non
      integer, parameter :: iFeOA = 4          ! FeOOHA in sediment 
      integer, parameter :: iFeOB = 5          ! FeOOHB in sediment 
      integer, parameter :: iFeOP = 6          ! FeOOH-PO4 in sediment 
      integer, parameter :: iMnOA = 7          ! MnO2A in sediment 
      integer, parameter :: iMnOB = 8          ! MnO2B in sediment 
      integer, parameter :: iS0__ = 9          ! S0 in sediment 
      integer, parameter :: iFeS_ = 10         ! FeS in sediment 
      integer, parameter :: iFes2 = 11         ! FeS2 in sediment