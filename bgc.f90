!       SUBROUTINE bgc (Istr, Iend, LBi, UBi, LBj, UBj, UBk, UBt,         &
!      &                IminS, ImaxS, j, dtdays,                          &
!      &                Hz, omn,                                          &
!      &                Bio_bottom,                                       &
!      &                bgcpw, bgcsm, bgcflux)

      PROGRAM bgc
!
!**************************************************** OKADA Teruhisa ***
!  Copyright (c) 2012-2014 The Water Eng. Group Osaka Univ.            !
!    Licensed under a MIT/X style license                              !
!***********************************************************************
!                                                                      !
!  References:                                                         !
!                                                                      !
!    Fossing, H., Berg, P., Thamdrup, B., Pysgaard, S., Sorensen,      !
!      H.M. and Nielsen, K. 2004: A model set-up for an oxygen and     !
!      nutrient flux model for Aarhus Bay (Denmark). NERI Technical    !
!      Report, No. 483.                                                !
!                                                                      !
!    Wijsman, J. W. M., Herman, P. M. J., Middelburg, J. J., &         !
!      Soetaert, K. (2002). A Model for Early Diagenetic Processes     !
!      in Sediments of the Continental Shelf of the Black Sea.         !
!      Estuarine, Coastal and Shelf Science, 54(3), 403–421.           !
!      doi:10.1006/ecss.2000.0655                                      !
!                                                                      !
!    Berg, P., Rysgaard, S. and Thamdrup, B. 2003: Dynamic Modeling    !
!      of Early Diagenesis and Nutrient Cycling. A Case Study in an    !
!      Artic Marine Sediment. American Journal of Science, 303(10),    !
!      905–955. doi:10.2475/ajs.303.10.905                             !
!                                                                      !
!***********************************************************************

!       USE mod_param
!       USE mod_biology
!       USE mod_ncparam
!       USE mod_scalars

!       USE mod_parallel
!       USE mod_iounits

      implicit none

! !
! !  Imported variable declarations.
! !
!       integer, intent(in) :: LBi, UBi, LBj, UBj, UBk, UBt, IminS, ImaxS
!       integer, intent(in) :: Istr, Iend, j
!       real(8), intent(in) :: dtdays

! #ifdef ASSUMED_SHAPE
!       real(8), intent(in) :: Hz(LBi:,LBj:,:)
!       real(8), intent(in) :: omn(LBi:,LBj:)
!       real(8), intent(inout) :: Bio_bottom(IminS:,:)
!       real(8), intent(inout) :: bgcpw(LBi:,LBj:,:,:)
!       real(8), intent(inout) :: bgcsm(LBi:,LBj:,:,:)
!       real(8), intent(inout) :: bgcflux(LBi:,LBj:,:)
! #else
!       real(8), intent(in) :: Hz(LBi:UBi,LBj:Ubj,UBk)
!       real(8), intent(in) :: omn(LBi:UBi,LBj:Ubj)
!       real(8), intent(inout) :: Bio_bottom(IminS:ImaxS,UBt)
!       real(8), intent(inout) :: bgcpw(LBi:UBi,LBj:UBj,Nbed,NBGCPW)
!       real(8), intent(inout) :: bgcsm(LBi:UBi,LBj:UBj,Nbed,NBGCSM)
!       real(8), intent(inout) :: bgcflux(LBi:UBi,LBj:UBj,MBGCF)
! #endif

      include 'use_mods.f'

      real(8) :: Hz(LBi:UBi,LBj:UBj,N)
      real(8) :: omn(LBi:UBi,LBj:UBj)
      real(8) :: Bio_bottom(IminS:ImaxS,NBT)
      real(8) :: bgcpw(LBi:UBi,LBj:UBj,Nbed,NBGCPW)
      real(8) :: bgcsm(LBi:UBi,LBj:UBj,Nbed,NBGCSM)
      real(8) :: bgcflux(LBi:UBi,LBj:UBj,NBGCF)
!
!  Local variable declarations.
!
      integer :: i, k, Iter, iter2, itrc, ireac
      real(8) :: dtBgc
      real(8) :: fac, cff, cff1, cff2, cff3, cff4, T
!
!  convert constants
!
      real(8), parameter :: cm2m   = 0.01d0
      real(8), parameter :: m2cm   = 100.0d0
      real(8), parameter :: year2s = 365.0d0*24.0d0*3600.0d0
      real(8), parameter :: day2s  = 24.0d0*3600.0d0
!
!  state variables
!
      real(8), dimension(NBGCPW) :: cff_pw
      real(8), dimension(NBGCPW) :: Rpw
      real(8), dimension(NBGCSM) :: Rsm
      real(8), dimension(IminS:ImaxS,NBGCPW) :: bw            ! uM = mmol m-3 = nmol cm-3
      real(8), dimension(IminS:ImaxS,Nbed)   :: tsm           ! Celius
      real(8), dimension(IminS:ImaxS,0:Nbed,NBGCPW) :: pw       ! uM = mmol m-3 = nmol cm-3
      real(8), dimension(IminS:ImaxS,Nbed,NBGCSM) :: sm       ! nmol g-1
      real(8), dimension(IminS:ImaxS,0:Nbed,NBGCPW) :: pwflux ! nmol cm-2 s-1
      real(8), dimension(IminS:ImaxS,0:Nbed,NBGCSM) :: smflux ! nmol cm-2 s-1
!
!  layer (cm)
!
      real(8), dimension(IminS:ImaxS,0:Nbed) :: dz
      real(8), dimension(IminS:ImaxS,0:Nbed) :: dzd
      real(8), dimension(IminS:ImaxS,Nbed)   :: depth
!
!------------------------------------------------------------------------
!  biogeochemical model variables and parameters (Fossing et al., 2004)
!------------------------------------------------------------------------
!
!  porosity (nondimensional)
!
      real(8), dimension(0:Nbed) :: poro
      real(8), dimension(0:Nbed) :: porod
!
!  Burial rate (cm year-1 to cm s-1)
!
      real(8), dimension(IminS:ImaxS,Nbed)   :: wsm
      real(8), dimension(IminS:ImaxS,0:Nbed) :: wsmd
!
!  difusivity i free water (cm2 s-1)
!
      real(8), dimension(IminS:ImaxS,Nbed,NBGCPW)   :: Diff
      real(8), dimension(IminS:ImaxS,0:Nbed,NBGCPW) :: Diffd
!
!  Biodiffusivity of solutes (cm2 s-1)
!
      real(8), dimension(IminS:ImaxS,Nbed)   :: DBw
      real(8), dimension(IminS:ImaxS,0:Nbed) :: DBwd
!
!  Biodiffusivity of solids
!
      real(8), dimension(IminS:ImaxS,Nbed)   :: DBs
      real(8), dimension(IminS:ImaxS,0:Nbed) :: DBsd
!
!  Bioirrigation parameter
!
      real(8), dimension(IminS:ImaxS,Nbed) :: irr
      real(8), dimension(IminS:ImaxS,0:Nbed,NBGCPW) :: irrigation
!
!  Kadsorption constants (cm3 g-1)
!
      real(8), dimension(IminS:ImaxS,0:Nbed,NBGCPW) :: Kads
      real(8), dimension(IminS:ImaxS,0:Nbed,NBGCPW) :: Kadsd
!
!  Bio-chemical variables
!
      real(8), dimension(25) :: R = 0.0d0
      real(8) :: Rdomf = 0.0d0
      real(8) :: Rdoms = 0.0d0
      real(8) :: Rpomff = 0.0d0
      real(8) :: Rpomfs = 0.0d0
      real(8) :: Rpomsf = 0.0d0
      real(8) :: Rpomss = 0.0d0
      real(8),dimension(IminS:ImaxS,60) :: F  
      
!
!  Thomas algorism input data
!
      real(8), dimension(0:Nbed) :: Thomas_a
      real(8), dimension(0:Nbed) :: Thomas_b
      real(8), dimension(0:Nbed) :: Thomas_c
      real(8), dimension(0:Nbed) :: Thomas_d
!
!  bgc parameters
!
      include 'bgc_param.f'
!
!-----------------------------------------------------------------------
!  Initial conditions.
!-----------------------------------------------------------------------
!
      include 'bgc_initialize.f'
!
!  Delta time
!
      dtBgc=dtdays*86400.0d0/REAL(BgcIter,8)
!
!  i-loop
!
      INI_LOOP : DO i=Istr,Iend
!
!  Bed thickness (cm)
!
        dz(i,0)=Hz(i,j,1)
        DO k=1,Nbed
          dz(i,k)=real(k,8)/100.0d0 !change 10-100
        END DO
!
        dzd(i,0)=dz(i,1)
        DO k=1,Nbed-1
          dzd(i,k)=0.5d0*(dz(i,k)+dz(i,k+1))
        END DO
        dzd(i,Nbed)=dz(i,Nbed)

        depth(i,1)=dz(i,1)
        DO k=2,Nbed
          depth(i,k)=depth(i,k-1)+dz(i,k)
        END DO
!
!  porosity (nondimensional)
!
        poro(0)=1.0d0
        DO k=1,Nbed
          poro(k)=a_poro+b_poro*10.0d0**(-c_poro*depth(i,k))
        END DO
!
        porod(0)=poro(1)
        DO k=1,Nbed-1
          porod(k)=0.5d0*(poro(k)+poro(k+1))
        END DO
        porod(Nbed)=poro(Nbed)
!
!  Burial rate (cm year-1 to cm s-1)
!
       !wsm(i)=c_wsm/year2s
       !wsmd(i)=wsm(i)

        DO k=1,Nbed
          wsm(i,k)=a_wsm+b_wsm*10.0d0**(-c_wsm*depth(i,k))
          wsm(i,k)=wsm(i,k)/year2s
        END DO

        wsmd(i,0)=wsm(i,1)
        DO k=1,Nbed-1
          wsmd(i,k)=0.5d0*(wsm(i,k)+wsm(i,k+1))
        END DO
        wsmd(i,Nbed)=wsm(i,Nbed)
!
!  import water colomn tracer's consentrations (mmol m-3)
!
        bw(i,iwO2_)=MAX(Bio_bottom(i,iOxyg),0.0d0)
        bw(i,iwNO3)=MAX(Bio_bottom(i,iNO3_),0.0d0)
        bw(i,iwNH4)=MAX(Bio_bottom(i,iNH4_),0.0d0)
        bw(i,iwPO4)=MAX(Bio_bottom(i,iPO4_),0.0d0)
        bw(i,iwH2S)=MAX(Bio_bottom(i,iH2S_),0.0d0)
        bw(i,iwSO4)=bwSO4
        bw(i,iwMn_)=bwMn
        bw(i,iwFe_)=bwFe
        bw(i,iwCH4)=bwCH4
        bw(i,iwDOMf)=bwDOMf                                  !Nchange
        bw(i,iwDOMs)=bwDOMs                                  !Nchange
!
!  state variables
!
        DO itrc=1,NBGCPW
          pw(i,0,itrc)=bw(i,itrc)
          DO k=1,Nbed
            pw(i,k,itrc)=bgcpw(i,j,k,itrc) ! uM
          END DO
        END DO
        DO k=1,Nbed
          DO itrc=1,NBGCSM
            sm(i,k,itrc)=bgcsm(i,j,k,itrc) ! nmol g-1
          END DO
        END DO
!
!  bed temperature (Celius)
!
        DO k=1,Nbed
          tsm(i,k)=20.0d0 !Bio_bottom(i,itemp)
        END DO
!
      END DO INI_LOOP
!
!  ITER_LOOP
!
      !ITER_LOOP : DO Iter=0,BgcIter
      ITER_LOOP : DO Iter=0,ndays/dtdays*BgcIter
!
!  (only bgc0.f90) bed temperature (Celius)
!
        cff=dtBgc*real(Iter,8)/day2s
        DO i=Istr,Iend
          DO k=1,Nbed
            tsm(i,k)=17.3d0+0.7d0*9.67d0*sin(3.141592d0/180.0d0*(cff+210.0d0))
          END DO
        END DO
!
        DO i=Istr,Iend
          bw(i,iwO2_)=-12.0d0*tsm(i,1)+300.0d0
        END DO
!
        DO i=Istr,Iend
          DO k=0,Nbed
            DO itrc=1,NBGCPW
              pwflux(i,k,itrc)=0.0d0
            END DO
            DO itrc=1,NBGCSM
              smflux(i,k,itrc)=0.0d0
            END DO
          END DO
        END DO
!
        DO i=Istr,Iend
          DO itrc=1,NBGCPW
            pw(i,0,itrc)=bw(i,itrc)/poro(0)
          END DO
        END DO
!
!-----------------------------------------------------------------------
!  Diffusion parameters (cm2 s-1)
!-----------------------------------------------------------------------
!
        DIFF_LOOP : DO i=Istr,Iend
!
!  Diffusion paremeter depneding on benthic temperature
!
          DO k=1,Nbed
            cff1=1.0d-6
            T=tsm(i,k)
!
!  - Fossing
!
            Diff(i,k,iwFe_)=cff1*(D0Fe +aFe*T)
            Diff(i,k,iwMn_)=cff1*(D0Mn +aMn*T)
            Diff(i,k,iwNH4)=cff1*(D0NH4+aNH4*T)
            Diff(i,k,iwNO3)=cff1*(D0NO3+aNO3*T)
            Diff(i,k,iwSO4)=cff1*(D0SO4+aSO4*T)
            Diff(i,k,iwO2_)=cff1*(D0O2 +aO2*T +bO2*(T**2.0d0))
            Diff(i,k,iwH2S)=cff1*(D0H2S+aH2S*T+bH2S*(T**2.0d0))
            Diff(i,k,iwPO4)=cff1*(D0PO4+aPO4*T)
            Diff(i,k,iwDOMf)=cff1*(D0DOM+aDOM*T)        !Nchange
            Diff(i,k,iwDOMs)=cff1*(D0DOM+aDOM*T)        !Nchange
!
!  - Wijsman
!
            !Diff(i,k,iwCH4)=4.72d0*10.0d-9*(T+273.0d0)/(smu*Vb1**0.6d0)
          END DO
!
          DO itrc=1,NBGCPW
            Diffd(i,0,itrc)=Diff(i,1,itrc)
            DO k=1,Nbed-1
              Diffd(i,k,itrc)=(Diff(i,k,itrc)*dz(i,k+1)+                    &
     &                         Diff(i,k+1,itrc)*dz(i,k))*0.5d0/dzd(i,k)
            END DO
            Diffd(i,Nbed,itrc)=0.0d0
          END DO
!
!  Biodiffusivity of solutes (cm2 s-1)
!
          DO k=1,Nbed
            cff1=1.03d0**(tsm(i,k)-20.0d0)
            IF (depth(i,k).LE.z_DBw) THEN
              DBw(i,k)=u_DBw*cff1
            ELSE
              DBw(i,k)=u_DBw*cff1*exp(a_DBw*(depth(i,k)-z_DBw))
            ENDIF
          END DO
!
          DO k=1,Nbed
            IF (bw(i,iwO2_).lt.62.5d0) THEN
              DBw(i,k)=0.0d0
            ELSEIF ((bw(i,iwO2_).ge.62.5d0).and.(bw(i,iwO2_).lt.93.75d0)) THEN
              DBw(i,k)=DBw(i,k)*(bw(i,iwO2_)-62.5d0)/31.25d0
            ENDIF
          END DO
!
          DO k=1,Nbed
            DBw(i,k)=0.0d0 !*!
          END DO
!
          DBwd(i,0)=DBw(i,1)
          DO k=1,Nbed-1
            DBwd(i,k)=(DBw(i,k)*dz(i,k+1)+DBw(i,k+1)*dz(i,k))*0.5d0/dzd(i,k)
          END DO
          DBwd(i,Nbed)=0.0d0
!
!  Biodiffusivity of solids (cm2 s-1)
!
          DO k=1,Nbed
            DBs(i,k)=DBw(i,k)/a_DBs
          END DO

          DO k=0,Nbed
            DBsd(i,k)=DBwd(i,k)/a_DBs
          END DO
!
!  Absorption constants (cm3 g-1)
!
          DO itrc=1,NBGCPW
            Kads(i,0,itrc)=0.0d0
          END DO
          DO k=1,Nbed
            cff1=1.03d0**(tsm(i,k)-20.0d0)
            cff2=1.0d0/cff1
            Kads(i,k,iwCH4)=0.0d0
            Kads(i,k,iwH2S)=0.0d0
            Kads(i,k,iwNO3)=0.0d0
            Kads(i,k,iwO2_)=0.0d0
            Kads(i,k,iwSO4)=0.0d0
            Kads(i,k,iwFe_)=KdFe*cff2
            Kads(i,k,iwMn_)=KdMn*cff2
            Kads(i,k,iwNH4)=KdNH4*cff2
!           Kads(i,k,iwPO4)=KdPO4*cff2
            Kads(i,k,iwSO4)=0.0d0
            Kads(i,k,iwDOMs)=0.0d0                 !Nchange
            Kads(i,k,iwDOMf)=0.0d0                 !Nchange   
          END DO
!
!  Absorption constant of PO4 (Horie 1984)
!
          DO k=1,Nbed
            cff1=bw(i,iwO2_)*32.0d0/1000.0d0
            cff2=11.5d0*0.748d0**cff1*1.02d0**(tsm(i,k)-20.0d0)
            Kads(i,k,iwPO4)=poro(k)/(1.0d0-poro(k))/cff2
          END DO
!
          DO itrc=1,NBGCPW
            Kadsd(i,0,itrc)=Kads(i,1,itrc)
            DO k=1,Nbed-1
              Kadsd(i,k,itrc)=(Kads(i,k,itrc)*dz(i,k+1)+Kads(i,k+1,itrc)*dz(i,k))*0.5d0/dzd(i,k)
            END DO
            Kadsd(i,Nbed,itrc)=Kads(i,Nbed,itrc)
          END DO
!
        END DO DIFF_LOOP
!
!-----------------------------------------------------------------------
!  Compute external & burial fluxes (nmol cm-2 s-1)
!-----------------------------------------------------------------------
!
        FLUX_LOOP : DO i=Istr,Iend
!
!  External (surface) fluxes (cm2 s-1 * nmol cm-3 / cm = nmol cm-2 s-1)
!
          k=0
!
!  - pore water including  interaction pelagic and benthic quality
!
!          cff1=DBwd(i,k)
!          DO itrc=1,NBGCPW
!            cff2=Diffd(i,k,itrc)/(1.0d0-3.0d0*(1.0d0-porod(k)))
!            pwflux(i,k,itrc)=porod(k)*(cff1+cff2)*(bw(i,itrc)-pw(i,1,itrc))/dzd(i,k)
!          END DO
!
!  - sediment flux (F) (nmol/cm2s)
!
!           smflux(i,k,iPOMf)=FPOM*ratio_f
!           smflux(i,k,iPOMs)=FPOM*(1.0d0-ratio_n-ratio_f)
!           smflux(i,k,iPOMn)=FPOM*ratio_n
!
!  - sediment flux (nmol/cm2/s = cm/s * nmol/cm3) (Okada)
!
!         m/day -> cm/s
!
          cff=m2cm/day2s*1.05d0**(tsm(i,1)-20.0d0)
          cff1=cff*wLDet*Bio_bottom(i,iLDeC)
          cff2=cff*wSDet*Bio_bottom(i,iSDeC)
          smflux(i,k,iPOMf)=(cff1+cff2)*ratio_f
          smflux(i,k,iPOMs)=(cff1+cff2)*(1.0d0-ratio_n-ratio_f)
          smflux(i,k,iPOMn)=(cff1+cff2)*ratio_n
!
!         mmol/m2/day -> nmol/cm2/s
!         
          cff=1.0d6/(m2cm*m2cm*day2s)
          smflux(i,k,iFeOA)=cff*FFeOOH*ratio_FA
          smflux(i,k,iFeOB)=cff*FFeOOH*(1.0d0-ratio_FA)
          smflux(i,k,iFeOP)=0.0d0
          smflux(i,k,iMnOA)=cff*FMnO2*ratio_MA
          smflux(i,k,iMnOB)=cff*FMnO2*(1.0d0-ratio_MA)
          smflux(i,k,iS0__)=0.0d0
          smflux(i,k,iFeS_)=0.0d0
          smflux(i,k,iFes2)=0.0d0
!
!  Burial flux (cm s-1 * nmol cm-3 / cm = nmol cm-2 s-1)
!
!          DO itrc=1,NBGCPW
!            DO k=1,Nbed-1
!              cff1=porod(k)*usmd(i,k)
!              cff2=dens*(1.0d0-porod(k))*usmd(i,k)*Kads(i,k,itrc)
!              pwflux(i,k,itrc)=(cff1+cff2)*(pw(i,k,itrc)-pw(i,k+1,itrc))/dzd(i,k)
!            END DO
!            pwflux(i,Nbed,itrc)=pwflux(i,Nbed-1,itrc)
!          END DO
!
          DO itrc=1,NBGCSM
            DO k=1,Nbed
              fac=dens*(1.0d0-porod(k))*wsmd(i,k)
              smflux(i,k,itrc)=fac*sm(i,k,itrc)
            END DO
          END DO
!
!  Bioirrigation flux (year-1 to s-1)
!
          cff=bw(i,iwO2_)
          DO k=1,Nbed
            irr(i,k)=10.0d0**(a_irr-b_irr*depth(i,k)+c_irr*exp(-d_irr*depth(i,k)))/year2s
            IF (cff.lt.62.5d0) THEN
              irr(i,k)=0.0d0
            ELSEIF ((cff.ge.62.5d0).and.(cff.lt.93.75d0)) THEN
              irr(i,k)=irr(i,k)*(cff-62.5d0)/31.25d0
            ENDIF
          END DO
!
          DO itrc=1,NBGCPW
            irrigation(i,0,itrc)=0.0d0
            DO k=1,Nbed
              irrigation(i,k,itrc)=poro(k)*irr(i,k)*(pw(i,0,itrc)-pw(i,k,itrc))
              irrigation(i,k,itrc)=0.0d0 !*!
            END DO
          END DO
!
        END DO FLUX_LOOP
!
!------------------------------------------------------------------------
!  Compute simultaneous equation by Thomas algorizm
!------------------------------------------------------------------------
!
        DO i=Istr,Iend
!
!  Pore Water
!
          DO itrc=1,NBGCPW

            Thomas_a(0)=0.0d0
            DO k=1,Nbed
              fac=poro(k)+dens*(1.0d0-poro(k))*Kads(i,k,itrc)
              cff=dtBgc/fac/dz(i,k)
              cff1=porod(k-1)*(Diffd(i,k-1,itrc)/(1.0d0+3.0d0*(1.0d0-porod(k-1)))+DBwd(i,k-1))
              cff3=dens*(1.0d0-porod(k-1))*DBsd(i,k-1)*Kadsd(i,k-1,itrc)
              Thomas_a(k)=cff*(cff1+cff3)/dzd(i,k-1)
            END DO

            Thomas_c(Nbed)=0.0d0
            DO k=0,Nbed-1
              fac=poro(k)+dens*(1.0d0-poro(k))*Kads(i,k,itrc)
              cff=dtBgc/fac/dz(i,k)
              cff2=porod(k)*(Diffd(i,k,itrc)/(1.0d0+3.0d0*(1.0d0-porod(k)))+DBwd(i,k))
              cff4=dens*(1.0d0-porod(k))*DBsd(i,k)*Kadsd(i,k,itrc)
              Thomas_c(k)=cff*(cff2+cff4)/dzd(i,k)
            END DO

            DO k=0,Nbed
              fac=poro(k)+dens*(1.0d0-poro(k))*Kads(i,k,itrc)
              Thomas_b(k)=1.0d0+Thomas_a(k)+Thomas_c(k)
              Thomas_d(k)=pw(i,k,itrc)!+                                 &
    !&                    irrigation(i,k,itrc)*dtBgc/fac
    !&                    (pwflux(i,k,itrc)+irrigation(i,k,itrc))*dtBgc/fac
            END DO

            CALL Thomas (IminS, ImaxS, 0, Nbed, NBGCPW, i, itrc,        &
     &                   Thomas_a,                                      &
     &                   Thomas_b,                                      &
     &                   Thomas_c,                                      &
     &                   Thomas_d,                                      &
     &                   pw)

            cff=(poro(0)*pw(i,0,itrc)-bw(i,itrc))*dz(i,0)/dtBgc
            pwflux(i,0,itrc)=pwflux(i,0,itrc)+cff

          END DO
!
!  Sediment Mud
!
          DO itrc=1,NBGCSM

            Thomas_a(1)=0.0d0
            DO k=2,Nbed
              fac=dens*(1.0d0-poro(k))
              cff=dtBgc/fac/dz(i,k)
              cff3=dens*(1.0d0-porod(k-1))*DBsd(i,k-1)
              Thomas_a(k)=cff*cff3/dzd(i,k-1)
            END DO

            Thomas_c(Nbed)=0.0d0
            DO k=1,Nbed-1
              fac=dens*(1.0d0-poro(k))
              cff=dtBgc/fac/dz(i,k)
              cff4=dens*(1.0d0-porod(k))*DBsd(i,k)
              Thomas_c(k)=cff*cff4/dzd(i,k)
            END DO

            DO k=1,Nbed
              fac=dens*(1.0d0-poro(k))
              Thomas_b(k)=1.0d0+Thomas_a(k)+Thomas_c(k)
              Thomas_d(k)=sm(i,k,itrc)+                                 &
     &                    (smflux(i,k-1,itrc)-smflux(i,k,itrc))*dtBgc/fac/dz(i,k)
            END DO

            CALL Thomas (IminS, ImaxS, 1, Nbed, NBGCSM, i, itrc,        &
     &                   Thomas_a(1:Nbed),                              &
     &                   Thomas_b(1:Nbed),                              &
     &                   Thomas_c(1:Nbed),                              &
     &                   Thomas_d(1:Nbed),                              &
     &                   sm)

          END DO
!
        END DO
!
!------------------------------------------------------------------------
!  Bio-chemical changing part
!------------------------------------------------------------------------
!
        DO i=Istr,Iend
          DO itrc=1,50
            F(i,itrc)=0.0d0
          END DO
        END DO
 
        BIO_LOOP : DO i=Istr,Iend
          DO k=1,Nbed
            DO itrc=1,NBGCPW
              Rpw(itrc)=0.0d0
            END DO
            DO itrc=1,NBGCSM
              Rsm(itrc)=0.0d0
            END DO
            
!  Decomposition of POM (nmol s-1 cm-3)                         !Nchange             
!
            cff1=(1.0d0-poro(k))*dens
            
            Rpomff=cff1*KPOMf*sm(i,k,iPOMf)*ratio_DOMf          !fast to fast
            Rpomfs=cff1*KPOMf*sm(i,k,iPOMf)*(1-ratio_DOMf)      !fast to slow
            Rpomsf=cff1*KPOMs*sm(i,k,iPOMs)*ratio_DOMf          !slow to fast
            Rpomss=cff1*KPOMs*sm(i,k,iPOMs)*(1-ratio_DOMf)      !slow to slow           
            
            Rsm(iPOMf)=Rsm(iPOMf)-(Rpomff+Rpomfs)                          
            Rsm(iPOMs)=Rsm(iPOMs)-(Rpomsf+Rpomss)
            Rpw(iwDOMf)=Rpw(iwDOMf)+(Rpomff+Rpomsf)
            Rpw(iwDOMs)=Rpw(iwDOMs)+(Rpomfs+Rpomss)
            
            F(i,1)=F(i,1)-dz(i,k)*Rpomff
            F(i,2)=F(i,2)-dz(i,k)*Rpomfs
            F(i,3)=F(i,3)-dz(i,k)*Rpomsf
            F(i,4)=F(i,4)-dz(i,k)*Rpomss
!
!  First order reaction (nmol s-1 cm-3)
!
            cff=1.03d0**(tsm(i,k)-20.0d0)*poro(k)                !Nchange
            Rdomf=KDOMf*cff*pw(i,k,iwDOMf)                       !Nchange
            Rdoms=KDOMs*cff*pw(i,k,iwDOMs)
             
            F(i,5)=F(i,5)-dz(i,k)*Rdomf
            F(i,6)=F(i,6)-dz(i,k)*Rdoms
!
!  - Wijsman
!
!             R(1)=pw(i,k,iwO2_)/(pw(i,k,iwO2_)+KsO2)
!             R(2)=pw(i,k,iwNO3)/(pw(i,k,iwNO3)+KsNO3)*                   &
!      &           (1.0d0-pw(i,k,iwO2_)/(pw(i,k,iwO2_)+KinO2dn))
!             R(3)=sm(i,k,iMnOA)/(sm(i,k,iMnOA)+KsMnO2)*                  &
!      &           (1.0d0-pw(i,k,iwO2_)/(pw(i,k,iwO2_)+KinO2))*           &
!      &           (1.0d0-pw(i,k,iwNO3)/(pw(i,k,iwNO3)+KinNO3))
!             R(4)=sm(i,k,iFeOA)/(sm(i,k,iFeOA)+KsFeOH)*                  &
!      &           (1.0d0-pw(i,k,iwO2_)/(pw(i,k,iwO2_)+KinO2))*           &
!      &           (1.0d0-pw(i,k,iwNO3)/(pw(i,k,iwNO3)+KinNO3))*          &
!      &           (1.0d0-sm(i,k,iMnOA)/(sm(i,k,iMnOA)+KinMnO2))
!             R(5)=pw(i,k,iwSO4)/(pw(i,k,iwSO4)+KsSO4)*                   &
!      &           (1.0d0-pw(i,k,iwO2_)/(pw(i,k,iwO2_)+KinO2))*           &
!      &           (1.0d0-pw(i,k,iwNO3)/(pw(i,k,iwNO3)+KinNO3))*          &
!      &           (1.0d0-sm(i,k,iMnOA)/(sm(i,k,iMnOA)+KinMnO2))*         &
!      &           (1.0d0-sm(i,k,iFeOA)/(sm(i,k,iFeOA)+KinFeOH))
!             R(6)=(1.0d0-pw(i,k,iwO2_)/(pw(i,k,iwO2_)+KinO2))*           &
!      &           (1.0d0-pw(i,k,iwNO3)/(pw(i,k,iwNO3)+KinNO3))*          &
!      &           (1.0d0-sm(i,k,iMnOA)/(sm(i,k,iMnOA)+KinMnO2))*         &
!      &           (1.0d0-sm(i,k,iFeOA)/(sm(i,k,iFeOA)+KinFeOH))*         &
!      &           (1.0d0-pw(i,k,iwSO4)/(pw(i,k,iwSO4)+KinSO4))
!
!  - Fossing
!
            IF (pw(i,k,iwO2_).gt.KO2) THEN
              R(2)=0.0d0
              R(1)=1.0d0
            ELSE
              cff=pw(i,k,iwO2_)/KO2
              R(2)=1.0d0-cff
              R(1)=cff
            END IF

            IF (pw(i,k,iwNO3).gt.KNO3) THEN
              R(3)=R(2)*0.0d0
              R(2)=R(2)*1.0d0
            ELSE
              cff=pw(i,k,iwNO3)/KNO3
              R(3)=R(2)*(1.0d0-cff)
              R(2)=R(2)*cff
            END IF

            IF (sm(i,k,iMnOA).gt.KMnO2) THEN
              R(4)=R(3)*0.0d0
              R(3)=R(3)*1.0d0
            ELSE
              cff=sm(i,k,iMnOA)/KMnO2
              R(4)=R(3)*(1.0d0-cff)
              R(3)=R(3)*cff
            END IF

            IF (sm(i,k,iFeOA).gt.KFeOOH) THEN
              R(5)=R(4)*0.0d0
              R(4)=R(4)*1.0d0
            ELSE
              cff=sm(i,k,iFeOA)/KFeOOH
              R(5)=R(4)*(1.0d0-cff)
              R(4)=R(4)*cff
            END IF
!
!  Backword
!
            DO ireac=1,6
              R(ireac)=(Rdomf+Rdoms)*R(ireac)
            END DO

            Rpw(iwDOMf)=Rpw(iwDOMf)-Rdomf                          !Nchange 
            Rpw(iwDOMs)=Rpw(iwDOMs)-Rdoms                          !Nchange 
            Rpw(iwNH4)=Rpw(iwNH4)+(Rdomf+Rdoms)/ratio_CN 
            Rpw(iwPO4)=Rpw(iwPO4)+(Rdomf+Rdoms)/ratio_CP

            Rpw(iwO2_)=Rpw(iwO2_)-R(1)
            Rpw(iwNO3)=Rpw(iwNO3)-R(2)*0.8d0
            Rsm(iMnOA)=Rsm(iMnOA)-R(3)*2.0d0
            Rpw(iwMn_)=Rpw(iwMn_)+R(3)*2.0d0
            Rsm(iFeOA)=Rsm(iFeOA)-R(4)*4.0d0
            Rpw(iwFe_)=Rpw(iwFe_)+R(4)*4.0d0
            Rpw(iwSO4)=Rpw(iwSO4)-R(5)*0.5d0
            Rpw(iwH2S)=Rpw(iwH2S)+R(5)*0.5d0
            
            F(i,7)=F(i,7)-dz(i,k)*R(1)          !O2
            F(i,8)=F(i,8)-dz(i,k)*R(1)          !dom
            F(i,9)=F(i,9)-dz(i,k)*R(2)*4.0d0    !NO3
            F(i,10)=F(i,10)-dz(i,k)*R(2)        !dom
            F(i,11)=F(i,11)-dz(i,k)*R(3)*2.0d0  !MnO2
            F(i,12)=F(i,12)-dz(i,k)*R(3)        !dom
            F(i,13)=F(i,13)-dz(i,k)*R(4)*4.0d0  !FeOOH
            F(i,14)=F(i,14)-dz(i,k)*R(4)        !dom
            F(i,15)=F(i,15)-dz(i,k)*R(5)        !SO4
            F(i,16)=F(i,16)-dz(i,k)*R(5)        !dom
            
            F(i,17)=F(i,17)-dz(i,k)*(Rdomf+Rdoms)/ratio_CN    !NH4
            F(i,18)=F(i,18)-dz(i,k)*(Rdomf+Rdoms)/ratio_CP    !PO4
!
!  Secondary reactions (nmol cm-3 s-1)
!
            cff1=(1.0d0-poro(k))*dens
!
!  - Re-oxydations of 
!
           !R(6)=K06*poro(k)*pw(i,k,iwNH4)*pw(i,k,iwO2_)/(pw(i,k,iwO2_)+Ksnit) ! Wijsman
            R(6)=K06*poro(k)*pw(i,k,iwNH4)*pw(i,k,iwO2_) ! Fossing
            Rpw(iwNH4)=Rpw(iwNH4)-R(6)
            Rpw(iwO2_)=Rpw(iwO2_)-R(6)*2.0d0
            Rpw(iwNO3)=Rpw(iwNO3)+R(6)
            
            F(i,19)=F(i,19)-dz(i,k)*R(6)          !NH4
            F(i,20)=F(i,20)-dz(i,k)*R(6)*2.0d0    !O2
            
           !R(7)=K07*cff1*(sm(i,k,iFeOA)+sm(i,k,iFeOB))*pw(i,k,iwPO4) ! (B)
            R(7)=K07*cff1*sm(i,k,iFeOA)*pw(i,k,iwPO4) ! (A)
            Rsm(iFeOA)=Rsm(iFeOA)-R(7)
            Rpw(iwPO4)=Rpw(iwPO4)-R(7)
            Rsm(iFeOP)=Rsm(iFeOP)+R(7)
            
            F(i,21)=F(i,21)-dz(i,k)*R(7)         !FeOA
            F(i,22)=F(i,22)-dz(i,k)*R(7)         !PO4
!
!  - Re-oxydations using MnO2
!
           !R(8)=K08*cff1*pw(i,k,iwFe_)*(sm(i,k,iMnOA)+sm(i,k,iMnOB)) ! (B)
            R(8)=K08*cff1*pw(i,k,iwFe_)*sm(i,k,iMnOA) ! (A)
            Rpw(iwFe_)=Rpw(iwFe_)-R(8)*2.0d0
            Rsm(iMnOA)=Rsm(iMnOA)-R(8)
            Rpw(iwMn_)=Rpw(iwMn_)+R(8)
            Rsm(iFeOA)=Rsm(iFeOA)-R(8)*2.0d0
            
            F(i,23)=F(i,23)-dz(i,k)*R(8)*2.0d0    !Fe
            F(i,24)=F(i,24)-dz(i,k)*R(8)          !MnOA

            R(9)=K09*poro(k)*pw(i,k,iwMn_)*pw(i,k,iwO2_)
            Rpw(iwMn_)=Rpw(iwMn_)-R(9)
            Rpw(iwO2_)=Rpw(iwO2_)-R(9)*0.5d0
            Rsm(iMnOA)=Rsm(iMnOA)+R(9)
            
            F(i,25)=F(i,25)-dz(i,k)*R(9)          !Mn
            F(i,26)=F(i,26)-dz(i,k)*R(9)*0.5d0    !O2
!
!  - Re-oxydations using FeOOH (A,B,=PO4)
!
           !R(10)=K10*cff1*pw(i,k,iwH2S)*(sm(i,k,iFeOA)+sm(i,k,iFeOB)) ! (B)
            R(10)=K10*cff1*pw(i,k,iwH2S)*sm(i,k,iFeOA) ! (A)
            Rpw(iwH2S)=Rpw(iwH2S)-R(10)
            Rsm(iFeOA)=Rsm(iFeOA)-R(10)*2.0d0
            Rpw(iwFe_)=Rpw(iwFe_)+R(10)*2.0d0
            Rsm(iS0__)=Rsm(iS0__)+R(10)
            
            F(i,27)=F(i,27)-dz(i,k)*R(10)         !H2S
            F(i,28)=F(i,28)-dz(i,k)*R(10)*2.0d0   !FeOA
            
            R(25)=K25*cff1*pw(i,k,iwH2S)*sm(i,k,iFeOP)
            Rpw(iwH2S)=Rpw(iwH2S)-R(25)
            Rsm(iFeOP)=Rsm(iFeOP)-R(25)*2.0d0
            Rpw(iwFe_)=Rpw(iwFe_)+R(25)*2.0d0
            Rsm(iS0__)=Rsm(iS0__)+R(25)
            Rpw(iwPO4)=Rpw(iwPO4)+R(25)*2.0d0
            
            F(i,29)=F(i,29)-dz(i,k)*R(25)         !H2S
            F(i,30)=F(i,30)-dz(i,k)*R(25)*2.0d0   !FeOP
            
           !R(11)=K11*poro(k)*pw(i,k,iwFe_)*pw(i,k,iwO2_)*OH**2.0d0 !Wijsman
            R(11)=K11*poro(k)*pw(i,k,iwFe_)*pw(i,k,iwO2_) !Fossing
            Rpw(iwFe_)=Rpw(iwFe_)-R(11)
            Rpw(iwO2_)=Rpw(iwO2_)-R(11)*0.25d0
            Rsm(iFeOA)=Rsm(iFeOA)+R(11)
            
            F(i,31)=F(i,31)-dz(i,k)*R(11)          !Fe
            F(i,32)=F(i,32)-dz(i,k)*R(11)*0.25d0   !O2
            
           !R(12)=K12*cff1*pw(i,k,iwH2S)*(sm(i,k,iMnOA)+sm(i,k,iMnOB)) ! (B)
            R(12)=K12*cff1*pw(i,k,iwH2S)*sm(i,k,iMnOA) ! (A)
            Rpw(iwH2S)=Rpw(iwH2S)-R(12)
            Rsm(iMnOA)=Rsm(iMnOA)-R(12)
            Rpw(iwMn_)=Rpw(iwMn_)+R(12)
            Rsm(iS0__)=Rsm(iS0__)+R(12)
            
            F(i,33)=F(i,33)-dz(i,k)*R(12)         !H2S
            F(i,34)=F(i,34)-dz(i,k)*R(12)         !MnOA
            
           !R(13)=K13*poro(k)*pw(i,k,iwFe_)*HS/H1/KsFeS-1.0d0 ! (W)
            R(13)=K13*poro(k)*pw(i,k,iwFe_)*pw(i,k,iwH2S) ! (F)(A)
            Rpw(iwFe_)=Rpw(iwFe_)-R(13)
            Rpw(iwH2S)=Rpw(iwH2S)-R(13)
            Rsm(iFeS_)=Rsm(iFeS_)+R(13)
            
            F(i,35)=F(i,35)-dz(i,k)*R(13)         !Fe 
            F(i,36)=F(i,36)-dz(i,k)*R(13)         !H2S
            
            R(14)=K14*cff1*sm(i,k,iFeS_)*sm(i,k,iS0__)
            Rsm(iFeS_)=Rsm(iFeS_)-R(14)
            Rsm(iS0__)=Rsm(iS0__)-R(14)
            Rsm(iFeS2)=Rsm(iFeS2)+R(14)
            
            F(i,37)=F(i,37)-dz(i,k)*R(14)         !FeS
            F(i,38)=F(i,38)-dz(i,k)*R(14)         !S0  
            
            R(15)=K15*cff1*sm(i,k,iFeS_)*pw(i,k,iwH2S)*pw(i,k,iwSO4)
            Rsm(iFeS_)=Rsm(iFeS_)-R(15)*4.0d0
            Rpw(iwH2S)=Rpw(iwH2S)-R(15)*3.0d0
            Rpw(iwSO4)=Rpw(iwSO4)-R(15)
            Rsm(iFeS2)=Rsm(iFeS2)+R(15)*4.0d0
            
            F(i,39)=F(i,39)-dz(i,k)*R(15)*4.0d0   !FeS
            F(i,40)=F(i,40)-dz(i,k)*R(15)*3.0d0   !H2S
            F(i,41)=F(i,41)-dz(i,k)*R(15)         !SO4
!
!  S + O2 -> SO4
!
            R(16)=K16*poro(k)*pw(i,k,iwH2S)*pw(i,k,iwO2_)
            Rpw(iwH2S)=Rpw(iwH2S)-R(16)
            Rpw(iwO2_)=Rpw(iwO2_)-R(16)*2.0d0
            Rpw(iwSO4)=Rpw(iwSO4)+R(16)
            
            F(i,42)=F(i,42)-dz(i,k)*R(16)         !H2S
            F(i,43)=F(i,43)-dz(i,k)*R(16)*2.0d0   !O2

            R(17)=K17*cff1*sm(i,k,iFeS_)*pw(i,k,iwO2_)
            Rsm(iFeS_)=Rsm(iFeS_)-R(17)
            Rpw(iwO2_)=Rpw(iwO2_)-R(17)*2.0d0
            Rpw(iwFe_)=Rpw(iwFe_)+R(17)
            Rpw(iwSO4)=Rpw(iwSO4)+R(17)
            
            F(i,44)=F(i,44)-dz(i,k)*R(17)         !FeS
            F(i,45)=F(i,45)-dz(i,k)*R(17)*2.0d0   !O2  

            R(18)=K18*cff1*sm(i,k,iFeS2)*pw(i,k,iwO2_)
            Rsm(iFeS2)=Rsm(iFeS2)-R(18)
            Rpw(iwO2_)=Rpw(iwO2_)-R(18)*3.5d0
            Rpw(iwFe_)=Rpw(iwFe_)+R(18)
            Rpw(iwSO4)=Rpw(iwSO4)+R(18)*2.0d0
            
            F(i,46)=F(i,46)-dz(i,k)*R(18)         !FeS2
            F(i,47)=F(i,47)-dz(i,k)*R(18)*3.5d0   !O2
            
            R(19)=K19*cff1*sm(i,k,iS0__)
            IF (pw(i,k,iwH2S).lt.H2Sstop) THEN
              R(19)=R(19)*(1.0d0-pw(i,k,iwH2S)/H2Sstop)
            ELSE
              R(19)=0.0d0
            END IF
            Rsm(iS0__)=Rsm(iS0__)-R(19)*4.0d0
            Rpw(iwH2S)=Rpw(iwH2S)+R(19)*3.0d0
            Rpw(iwSO4)=Rpw(iwSO4)+R(19)
            
            F(i,48)=F(i,48)-dz(i,k)*R(19)*4.0d0   !S0 
!
!  A to B
!
            R(20)=K20*cff1*sm(i,k,iMnOA)
            Rsm(iMnOA)=Rsm(iMnOA)-R(20)
            Rsm(iMnOB)=Rsm(iMnOB)+R(20)
            
            F(i,49)=F(i,49)-dz(i,k)*R(20)         !MnOA
            
            R(21)=K21*cff1*sm(i,k,iFeOA)
            Rsm(iFeOA)=Rsm(iFeOA)-R(21)
            Rsm(iFeOB)=Rsm(iFeOB)+R(21)
            
            F(i,50)=F(i,50)-dz(i,k)*R(21)         !FeOA
!
!  - Re-oxydations using SO4
!
           !R(23)=K23*poro(k)*pw(i,k,iwCH4)*pw(i,k,iwSO4)         
!
!  Add reaction
!
            DO itrc=1,NBGCPW
              cff=dtBgc/(poro(k)+dens*(1.0d0-poro(k))*Kads(i,k,itrc))
              pw(i,k,itrc)=pw(i,k,itrc)+Rpw(itrc)*cff
            END DO

            DO itrc=1,NBGCSM
              cff=dtBgc/(dens*(1.0d0-poro(k)))
              sm(i,k,itrc)=sm(i,k,itrc)+Rsm(itrc)*cff
            END DO
!
!  check values
!
            DO itrc=1,NBGCPW
              IF (pw(i,k,itrc).lt.0.0d0) THEN
                pw(i,k,itrc)=0.0d0
              ELSE IF ( isnan(pw(i,k,itrc)) ) THEN
                print*,'pw',itrc,i,k
                stop
              END IF
            END DO
!
            DO itrc=1,NBGCSM
              IF (sm(i,k,itrc).lt.0.0d0) THEN
                sm(i,k,itrc)=0.0d0
              ELSE IF ( isnan(sm(i,k,itrc)) ) THEN
                print*,'sm',itrc,i,k
                stop
              END IF
            END DO
!
          END DO
        END DO BIO_LOOP
!
!  output
!
        include 'bgc_output.f'
!
      END DO ITER_LOOP
!
      CLOSE (10)
!
!-----------------------------------------------------------------------
!  write out global variables
!-----------------------------------------------------------------------
!
      DO i=Istr,Iend

        DO itrc=1,NBGCPW
          DO k=1,Nbed
            bgcpw(i,j,k,itrc)=pw(i,k,itrc)
          END DO
        END DO

        DO itrc=1,NBGCSM
          DO k=1,Nbed
            bgcsm(i,j,k,itrc)=sm(i,k,itrc)
          END DO
        END DO

      END DO
!
      DO i=Istr,Iend
        Bio_bottom(i,iOxyg)=MIN(Bio_bottom(i,iOxyg),0.0d0)+bw(i,iwO2_)
        Bio_bottom(i,iNH4_)=MIN(Bio_bottom(i,iNH4_),0.0d0)+bw(i,iwNH4)
        Bio_bottom(i,iNO3_)=MIN(Bio_bottom(i,iNO3_),0.0d0)+bw(i,iwNO3)
        Bio_bottom(i,iPO4_)=MIN(Bio_bottom(i,iPO4_),0.0d0)+bw(i,iwPO4)
        Bio_bottom(i,iH2S_)=MIN(Bio_bottom(i,iH2S_),0.0d0)+bw(i,iwH2S)
      END DO
!
!  bottom elution flux
!
!      DO i=Istr,Iend
!        DO itrc=1,NBGCF
!          bgcflux(i,j,itrc)=
!        END DO
!      END DO
!
!       RETURN
!       END SUBROUTINE bgc

      STOP
      CONTAINS

      SUBROUTINE Activity (Nbed, O2, act)
!
!***********************************************************************
!  Bioactivity for bioturbation and bioirrigation sediment fluxes.     !
!***********************************************************************
!
!       USE mod_kinds
!
      implicit none
!
!  Imported variable declarations.
!
      integer, intent(in) :: Nbed
!
! #ifdef ASSUMED_SHAPE
      real(8), intent(in) :: O2(:)
      real(8), intent(inout) :: act(:)
! #else
!       real(8), intent(in) :: O2(:)
!       real(8), intent(inout) :: act(:)
! #endif
!
!  Local variable declarations.
!
      integer :: k
      real(8),dimension(Nbed) :: v1
      real(8),dimension(Nbed) :: v2
!
      DO k=1,Nbed
        v1(k)=O2(k)
        v2(k)=0
        act(k)=act(k)+dtBgc*( v1(k)+v2(k) )
      END DO
!
      RETURN
      END SUBROUTINE Activity
!
      SUBROUTINE Thomas (IminS, ImaxS, Kmin, Kmax, NBGC, i, itrc,             &
     &                   a, b, c, d, U)
!
!***********************************************************************
!  Thomas algorithm for tridiagonal matrix equations.                  !
!***********************************************************************
!
!       USE mod_kinds
!
      implicit none
!
!  Imported variable declarations.
!
      integer,  intent(in) :: IminS, ImaxS, Kmin, Kmax, NBGC, i, itrc
!
! #ifdef ASSUMED_SHAPE
!      real(8), intent(in) :: a(Kmin:), b(Kmin:), c(Kmin:), d(Kmin:)
!      real(8), intent(inout) :: U(IminS:,Kmin:,:)
! #else
      real(8), intent(in) :: a(Kmin:Kmax), b(Kmin:Kmax), c(Kmin:Kmax), d(Kmin:Kmax)
      real(8), intent(inout) :: U(IminS:ImaxS,Kmin:Kmax,NBGC)
! #endif
!
!  Local variable declarations.
!
      integer :: k
      real(8) :: e(Kmin:Kmax), f(Kmin:Kmax)
!
!  Thomas algorizm
!
      e(Kmin)=c(Kmin)/b(Kmin)
      f(Kmin)=d(Kmin)/b(Kmin)
      DO k=Kmin+1,Kmax
        e(k)=c(k)/(b(k)-a(k)*e(k-1))
        f(k)=(d(k)+a(k)*f(k-1))/(b(k)-a(k)*e(k-1))
      END DO
!
      U(i,Kmax,itrc)=f(Kmax)
      DO k=Kmax-1,Kmin,-1
        U(i,k,itrc)=e(k)*U(i,k+1,itrc)+f(k)
      END DO
!
      RETURN
      END SUBROUTINE Thomas
!
      END PROGRAM bgc
