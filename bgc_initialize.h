!
!=======================================================================
!  initialize conditions
!=======================================================================
!
!  bottom water tracers
!
      DO i=Istr,Iend
        Bio_bottom(i,iNO3_)=bwNO3(i)
        Bio_bottom(i,iNH4_)=bwNH4(i)
        Bio_bottom(i,iPO4_)=bwPO4(i)
        Bio_bottom(i,iH2S_)=bwH2S(i)

!       Bio_bottom(i,iOxyg)=bwO2(i)
!       Bio_bottom(i,iLDeC)=bwPOM(i)
!       Bio_bottom(i,iSDeC)=bwPOM(i)

      END DO
!
!  fluxes
!
      DO itrc=1,NBGCPW
        DO i=Istr,Iend
          bpwflux(i,j,itrc)=0.0d0
        END DO
      END DO
      DO itrc=1,NBGCSM
        DO i=Istr,Iend
          bsmflux(i,j,itrc)=0.0d0
        END DO
      END DO

#ifdef RST
!
!-----------------------------------------------------------------------
!  If you "make", bpw & bsm restart from "rst.csv".
!-----------------------------------------------------------------------
!
      DO i=Istr,Iend
# ifdef GREEN
        WRITE(filename,'("ini",i1,".csv")') i
        OPEN(10+i,file=filename,status='old')
# else
        WRITE(filename,'("rst",i1,".csv")') i
        OPEN(10+i,file=filename,status='old')
# endif
        READ(10+i,'()',end=99)
        DO k=1,Nbed
          READ(10+i,*) (bpw(i,j,k,itrc),itrc=1,NBGCPW),                 &
     &                 (bsm(i,j,k,itrc),itrc=1,NBGCSM)
        END DO
   99   CLOSE(10+i)
      END DO

#else
!
!-----------------------------------------------------------------------
!  If you "make first", bpw & bsm start from following.
!-----------------------------------------------------------------------
!
      DO k=1,Nbed
        DO i=Istr,Iend
!
!  pore water (mmol m-3 = nmol cm-3)
!
          bpw(i,j,k,iwO2_)=0.0d0
          bpw(i,j,k,iwNH4)=0.0d0
          bpw(i,j,k,iwNO3)=0.0d0
          bpw(i,j,k,iwPO4)=0.0d0
          bpw(i,j,k,iwH2S)=0.0d0
          bpw(i,j,k,iwSO4)=27300.0d0
          bpw(i,j,k,iwMn_)=0.0d0
          bpw(i,j,k,iwFe_)=0.0d0
          bpw(i,j,k,iwCH4)=0.0d0
!
!  sediment mud (nmol g-1)
!
          bsm(i,j,k,iPOMf)=0.0d0
          bsm(i,j,k,iPOMs)=0.0d0
          bsm(i,j,k,iPOMn)=0.0d0
          bsm(i,j,k,iMnOA)=0.0d0
          bsm(i,j,k,iMnOB)=0.0d0
          bsm(i,j,k,iFeOA)=0.0d0
          bsm(i,j,k,iFeOB)=0.0d0
          bsm(i,j,k,iFeOP)=0.0d0
          bsm(i,j,k,iS0__)=0.0d0
          bsm(i,j,k,iFeS_)=0.0d0
          bsm(i,j,k,iFeS2)=0.0d0
        END DO
      END DO
#endif
#ifdef GREEN
!
!  input parameters from new_param.tmp
!
      read(5,*) ndays
      read(5,*) KPOMf
      read(5,*) KPOMs
      read(5,*) KDOMf
      read(5,*) KDOMs
      read(5,*) K06
      read(5,*) ratio_n
      read(5,*) ratio_f
      read(5,*) ratio_CN
      read(5,*) ratio_CP
      read(5,*) ratio_DOMf
      read(5,*) D0DOMf
      read(5,*) aDOMf
      read(5,*) D0DOMs
      read(5,*) aDOMs
!      read(5,*) FMnO2
!      read(5,*) FFeOOH
#endif
