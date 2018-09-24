C******************************************************************************
C  MODULE ICE_DRIFT
C    Written by Brian Joyce,UND 3/9/2018
C    Modified from owiwind.F and owi_ice.F 
C
C    The values read in from the fort.226/fort.228 contain ice drift speeds
C     in the same format as fort.222 wind speeds
C****************************************************************************** 
C 
      MODULE ICE_DRIFT
C
      USE SIZES,ONLY : SZ,MYPROC
      USE GLOBAL,ONLY : NSCREEN, ScreenUnit
#ifdef CMPI
      USE MESSENGER,ONLY : MSG_FINI
#endif         
      IMPLICIT NONE
      character*100 :: title

!      tcm v48.4645 Changed the way the header line is read
!      character :: part1*12,part2*7,part3*6
      character :: owiheader*80

      real(SZ), dimension(:,:), allocatable :: u_iceR,u_iceB,v_iceR,v_iceB
      real(SZ), dimension(:), allocatable :: latR,longR,latB,longB
      real(SZ) :: Along, Alat
      real(SZ) :: ramp,rampfrac
      real(SZ) :: uu_ice,vv_ice
      real(SZ) :: ice_drift_env
      integer(8):: date1R,date2R,date1B,date2B
      integer(8):: date1ice_drift,date2ice_drift
      integer :: iLatR,iLongR,iCYMDHR,iMinR
      real(SZ)  :: dxR,dyR,swlatR,swlongR
      integer :: iLatB,iLongB,iCYMDHB,iMinB
      real(SZ)  :: dxB,dyB,swlatB,swlongB
      integer,save :: iLatc,iLongc,iCYMDHc,iMinc
      real(SZ)  :: dxc,dyc,swlatc,swlongc
      integer :: isnapR,updateR
      integer :: isnapB,updateB
	  integer :: errorIO   ! zero if the file opened successfully
      logical :: regionExists

      integer,allocatable :: swpointsR(:,:)
      integer,allocatable :: swpointsB(:,:)

      real(SZ) :: w,w1,w2,w3,w4
      real(SZ),allocatable :: wR(:,:)
      real(SZ),allocatable :: wB(:,:)

      CHARACTER FNAME*1024

      integer :: numSets,numBlankSnaps,cntSnaps,numSkipSnaps

      PUBLIC

C---------------------end of data declarations--------------------------------C

	  CONTAINS
	  
C***********************************************************************
C   SOBROUTINE ICE_DRIFT_INIT
C***********************************************************************

      SUBROUTINE ICE_DRIFT_INIT(IVNX,IVNY,NP)

      USE SIZES, ONLY : SZ,MYPROC, GBLINPUTDIR

      IMPLICIT NONE
      INTEGER   NP,I
      REAL(SZ), intent(out), dimension(:) ::  IVNX,IVNY

      allocate(swpointsB(NP,2),wB(NP,4))
      allocate(swpointsR(NP,2),wR(NP,4))
      
! Read meta info -------------------------------------------------

      OPEN(26,FILE=TRIM(GBLINPUTDIR)//'/'//'fort.26',STATUS='OLD',
     & ACTION='READ')

      ! Read the number of sets of .ice files.
      ! If numSets = 1 then ADCIRC requires UNIT 226
      ! If numSets = 2 then ADCIRC requires UNIT 226 and 228.
      !   UNIT 226 and 228 are ice concentration fields

      read(26,*,err=99999) numSets

      if(numSets.NE.1.AND.numSets.NE.2) then
        if (myproc == 0) write(screenunit,1004)
        write(16,1004)
#ifdef CMPI
        call msg_fini()
#endif
        stop
      endif

      ! Read the number of blank snaps to be inserted before OWI ice fields start
      read(26,*,err=99999) numBlankSnaps

      ! If numBlankSnaps < 0, ABS(numBlankSnaps) snaps in OWI ice files (UNIT226 and 228) will be skipped.
      if(numBlankSnaps.LT.0) then
        numSkipSnaps = ABS(numBlankSnaps)
        numBlankSnaps = 0

      ! v48.4628 TCM 10/28/2009 -- Added else to initialize numSkipSnaps to be 0 
      else
        numSkipSnaps = 0 
      endif

      close(26)

! Read basin ice file header ------------------------------------------------

      OPEN(226,FILE=TRIM(GBLINPUTDIR)//'/'//'fort.226', STATUS='OLD')

      ! Read begining/ending dates of ice file
!      tcm v48.4645 changed the way the header is read
!      read(226,11,err=99999,ADVANCE="NO")part1,part2,part3
! 11   FORMAT(a,1x,a,1x,a)
!      read(226,*)date1ice_drift,date2ice_drift
      owiheader(:) = ' '  !set owiheader to blanks before read
      read(226,fmt='(a80)',err=99999) owiheader
      read(owiheader(56:65),'(I10)') date1ice_drift
      read(owiheader(71:80),'(I10)') date2ice_drift

      if (myproc == 0)then
         write(screenunit,*)'date1 in basin ice file = ',date1ice_drift
         write(screenunit,*)'date2 in basin ice file = ',date2ice_drift
      endif

      date1B = date1ice_drift
      date2B = date2ice_drift

! Check if region scale data exist

      IF(numSets.eq.1) GOTO 100

! Read region ice file header -----------------------------------------------

      OPEN(228,FILE=TRIM(GBLINPUTDIR)//'/'//'fort.228',STATUS='OLD')

      ! Read begining/ending dates of ice file
!      tcm v48.4645 changed the way the header is read
!      read(228,'(a,1x,a,1x,a)',err=99999,ADVANCE="NO")part1,part2,part3
!      read(228,*)date1ice_drift,date2ice_drift
      owiheader(:) = ' '  !set owiheader to blanks before read
      read(228,fmt='(a80)',err=99999) owiheader
      read(owiheader(56:65),'(I10)') date1ice_drift
      read(owiheader(71:80),'(I10)') date2ice_drift

      if (myproc == 0)then
         write(screenunit,*)'date1 in region ice file = ',date1ice_drift
         write(screenunit,*)'date2 in region ice file = ',date2ice_drift
      endif

      date1R = date1ice_drift
      date2R = date2ice_drift

 100  CONTINUE


! Initialize flags ----------------------------------------------------------
      isnapB = 0
      isnapR = 0
      updateB = 1
      updateR = 1
      cntSnaps = 0

! Skip snaps if necessary ---------------------------------------------------
      do i = 1,numSkipSnaps
        CALL ICE_DRIFT_GET(IVNX,IVNY,NP)
      enddo

      RETURN


 10   format(a51,i12,a5,i12)
 1004 FORMAT(//,1X,' NUMBER OF SETS WAS SPECIFIED'//
     &       'INCORRECTLY IN UNIT26.'/
     &       ' IT MUST BE ETEHR OF 1 or 2'/
     &       ' EXECUTION WILL BE TERMINATED.'//)

99999 CONTINUE

#ifdef CMPI
      call msg_fini()
#endif
      STOP 'ICE_DRIFT READ ERROR (1)'

C-----------------------------------------------------------------------
      END SUBROUTINE ICE_DRIFT_INIT
C-----------------------------------------------------------------------

C***********************************************************************
C   SUBROUTINE ICE_DRIFT_GET
C***********************************************************************

      SUBROUTINE ICE_DRIFT_GET(IVNX,IVNY,NP)
      USE SIZES,ONLY : MYPROC,MNPROC

      IMPLICIT NONE

      INTEGER   NP,I,J,XI,YI,K,ibar
      REAL(SZ)  IVNY(*)
	  REAL(SZ)  IVNX(*)
      REAL(SZ)  CV(4),CVbar
      CHARACTER*80 PBLJAGF


! Read basin data  ---------------------------------------------------------

      ! Increment counter (cntSnaps initialized to zero in ncice1_init)
      cntSnaps = cntSnaps+1

      ! Put a blank snap for the first 'numBlankSnaps' snaps
      if(cntSnaps.LE.numBlankSnaps) then
        do i=1,NP
            IVNX(I)=0.d0
            IVNY(I)=0.d0
        enddo
        IF(NSCREEN.GE.1) THEN
          if(MYPROC.EQ.0) then
            WRITE(screenunit,16) cntSnaps
          endif
          WRITE(16,16) cntSnaps    !TCMv48.4629 (Changed format number from 15 to 16)
        ENDIF
        return
      endif


      ! Increment counter
      isnapB = isnapB+1

      ! Read grid specifications/date in ice drift file
      read (226,11,end=10000,err=99999) 
     &      iLatc,iLongc,dxc,dyc,swlatc,swlongc,iCYMDHc,iMinc


      ! Check if header info has changed from the previous snapshot
      if(isnapB.gt.1) then
        if(iLatc.ne.iLatB.or.iLongc.ne.iLongB.or.dxc.ne.dxB.or.
     &       dyc.ne.dyB.or.swlatc.ne.swlatB.or.
     &       swlongc.ne.swlongB) then
          updateB = 1
        else
          updateB = 0
        endif
      endif
      
      iCYMDHB = iCYMDHc
      iMinB = iMinc
      
      ! Update coordinate mapping coefficients if necessary
      if(updateB.eq.1) then
        call ice_drift1_interp_basin(np)
      endif
      

	 ! Read basin scale snapshot of u/v components of the icedrift
C      errorVar = "basin scale wind u-velocity snapshot"
      read(226,22,end=10000,err=9999,iostat=errorIO)
     &   ((u_iceB(i,j),i=1,iLongB),j=1,iLatB)
     
C      errorVar = "basin scale wind v-velocity snapshot"
      read(226,22,end=10000,err=9999,iostat=errorIO)
     &   ((v_iceB(i,j),i=1,iLongB),j=1,iLatB)
      

! Read region data  --------------------------------------------------------

      regionExists = .FALSE.

      IF(numSets.EQ.1) GOTO 100
      if(iCYMDHB.lt.date1R) goto 100
      if(iCYMDHB.eq.date2R.and.iMinR.ne.0) goto 100
      if(iCYMDHB.gt.date2R) goto 100

      regionExists = .TRUE.

      ! Increment counter
      isnapR = isnapR+1

      ! Read grid specifications/date in ice file
      read (228,11,end=10000,err=99999) 
     &     iLatc,iLongc,dxc,dyc,swlatc,swlongc,iCYMDHc,iMinc

      ! Check if header info has changed from the previous snapshot
      if(isnapR.gt.1) then
        if(iLatc.ne.iLatR.or.iLongc.ne.iLongR.or.dxc.ne.dxR.or. 
     &       dyc.ne.dyR.or.swlatc.ne.swlatR.or.
     &       swlongc.ne.swlongR) then
          updateR = 1
        else
          updateR = 0
        endif

      endif


      iCYMDHR = iCYMDHc
      iMinR = iMinc

      if(iCYMDHB.ne.iCYMDHR.or.iMinB.ne.iMinR) then
        if (myproc == 0) then
          WRITE(screenunit,*) 'SNAPSHOTS NOT SYNCRONIZED'
          WRITE(screenunit,*) '  iCYMDHB=',iCYMDHB, ' iMinB=',iMinB
          WRITE(screenunit,*) '  iCYMDHR=',iCYMDHR, ' iMinR=',iMinR
          WRITE(screenunit,*) 'EXECUTION WILL BE TERMINATED'
        endif
        WRITE(16,*) 'SNAPSHOTS NOT SYNCRONIZED'
        WRITE(16,*) '  iCYMDHB=',iCYMDHB, ' iMinB=',iMinB
        WRITE(16,*) '  iCYMDHR=',iCYMDHR, ' iMinR=',iMinR
        WRITE(16,*) 'EXECUTION WILL BE TERMINATED'
#ifdef CMPI
        call msg_fini()
#endif
        STOP
      endif

      ! Update coordinate mapping coefficients if necessary
      if(updateR.eq.1) then
        call ice_drift1_interp_region(np)
      endif

      ! Read ice concentrations
	 ! Read basin scale snapshot of u/v components of the icedrift
C      errorVar = "region scale ice u-velocity snapshot"
      read(228,22,end=10000,err=9999,iostat=errorIO)
     &   ((u_iceR(i,j),i=1,iLongR),j=1,iLatR)
      
C      errorVar = "region scale ice v-velocity snapshot"
      read(228,22,end=10000,err=9999,iostat=errorIO)
     &   ((v_iceR(i,j),i=1,iLongR),j=1,iLatR)
     


 100  CONTINUE

! Interpolate onto ADCIRC grid and write to file -------------------------
      
      rampfrac = isnapB-1
c      if (rampfrac<36) then
c        ramp = tanh(18d0*rampfrac/36d0)
c      end if
      ramp = 1.0

      IF(NSCREEN.GE.1) THEN
        if(regionExists.EQV..TRUE.) then
          if(MYPROC.EQ.0) then
            WRITE(screenunit,15) iCYMDHB,iMinB
          endif
          WRITE(16,15) iCYMDHB,iMinB
        else
          if(MYPROC.EQ.0) then
            WRITE(screenunit,14) iCYMDHB,iMinB
          endif
          WRITE(16,14) iCYMDHB,iMinB
        endif
      ENDIF

      do i=1,NP

C        Cice1=-9999.9D0

        ! BASIN ---------------------------------------------------------

        if (swpointsB(i,1).gt.0) then
        
          xi = swpointsB(i,1)
          yi = swpointsB(i,2)

          w1=wB(i,1)
          w2=wB(i,2)
          w3=wB(i,3)
          w4=wB(i,4)

          uu_ice=w1*u_iceB(xi,yi)+w2*u_iceB(xi+1,yi)+w3*
     &         u_iceB(xi+1,yi+1)+w4*u_iceB(xi,yi+1)
          vv_ice=w1*v_iceB(xi,yi)+w2*v_iceB(xi+1,yi)+w3*
     &         v_iceB(xi+1,yi+1)+w4*v_iceB(xi,yi+1)


        endif

        ! REGION ---------------------------------------------------------
        ! Cice1 will be overwritten if region data exist.

        if ((regionExists).and.(swpointsR(i,1).gt.0)) then
          xi = swpointsB(i,1)
          yi = swpointsB(i,2)

          w1=wR(i,1)
          w2=wR(i,2)
          w3=wR(i,3)
          w4=wR(i,4)

          uu_ice=w1*u_iceR(xi,yi)+w2*u_iceR(xi+1,yi)+w3*
     &         u_iceR(xi+1,yi+1)+w4*u_iceR(xi,yi+1)
          vv_ice=w1*v_iceR(xi,yi)+w2*v_iceR(xi+1,yi)+w3*
     &         v_iceR(xi+1,yi+1)+w4*v_iceR(xi,yi+1)


        endif

        ! COPY TO ARRAYS ----------------------------------------------------------
        if(uu_ice.eq.-9999.9D0) then
          
          IVNX(I)=0.d0
          IVNY(I)=0.d0
          
        else
          if (rampfrac<36) then
            uu_ice=uu_ice*ramp
            vv_ice=vv_ice*ramp
            
          endif

          IVNX(i) = uu_ice
          IVNY(i) = vv_ice

        end if

      enddo

      RETURN

9999  continue

      if (myproc == 0) then
      WRITE(screenunit,*)''
      WRITE(screenunit,*)' !!!!!FATAL ERROR!!!!!'
      WRITE(screenunit,*)'EITHER UNIT 226 or 228',
     & ' COULD NOT BE READ'
      WRITE(screenunit,*)' EXECUTION WILL BE TERMINATED'
      WRITE(screenunit,*)''
      endif

      WRITE(16,*) ''
      WRITE(16,*) ' !!!!!FATAL ERROR!!!!!'
      WRITE(16,*) ' EITHER UNIT 226 or 228 COULD NOT BE READ'
      WRITE(16,*) ' EXECUTION WILL BE TERMINATED'
      WRITE(16,*) ''
#ifdef CMPI
      call msg_fini()
#endif
      STOP

10000 continue

      IF(MYPROC.EQ.0) THEN
        WRITE(screenunit,*) ''
        WRITE(screenunit,*) ' !!! WARNING !!!'
        WRITE(screenunit,*) ' EITHER UNIT 226 or 228 RAN OUT'
        WRITE(screenunit,*) ' EXECUTION WILL CONTINUE'
        WRITE(screenunit,*) ''
      ENDIF

      WRITE(16,*) ''
      WRITE(16,*) ' !!! WARNING !!!'
      WRITE(16,*) ' EITHER UNIT 226 or 228 RAN OUT'
      WRITE(16,*) ' EXECUTION WILL CONTINUE'
      WRITE(16,*) ''

      do i=1,NP
        IVNX(I)=0.d0
        IVNY(I)=0.d0
      enddo

      RETURN


 11   format(t6,i4,t16,i4,t23,f6.0,t32,f6.0,
     &     t44,f8.0,t58,f8.0,t69,i10,i2)
 12   format(1X,'SNAPSHOT HEADER IN ICE FILES DO NOT MATCH')
 13   format(1X,'EXECUTION WILL BE TERMINATED')
 14   format(/,1X,'PROCESSING BASIN-SCALE ICE DATA',i12,' ',i2)
 15   format(/,1X,'PROCESSING BASIN&REGION-SCALE DATA',i12,' ',i2)
 16   format(/,1X,'INSERTING A BLANK ICE SNAP, COUNT=',i4)
 
 22   format(8f10.0)
      
99999 CONTINUE

#ifdef CMPI
      call msg_fini()
#endif
      STOP 'OWI_ICE READ ERROR (2)'

      END SUBROUTINE
	  
	  
	  
	  

C***********************************************************************
C   SUBROUTINE ICE_DRIFT1_INTERP_BASIN 
C
C   This generates and saves interpolation coefficients for mapping
C   from a basin-scale OWI to a ADCIRC grid.
C
C***********************************************************************

      SUBROUTINE ICE_DRIFT1_INTERP_BASIN(NP)
      USE GLOBAL,ONLY : SLAM,SFEA,RAD2DEG

      IMPLICIT NONE

      INTEGER   NP,I,J,K,XI,YI
      REAL(SZ)  adcLat,adcLong

      WRITE(16,*) ''
      WRITE(16,*) 'BASIN-SCALE ICE MAPPING UPDATED'
      WRITE(16,*) ''
        
      iLatB = iLatc
      iLongB = iLongc
      dxB = dxc
      dyB = dyc
      swlatB = swlatc
      swlongB = swlongc
        
      ! Allocate and create matrices
      if(allocated(u_iceB))    deallocate(u_iceB)
	  if(allocated(v_iceB))    deallocate(v_iceB)
      if(allocated(longB)) deallocate(longB)
      if(allocated(latB))  deallocate(latB)

      allocate(u_iceB(iLongB,iLatB))
	  allocate(v_iceB(iLongB,iLatB))
      allocate(longB(iLongB),latB(iLatB))
        
      ! Generate long&lat on each grid point
      do i=1,iLatB
        latB(i) = swlatB+(i-1)*dyB
      enddo
      do i=1,iLongB
        longB(i) = swlongB+(i-1)*dxB
      enddo
        
      ! Generate interpolation coefficients (south west point and weights)
      do i=1,NP
        adcLat  = RAD2DEG*SFEA(i)
        adcLong = RAD2DEG*SLAM(i)

        if (adcLong>=longB(1).and.adcLong<longB(iLongB) .and. 
     &       adcLat>=latB(1).and.adcLat<latB(iLatB)) then
          do j=1,iLongB-1
            if (adcLong>=longB(j) .and.  
     &           adcLong<longB(j+1)) then
              xi=j
              goto 200
            endif
          enddo
 200      continue
            
          do k=1,iLatB-1
            if (adcLat>=latB(k) .and. 
     &           adcLat<latB(k+1)) then
              yi=k
              goto 300
            endif
          enddo
 300      continue
            
          swpointsB(i,1) = xi
          swpointsB(i,2) = yi
            
          w=(longB(xi+1)-longB(xi))*(latB(yi+1)-latB(yi))
          w1=(longB(xi+1)-adcLong)*(latB(yi+1)-adcLat)
          w2=(adcLong-longB(xi))*(latB(yi+1)-adcLat)
          w3=(adcLong-longB(xi))*(adcLat-latB(yi))
          w4=(longB(xi+1)-adcLong)*(adcLat-latB(yi))
            
          wB(i,1)=w1/w
          wB(i,2)=w2/w
          wB(i,3)=w3/w
          wB(i,4)=w4/w

        else
          swpointsB(i,1) = 0
          swpointsB(i,2) = 0
            
        endif
      enddo
      END SUBROUTINE


C***********************************************************************
C   SUBROUTINE ICE_DRIFT1_INTERP_REGION
C
C   This generates and saves interpolation coefficients for mapping
C   from a region-scale OWI to a ADCIRC grid.
C
C***********************************************************************

      SUBROUTINE ICE_DRIFT1_INTERP_REGION(NP)
      USE GLOBAL,ONLY : SLAM,SFEA,RAD2DEG
      IMPLICIT NONE

      INTEGER   NP,I,J,K,XI,YI
      REAL(SZ)  adcLat,adcLong

      WRITE(16,*) ''
      WRITE(16,*) 'REGION-SCALE ICE MAPPING UPDATED'
      WRITE(16,*) ''
        
      iLatR = iLatc
      iLongR = iLongc
      dxR = dxc
      dyR = dyc
      swlatR = swlatc
      swlongR = swlongc
 
      ! Allocate and create matrices
      if(allocated(u_iceR))    deallocate(u_iceR)
	  if(allocated(v_iceR))    deallocate(v_iceR)
      if(allocated(longR)) deallocate(longR)
      if(allocated(latR))  deallocate(latR)

      allocate(longR(iLongR),latR(iLatR))

      ! Generate long&lat on each grid point
      do i=1,iLatR
        latR(i) = swlatR+(i-1)*dyR
      enddo
      do i=1,iLongR
        longR(i) = swlongR+(i-1)*dxR
      enddo

      ! Generate interpolation info (south west point and weights)
      do i=1,NP
        adcLat  = RAD2DEG*SFEA(i)
        adcLong = RAD2DEG*SLAM(i)

        if (adcLong>=longR(1).and.adcLong<longR(iLongR) .and. 
     &       adcLat>=latR(1).and.adcLat<latR(iLatR)) then
          do j=1,iLongR-1
            if (adcLong>=longR(j).and.adcLong<longR(j+1)) then
              xi=j
              goto 201
            endif
          enddo
 201      continue

          do k=1,iLatR-1
            if (adcLat>=latR(k).and.adcLat<latR(k+1)) then
              yi=k
              goto 301
            endif
          enddo
 301      continue

          swpointsR(i,1) = xi
          swpointsR(i,2) = yi

          w=(longR(xi+1)-longR(xi))*(latR(yi+1)-latR(yi))
          w1=(longR(xi+1)-adcLong)*(latR(yi+1)-adcLat)
          w2=(adcLong-longR(xi))*(latR(yi+1)-adcLat)
          w3=(adcLong-longR(xi))*(adcLat-latR(yi))
          w4=(longR(xi+1)-adcLong)*(adcLat-latR(yi))

          wR(i,1)=w1/w
          wR(i,2)=w2/w
          wR(i,3)=w3/w
          wR(i,4)=w4/w
        else
          swpointsR(i,1) = 0
          swpointsR(i,2) = 0
        endif
      enddo
      END SUBROUTINE

	  END MODULE ICE_DRIFT