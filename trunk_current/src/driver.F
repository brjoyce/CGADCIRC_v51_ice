C******************************************************************************
C******************************************************************************
C*    Non-ESMF ADCIRC DRIVER
C******************************************************************************
C******************************************************************************
      PROGRAM ADCIRC
C
      USE ADCIRC_Mod, ONLY : ADCIRC_Init, ADCIRC_Run, ADCIRC_Final
C
#ifdef CSWAN
Casey 090302: Include the following routines for coupling to unstructured SWAN.
      USE Couple2swan, ONLY: PADCSWAN_INIT, PADCSWAN_FINAL
      USE SIZES, ONLY: MNPROC,MYPROC
#endif
C
      IMPLICIT NONE
C
      CALL ADCIRC_Init
C      PRINT*, "Done............with ADCIRC_Init"  ;
C
#ifdef CSWAN
Casey 090302: Allow SWAN to initialize stuff before the start
C             of the time step loop.  This subroutine is inside
C             the 'couple2swan.F' src file.
Casey 110518: Added this IF statement.
C      PRINT*, "Enter PADCSWAN_INT.............." ;
      IF(MYPROC.LT.MNPROC)THEN
         CALL PADCSWAN_INIT
      ENDIF
C      PRINT*, "Done........... with PADCSWAN_INIT"  
#endif
C
C      PRINT*, "Enter........... ADCIRC_run" ;
      CALL ADCIRC_Run
C
#ifdef CSWAN
Casey 090302: Let SWAN clean up stuff.
Casey 110518: Added this IF statement.
      IF(MYPROC.LT.MNPROC)THEN
         CALL PADCSWAN_FINAL
      ENDIF
#endif
C
      CALL ADCIRC_Final
C
      STOP
      END PROGRAM ADCIRC
