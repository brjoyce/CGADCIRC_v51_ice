PROGRAM CHECK_HOTSTFILE
  IMPLICIT NONE

  LOGICAL:: FOUND
  CHARACTER (LEN=80):: hotfilename

  
  INTEGER:: IHOT, IHOTSTP
  integer :: InputFileFmtVn

  INTEGER IMHSF,ITHSF
  REAL(8) TIMEHSF

  CALL get_command_argument(1, hotfilename) ;
  IF ( LEN_TRIM(hotfilename) == 0 ) THEN
    PRINT*, "Error: name of hot-start file is not supplied" ;
    STOP ;
  END IF

  INQUIRE(FILE=trim(hotfilename),EXIST=FOUND)    
  IF ( .NOT. FOUND ) THEN
     PRINT*, 'Error: File named " ',  trim(hotfilename), ' " is not found.' ;
     STOP ; 
  END IF

  IHOT = 20
  OPEN(IHOT, FILE = trim(hotfilename), ACCESS='DIRECT',RECL=8) ; 
  
  IHOTSTP = 1 ; 
  READ(IHOT,REC=IHOTSTP) InputFileFmtVn ; IHOTSTP = IHOTSTP + 1 ; 
  PRINT*, InputFileFmtVn ;

  ! PRINT*, InputFileFmtVn ;
  READ(IHOT,REC=IHOTSTP) IMHSF        ; IHOTSTP = IHOTSTP + 1
  READ(IHOT,REC=IHOTSTP) TIMEHSF      ; IHOTSTP = IHOTSTP + 1
  READ(IHOT,REC=IHOTSTP) ITHSF        ; IHOTSTP = IHOTSTP + 1
  PRINT*, "IM = ", IMHSF ; 
  PRINT*, "TIMES = ", TIMEHSF, ITHSF ; 

  CLOSE(IHOT) ; 

  STOP ;
END PROGRAM CHECK_HOTSTFILE
