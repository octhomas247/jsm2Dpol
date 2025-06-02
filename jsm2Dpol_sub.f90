SUBROUTINE ENSURE_QFILES(VVAC, POR, DEL0, AB)
    IMPLICIT NONE
    REAL(KIND=8), INTENT(OUT) :: VVAC, POR, DEL0, AB
    LOGICAL :: EXISTS_SI, EXISTS_AC, EXISTS_DARK
    CHARACTER(LEN=*), PARAMETER :: INPUTS_DIR = './Input/'
    CHARACTER(LEN=256) :: FILE_SI, FILE_AC, FILE_DARK
    LOGICAL :: MODELFOLDER, DARKFORSFOLDER

    FILE_SI   = INPUTS_DIR // 'd.QellipSi'
    FILE_AC   = INPUTS_DIR // 'd.QellipaC'
    FILE_DARK = INPUTS_DIR // 'd.QellipDark'
    
    ! Check if files already exist
    INQUIRE(FILE=FILE_SI,   EXIST=EXISTS_SI)
    INQUIRE(FILE=FILE_AC,   EXIST=EXISTS_AC)
    INQUIRE(FILE=FILE_DARK, EXIST=EXISTS_DARK)
    
    IF (.NOT. (EXISTS_SI .AND. EXISTS_AC .AND. EXISTS_DARK)) THEN
        PRINT *, 'Some Q files are missing — detecting folder and copying files.'
    
    
        CALL DETECT_MODEL_FOLDER(VVAC, POR, DEL0, AB, MODELFOLDER)
        IF (.NOT. MODELFOLDER) THEN
            CALL DETECT_DARKFORS_FOLDER(VVAC, POR, DEL0, AB, DARKFORSFOLDER)
        END IF
    
    ! PRINT *, 'VVAC, POR, DEL0, AB'
    ! WRITE(6,*), VVAC, POR, DEL0, AB
        IF (MODELFOLDER .OR. DARKFORSFOLDER) THEN
            CALL COPY_QFILES(VVAC, POR, DEL0, AB, MODELFOLDER)
        ELSE
            PRINT *, "❌ No valid folder structure detected. Aborting."
        END IF
    ELSE
        PRINT *, 'Q files ready — Continuing.'
    END IF
END SUBROUTINE ENSURE_QFILES

SUBROUTINE SET_DARK_DUST_VOLUMES(VVAC, VSI, VC)
    IMPLICIT NONE
    REAL(KIND=8), INTENT(IN)  :: VVAC
    REAL(KIND=8), INTENT(OUT) :: VSI, VC
    REAL(KIND=8), PARAMETER   :: EPS = 1.0D-6

    IF (ABS(VVAC - 0.05D0) < EPS) THEN
        VSI = 0.57D0
        VC  = 0.38D0
    ELSE IF (ABS(VVAC - 0.10D0) < EPS) THEN
        VSI = 0.53D0
        VC  = 0.37D0
    ELSE IF (ABS(VVAC - 0.20D0) < EPS) THEN
        VSI = 0.48D0
        VC  = 0.32D0
    ELSE
        PRINT *, '⚠️ Unknown VVAC value: ', VVAC
        PRINT *, '   Supported values: 0.05, 0.10, 0.20'
        VSI = -1.0D0
        VC  = -1.0D0
    END IF
END SUBROUTINE SET_DARK_DUST_VOLUMES

SUBROUTINE COPY_QFILES(VVAC, POR, DEL0, AB, MODELFOLDER)
    IMPLICIT NONE
    REAL(KIND=8), INTENT(IN) :: VVAC, POR, DEL0, AB
    LOGICAL, INTENT(IN)      :: MODELFOLDER

    CHARACTER(LEN=512) :: SRCDIR, DESTDIR, THISDIR
    CHARACTER(LEN=128) :: SRCNAME(3), DESTNAME(3)
    CHARACTER(LEN=512) :: SRCPATH, DESTPATH
    INTEGER :: I
    CHARACTER(LEN=4) :: VSTR, PSTR, DSTR
    CHARACTER(LEN=5) :: ABSTR
    CHARACTER(LEN=32) :: D_TAG
    LOGICAL :: STATUS

    CALL GETCWD(THISDIR)

    WRITE(VSTR, '(I2.2)') INT(VVAC * 100.0D0)
    WRITE(PSTR, '(I2.2)') INT(POR * 100.0D0)
    WRITE(ABSTR, '(F3.1)') AB

    ! Try to convert DEL0 to short int
    IF (DEL0 >= 0.0D0) THEN
        WRITE(DSTR, '(I2.2)') INT(DEL0)
        D_TAG = "d" // TRIM(DSTR)
    ELSE
        WRITE(D_TAG, '(F10.3)') DEL0
        D_TAG = TRIM(ADJUSTL(D_TAG))
        ! D_TAG = TRIM(ADJUSTL(CHAR(DEL0)))  ! fallback, if e.g. 'pdg'
    END IF

    ! Build source and destination filenames
    ! SRCNAME(1) = 'd.Dk' // VSTR // 'ab' // TRIM(ABSTR) // D_TAG
    ! SRCNAME(2) = 'd.Si' // PSTR // 'ab' // TRIM(ABSTR) // D_TAG
    ! SRCNAME(3) = 'd.aC' // PSTR // 'ab' // TRIM(ABSTR) // D_TAG
    SRCNAME(1) = 'd.Dk' // TRIM(VSTR) // 'ab' // TRIM(ABSTR) // TRIM(D_TAG)
    SRCNAME(2) = 'd.Si' // TRIM(PSTR)  // 'ab' // TRIM(ABSTR) // TRIM(D_TAG)
    SRCNAME(3) = 'd.aC' // TRIM(PSTR)  // 'ab' // TRIM(ABSTR) // TRIM(D_TAG)

    DESTNAME(1) = 'd.QellipDark'
    DESTNAME(2) = 'd.QellipSi'
    DESTNAME(3) = 'd.QellipaC'

    IF (MODELFOLDER) THEN
        SRCDIR = TRIM(THISDIR) // '/../Qfile_tv'
    ELSE
        SRCDIR = TRIM(THISDIR) // '/../../Qfile_tv'
    END IF

    DESTDIR = TRIM(THISDIR) // '/Input'

    ! Create destination folder if it doesn't exist
    INQUIRE(FILE=DESTDIR, EXIST=STATUS)
    IF (.NOT. STATUS) THEN
        CALL SYSTEM('mkdir -p ' // TRIM(DESTDIR))
    END IF

    ! Attempt copies
    DO I = 1, 3
        SRCPATH = TRIM(SRCDIR) // '/' // TRIM(SRCNAME(I))
        DESTPATH = TRIM(DESTDIR) // '/' // TRIM(DESTNAME(I))
        INQUIRE(FILE=SRCPATH, EXIST=STATUS)
        IF (STATUS) THEN
            CALL SYSTEM('cp ' // TRIM(SRCPATH) // ' ' // TRIM(DESTPATH))
            WRITE(*,'(A)') 'Copied ' // TRIM(SRCNAME(I)) // ' → ' // TRIM(DESTNAME(I))
        ELSE
            WRITE(*,'(A)') '⚠️  File not found: ' // TRIM(SRCPATH)
        END IF
    END DO

END SUBROUTINE COPY_QFILES


! SUBROUTINE DETECT_DARKFORS_FOLDER(VVAC, POR, DEL0, AB, STATUS)
!     IMPLICIT NONE
!     REAL(KIND=8), INTENT(OUT) :: VVAC, POR, DEL0, AB
!     LOGICAL, INTENT(OUT) :: STATUS
!     CHARACTER(LEN=512) :: FULLPATH, LAST, PARENT
!     CHARACTER(LEN=20) :: VVAC_STR, POR_STR, D_STR, AB_STR
!     INTEGER :: SLASH1, SLASH2, STAT, LEN_PATH, I, POS_D, POS_AB
!     INTEGER :: STATUS_CWD
!     LOGICAL :: HAS_PDG
!     STATUS = .FALSE.
!     VVAC = 0.0D0
!     POR  = 0.0D0
!     DEL0 = 0.0D0
!     AB   = 0.0D0
!     STATUS_CWD = 0

  
!     CALL GET_ENVIRONMENT_VARIABLE("PWD", FULLPATH, STATUS=STAT)
!     IF (STAT /= 0) THEN
!        PRINT *, "Could not get current path"
!        STATUS_CWD = 1
!        RETURN
!     END IF
  
!     ! Extract last two folders: .../porXXDarkForsRsv/VYYdZZ.abWW/
!     LEN_PATH = LEN_TRIM(FULLPATH)
  
!     SLASH1 = 0
!     SLASH2 = 0
!     DO I = LEN_PATH, 1, -1
!        IF (FULLPATH(I:I) == '/') THEN
!           IF (SLASH1 == 0) THEN
!              SLASH1 = I
!           ELSEIF (SLASH2 == 0) THEN
!              SLASH2 = I
!              EXIT
!           END IF
!        END IF
!     END DO
  
!     IF (SLASH1 == 0 .OR. SLASH2 == 0) THEN
!        STATUS_CWD = 2
!        PRINT *, "Failed to parse directory levels"
!        RETURN
!     END IF
  
!     LAST = FULLPATH(SLASH1+1:LEN_PATH)
!     PARENT = FULLPATH(SLASH2+1:SLASH1-1)
  
!     IF (INDEX(PARENT, "DarkForsRsv") == 0) THEN
!        STATUS_CWD = 3
!        PRINT *, "Not in DarkFors folder structure"
!        RETURN
!     END IF
  
!     ! Parse vvac from porXXDarkForsRsv → XX
!     VVAC_STR = PARENT(4:5)
!     READ(VVAC_STR, *, IOSTAT=STAT) VVAC
!     IF (STAT /= 0) THEN
!        PRINT *, "Failed to parse VVAC"
!        STATUS_CWD = 4
!        RETURN
!     END IF
!     VVAC = VVAC / 100.0D0
  
!     ! Parse POR, DEL0, AB from LAST
!     POR_STR = LAST(2:3)
!     READ(POR_STR, *, IOSTAT=STAT) POR
!     IF (STAT /= 0) THEN
!        PRINT *, "Failed to parse POR"
!        STATUS_CWD = 5
!        RETURN
!     END IF
!     POR = POR / 100.0D0
  
!     HAS_PDG = INDEX(LAST, "pdg") > 0
!     IF (HAS_PDG) THEN
!        DEL0 = -1.0D0  ! Convention: -1 means 'pdg'
!     ELSE
!        POS_D = INDEX(LAST, "d")
!        POS_AB = INDEX(LAST, "ab")
!        IF (POS_D == 0 .OR. POS_AB == 0) THEN
!           PRINT *, "Missing 'd' or 'ab' in directory name"
!           STATUS_CWD = 6
!           RETURN
!        END IF
!        D_STR = LAST(POS_D+1:POS_AB-1)
!        AB_STR = LAST(POS_AB+2:)
  
!        READ(D_STR, *, IOSTAT=STAT) DEL0
!        IF (STAT /= 0) THEN
!           PRINT *, "Failed to parse DEL0"
!           STATUS_CWD = 7
!           RETURN
!        END IF
  
!        READ(AB_STR, *, IOSTAT=STAT) AB
!        IF (STAT /= 0) THEN
!           PRINT *, "Failed to parse AB"
!           STATUS_CWD = 8
!           RETURN
!        END IF
!     END IF
  
!     ! Success
!     PRINT *, "DarkFors folder structure detected"
!     STATUS = .TRUE.
!     PRINT *, TRIM(FULLPATH)
!     PRINT *, "vvac = ", VVAC
!     PRINT *, "por  = ", POR
!     IF (HAS_PDG) THEN
!        PRINT *, "d    = 'pdg'"
!     ELSE
!        PRINT *, "d    = ", DEL0
!     END IF
!     PRINT *, "ab   = ", AB
!   END SUBROUTINE DETECT_DARKFORS_FOLDER

SUBROUTINE DETECT_DARKFORS_FOLDER(VVAC, POR, DEL0, AB, STATUS)
    IMPLICIT NONE
    REAL(KIND=8), INTENT(OUT) :: VVAC, POR, DEL0, AB
    LOGICAL, INTENT(OUT) :: STATUS
    CHARACTER(LEN=512) :: FULLPATH
    CHARACTER(LEN=256) :: LAST, PARENT, D_STR, AB_STR, POR_STR
    INTEGER :: POSD, POSAB, POSPDG, LAST_SLASH, STAT
    REAL(KIND=8) :: TMP
    CHARACTER(LEN=32) :: D_TAG

    VVAC = 0.0D0
    POR = 0.0D0
    DEL0 = 0.0D0
    AB   = 0.0D0
    STATUS = .FALSE.

    CALL GETCWD(FULLPATH)
    PRINT *, 'DarkFors folder structure detected'
    PRINT *, TRIM(FULLPATH)

    ! Extract LAST (e.g. V00d00.2ab1.5) and PARENT (e.g. por05DarkForsRsv)
    LAST_SLASH = INDEX(FULLPATH, '/', BACK=.TRUE.)
    LAST = FULLPATH(LAST_SLASH+1:)
    PARENT = FULLPATH(1:LAST_SLASH-1)
    LAST_SLASH = INDEX(PARENT, '/', BACK=.TRUE.)
    PARENT = PARENT(LAST_SLASH+1:)

    ! Extract VVAC from parent name (e.g. por05 → 0.05)
    READ(PARENT(4:5), *, IOSTAT=STAT) TMP
    IF (STAT /= 0) THEN
        PRINT *, 'Warning: Failed to parse VVAC from folder name.'
        STATUS = .TRUE.
        RETURN
    END IF
    VVAC = TMP / 100.0D0

    ! Extract POR from last folder name (characters 2-3 of 'V00...')
    READ(LAST(2:3), *, IOSTAT=STAT) TMP
    IF (STAT /= 0) THEN
        PRINT *, 'Warning: Failed to parse POR from folder name.'
        STATUS = .TRUE.
        RETURN
    END IF
    POR = TMP / 100.0D0

    ! Locate positions of 'pdg' or 'd' and 'ab'
    POSPDG = INDEX(LAST, 'pdg')
    POSD   = INDEX(LAST, 'd')      ! fallback if 'pdg' not found
    POSAB  = INDEX(LAST, 'ab')
    
    IF (POSAB == 0) THEN
        PRINT *, 'Warning: Missing "ab" in folder name.'
        STATUS = .TRUE.
        RETURN
    END IF
    
    IF (POSPDG > 0) THEN
        D_STR = LAST(POSPDG:POSAB-1)   ! e.g., "pdg60"
    ELSE IF (POSD > 0) THEN
        D_STR = LAST(POSD:POSAB-1)     ! e.g., "d00.2"
    ELSE
        PRINT *, 'Warning: Missing "d" or "pdg" in folder name.'
        STATUS = .TRUE.
        RETURN
    END IF
    
    AB_STR = LAST(POSAB+2:)           ! e.g., "1.5"
    ! D_TAG =  Tri(D_STR)
    ! Parse DEL0: either from numeric part or mark as 0.0 for pdg*
    ! D_TAG now contains something like 'pdg60' or 'd00.2'
    ! PRINT *, D_STR
    ! D_STR = TRIM(ADJUSTL(D_STR))
    ! D_STR = TRIM(D_STR)
    ! PRINT *, D_STR
    ! ! Check for special tag
    ! IF (INDEX(D_TAG, 'pdg') == 1) THEN
    !     D_STR = D_TAG(4:)  ! Skip 'pdg', keep the numeric part
    !     READ(D_STR, *, IOSTAT=STAT) TMP
    !     IF (STAT /= 0) THEN
    !         PRINT *, 'Warning: Failed to read DEL0 from pdg-tag.'
    !         STATUS = .TRUE.
    !         RETURN
    !     END IF
    !     DEL0 = TMP
    ! ELSE
    !     ! Fallback for standard dXX.Y format
    !     IF (D_TAG(1:1) == 'd') THEN
    !         D_STR = D_TAG(2:)  ! Remove only the 'd'
    !         READ(D_STR, *, IOSTAT=STAT) TMP
    !         IF (STAT /= 0) THEN
    !             PRINT *, 'Warning: Failed to read DEL0 from d-tag.'
    !             STATUS = .TRUE.
    !             RETURN
    !         END IF
    !         DEL0 = TMP
    !     ELSE
    !         PRINT *, 'Warning: D_TAG format not recognized:', D_TAG
    !         STATUS = .TRUE.
    !         RETURN
    !     END IF
    ! END IF
    IF (D_STR(1:3) == 'pdg') THEN
        READ(D_STR(4:), *, IOSTAT=STAT) TMP
        IF (STAT /= 0) THEN
            PRINT *, 'Warning: Failed to read DEL0 from pdg-tag.'
            STATUS = .TRUE.
            RETURN
        END IF
        DEL0 = TMP
    ELSE
        READ(D_STR(2:), *, IOSTAT=STAT) TMP
        IF (STAT /= 0) THEN
            PRINT *, 'Warning: Failed to read DEL0 from d-tag.'
            STATUS = .TRUE.
            RETURN
        END IF
        DEL0 = TMP
    END IF

    ! Parse AB
    READ(AB_STR, *, IOSTAT=STAT) TMP
    IF (STAT /= 0) THEN
        PRINT *, 'Warning: Failed to parse AB.'
        STATUS = .TRUE.
        RETURN
    END IF
    AB = TMP
END SUBROUTINE DETECT_DARKFORS_FOLDER

SUBROUTINE DETECT_MODEL_FOLDER(VVAC, POR, DEL0, AB, STATUS)
    IMPLICIT NONE
    REAL(KIND=8), INTENT(OUT) :: VVAC, POR, DEL0, AB
    LOGICAL, INTENT(OUT) :: STATUS
    CHARACTER(LEN=512) :: FULLPATH
    INTEGER :: STAT
  
    CALL GET_ENVIRONMENT_VARIABLE("PWD", FULLPATH, STATUS=STAT)
  
    IF (STAT /= 0) RETURN
  
    IF (INDEX(FULLPATH, 'Model24stars') > 0) THEN
        STATUS = .TRUE.
        VVAC = 0.10D0
        POR  = 0.10D0
        DEL0 = 10.0D0
        AB   = 2.5D0
        PRINT *, "Model folder detected"
        PRINT *, "Current path: ", TRIM(FULLPATH)
        RETURN
    END IF

    STATUS = .FALSE.
END SUBROUTINE DETECT_MODEL_FOLDER


SUBROUTINE READ_POR_OR_VVAC(testfile, por, vvac)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: testfile
    REAL(8), INTENT(OUT) :: por, vvac
  
    CHARACTER(LEN=256) :: line, porname, porval_str
    CHARACTER(LEN=256) :: parts(2)
    INTEGER :: i, ios
    INTEGER :: unit
  
    por = 0.0D0
    vvac = 0.0D0
  
    ! Open the file for reading
    INQUIRE(FILE=testfile, NUMBER=unit)
    IF (unit == 0) THEN
      unit = 10  ! Use fallback unit number
    END IF
    OPEN(unit, FILE=testfile, STATUS='OLD', ACTION='READ', IOSTAT=ios)
    IF (ios /= 0) THEN
      PRINT *, 'Error opening file: ', testfile
      RETURN
    END IF
  
    ! Skip first two lines
    READ(unit, '(A)', IOSTAT=ios)
    READ(unit, '(A)', IOSTAT=ios)
  
    ! Read the third line
    READ(unit, '(A)', IOSTAT=ios) line
    IF (ios /= 0) THEN
      PRINT *, 'Error reading third line'
      CLOSE(unit)
      RETURN
    END IF
  
    ! Close the file
    CLOSE(unit)
  
    ! Split line on '=' manually
    CALL SPLIT_ON_EQUAL(TRIM(line), parts)
  
    porname = ADJUSTL(parts(1))
    porval_str = ADJUSTL(parts(2))
  
    CALL REMOVE_CHARS(porname, '* ')
    CALL REMOVE_CHARS(porval_str, '* ')
  
    READ(porval_str, *, IOSTAT=ios) por
    IF (ios /= 0) THEN
      PRINT *, 'Error parsing numeric value:', porval_str
      RETURN
    END IF
  
    IF (porname == 'por') THEN
      ! Value is already in por
    ELSEIF (porname == 'vvac') THEN
      vvac = por
      por = 0.0D0
    ELSE
      por = 0.0D0
      vvac = 0.0D0
      PRINT *, 'Unknown variable name in file:', porname
    END IF
  
END SUBROUTINE READ_POR_OR_VVAC

SUBROUTINE SPLIT_ON_EQUAL(input_line, parts)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: input_line
    CHARACTER(LEN=256), INTENT(OUT) :: parts(2)
    INTEGER :: idx
  
    idx = INDEX(input_line, '=')
    IF (idx > 0) THEN
      parts(1) = input_line(1:idx-1)
      parts(2) = input_line(idx+1:)
    ELSE
      parts(1) = input_line
      parts(2) = ''
    END IF
END SUBROUTINE SPLIT_ON_EQUAL
  
SUBROUTINE REMOVE_CHARS(str, chars)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(INOUT) :: str
    CHARACTER(LEN=*), INTENT(IN) :: chars
    CHARACTER(LEN=LEN(str)) :: result
    INTEGER :: i, j, k, found
  
    j = LEN_TRIM(str)
    k = 0
    DO i = 1, j
      found = INDEX(chars, str(i:i))
      IF (found == 0) THEN
        k = k + 1
        result(k:k) = str(i:i)
      END IF
    END DO
    str = result
END SUBROUTINE REMOVE_CHARS

SUBROUTINE EXTRACT_POR_FROM_PATH(por)
    IMPLICIT NONE
    REAL(8), INTENT(OUT) :: por
    CHARACTER(LEN=256) :: path
    CHARACTER(LEN=2) :: subdir
    INTEGER :: i, len_path, start_idx
    INTEGER :: ios
  
    CALL GETCWD(path)  ! Get current working directory
  
    ! Find the position of the last '/'
    len_path = LEN_TRIM(path)
    DO i = len_path, 1, -1
      IF (path(i:i) == '/') EXIT
    END DO
  
    start_idx = i + 2  ! Second and third characters of final directory
  
    IF (start_idx <= len_path - 1) THEN
      subdir = path(start_idx:start_idx+1)
      READ(subdir, *, IOSTAT=ios) por
      IF (ios == 0) THEN
        por = por * 0.01
      ELSE
        por = 0.0D0
        PRINT *, 'Warning: Failed to read number from path segment.'
      END IF
    ELSE
      por = 0.0D0
      PRINT *, 'Warning: Path too short or in unexpected format.'
    END IF
  END SUBROUTINE EXTRACT_POR_FROM_PATH

SUBROUTINE pah_wq
    USE constants_mod
    USE tempmrn_mod
    USE effi_mod
    USE vsg_mod
    USE vec_lr_mod
    USE vec_nf_mod
    USE vec_sig_mod
    USE fest_mod
    USE paheva_mod
    USE const_mod
    USE paraDark_mod
    USE abu_mod
    USE rdark_mod
    IMPLICIT NONE
    REAL(KIND=8), DIMENSION(NRES) :: S_RES
    ! c
    ! c   Berechnet PAH cross sections
    ! INCLUDE "jsm2Dpol.com"
    CHARACTER(80) :: CDUM
        
        
    ! c Achtung U band und wel_res bei 0.097 und 0.0772 nicht in neumen wel
    ! c    1    2    3    4    5    6    7    8    9   10   11  12  13  14  15  16  17
    
    ! c 23.1 21.1 18.2 16.4 15.7 15.1 14.3 13.6 12.7 12.0 11.3 8.6 7.7 7.0 6.2 5.2 3.3
    ! c
    ! c ------------ ISM 
    REAL(8), DIMENSION(NRES) :: WEL_RES = [ &
    23.09D-4, 21.09D-4, 18.19D-4, 16.5D-4, &
    15.7D-4, 15.09D-4, 14.29D-4, 13.59D-4, &
    12.7D-4, 11.95D-4, 11.28D-4, 8.59D-4, 7.7D-4, &
    7.0D-4, 6.2D-4, 5.1D-4, 3.3D-4, 2.175D-5, &
    0.97D-5, 0.772D-5 ]

! c ISM settings:  A&A 561, A82 (2014)
    REAL(8), DIMENSION(NRES) :: GAM_RES = [ &
    10.0D12, 10.D12, 10.D12, 10.D12, &
    5.D12, 4.D12, 5.D12, 4.D12, &
    5.D12, 7.D12, 6.D12, 6.D12, 22.D12, &
    5.D12, 14.D12, 20.D12, 20.D12, 1.8D15, &
    3.9D15, 5.D15 ]
    
        
    REAL(8), DIMENSION(NRES) :: AREA_RES = [ &
    2.0D-26, 2.0D-26, 3.0D-26, 5.D-26, &
    0.3D-26, 0.3D-26, 0.9D-26, 3.7D-26, &
    12.D-25, 6.D-25, 30.D-25, 2.D-25, 3.5D-25, &
    5.D-26, 1.D-25, 9.D-26, 2.D-25, 4.D-23, &
    1.D-99, 3.5D-23 ]
    ! c
    ! c org:      data area_res / 2.0d-26, 2.0d-25, 3.0d-25, 5d-26, 
    ! c     $                0.3d-26, 0.3d-26, 0.9d-26, 3.7d-26, 
    ! c     $                15.d-25, 6.d-25, 25d-25, 2d-25, 3.5d-25,
    ! c     $                5.d-26, 1.d-25, 2.7d-26, 2d-25, 4d-23, 
    ! c     $                1d-99, 3.5d-23 /
    ! c
    ! c ---------------------------------------------------------------
    ! c original parameters for N1808 as in AA377, 735 2001
    ! c      data gam_res /  10.0D12,  10D12,  10d12,  3.d12,  
    ! c     $                2.0d12,  3d12,  5d12,  4d12, 
    ! c     $                3.5d12,  7d12,  4d12,  6d12,  22d12, 
    ! c     $                5.9d12, 14d12, 20d12, 20d12, 1.8d15 / 
    ! c
    ! c      data area_res / 2.0d-26, 2.0d-26, 1.0d-26, 0.5d-26, 
    ! c     $                0.3d-26, 0.3d-26, 0.9d-26, 3.7d-25, 
    ! c     $                2.8d-25, 12d-25, 3.6d-25,  3.5d-25, 5.5d-25, 
    ! c     $                1.25d-25, 21d-25, 1.1d-26, 1.2d-25,4d-23, 
    ! c     $                1d-99, 3.5d-23 /
    ! c
    ! c ---------------------------------------------------------------

! c ---------------------------------------------------------------
! c

    REAL(KIND=8) :: WEL2170, WELCUT, C_PAH_UV
    REAL(KIND=8) :: X2170, GAM2170
    INTEGER :: KRES, I, J, K, KB, KUV
    ! External function declaration
    ! INTERFACE
    !   FUNCTION BPL(FR, FR3, T) RESULT(BPL_VAL)
    !     IMPLICIT NONE
    !     REAL(KIND=8), INTENT(IN) :: FR, FR3, T
    !     REAL(KIND=8) :: BPL_VAL
    !   END FUNCTION BPL
    ! END INTERFACE

    ! INTERFACE
    !     SUBROUTINE LOCAT(XY, N, X, J)
    !         REAL(KIND=8), DIMENSION(N), INTENT(IN) :: XY
    !         INTEGER, INTENT(IN) :: N
    !         REAL(KIND=8), INTENT(IN) :: X
    !         INTEGER, INTENT(OUT) :: J
    !     END SUBROUTINE LOCAT
    ! END INTERFACE

    IEVAP    = 0
    SIGA_PAHA = 0.0
    QABS = 0
    EMIS = 0

    ! Loop over the number of resolutions
    DO I = 1, NRES
        CALL LOCAT(WEL, MM, WEL_RES(I), KRES)
        IR_RES(I) = KRES
    END DO

      ! Reset PAH parameters using observed Drude Profile at 2170AA bump in notation of Gordon et al (2009)
      ! Converting notation by Endrik using fr(k), gam_e
      ! gam_e = gam2170 * sqrt(4.) * pi * 1d4 * clicht
      ! Standard values to use in PAH2170.wq : 4.6082949  0.95559058

    ! Open and read PAH2170.wq file
    OPEN(unit=24, FILE='./Input/PAH2170.wq', FORM='FORMATTED')
    REWIND(24)
    READ(24,*) cdum
    READ(24,*) x2170, gam2170
    CLOSE(24)

    wel2170 = 1.D-4 / x2170
    CALL locat(wel, mm, wel2170, kres)
    ir_res(18) = kres

    IF (ibug .GE. 2) THEN
        ! Output information if ibug >= 2
        PRINT *, '  Change 2175AA PAH bump using ./Input/PAH2170.wq'
        PRINT *, '  wel(ir_res(18)), x2170, gam2170,  gam_res(18) '
        WRITE(6, '(1X, 0P1F9.4, 2F9.2, 1P1E11.3, A35)') &
            wel(ir_res(18)) * 1.D4, 1.D-4 / wel(ir_res(18)), gam_res(18) / PI / CLICHT * 1.D-4, &
            gam_res(18), ' old : as in data statement'
        WRITE(6, '(1X, 0P1F9.4, 2F9.2, 1P1E11.3, A35)') &
            wel2170 * 1.D4, x2170, gam2170, &
            gam2170 * SQRT(4.D0) * PI * CLICHT * 1.D4, ' used: as in ./Input/PAH2170.wq'
    END IF

    gam_res(18) = gam2170 * SQRT(4.D0) * PI * CLICHT * 1.D4


! c
! c ---------------------------------------------------------------------------
! c  The following PAH bands depend on No of H atoms

    s_res( 8) = zhpah *gam_res( 8)*clicht*area_res( 8)/wel(ir_res( 8))**2
    s_res( 9) = zhpah *gam_res( 9)*clicht*area_res( 9)/wel(ir_res( 9))**2
    s_res(10) = zhpah *gam_res(10)*clicht*area_res(10)/wel(ir_res(10))**2
    s_res(11) = zhpah *gam_res(11)*clicht*area_res(11)/wel(ir_res(11))**2
    s_res(12) = zhpah *gam_res(12)*clicht*area_res(12)/wel(ir_res(12))**2
    s_res(14) = zhpah *gam_res(14)*clicht*area_res(14)/wel(ir_res(14))**2
    s_res(17) = zhpah *gam_res(17)*clicht*area_res(17)/wel(ir_res(17))**2

! c  The following PAH bands depend on No of C atoms

    s_res( 1) = zcpah *gam_res( 1)*clicht*area_res( 1)/wel(ir_res( 1))**2
    s_res( 2) = zcpah *gam_res( 2)*clicht*area_res( 2)/wel(ir_res( 2))**2
    s_res( 3) = zcpah *gam_res( 3)*clicht*area_res( 3)/wel(ir_res( 3))**2
    s_res( 4) = zcpah *gam_res( 4)*clicht*area_res( 4)/wel(ir_res( 4))**2
    s_res( 5) = zcpah *gam_res( 5)*clicht*area_res( 5)/wel(ir_res( 5))**2
    s_res( 6) = zcpah *gam_res( 6)*clicht*area_res( 6)/wel(ir_res( 6))**2
    s_res( 7) = zcpah *gam_res( 7)*clicht*area_res( 7)/wel(ir_res( 7))**2
    s_res(13) = zcpah *gam_res(13)*clicht*area_res(13)/wel(ir_res(13))**2
    s_res(15) = zcpah *gam_res(15)*clicht*area_res(15)/wel(ir_res(15))**2
    s_res(16) = zcpah *gam_res(16)*clicht*area_res(16)/wel(ir_res(16))**2
    s_res(18) = zcpah *gam_res(18)*clicht*area_res(18)/wel(ir_res(18))**2
    s_res(19) = zcpah *gam_res(19)*clicht*area_res(19)/wel(ir_res(19))**2
    s_res(20) = zcpah *gam_res(19)*clicht*area_res(20)/wel(ir_res(20))**2


! c      welcut = (1630. + 450. * 1.29 * sqrt(0.4*zcpah) ) * 1d-8
      ! New welcut following: Salama, Bakes Alamandola, Tielens, 1996, ApJ458, 621
    WELCUT = 1.0D0 / (3.804 / SQRT(0.4 * ZCPAH) + 1.0D0) * 1.D-4
    IF (WELCUT .LE. 0.55D-4) THEN
        WELCUT = 0.55D-4
    END IF

    ! Abs efficiencies [Schu93]: IR lines + IR continuum + UV
    ! UV Querschnit ist: i) zunaechst konst*Nc, dann 
    ! ii) ab 912AA ~ zu kleinen aC Koerner und iii) ab 100eV: q=1
    C_PAH_UV = 3.D-18  ! (SKB14)

    CALL LOCAT(WEL, MM, 0.44D-4, KB)
    CALL LOCAT(WEL, MM, 0.167D-4, KUV)

    DO K = 1, MM
        ! IR features without 2200 AA bump
        DO J = 1, NRES - 2
            QABS(K) = QABS(K) + S_RES(J) * FR(K)**2 / (PI**2 * (FR(K)**2 - FR(IR_RES(J))**2)**2 + FR(K)**2 * GAM_RES(J)**2 / 4.D0)
        END DO

        IF (WEL(K) .GE. WELCUT) THEN
            ! NIR of ionized PAH (Mattioda et al.2005, apj629,1183; gion=0.5)
            QABS(K) = QABS(K) + ZCPAH * 3.5 / 2.0D0 * 10.0D0**(-19.0D0 - 1.45D0 * WEL(K) * 1.D4)
        ELSE
            DO J = NRES - 2, NRES
                QABS(K) = QABS(K) + S_RES(J) * FR(K)**2 / (PI**2 * (FR(K)**2 - FR(IR_RES(J))**2)**2 + FR(K)**2 * GAM_RES(J)**2 / 4.D0)
            END DO
        END IF
    END DO

! c
       
! c   UV and X-rays:  qpah ~ qac(1,k) E < 100eV
! c        if(wel(k).lt. wel(kuv)) 
! c     $    qabs(k)= qabs(kuv) * qagr(1,k)/qagr(1,kuv)
! c
! c        if(wel(k).le. 20.d-4) write(65,'(1p2e10.2)') wel(k), qabs(k)

! c
! c no PAH continuum:       
! c        qabs(k) = qabs(k) + qcontpah(wel(k),zcpah)

72  CONTINUE

    ! B-V range constant cross section:
    ! DO K = 1, MM
    !     IF (WEL(K) .LE. 0.55D-4 .AND. WEL(K) .GE. 0.436D-4) QABS(K) = QABS(KB)
    ! END DO
  
    IF (IBUG .GE. 2) THEN
        WRITE(26,*) 'WEL (CM), 1/WEL (MIC), QPAH(K) /C-ATOM'
        REWIND(26)
        DO K = 1, MM
            IF (1.D-4 / WEL(K) .GE. 1) THEN
                WRITE(26, '(1P4E11.2)') WEL(K), 1.D-4 / WEL(K), HWIRK * FR(K) / EVOLT, QABS(K) / ZCPAH
            END IF
        END DO
    END IF
  
    RETURN
END SUBROUTINE pah_wq

! c
! c --------------------------------------------------------
! c
SUBROUTINE PAH_EMIS

    ! c   Berechnet Emission genau eines PAH.  Spektrum auf 'emis(mm)' geschrieben.
    USE constants_mod
    USE tempmrn_mod
    USE effi_mod
    USE vsg_mod
    USE vec_lr_mod
    USE vec_nf_mod
    USE vec_sig_mod
    USE fest_mod
    USE paheva_mod
    USE const_mod
    USE paraDark_mod
    USE abu_mod
    USE rdark_mod
    USE functions_mod
    ! INCLUDE "JSM2DPOL.COM"
    ! DIMENSION XX(NNN)
    ! REAL(KIND=8), DIMENSION(nnn) :: XX
    ! REAL(KIND=8) :: SUM, BPL, TOTEMIS, TFAK1, FAK
    REAL(KIND=8) :: SUM, TOTEMIS, TFAK1, TFAK, FAK
    INTEGER :: JMAXPW
    
      IEvap     = 0
      Totabs    = 0.0
    
    ! c total absorbed energy per second
    DO K = 1, MM
        Emis(K)  = 0.0
        Totabs   = Totabs + PI4 * PI * ARAD**2 * QABS(K) * J_V(K) * DFR(K)
    END DO
    
    ! c     Verdampfung by hard (>5eV) photons
    ! c      CALL PAHEVAP
    
    ! c      IF (IEvap .EQ. 1) GOTO 900
    ! c     PAH temp fluctuation
    TEvap       = 2.5D3
    TFAK1       = 1D-12
    TEM(1)      = 3.0D0
    TEM(NNTVSG) = 6.0D3

    JMAXPW = 100
    DO 40 ITERPW = 1, JMAXPW

    DO 100 I = 2, NNTVSG
        TEM(I) = TEM(1) + (I-1) * (TEM(NNTVSG) - TEM(1)) / FLOAT(NNTVSG-1)
    100 CONTINUE
    
    ! c  Setzen der Enthalpie - Intervalle
    
    CALL ENTH
    CALL TRANSMAT
    CALL PW_ZUSTAND(IMIN, IMAX, IPWMAX, ITERPW)
    

! c ----------------------------------------------------------------------------
! c   Berechnung der Emission pro Teilchen in [erg/s/ster/Hz]

    TOTEMIS  = 0.0
    DO K = 1, MM
        SUM      = 0.0
  
        DO I = 1, NNTVSG
            SUM = SUM + PW(I) * BPL(FR(K), FR3(K), TEM(I))
        END DO
  
        EMIS(K) = PI * ARAD**2 * QABS(K) * SUM
        TOTEMIS = TOTEMIS + PI4 * EMIS(K) * DFR(K)
    END DO
  
    RATIO = TOTABS / TOTEMIS
    FAK = ABS(1.0D0 - ABS(RATIO))
  
  ! c ----------------------------------------------------------------------------
  ! c  Eventuell neue Einteilung des Temperaturgitters

    IF (totabs .EQ. 0.0) THEN
        GOTO 4
    END IF

    IF (iterpw .EQ. jmaxpw) THEN
        GOTO 4
    END IF

    iratio = 0
    IF (iterpw .GT. 5 .AND. ratio .GE. 0.95 .AND. ratio .LE. 1.05) THEN
        iratio = 1
    END IF

    IF (iterpw .GT. 9 .AND. ratio .GE. 0.9 .AND. ratio .LE. 1.1) THEN
        iratio = 1
    END IF

    IF (iratio .EQ. 1 .AND. pw(nnTvsg) .LT. 1D-6 .AND. pw(1) .LT. 1D-6) THEN
        GOTO 4
    END IF

    IF (iratio .EQ. 1 .AND. pw(nnTvsg) .LT. 1D-6 .AND. pw(1) .GE. 1D-6 .AND. tem(1) .LT. 3.1) THEN
        GOTO 4
    END IF

    Tfak = 1.3
    IF (imin .EQ. 1) THEN
        tem(1) = MAX(3.0D0, tem(1) / Tfak)
    END IF

    IF (imin .GT. 1) THEN
        tem(1) = tem(imin)
    END IF

    IF (imax .LT. nnTvsg) THEN
        tem(nnTvsg) = tem(imax)
    END IF

    IF (imax .EQ. nnTvsg) THEN
        IF (pw(imax) .LE. 1D-12) THEN
            Tfak = 2.0D0
        END IF
        tem(nnTvsg) = tem(nnTvsg) * Tfak
    END IF

    IF (ibug .GE. 3) THEN
        PRINT *, iterpw, ipwmax, nnTvsg, imin, imax, &
               tem(1), tem(ipwmax), tem(nnTvsg), tem(imin), tem(imax), &
               pw(1), pw(ipwmax), pw(nnTvsg), pw(imin), pw(imax)
    END IF

40   CONTINUE
4    CONTINUE


    IF (ibug .GE. 2) THEN
        PRINT 265, iterpw, ipwmax, nnTvsg, imin, imax, &
            tem(1), tem(ipwmax), tem(nnTvsg), tem(imin), tem(imax), &
            pw(1), pw(ipwmax), pw(nnTvsg), pw(imin), pw(imax)

        WRITE(6,936) iterpw, ratio, totabs/eVolt, sumevap
    END IF

    WRITE(6,936) iterpw, ratio, totabs/eVolt, sumevap
    fak = ABS(1D0 - ABS(ratio))

    IF (fak .GE. 0.5) THEN
        PRINT *, ' *** Check energy balance for PAH *** '
    END IF

    IF (sumevap .GT. fevap) THEN
        ievap = 1
    END IF

    936   FORMAT(' PAH: Iter=',I3,' abs/emis = ',1PE8.2, &
        ' Energy_abs (eV) =', E8.2, ' sumevap = ', E8.2)

    265   FORMAT(I4, 4I10 / ' T  =', 0P5F10.1 / , ' pw =', 1P5E10.2)

! c ----------------------------------------------------------------------------
    900 CONTINUE

    IF (ievap .EQ. 1) THEN
        DO k = 1, mm
            emis(k) = 0.
        END DO
        WRITE(6, '(A30, 1P2E9.1)') ' PAH Verdampfung: arad, sumevap = ', arad, sumevap
    END IF
    
    RETURN
END SUBROUTINE PAH_EMIS

! c  *********************************************************************

FUNCTION QCONTPAH(WEL, ZCPAH) RESULT(QCONTPAH_VAL)

    ! c   Absorption coefficient for emission per C-atom of PAH. A smoothed
    ! c   version of Eq(16) of Schutte et al. ApJ 415, 397 (1993)
    USE functions_mod
    USE vsg_mod
    IMPLICIT NONE
    
    ! Arguments
    REAL(KIND=8), INTENT(IN) :: WEL, ZCPAH
    
    ! Return value
    REAL(KIND=8) :: QCONTPAH_VAL
    
    ! Local variables
    REAL(KIND=8) :: CLICHT, FRMAX, FRMAX3, WMAX, QNORM
    REAL(KIND=8) :: FREQ, FREQ3, QCONTPAH_TEMP
    
    ! Constants
    REAL(KIND=8), PARAMETER :: A = 25.D-23
    REAL(KIND=8), PARAMETER :: TCONT = 500.D0
    
    ! External function declaration
    ! INTERFACE
    !   FUNCTION BPL(FR, FR3, T) RESULT(BPL_VAL)
    !     IMPLICIT NONE
    !     REAL(KIND=8), INTENT(IN) :: FR, FR3, T
    !     REAL(KIND=8) :: BPL_VAL
    !   END FUNCTION BPL
    ! END INTERFACE
    
    ! Speed of light [cm/s]
    CLICHT = 2.997925D10
    
    ! Maximum frequency and corresponding parameters
    FRMAX  = TCONT / 1.7D-11
    FRMAX3 = FRMAX ** 3
    WMAX   = CLICHT / FRMAX * 1D4
    
    ! Normalization using Planck function
    QNORM = BPL(FRMAX, FRMAX3, TCONT)
    
    ! Frequency for current wavelength
    FREQ  = CLICHT / WEL
    FREQ3 = FREQ ** 3
    
    ! Compute QCONTPAH
    QCONTPAH_TEMP = BPL(FREQ, FREQ3, TCONT)
    
    QCONTPAH_VAL = ZCPAH * A * QCONTPAH_TEMP / QNORM
    
    ! Optional debugging output (commented)
    ! IF (WEL < WMAX / 1D4) THEN
    !   PRINT*, 'qcont at wmax= ', WMAX, BPL(FRMAX, FRMAX3, TCONT), QNORM
    !   PRINT*, 'qcont at wel= ', WEL, QCONTPAH_VAL, BPL(FRMAX, FRMAX3, TCONT) / QNORM
    !   STOP
    ! END IF
    
    ! c      if(wel .ge. welmax) then
    ! c      qcontpah = a / (1d4 * wel)**1.2
    ! c      else 
    ! c      qcontpah = 0
    ! c      end if
    ! c      write(6,*) wel, qcontpah
    ! c      write(16, '(1p2e10.2)') wel, qcontpah

    RETURN
    
END FUNCTION QCONTPAH

! c
! c --------------------
! c
SUBROUTINE vsg

    ! c   Berechnet Emission emis(k) genau eines kleinen Staubkorns [erg/s/ster/Hz],
    ! c   das Temperatur-Fluktuationen unterliegt.
    
    ! INCLUDE "jsm2Dpol.com"
    USE constants_mod
    USE tempmrn_mod
    USE effi_mod
    USE vsg_mod
    USE vec_lr_mod
    USE vec_nf_mod
    USE vec_sig_mod
    USE fest_mod
    USE paheva_mod
    USE const_mod
    USE paraDark_mod
    USE abu_mod
    USE rdark_mod
    USE functions_mod
    IMPLICIT NONE
    ! REAL(KIND=8) :: BPL, SUM, TOTEMIS
    REAL(KIND=8) :: SUM, TOTEMIS, FAK, Tfak1, Tfak
    REAL(KIND=8), DIMENSION(nnn) :: xx
    REAL(KIND=8), DIMENSION(nf) :: emis_eq
    INTEGER :: k, jmaxpw, iterpw, i, iratio, IMIN, IMAX, IPWMAX

! c ---------------------------------------------------------------------------
    IF(ibug .GE. 2 .AND. material .EQ. 0) THEN
        WRITE(6,920) xatom, arad, nnTvsg
    END IF
    920 FORMAT(' No of C-atoms =', 1PE9.2, ' grain radius [cm] =', E9.2, I6, ' enthalpy bins')

! c   Verdampfungs-Temperatur. ievap = 1: Verdampfung.

    totabs = 0.D0
    DO k = 1, mm
        totabs = totabs + pi4 * pi * arad**2 * qabs(k) * j_v(k) * dfr(k)
        emis_eq(k) = pi * arad**2 * qabs(k) * bpl(fr(k), fr3(k), Tmrn)
    END DO

    Tevap = 2.5D3
    Tfak1 = 1.D-12

    IF(zeitphot .LT. zeitfein) THEN
        IF(ibug .EQ. 2) PRINT *, ' feines T-Gitter'
        fak = 3.D-1
        IF(arad .LT. 15.D-8) fak = 4.D-1
        tem(1) = Tmrn / (1.D0 + fak)
        tem(nnTvsg) = Tmrn * (1.D0 + fak)
        Tfak = 1.D0 + fak / 2.D0
    ELSE
        tem(1) = 3.D0
        tem(nnTvsg) = 5.D3
    END IF
    

! c ---------------------------------------------------------------------------
    jmaxpw = 100
    DO iterpw = 1, jmaxpw

        DO i = 2, nnTvsg
            tem(i) = tem(1) + (i - 1) * (tem(nnTvsg) - tem(1)) / REAL(nnTvsg - 1,8)
        END DO

! c  Setzen der Enthalpie - Intervalle

        CALL enth
        CALL transmat
        CALL pw_zustand(imin, imax, ipwmax, iterpw)

! c   Berechnung der Emission pro Teilchen in [erg/s/ster/Hz]

        totemis = 0.D0
        DO k = 1, mm
            sum = 0.D0

            DO i = 1, nnTvsg
                sum = sum + pw(i) * bpl(fr(k), fr3(k), tem(i))
            END DO
            emis(k) = pi * arad**2 * qabs(k) * sum
            totemis = totemis + pi4 * emis(k) * dfr(k)
        END DO

        ratio = totabs / totemis

! c ----------------------------------------------------------------------------
! c  Eventuell neue Einteilung des Temperaturgitters

        IF (totabs .EQ. 0.D0) THEN
            GOTO 4
        END IF
        IF (iterpw .EQ. jmaxpw) THEN
            GOTO 4
        END IF
        iratio = 0
        IF (iterpw .GT. 5 .AND. ratio .GE. 0.95 .AND. ratio .LE. 1.05) THEN
            iratio = 1
        END IF
        IF (iterpw .GT. 9 .AND. ratio .GE. 0.9 .AND. ratio .LE. 1.1) THEN
            iratio = 1
        END IF
        IF (iratio .EQ. 1 .AND. pw(nnTvsg) .LT. 1.D-6 .AND. pw(1) .LT. 1.D-6) THEN
            GOTO 4
        END IF
        IF (iratio .EQ. 1 .AND. pw(nnTvsg) .LT. 1.D-6 .AND. pw(1) .GE. 1.D-6 .AND. tem(1) .LT. 3.1) THEN
            GOTO 4
        END IF
   
        Tfak = 1.3
        IF (imin .EQ. 1) THEN
            tem(1) = MAX(3.D0, tem(1) / Tfak)
        END IF
        IF (imin .GT. 1) THEN
        tem(1) = tem(imin)
        END IF
   
        IF (imax .LT. nnTvsg) THEN
        tem(nnTvsg) = tem(imax)
        END IF

        IF (imax .EQ. nnTvsg) THEN
            IF (pw(imax) .LE. 1.D-12) THEN
               Tfak = 2.D0
            END IF
            tem(nnTvsg) = tem(nnTvsg) * Tfak
        END IF
   
        IF (ibug .GE. 3) THEN
            PRINT *, iterpw, ipwmax, nnTvsg, imin, imax
            PRINT *, 'tem(1) =', tem(1), 'tem(ipwmax) =', tem(ipwmax), 'tem(nnTvsg) =', tem(nnTvsg)
            PRINT *, 'tem(imin) =', tem(imin), 'tem(imax) =', tem(imax)
            PRINT *, 'pw(1) =', pw(1), 'pw(ipwmax) =', pw(ipwmax), 'pw(nnTvsg) =', pw(nnTvsg)
            PRINT *, 'pw(imin) =', pw(imin), 'pw(imax) =', pw(imax)
        END IF
   
        ! 40 CONTINUE
    END DO

    4 CONTINUE
! END DO

   IF (IBUG >= 2) THEN
        WRITE(6, 265) ITERPW, IPWMAX, NNTVSG, IMIN, IMAX, &
                  TEM(1), TEM(IPWMAX), TEM(NNTVSG), TEM(IMIN), TEM(IMAX), &
                  PW(1), PW(IPWMAX), PW(NNTVSG), PW(IMIN), PW(IMAX)

        WRITE(6, 935) ITERPW, RATIO, TOTABS / EVOLT, SUMEVAP
    END IF

    265 FORMAT(I4, 4I10, /, ' T  =', 0P5F10.1, /, ' pw =', 1P5E10.2)
    935 FORMAT('  VSG: Iter =', I3, ' abs/emis = ', 1PE8.2, &
            ' Energy_abs (eV) =', E8.2, ' sumevap = ', E8.2)

    FAK = ABS(1.D0 - ABS(RATIO))

    IF (IBUG >= 2 .OR. FAK >= 0.5D0) THEN
        WRITE(6, 935) ITERPW, RATIO, TOTABS / EVOLT, SUMEVAP
    END IF

    IF (FAK >= 0.5D0) THEN
        WRITE(*,*) ' *** Mist bei VSG *** '
    END IF

! c ----------------------------------------------------------------------------
    IEVAP = 0
    IF(SUMEVAP .GT. FEVAP) THEN
        DO K = 1, MM
            EMIS(K) = 0.
        END DO
        IEVAP = 1
        PRINT *, ' VSG VERDAMPFUNG FUR ARAD = ', ARAD, SUMEVAP
        RETURN
    END IF

 RETURN 
 END SUBROUTINE vsg
! c  *********************************************************************

! c  *********************************************************************
 SUBROUTINE TRANSMAT

    ! c   CALCULATES TRANSITION MATRIX 'AT':  A(F,I) = A(FINAL,INITIAL)
    ! c   FOR HEATING F > I, FOR COOLING I > F = I-1.
    USE constants_mod
    USE tempmrn_mod
    USE effi_mod
    USE vsg_mod
    USE vec_lr_mod
    USE vec_nf_mod
    USE vec_sig_mod
    USE fest_mod
    USE paheva_mod
    USE const_mod
    USE paraDark_mod
    USE abu_mod
    USE rdark_mod

    ! INCLUDE "JSM2DPOL.COM"
    REAL(KIND=8) :: FAK, EJP, QP, XFREQ
    
    ! c   SET TRANSITION MATRIX TO ZERO
    
    DO I = 1, NNTVSG
        DO J = 1, NNTVSG
            AT(I,J) = 0.
            BT(I,J) = 0.
        END DO
    END DO
    
    ! c  --------------------------------------------------------------------------
    ! c   COOLING FROM F+1 -> F.  THIS IS THE DIAGONAL LINE ABOVE THE MAIN DIAGONAL
    ! c  --------------------------------------------------------------------------
    
    DO JF = 1, NNTVSG-1
        AT(JF,JF+1) = DTUN(JF+1) / DUN(JF+1)
        IF (AT(JF,JF+1) .LT. 0.0D0) GO TO 999
    END DO
    
    ! c  --------------------------------------------------------------------------
    ! c   DISCRETE HEATING  I -> F  OCCUPIES ALL ELEMENTS BELOW THE MAIN DIAGONAL
    ! c  --------------------------------------------------------------------------
    
    DO JF = 1, NNTVSG 
        DO JI = 1, JF - 1
    
            ! c  J_V UND QABS WERDEN AUS  INTERPOLATION BEI FREQUENZ XFREQ BERECHNET
    
            XFREQ = (UN(JF) - UN(JI)) / HWIRK
            CALL LOCAT(FR, MM, XFREQ, K)
    
            IF (K .EQ. 0 .OR. K .EQ. MM) THEN
                FAK = 0.0D0
            ELSE
                FAK = (XFREQ - FR(K)) / (FR(K+1) - FR(K))
                EJP = J_V(K)  + (J_V(K+1) - J_V(K)) * FAK
                QP  = QABS(K) + (QABS(K+1) - QABS(K)) * FAK
                FAK = EJP * QP / XFREQ
            END IF
    
            AT(JF,JI) = PI * ARAD**2 * PI4 / HWIRK**2 * FAK * DUN(JF)
    
        END DO
    END DO
    
    ! c ----------------------------------------------------------------------------
    ! c   MAIN DIAGONAL OF MATRIX
    
    DO J = 1, NNTVSG
        DO K = J+1, NNTVSG
            IF (J .EQ. K) CYCLE
            AT(J,J) = AT(J,J) - AT(K,J)
        END DO
    END DO
    
    ! c ----------------------------------------------------------------------------
    ! c   UMSCHREIBEN DER MATRIX 'AT' NACH 'BT' (S. GL.(2.17)).
    
    BT(NNTVSG,NNTVSG-1) = AT(NNTVSG,NNTVSG-1)
    DO JJ = 1, NNTVSG - 2 
        BT(NNTVSG,JJ) = AT(NNTVSG,JJ)
        DO JF = NNTVSG-1, JJ+1, -1 
            BT(JF,JJ) = BT(JF+1,JJ) + AT(JF,JJ)
        END DO
    END DO
    
    RETURN
999   WRITE(6,*) ' ** MIST IN TRANSMAT **'
    STOP
END SUBROUTINE TRANSMAT

! c  *********************************************************************

SUBROUTINE ENTH

    ! c   Berechnet Enthalpien un(i): Fur Graphit (Gl.(3.3)) und fur Si (Gl.(3.4)).
    ! c   Enthalpie-Intervalle dun, Ableitung von un(i) nach der Zeit: dtun (Gl.(2.3))
    ! c   igrid = 0: konstante T-Intervalle, igrid = 1: konstante U-Intervalle.

    USE constants_mod
    USE tempmrn_mod
    USE effi_mod
    USE vsg_mod
    USE vec_lr_mod
    USE vec_nf_mod
    USE vec_sig_mod
    USE fest_mod
    USE paheva_mod
    USE const_mod
    USE paraDark_mod
    USE abu_mod
    USE rdark_mod
    USE functions_mod
    ! INCLUDE 'jsm2Dpol.com'
    REAL(KIND=8), DIMENSION(4) :: tgitsi, ugitsi
    ! REAL(KIND=8) :: BPL
    REAL(KIND=8) :: T, ZAHLER, DENOM, FT, FTS, DT, FAK
    INTEGER :: igrid = 0

    ! Initialize arrays using Fortran90 style
    tgitsi = [0.0D0, 50.0D0, 150.0D0, 500.0D0]
    ugitsi = [0.0D0, 5.833D7, 9.486D8, 9.432D9]

  
    ! c ----------------------------------------------------------------------------
    ! c   igrid = 1:  konstante U-Intervalle fur Graphit (material = 0)
  
    IF (igrid .EQ. 1 .AND. material .EQ. 0) THEN
        t = tem(1)
        zahler = xatom * (1.0D0 - 2.0D0 / xatom) * 4.15D-22 * t**3.3
        denom = 1.0D0 + 6.51D-3 * t + 1.5D-6 * t**2 + 8.3D-7 * t**2.3
        un(1) = zahler / denom
        
        t = tem(nnTvsg)
        zahler = xatom * (1.0D0 - 2.0D0 / xatom) * 4.15D-22 * t**3.3
        denom = 1.0D0 + 6.51D-3 * t + 1.5D-6 * t**2 + 8.3D-7 * t**2.3
        un(nnTvsg) = zahler / denom

        DO i = 2, nnTvsg - 1
            un(i) = un(1) + (i - 1) * (un(nnTvsg) - un(1)) / REAL(nnTvsg - 1,8)
            iter = 0

            ! Iterate until convergence
            DO
                iter = iter + 1
                zahler = xatom * (1.0D0 - 2.0D0 / xatom) * 4.15D-22 * t**3.3
                denom = 1.0D0 + 6.51D-3 * t + 1.5D-6 * t**2 + 8.3D-7 * t**2.3
                ft = zahler / denom - un(i)
                fts = denom * xatom * (1.0D0 - 2.0D0 / xatom) * 3.3 * 4.15D-22 * t**2.3 - &
                     zahler * (6.51D-3 + 2.0D0 * 1.5D-6 * t + 2.3 * 8.3D-7 * t**1.3)
                fts = fts / denom**2
                dt = - ft / fts
                t = t + dt

                IF (ABS(dt) .GE. 1.0D-4) EXIT
                IF (iter .EQ. 25) STOP ' ENTH'

            END DO

            tem(i) = t
        END DO
    END IF
  

! c ----------------------------------------------------------------------------
! c   igrid = 0:  konstante T-Intervalle fur Graphit (material = 0)

    IF (IGRID .EQ. 0 .AND. MATERIAL .EQ. 0) THEN
        DO I = 1, NNTVSG
            TEM(I) = TEM(1) + (I - 1) * (TEM(NNTVSG) - TEM(1)) / REAL(NNTVSG - 1,8)
            T = TEM(I)
            ZAHLER = XATOM * (1.0D0 - 2.0D0 / XATOM) * 4.15D-22 * T**3.3
            DENOM = 1.0D0 + 6.51D-3 * T + 1.5D-6 * T**2 + 8.3D-7 * T**2.3
            UN(I) = ZAHLER / DENOM
        END DO
    END IF

! c ----------------------------------------------------------------------------
! c   igrid = 1:  konstante U-Intervalle fur Si (material = 1)

    IF (IGRID .EQ. 1 .AND. MATERIAL .EQ. 1) THEN
        ! T = TEM(1)
        T = REAL(TEM(1),8)
        CALL LOCAT(TGITSI, 4, T, J)
        IF (J .EQ. 0) STOP 'SUB ENTH'
        IF (J .EQ. 1) FAK = 1.4D3 / 3.0D0 * T**3
        IF (J .EQ. 2) FAK = UGITSI(J) + 2.2D4 / 2.3 * (T**2.3 - TGITSI(J)**2.3)
        IF (J .EQ. 3) FAK = UGITSI(J) + 4.8D5 / 1.68 * (T**1.68 - TGITSI(J)**1.68)
        IF (J .EQ. 4) FAK = UGITSI(J) + 3D7 * (T - TGITSI(J))
        UN(1) = FAK * PI4 / 3.0D0 * ARAD**3
      
        T = TEM(NNTVSG)
        CALL LOCAT(TGITSI, 4, T, J)
        IF (J .EQ. 0) STOP 'SUB ENTH'
        IF (J .EQ. 1) FAK = 1.4D3 / 3.0D0 * T**3
        IF (J .EQ. 2) FAK = UGITSI(J) + 2.2D4 / 2.3 * (T**2.3 - TGITSI(J)**2.3)
        IF (J .EQ. 3) FAK = UGITSI(J) + 4.8D5 / 1.68 * (T**1.68 - TGITSI(J)**1.68)
        IF (J .EQ. 4) FAK = UGITSI(J) + 3D7 * (T - TGITSI(J))
        UN(NNTVSG) = FAK * PI4 / 3.0D0 * ARAD**3
      
        DO I = 2, NNTVSG - 1
            UN(I) = UN(1) + (I - 1) * (UN(NNTVSG) - UN(1)) / REAL(NNTVSG - 1,8)
            STOP 'HIER FEHLT NOCH WAS'
        END DO
    END IF

! c ----------------------------------------------------------------------------
! c   igrid = 0:  konstante T-Intervalle fur Si (material = 1)

    IF (IGRID .EQ. 0 .AND. MATERIAL .EQ. 1) THEN
        DO I = 1, NNTVSG
            TEM(I) = TEM(1) + (I - 1) * (TEM(NNTVSG) - TEM(1)) / REAL(NNTVSG - 1,8)
            T = TEM(I)
      
            CALL LOCAT(TGITSI, 4, T, J)
            IF (J .EQ. 0) STOP 'SUB ENTH'
            IF (J .EQ. 1) FAK = 1.4D3 / 3.0D0 * T**3
            IF (J .EQ. 2) FAK = UGITSI(J) + 2.2D4 / 2.3 * (T**2.3 - TGITSI(J)**2.3)
            IF (J .EQ. 3) FAK = UGITSI(J) + 4.8D5 / 1.68 * (T**1.68 - TGITSI(J)**1.68)
            IF (J .EQ. 4) FAK = UGITSI(J) + 3D7 * (T - TGITSI(J))
            UN(I) = FAK * PI4 / 3.0D0 * ARAD**3
        END DO
    END IF

! c ----------------------------------------------------------------------------
! c   Bestimme dun(i) und dtun(i)

    DO I = 2, NNTVSG - 1
        DUN(I) = 5D-1 * (UN(I+1) - UN(I-1))
    END DO
    DUN(1) = 5D-1 * UN(2)
    DUN(NNTVSG) = DUN(NNTVSG-1)
      
    DO I = 2, NNTVSG
        DTUN(I) = 0.D0
        DO K = 1, MM
            DTUN(I) = DTUN(I) + QABS(K) * BPL(FR(K), FR3(K), TEM(I)) * DFR(K)
            ! DTUN(I) = DTUN(I) + REAL(QABS(K),8) * BPL(REAL(FR(K),8), REAL(FR3(K),8), REAL(TEM(I),8)) * REAL(DFR(K),8)
        END DO
        DTUN(I) = DTUN(I) * PI4 * PI * ARAD**2
    END DO
      
    RETURN
END SUBROUTINE ENTH

! c  *********************************************************************

SUBROUTINE ISRF(RDUST, TOTLUM, TSTAR, POWL)
! c
! c   Schwachung des Strahlungsfelds um absorptions des Gases
! c   sowie unless ISRF a reduction by  exp(-tauV * t(v)/t(vis)) 
! c
! c 1)  Eingabe bei Strahlung vom Stern:
! c      0 < fakisrf < 1e-5
! c               rdust  = Entfernung vom Staub zum Stern [cm]
! c               totlum = Leuchtkraft des Sterns [erg/s]
! c               tstar  = Sterntemperatur
! c               tauV    = visuelle Extinktion  =0. z.Z.
! c 
! c  2) Eingabe bei Strahlung von AGN:
! c     -2 <  fakisrf < 0
! c               rdust  = Entfernung vom Staub zum Stern [cm]
! c               totlum = Leuchtkraft des Sterns [erg/s]
! c               powl   = j_nu ~ v**{-powl} ; powl > 0
! c
! c  3) Interstellares Strahlungsfeld nach Perrault et al.:
! c      fakisrf > 1e-5
! c      Wellenlange:   wel_per [mue]
! c      Intensitat:    u_per  = 1d-2 [eV/cm**3] = u(v) * v
! c  4) Eingabe 4 Black-bodies sonst wie bei Stern Strahlung 
! c      fakisrf =-9
! c  5) Lesen einies beliebiges intensitiates Spektrum j_v (erg/s/cm**2/Hz/ster)
! c     Eingabe:  fakisrf =-999
! c ----------------------------------------------------------
! c   Ausgabe: j_v(k) 
! c   j_v(k) = mittlere Intensitat  [erg/s/cm**2/Hz/ster]  (bei ISRF)
! c   j_v(k) = Fluss/pi4            [erg/s/cm**2/Hz]       (bei Stern)
! c

    USE constants_mod
    USE tempmrn_mod
    USE effi_mod
    USE vsg_mod
    USE vec_lr_mod
    USE vec_nf_mod
    USE vec_sig_mod
    USE fest_mod
    USE paheva_mod
    USE const_mod
    USE paraDark_mod
    USE abu_mod
    USE rdark_mod
    USE functions_mod
    INTEGER, PARAMETER :: MISRF = 29
    ! INCLUDE "JSM2DPOL.COM"
    INTEGER :: IDUMMY(20)
    ! REAL(KIND=8) :: WELGAS(NF), BPL
    REAL(KIND=8) :: WELGAS(NF)
    REAL(KIND=8) :: U_PER(MISRF), WEL_PER(MISRF), W250(NF), XJ250(NF)

    REAL(KIND=8) :: TBB_IR, TBB_UV, TBB_SX, &
        FAK_IR, FAK_UV, FAK_SX, FAK_HX
    REAL(KIND=8) :: RADFAK, FAKUJ, FAKP, FAKN, EEV, FAK, UPER
    REAL(KIND=8) :: RADFAK_HX, RADFAK_IR, RADFAK_UV, RADFAK_SX
    REAL(KIND=8) :: SUMHX, SUMIR, SUMUV, SUMSX, SUMT
    REAL(KIND=8) :: RDUST, TOTLUM, TSTAR, POWL
    REAL(KIND=8) :: FREQ, SCALE, W20EV, W300EV, SUM, EXT
    
    ! INITIALIZE ARRAYS USING FORTRAN90 STYLE
    U_PER = [2.12D-6, 4.455D-2, 7.0D0, 18.0D0, 12.0D0, 5.5D0, 3.5D0, &
                3.0D0, 4.0D0, 4.0D0, 2.5D0, 5.5D0, 16.0D0, 22.0D0, &
                25.0D0, 20.0D0, 9.0D0, 5.0D0, 4.5D0, 5.5D0, 6.5D0, &
                6.5D0, 6.5D0, 6.0D0, 5.5D0, 5.0D0, 4.0D0, 3.0D0, 1.0D-2]
    
    WEL_PER = [1D4, 1.D3, 2.45D02, 1.48D02, 9.00D01, 5.46D01, 3.30D01, &
                2.00D01, 1.22D01, 7.40D00, 4.50D00, 2.70D00, 1.60D00, &
                1.00D00, 6.07D-1, 4.72D-1, 3.68D-1, 2.86D-1, 2.23D-1, &
                2.02D-1, 1.83D-1, 1.65D-1, 1.50D-1, 1.35D-1, 1.22D-1, &
                1.11D-1, 1.00D-1, 9.07D-2, 7.50D-2]
    
    FAKUJ = PI4 / CLICHT**2
              



! c
! c -------------------------------------------------------------
! c  1)  Strahlung vom Stern

    IF (FAKISRF .LE. 1D-5 .AND. FAKISRF .GE. 0.D0) THEN
        PRINT *, ' '
        PRINT *, ' 1)  STRAHLUNG VOM STERN'
        PRINT *, ' '
        RADFAK = PI * TOTLUM / (PI4 * SIGMA * TSTAR**4 * RDUST**2) / PI4

        EXT = 0.D0
        DO K = 1, MM
           J_V(K) = RADFAK * BPL(FR(K), FR3(K), TSTAR)
        END DO
    END IF

! c  --------------------------------------------------------------
! c   2) AGN power law.


    IF (FAKISRF .LT. 0.0D0 .AND. FAKISRF .GT. -2.0D0) THEN 
        PRINT *, ' '
        PRINT *, ' 2) AGN power law'
        PRINT *, ' '
    
        CALL LOCAT(WEL, MM, 2.0D-4, ISYN)
    
        IF (POWL .EQ. 1.0D0) THEN
            RADFAK = TOTLUM / LOG(FR(MM) / FR(ISYN))
        ELSE
            FAKP = 1.0D0 - POWL
            FAKN = FR(MM)**FAKP - FR(ISYN)**FAKP
            RADFAK = TOTLUM * FAKP / FAKN
        END IF
    
        RADFAK = RADFAK / PI4 / RDUST**2
        WRITE(6, *) ' Power-law spectrum between:'
        WRITE(6, *) ' WEL(ISYN) = ', WEL(ISYN) * 1.0D4, '  mum'
        WRITE(6, *) ' WEL(MM)   = ', WEL(MM) * 1.0D4, '  mum'
    
        DO K = ISYN, MM
            J_V(K) = RADFAK * FR(K)**(-POWL)
        END DO
    END IF

! c --------------------------------------
! c    Jv with 4  BlackBody  components:
! c
    IF (FAKISRF .EQ. -9) THEN
        PRINT *, ' Jv: For TTauri stars with 4 BlackBodies: '
        WRITE(6, *) ' BB components and strengths are:'
        
        TBB_IR = 4000.0D0     ! (=98.875%)
        TBB_UV = 15000.0D0    ! (+1%)
        TBB_SX = 3.0D5        ! (+0.1%)
    
        FAK_IR = 0.98875D0
        FAK_UV = 0.01D0
        FAK_SX = 0.001D0
        FAK_HX = 10.0D0 ** (-3.6D0)
    
        FAK_IR = 0.0D0        ! 0.98875
        FAK_UV = 0.0D0        ! 0.01
        FAK_SX = 0.001D0
        FAK_HX = 0.0D0       ! 10.0D0 ** (-3.6D0)
    
        WRITE(6, *) ' TBB_IR,   TBB_UV,   TBB_SX,   TBB_HX: '
        WRITE(6, '(1P3E10.2)') TBB_IR, TBB_UV, TBB_SX
    
        WRITE(6, *) ' FAK_IR, FAK_UV, FAK_SX, FAK_HX: '
        WRITE(6, '(1P4E10.2)') FAK_IR, FAK_UV, FAK_SX, FAK_HX

 
! c x-ray component:
        RADFAK_HX = 0.D0
        IF (FAK_HX .GT. 0.D0) THEN
            DO K = 1, MM
                EEV = HWIRK * FR(K) / EVOLT
                J_V(K) = 0.D0
                IF (EEV .GE. 1.D2 .AND. EEV .LE. 2.D3) J_V(K) = (EEV / 2.D3) ** 2.D0
                ! c IF (EEV .GE. 2.D3 .AND. EEV .LE. 1.D4) J_V(K) = (2.D3 / EEV) ** 3.D0
                IF (EEV .GE. 2.D3) J_V(K) = 0.D0
                IF (EEV .LT. 1.D2) J_V(K) = 0.D0
            END DO
            SUMHX = 0.D0
            DO K = 1, MM
                SUMHX = SUMHX + J_V(K) * DFR(K)
            END DO
            RADFAK_HX = FAK_HX * TOTLUM / SUMHX / PI4 / PI4 / RDUST ** 2
        END IF
        
        RADFAK_IR = FAK_IR * PI * TOTLUM / (PI4 * SIGMA * TBB_IR ** 4 * RDUST ** 2) / PI4
        RADFAK_UV = FAK_UV * PI * TOTLUM / (PI4 * SIGMA * TBB_UV ** 4 * RDUST ** 2) / PI4
        RADFAK_SX = FAK_SX * PI * TOTLUM / (PI4 * SIGMA * TBB_SX ** 4 * RDUST ** 2) / PI4
        
        SUMIR = 0.D0
        SUMUV = 0.D0
        SUMSX = 0.D0
        SUMHX = 0.D0
        
        DO K = 1, MM
            SUMIR = SUMIR + RADFAK_IR * BPL(FR(K), FR3(K), TBB_IR) * DFR(K)
            SUMUV = SUMUV + RADFAK_UV * BPL(FR(K), FR3(K), TBB_UV) * DFR(K)
            SUMSX = SUMSX + RADFAK_SX * BPL(FR(K), FR3(K), TBB_SX) * DFR(K)
            SUMHX = SUMHX + RADFAK_HX * J_V(K) * DFR(K)
            
            J_V(K) = RADFAK_HX * J_V(K) + &
                     RADFAK_IR * BPL(FR(K), FR3(K), TBB_IR) + &
                     RADFAK_UV * BPL(FR(K), FR3(K), TBB_UV) + &
                     RADFAK_SX * BPL(FR(K), FR3(K), TBB_SX)
        END DO


! c in erg/s: lum = (4p*j_v) * 4pi r^2
        SUMIR = SUMIR * PI4 * PI4 * RDUST ** 2
        SUMUV = SUMUV * PI4 * PI4 * RDUST ** 2
        SUMSX = SUMSX * PI4 * PI4 * RDUST ** 2
        SUMHX = SUMHX * PI4 * PI4 * RDUST ** 2
        SUMT = SUMIR + SUMUV + SUMSX + SUMHX
        
        ! c        IF (IBUG .GE. 2) THEN
        WRITE(6, '(A20, 1P1E10.2)') ' TOTLUM:', SUMT / TOTLUM
        WRITE(6, '(A20, 1P1E10.2)') ' IR:    ', SUMIR / TOTLUM
        WRITE(6, '(A20, 1P1E10.2)') ' UV:    ', SUMUV / TOTLUM
        WRITE(6, '(A20, 1P1E10.2)') ' SX:    ', SUMSX / TOTLUM
        WRITE(6, '(A20, 1P1E10.2)') ' HX:    ', SUMHX / TOTLUM
    END IF
! c      end if
! c
! c --------------------------------------------
! c    3) Interstellares Strahlungsfeld (ISRF)

    IF (FAKISRF .GT. 1D-5) THEN
        PRINT *, ' 3) Interstellares Strahlungsfeld (ISRF)'
    
        DO K = 1, MM
            CALL LOCAT(WEL_PER, MISRF, 1D4 * WEL(K), J)
            IF (J .EQ. 0) J = 1
            IF (J .EQ. MISRF) J = MISRF - 1
            FAK = (1D4 * WEL(K) - WEL_PER(J)) / (WEL_PER(J+1) - WEL_PER(J))
            UPER = LOG(U_PER(J)) + (LOG(U_PER(J+1)) - LOG(U_PER(J))) * FAK
            J_V(K) = FAKISRF * EXP(UPER) * 1.602D-14 / WEL(K) / FAKUJ / FR(K)**2
    
            J_V(K) = 1.12 * J_V(K)   ! Better match to DIRBE 100µm
    
            IF (WEL(K) .GT. WEL(1)) J_V(K) = 0.D0
            IF (WEL(K) .LT. 7.50D-7) J_V(K) = 0.D0
        END DO
    
    END IF

     
! c
! c --------------------------------------------
! c 5) beliebiges intensitiates Spektrum j_v (erg/s/cm**2/Hz/ster)
! c
    IF (FAKISRF .LE. -1000.D0) THEN
        PRINT *, ' '
        PRINT *, ' 5) Beliebiges Input Spektrum J_V (erg/s/cm**2/Hz/ster)'
        PRINT *, '    Hier Mathis et al.'
        PRINT *, ' '
        
        SCALE = ABS(FAKISRF + 1000.D0)
        PRINT *, '*** 250Myrs.isrf read and scaled by ', SCALE
    
        OPEN(UNIT=2, FILE='./Input/250Myrs.isrf', FORM='formatted')
        REWIND 2
        500 FORMAT(20A4)
    
        DO K = 1, MM
            READ(2,*) W250(K), XJ250(K)
            FREQ = CLIcht / W250(K) * 1.D4
        END DO
        CLOSE(2)
    
        DO K = 1, MM
            CALL LOCAT(W250, MM, 1.D4 * WEL(K), J)
            IF (J .EQ. 0) J = 1
            IF (J .EQ. MM) J = MM - 1
            FAK = (1.D4 * WEL(K) - W250(J)) / (W250(J+1) - W250(J))
            J_V(K) = XJ250(J) + (XJ250(J+1) - XJ250(J)) * FAK
    
            J_V(K) = J_V(K) * SCALE
        END DO
    END IF
! c --------------------------------------------
! c    Correct for absorption by Gas: 
! c    Apply Ionisation grad gIon: EUV:0.5, sonst:0
! c    Normalize to dust extinction cross section to 200cm^2/g-ISM
! c    and with gas-to-dust ratio Rdg=100 the gas absoption cross section
! c    as well to cm^2/g-ISM
! c

    IF (IGASABS .EQ. 0) GOTO 995

    OPEN(UNIT=3, FILE='/home/rsiebenm/STP/Mie/d.absgas887', FORM='formatted')
    REWIND(3)
    
    DO II = 1, 6
        READ(3, '(20A4)') (IDUMMY(I), I = 1, 20)
        WRITE(6, '(20A4)') (IDUMMY(I), I = 1, 20)
    END DO
    
    IF (MM .NE. 887) PRINT *, 'Interpolation zu neuen WEL notwendig'
    
    DO K = 1, MM      
        READ(3, *) WELGAS(K), SIGAS(K)
    END DO
    
    CLOSE(3)
    
    GION = 0.5
    RDG = 100.0
    
    WRITE(6, '(A30, 1F5.1)') ' *** EUV Ionisationsgrad       = ', GION
    WRITE(6, '(A30, 1F5.1)') ' *** Gas-to-dust ratio Rdg     = ', RDG
    
    ! Ionisationsgrad f. EUV Komponente (andere Komponenten ist GION = 0)
    W20EV = CLIcht / (20.0 * EVolt / HWirk)
    W300EV = CLIcht / (300.0 * EVolt / HWirk)
    
    DO K = 1, MM       
        IF (WEL(K) .GE. W20EV) THEN
            SIGAS(K) = SIGAS(K) * (1.0 - GION / 50.0) / RDG  
        END IF
        IF (WEL(K) .LE. W20EV .AND. WEL(K) .GT. W300EV) THEN
            SIGAS(K) = SIGAS(K) * (1.0 - GION) / RDG 
        END IF
        IF (WEL(K) .LE. W300EV) THEN
            SIGAS(K) = SIGAS(K) * (1.0 - GION / 50.0) / RDG 
        END IF
    END DO
    
    WRITE(6, '(A30, 1F8.4)') ' *** Extinction is tauV = ', TAUV
    
    IF (IGASABS .EQ. 1 .AND. TAUV .GT. 0.0) PRINT *, ' .. *** Gas+Staub absorption'
    IF (IGASABS .EQ. 0 .AND. TAUV .GT. 0.0) PRINT *, ' .. *** nur Staub absorption'
    
    DO K = 1, MM
        IF (ABS(1.0 - WELGAS(K) / WEL(K)) .GE. 0.01) THEN
            WRITE(6, *) K, WELGAS(K), WEL(K), ABS(1.0 - WELGAS(K) / WEL(K))
            STOP ' ** wrong wavelength'
        END IF
    END DO
    
    995 CONTINUE
! c
! c --------------------------------------------
! c Abschwaechung um e^-tau: NUR fals nicht ISRF alsi if fakisrf le 1d-5 !
! C /ohne  Gas absorption:


    EXT = 0.D0
    SUM = 0.D0
    
    DO K = 1, MM      
        IF (IGASABS .EQ. 0) THEN
            EXT = SIGT(K) / SIGT(KVIS)
        END IF
        IF (IGASABS .EQ. 1) THEN
            EXT = (SIGT(K) + SIGAS(K)) / (SIGT(KVIS) + SIGAS(KVIS))
        END IF
    
        IF (FAKISRF .LE. 1.D-5) THEN
            J_V(K) = J_V(K) * EXP(-TAUV * EXT)
        END IF
    
        SUM = SUM + PI4 * J_V(K) * DFR(K)
    
        IF (IBUG .GE. 3) THEN
            WRITE(6, '(1P4E10.2)') WEL(K) * 1.D4, MAX(J_V(K), 1.D-40), EXP(-TAUV * EXT), EXT
        END IF
    END DO
    
    WRITE(6, '(A40, 1P1E10.3)') '  Integ. mean Intensitat (erg/cm2/s): ', SUM
    
    RETURN
END SUBROUTINE ISRF

! c  *********************************************************************

SUBROUTINE TEMPMRN(TNEW)

    ! c   Calculates equilibrium temperature T of a large (> 50A) grain,
    ! c   a so-called MRN particle. Grain has absorption coefficient QABS(K)
    ! c   and sits in radiation field J_V(K).
  
    ! c   The total emission of the grain EMIS(K) [erg/s/Hz/cm**2]
    ! c   at frequency FR(K) is given by:
    ! c     EMIS(K) = 4*pi * pi*ARAD**2 * QABS(K) * BPL(FR(K),FR3(K),TNEW)
    ! c   where pi*ARAD**2 = geometrical cross section of the grain.
    ! c   The grain absorbs the total energy:
    ! c     SUMJV = 4*pi * pi*ARAD**2 * Int {J_V * QABS * DV}.
  
    ! c   First temperature guess is argument in call TEMPMRN.
  
    ! INCLUDE "JSM2DPOL.COM"
    USE constants_mod
    USE tempmrn_mod
    USE effi_mod
    USE vsg_mod
    USE vec_lr_mod
    USE vec_nf_mod
    USE vec_sig_mod
    USE fest_mod
    USE paheva_mod
    USE const_mod
    USE paraDark_mod
    USE abu_mod
    USE rdark_mod
    USE functions_mod
    INTEGER :: ITER
    ! REAL(KIND=8) :: BPL
    DOUBLE PRECISION :: TOLD, TNEW, SUMBQ, DTEMP
  
    ITER = 0
    TOLD = TNEW
  
    1 CONTINUE
    ! c
    ITER = ITER + 1
    TOLD = 0.5D0 * (TNEW + TOLD)
    SUMBQ = 0.D0
    DO K = 1, MM
       SUMBQ = SUMBQ + QABS(K) * BPL(FR(K), FR3(K), TOLD) * DFR(K)
    END DO
  
    TNEW = TOLD * (SUMJV / SUMBQ)**0.25
    DTEMP = ABS(TNEW - TOLD)
  
    IF (ITER .EQ. 50) STOP 'TempMRN'
    IF (DTEMP .GT. 0.001) GO TO 1
  
    IF (IBUG .GE. 3) THEN
        IF (ARAD .EQ. AMRN(LAC) .OR. ARAD .EQ. AMRN(LEC) .OR. &
            ARAD .EQ. AMRN(LASI) .OR. ARAD .EQ. AMRN(LESI)) THEN
            WRITE(6, 100) ARAD, TNEW, DTEMP, ITER
        END IF
    END IF
  
    IF (IBUG .GE. 2) THEN
        WRITE(6, 100) ARAD, TNEW, DTEMP, ITER
    END IF
  
    100 FORMAT(' ARAD [cm] =', 1PE10.2, ' Temp =', 0P2F11.3, I5)
  
    RETURN
END SUBROUTINE TEMPMRN

! c  *********************************************************************

! c  *********************************************************************
! INTERFACE
! REAL(KIND=8) FUNCTION BPL(FR, FR3, T) RESULT(BPL_OUT)
!     !-------------------------------------------------------------------------------
!     !   Berechnet Planck Funktion.
!     !   FR  = Frequenz
!     !   FR3 = FR**3
!     !   T   = Temperatur
!     !-------------------------------------------------------------------------------
!     ! USE constants_mod
!     IMPLICIT NONE
    
!     !-------------------------------------------------------------------------------
!     !   Declarations
!     !-------------------------------------------------------------------------------
!     ! REAL(KIND=8), INTENT(IN) :: FR, FR3, T
!     REAL(KIND=8), INTENT(IN) :: FR, FR3, T
!     ! REAL(KIND=8), INTENT(OUT) :: BPL_OUT
!     ! REAL(KIND=8) :: BPL_OUT
!     REAL(KIND=8)             :: C1, C2, X, BPL_OUT
    
!     !-------------------------------------------------------------------------------
!     !   Constants (Fortran90 style initialization)
!     !-------------------------------------------------------------------------------
!     C1 = 1.4745D-47
!     C2 = 4.7994D-11
    
!     !-------------------------------------------------------------------------------
!     !   Compute Planck Function
!     !-------------------------------------------------------------------------------
!     X = C2 * FR / T

!     IF (X > 4.0D2) THEN
!         BPL_OUT = 0.0D0
!     ELSE
!         BPL_OUT = C1 * FR3 / (EXP(X) - 1.0D0)
!         ! BPL_OUT = C1 * FR3 / (REAL(EXP(X),8) - 1.0D0)
!         ! BPL_OUT = C1 * REAL(FR3,8) / (EXP(X) - 1.0D0)
!         ! BPL_OUT = C1 * REAL(FR3,8) / (REAL(EXP(X),8) - 1.0D0)
!     END IF
    
! END FUNCTION BPL
! END INTERFACE
! c  *********************************************************************

FUNCTION ABBPL(FR, FR3, T) RESULT(ABBPL_OUT)
    !-------------------------------------------------------------------------------
    !   Berechnet Ableitung der Planck Funktion nach der Temperatur
    !-------------------------------------------------------------------------------
 
    IMPLICIT NONE
    
    !-------------------------------------------------------------------------------
    !   Declarations
    !-------------------------------------------------------------------------------
    REAL(KIND=8), INTENT(IN) :: FR, FR3, T
    REAL(KIND=8)             :: ABBPL_OUT
    REAL(KIND=8)             :: C1, C2, X, FR4, FAK
    
    !-------------------------------------------------------------------------------
    !   Constants
    !-------------------------------------------------------------------------------
    C1 = 1.4745D-47
    C2 = 4.7994D-11
    
    !-------------------------------------------------------------------------------
    !   Compute derivative of Planck Function
    !-------------------------------------------------------------------------------
    X   = C2 * FR / T
    FR4 = FR * FR3

    IF (X > 3.0D2) THEN
        ABBPL_OUT = 0.0D0
    ELSE
        FAK        = EXP(X)
        ABBPL_OUT  = C1 * C2 * FR4 * FAK / &
                        ( (FAK - 1.0D0)**2 * T**2 )
    END IF
    
END FUNCTION ABBPL

! c  *********************************************************************

SUBROUTINE LOCAT(XY, N, X, J)

    ! c   XY is an ordered vector of length N, meaning either
    ! c   XY(1) < XY(2) < ... < XY(N) or XY(1) > XY(2) > ... > XY(N).
    ! c   Given the number X, the program calculates J such that X lies in the interval [XY(J), XY(J+1)].
    ! c   If X < XY(1) < XY(N):     J = 0
    ! c   If XY(1) < XY(N) < X:     J = N
    ! c   If X > XY(1) > XY(N):     J = 0
    ! c   If XY(1) > XY(N) > X:     J = N
    IMPLICIT NONE
    REAL(KIND=8), DIMENSION(N) :: XY
    INTEGER :: N, J, JLOW, JUP, JM
    REAL*8 :: X
    
    JLOW = 0
    JUP  = N + 1
    
    10 CONTINUE
    IF (JUP - JLOW .GT. 1) THEN
        JM = (JUP + JLOW) / 2
        IF ( (XY(N) .GT. XY(1)) .EQV. (X .GT. XY(JM)) ) THEN
            JLOW = JM
        ELSE
            JUP = JM
        END IF
        GO TO 10
    END IF
    
    J = JLOW
    
    RETURN
END SUBROUTINE LOCAT


! c  *********************************************************************

SUBROUTINE PW_ZUSTAND(IMIN, IMAX, IPWMAX, ITERPW)

    ! c   Berechnung des Zustandsvektors, zuerst des nicht normierten
    ! c   XX(F) = PW(F)/PW(1), dann des normierten PW.
  
    ! INCLUDE 'jsm2Dpol.com'
    USE constants_mod
    USE tempmrn_mod
    USE effi_mod
    USE vsg_mod
    USE vec_lr_mod
    USE vec_nf_mod
    USE vec_sig_mod
    USE fest_mod
    USE paheva_mod
    USE const_mod
    USE paraDark_mod
    USE abu_mod
    USE rdark_mod  
    INTEGER :: IMIN, IMAX, IPWMAX, ITERPW
    REAL*8 :: xx(nnn), sumx, sum, fak, sumpw
    INTEGER :: jf, k
    REAL*8 :: Tfak1
  
    ! Initialize the scaling factor
    Tfak1 = 1d-12
  
    ! Initialize the first element of xx
    xx(1) = 1d0
    sumx = xx(1)
  
    ! Calculate the non-normalized state vector
    DO jf = 2, nnTvsg
       sum = 0.0
  
        ! Sum over previous elements
        DO k = 1, jf - 1
            fak = bt(jf, k) * xx(k)
            IF (bt(jf, k) * xx(k) .LT. 0.0) THEN
                STOP '** subpah ** :  bt * x < 0 '
            END IF
            sum = sum + bt(jf, k) * xx(k)
        END DO
  
        ! Calculate xx(jf) for the non-normalized state
        xx(jf) = sum / at(jf-1, jf)
        sumx = sumx + xx(jf)
    END DO
  
    ! Initialize variables for the normalized state vector
    sumevap = 1d-20
    sumpw = 0.0
  
    ! Normalize the state vector
    DO jf = 1, nnTvsg
        pw(jf) = xx(jf) / sumx
        sumpw = sumpw + pw(jf)
  
        IF (tem(jf) .GT. Tevap) THEN
            sumevap = sumevap + pw(jf)
        END IF
    END DO
  
    ! Optional debug print
    IF (ibug .GE. 3) THEN
        PRINT 125, Tevap, sumevap
    END IF
  
    ! Output format
    125 FORMAT(' Anteil  heisser als ', F5.0, 'K sind:', 1PE10.2)

! c ----------------------------------------------------------------------------

    IF (IBUG >= 3) THEN

        WRITE(6, 921) SUMX
        WRITE(6, 931) SUMPW
        WRITE(6, 923)
        WRITE(6, 911) (I, TEM(I), PW(I), I = 1, NNTVSG)
    
    END IF
    
    921 FORMAT(/' Normierungs-Konstante von Pw :', 1PE12.3)
    931 FORMAT(' Sum of probabilities = ', 1PE12.3)
    923 FORMAT(/'          Temp         pw      ')
    911 FORMAT(I5, 1P2E12.3)

! c ----------------------------------------------------------------------------
! c  Bestimmung des Maximum pwmax vom Vektor pw und der Temperaturen, wo pw(i)
! c  um Faktor Tfak1 = 1d12 abgefallen ist gegenüber pwmax.

    pwmax = 0.0d0
    DO 61 i = 3, nnTvsg
        IF (pw(i) .LE. pwmax) THEN
            GO TO 61
        END IF
        ipwmax = i
        pwmax = pw(i)
    61 CONTINUE
    
    imin = 1
    DO 62 i = ipwmax-1, 2, -1
        IF (pw(i) .GE. Tfak1 * pwmax) THEN
            GO TO 62
        END IF
    imin = i
    GO TO 2
    62 CONTINUE
    
    2 CONTINUE
    
    imax = nnTvsg
    DO 63 i = ipwmax+1, nnTvsg
        IF (pw(i) .GE. Tfak1 * pwmax) THEN
            GO TO 63
        END IF
    imax = i
    GO TO 3
    63 CONTINUE
    
    3 CONTINUE
    
    RETURN
END SUBROUTINE PW_ZUSTAND

SUBROUTINE sigtDark_EbvAvPol

    ! Computes the relative mass of nano-MRN and sub-micron dust using
    ! E(B-V) and Av(Dgaia): Rel. mass of dust in 1g of aC, Si, pah, vgr,
    ! vsi and Dark dust.  Output is the total extinction cross section
    ! of larg+nano+dark + Dark dust : sigt* (cm^2/g_dust ISM)

    USE constants_mod
    USE tempmrn_mod
    USE effi_mod
    USE vsg_mod
    USE vec_lr_mod
    USE vec_nf_mod
    USE vec_sig_mod
    USE fest_mod
    USE paheva_mod
    USE const_mod
    USE paraDark_mod
    USE abu_mod
    USE rdark_mod  
    IMPLICIT NONE

    REAL(KIND=8) :: abuwmol, del, fdark_old, fak, fak1, fL
    REAL(KIND=8) :: tbv, slb, slv, sdb, sdv, rvd, rvl, xnd1
    REAL(KIND=8) :: sigtV, Rv_mod, Ebv
    INTEGER :: iter, K, L, LRAD

    abuc_tot = abuc + abucvgr + abucpahs + abucpahb     
    abuwmol  = abuc_tot*wmolc + abusi*wmolsi  + abuvsi*wmolvsi

    ! c     Find fdark to previous fdark_old iteration to del < 5%

    xnd    = 0.
    xnl    = 0.
    fdark  = max(fdark,0.001)
    fdark  = min(fdark,0.999)

    del    = 1.


    do iter = 1, 11
        if (del .gt. 0.05) then
          
           fdark_old = fdark
       
       aCmass   = (1.-fdark) * abuc     * wmolc  /abuwmol
       pahmasss = (1.-fdark) * abucpahs * wmolc  /abuwmol
       pahmassb = (1.-fdark) * abucpahb * wmolc  /abuwmol
       vgrmass  = (1.-fdark) * abucvgr  * wmolc  /abuwmol
       Simass   = (1.-fdark) * abusi    * wmolsi /abuwmol
       vsimass  = (1.-fdark) * abuvsi   * wmolvsi /abuwmol
       pahmass = pahmasss + pahmassb

       tmassMRN =  Simass+ aCmass+ vgrmass+vsimass+ (pahmasss+pahmassb)
       tmassDark= fdark/(1.-fdark) * tmassMRN

       if(ibug .ge. 2) then
        write(6,*)       '    abusi    abuc_tot abucaC     abugr   abuvsi  abucpahs  abucpahb '
        write(6,'(7f9.1)') abusi, abuc_tot,   abuc,    abucvgr,   abuvsi, abucpahs, abucpahb
        write(6,*)        ' acmass,   Simass,   vgrmass,  vsimass,  pahmass,  tmassDark '
        write(6,'(1p6e10.2)') acmass,   Simass,   vgrmass,  vsimass,  pahmass, tmassDark    
        if(abs(tmassmrn+tmassDark-1.) .gt. 0.001) stop 'tot Mass MRN=Drak ne 1 '
        write(6,'(a30,4f7.2)')  ' Mass of MRN, Dark, Mtot, fDark : ', &
                                 tmassmrn, tmassDark, tmassmrn+tmassDark, fDark
     endif


    !  c
    !  c   a2 for geometrical cross section and qrmn already reduced by 1 above
    !  c   n(a) da = a^-(qmrn) da. 
    !  c   en1c    = Proportionalitats-Konstante fur Grossenverteilung
    !  c   ahc(l)  = geometrischer Querschnitt der Graphitkorner mit Radius
    !  c             amrn(l) bez. auf 1 g IM, analog ahsi(l), ahd(l)
     
     
                    
           en1C = 0. 
           do  L = Lac, Lec 
             en1C = en1C + amrn(L)**(3d0-qmrn)
           end do
           en1C = 3d0/pi4/rhc / en1C    * aCmass
     
           en1Si = 0.
           do  L = Lasi, Lesi 
             en1Si = en1Si + amrn(L)**(3d0-qmrn)
           end do
           en1Si = 3d0/pi4/rhsi / en1Si * Simass

           en1d  = 0.
           do  L = Lad, Led 
             en1d = en1d + adark(L)**(3d0-qmrn)
           end do
           en1d  = 3d0/pi4/rhd / en1d   * tmassDark
     
           do  L = Lac, Lec
             ahc(L) = en1C * pi * amrn(L)**(2d0-qmrn)
    !  c        write(6,'(1p3e9.2)') en1c, amrn(l), ahc(l)
           end do
     
           do  L     = Lasi, Lesi
             ahsi(L)  = en1Si * pi * amrn(L)**(2d0-qmrn)
           end do
     
           do  L   = Lad, Led
              ahd(L) = en1d * pi * adark(L)**(2d0-qmrn)
    !  c        write(6,'(1p3e9.2)') en1d, adark(l), ahd(l)
           end do



           call locat(adark, lrd, arad_polmin_aC, l)
           fak  = abs(adark(l)   /arad_polmin_aC -1.)
           fak1 = abs(adark(l+1) /arad_polmin_aC -1.)
           if(fak1 .lt. fak .and. l .lt. lrd-1) l  = l+1
           arad_polmin_aC = adark(l)
   
           call locat(adark, lrd, arad_polmin_Si, l)
           fak  = abs(adark(l)   /arad_polmin_Si -1.)
           fak1 = abs(adark(l+1) /arad_polmin_Si -1.)
           if(fak1 .lt. fak .and. l .lt. lrd-1) l  = l+1
           arad_polmin_Si = adark(l)
           
           call locat(adark, lrd, arad_polmax, l)
           fak   = abs(adark(l)   /arad_polmax -1.)
           fak1  = abs(adark(l+1)/arad_polmax -1.)
           if(fak1 .lt. fak .and. l .lt. lrd-1) l  = l+1
           arad_polmax = min(arad_polmax, adark(led))


        !    c       write(6,'(a40,1x,1p2e10.3)') '  Min grain alignment radius (cm) aC, SI : ', arad_polmin_aC, arad_polmin_Si
        !    c       write(6,'(a40,1x,1p2e10.3)') '  Max grain alignment radius (cm) used   : ', arad_polmax
           
                         
                 do        l   = Lac, Lec 
                   do      k   = 1, mm
                   qabs(k)     = qac(L,k)
                   qsca(k)     = qsc(L,k)
                   if(iblack .eq. 1)   qabs(k) = 1d0
                   if(iblack .eq. 1)   qsca(k) = 1d0
                   if(qabs(k) .eq.0.) stop 'qabs =0'
                   sigs_aC(k)  = sigs_aC(k)  + ahc(L) * qsca(k)
                   siga_aC(k)  = siga_aC(k)  + ahc(L) * qabs(k) 
                   if(amrn(l).ge.arad_polmin_ac .and. amrn(l).le.arad_polmax) then
                    sigp_aC(k)  = sigp_aC(k)  + ahc(L) * qpc(L,k)
                    sigcp_aC(k) = sigcp_aC(k) + ahc(L) * qcpc(L,k)
                   endif
        !    c        write(71,'(1p3e12.3)') wel(k), amrn(l), ahc(L)*qpc(L,k)
                 end do
                 end do

                 do          l = lasi, lesi
                    do        k = 1, mm
                    qabs(k)     = qasi(L,K)
                    qsca(k)     = qssi(L,K)
                    if(iblack .eq. 1)   qabs(k) = 1d0
                    if(iblack .eq. 1)   qsca(k) = 1d0
                    if(qabs(k) .eq.0.) stop 'qabs =0'
                    sigs_Si(k)  = sigs_Si(k)  + ahsi(L) * qsca(k)
                    siga_Si(k)  = siga_Si(k)  + ahsi(L) * qabs(k) 
                    if(amrn(l) .ge. arad_polmin_Si .and. amrn(l) .le.arad_polmax) then
                     sigp_Si(k)  = sigp_Si(k)  + ahsi(L) * qpsi(L,k)
                     sigcp_si(k) = sigcp_si(k) + ahsi(L) * qcpsi(L,k)
                    endif
                  end do
                  end do


                  do          l = lad, led
                    do         k = 1, mm
                     qabs(k)     = qad(L,K)
                     qsca(k)     = qsd(L,K)
                     if(iblack .eq. 1)   qabs(k) = 1d0
                     if(iblack .eq. 1)   qsca(k) = 1d0
                     if(qabs(k) .eq.0.) stop 'qabs =0'
                     sigs_d(k)   = sigs_d(k)  + ahd(L) * qsca(k)
                     siga_d(k)   = siga_d(k)  + ahd(L) * qabs(k)
                     if(adark(l) .ge. arad_polmin_aC .and. adark(l) .le.arad_polmax) then
                      sigp_d(k)  = sigp_d(k)  + ahd(L) * qpd(L,k)
                      sigcp_d(k) = sigcp_d(k) + ahd(L) * qcpd(L,k)
                     endif
                    end do
                   end do


                !    c
                !    c ----------------------------------------------------------------------------
                !    c            Optics of  Very Small Grains  (vsg)
                !    c ----------------------------------------------------------------------------
                !    c   abucvgr  = abundance of very small graphite in ppm.
                !    c   abuvsi   = abundance of very small silicates in ppm.
                !    c   material = 0 bedeutet:  vsg sind aus Graphit.
                !    c   material = 1 bedeutet:  vsg sind aus Silicates
                !    c   xatom    = No of C 

                   if(ispecvsg .ne. 1) goto 75
                   fL        = 0. 
                   do  L = Lav , Lev
                    fL = fL + avsg(L)**(3d0 - qvsg)
                   end do
                   fakgr = 3d0*vgrmass /pi4 /rhgr /fl
                   faksi = 3d0*vsimass /pi4 /rhvsi / fL
                   do  L = Lav, Lev
                    ! print*, fakgr, avsg(l), qvsg
                    ahvgr(L) = fakgr /avsg(l)**qvsg * pi*avsg(L)**2
                    if(ibug.ge.2) write(166,'(a6,1p2e11.3)') ' vGr:', avsg(l),  ahvgr(l)
                   end do
                   do  L = Lav, Lev
                    ahvsi(L) = faksi /avsg(l)**qvsg * pi*avsg(L)**2
                    if(ibug.ge.2) write(166,'(a6,1p2e11.3)') ' vSi:', avsg(l),  ahvsi(l)
                   end do
                   do k    = 1, mm
                    do  Lrad = Lav, Lev 
                     siga_vgr(k) = siga_vgr(k) + ahvgr(Lrad) * qagr(lrad,k)
                     sigs_vgr(k) = sigs_vgr(k) + ahvgr(Lrad) * qsgr(lrad,k)
                     siga_vsi(k) = siga_vsi(k) + ahvsi(Lrad) * qavsi(lrad,k)
                     sigs_vsi(k) = sigs_vsi(k) + ahvsi(Lrad) * qsvsi(lrad,k)
                    end do
                   end do
              75     continue

            !   c ----------------------------------------------------------------------------
            !   c      Optics of  P A Hs         (PAHs bestehen aus C  ==>  material = 0)
            !   c ----------------------------------------------------------------------------
            !   c hier:   xatom    = No of C  + H atoms
            !   c
              
                      if (ispecpah .eq. 0)   go to 1503
                      
            !   c      write(6,*)'***   P A H              ***'
                    material  = 0
                    zcpah   = zcpahs
                    zhpah   = zhpahs
                    xatom   = zcpah + zhpah
                    abucpah = abucpahs
                    arad    = sqrt(zcpah/1.2) * 1d-8
                    if(ibug .ge. 2) then 
                     write(6,*) '      small PAH: zcpah, zhpah'
                     write(6,'(16x,2f7.1)')  zcpah, zhpah
                    end if
              
                    pahmass = (1.-fdark) * abucpah * wmolc  / (abuc_tot*wmolc + abusi *wmolsi  + &
                                                                  abuvsi*wmolvsi)
                    
                    ahpah   = pahmass/(wmolc*zcpah*protm)  * pi*arad**2 
                    ahpahs  = ahpah
                     if(ibug.gt.1) write(166,'(a6,1p2e11.3)') ' PAH:', arad, ahpah
              
                     call pah_wq
              
            !   c     Qabs war bislang der WQ wird nun zu einer Effiziens C=pi a^2*Q umdefiniert
            !   c      absorbtion cross section per gramm dust
                     do        k  = 1, mm
                      qabs(k)     = qabs(k) / pi / arad**2
                      siga_pah(k) = qabs(k) * ahpah
                      qabspahs(k) = qabs(k)
                      siga_pahs(k)= siga_pah(k)
                    end do
              
                    if(ibug .gt. 1) then         
                       write(6,*) pahmass, arad, ahpah
                       write(36,*) ' wel(k), qabs(k), siga_pah(k) '
                       write(36,'(1p3e10.2)') (wel(k), qabs(k), siga_pah(k), k=1,mm)
                    endif
            !   c
            !   c   Big PAHs: Qabs war bislang der WQ wird nun zu einer Effiziens C=pi a^2*Q umdefiniert
            !   c             absorbtion cross section per gramm dust
              
                    if(zcpahb .gt. 10.) then 
                     zcpah   = zcpahb
                     zhpah   = zhpahb
                     xatom   = zcpah + zhpah
                     abucpah = abucpahb
                     arad    = sqrt(zcpah/1.2) * 1d-8
                          write(6,*) '      PAH cluster: zcpah, zhpah'
                          write(6,'(16x,2f7.1)')  zcpah, zhpah
                          
                     pahmass = (1.-fdark) * abucpah * wmolc  / (abuc_tot*wmolc + abusi *wmolsi  + &
                                                                  abuvsi*wmolvsi)
                     ahpah   = pahmass/(wmolc*zcpah*protm)  * pi*arad**2 
                     ahpahb  = ahpah
              
                      call pah_wq
              
                     do        k  = 1, mm
                      qabs(k)     = qabs(k) / pi / arad**2
                      siga_pah(k) = qabs(k) * ahpah
                      qabspahb(k) = qabs(k)
                      siga_pahb(k)= siga_pah(k)
                     end do
              
                     if(ibug .gt. 2) then         
                       write(6,*) pahmass, arad, ahpah
                       write(36,*) ' wel(k), qabs(k), siga_pah(k) '
                       write(36,'(1p3e10.2)') (wel(k), qabs(k), siga_pah(k), k=1,mm)
                     endif
                    endif
              
               1503 continue
            !   c
            !   c     -------------------------------------------------------------
            !   c     Cross section [cm^2/g-dust] -- Notation: t=total, a_= abs, s_=sca
            !   c     sigt_l               refer to large + nano dust
            !   c     sigt_d=siga_d+sigs_d refer to      dark    dust 
            !   c     sigt total cross section requires computation of column densities
            !   c     Nl, Nd.  The column densities are given by input 2 parameters, the
            !   c     total optical depth tauV and the observed reddening Ebv_obs, where
            !   c     tauV is estimated from the GAIA distance.
            !   c     -------------------------------------------------------------
              
                    
              
                    do k         = 1,mm
                     sigt_l(k)   = siga_aC(k)  + sigs_aC(k)   + &
                                   siga_Si(k)  + sigs_Si(k)   + &
                                   siga_vgr(k) + sigs_vgr(k)  + &
                                   siga_vsi(k) + sigs_vsi(k)  + &
                                   siga_pahs(k)+ siga_pahb(k)
                     sigt_d(k)   = siga_d(k)   + sigs_d(k)
              
                    end do
              
                    sigt   = sigt_l + sigt_d
              
            !   c
            !   c     open(unit=8, file='./Output/Kappa_AvEbv.out', form='formatted')
            !   c      rewind 8
            !   c      write(8,*) '# Optical depth (Siebenmorgen 2023, A&A 670A,115; SC 2023 doi: 10.48550/arXiv.2311.03310)'
            !   c      write(8,*) '# wel(cm) sa_aC    ss_aC    sa_Si    ss_Si    sa_vgr   ss_vgr   sa_vsi   ss_vsi   s_pahS+B sa_Dark  ss_Dark sigt '
            !   c      write(8,*) '# ------------------------------------------------------------------------------------------------------------- '
            !   c      write(8,'(1p1e9.3,11e9.2, 1e10.3)') (wel(k), 
            !   c     $   siga_aC(k),    sigs_aC(k),  siga_Si(k), sigs_Si(k), 
            !   c     $   siga_vgr(k),  sigs_vgr(k), siga_vsi(k), sigs_vsi(k),
            !   c     $   siga_pahS(k)+siga_pahB(k),   siga_d(k), sigs_d(k), sigt(k), k=1,mm)
            !   c      close(8)
            !   c
                     
                    tbv     = ebv_obs/1.0857d0  
              
                    slb     = sigt_l(kblue) 
                    slv     = sigt_l(kvis)  
                    sdb     = sigt_d(kblue) 
                    sdv     = sigt_d(kvis)  
              
              
            !   c     cross section in V and B could be for dark dust identical hence:
                    if (abs(sdb-sdv) .lt. 1.e-4) then
                       rvd = sdv * 1e4
                    else 
                      rvd = sdv / (sdb-sdv)
                    endif
                      rvl = slv / (slb-slv)
                    
                    if(sdv .gt. 0.) xnd  = (tauv/sdv - rvl * tbv/sdv) / ( 1.d0 - rvl/rvd) ! nd aus Eq.5 in Eq.6 einsetzen und Rl,d einfuehren OK
                    
                    xnd  = max(xnd,0.)
                    xnl  = tauv/slv - xnd  * sdv/slv   ! Eq,5
                    xnl  = max(xnl,0.)
              
            !   c ! zur Probe      
                    xnd1 = tauv/sdv - xnl * slv/sdv    
                    if(abs(xnd1/xnd -1.) .gt. 1.d-4 .and. xnd1 .gt. 0.01 ) then
                       write(6,*) ' *** Check: cannot be nd ne nd1 ?'
                       write(6,'(a30, 1p2e12.3)') ' xnd, xnd1 = ', xnd, xnd1
            !   c        stop ' cannot be nd ne nd1 ?'
                    endif
              
                    
                    if(ibug .ge. 2) then
                     write(6,'(a40, 2f10.3)') ' slv, slv *xnl = ', slv, slv*xnl
                     write(6,'(a40, 2f10.3)') ' sdv, sdv *xnd = ', sdv, sdv*xnd
                     write(6,'(a40, 2f10.3)') ' slb, slb *xnl = ', slb, slb*xnl
                     write(6,'(a40, 2f10.3)') ' sdb, sdb *xnd = ', sdb, sdb*xnd
                     write(6,'(a40, 2f10.3)') ' tauv= sdv*xnd,slv*xnl ? ', tauv, slv*xnl+sdv*xnd 
                     write(6,'(a40, 1f10.3)') ' obs Ebv/1.086 = tbv = ', tbv
                    endif
                    
              
            !   c  optical depth = cross section (sig*)  times column (N*) 
                       sigt_l    = sigt_l    * xnl
                       siga_aC   = siga_aC   * xnl         
                       sigs_aC   = sigs_aC   * xnl
                       siga_Si   = siga_Si   * xnl
                       sigs_Si   = sigs_Si   * xnl
                       siga_vgr  = siga_vgr  * xnl
                       sigs_vgr  = sigs_vgr  * xnl
                       siga_vsi  = siga_vsi  * xnl
                       sigs_vsi  = sigs_vsi  * xnl
                       siga_pahS = siga_pahS * xnl
                       siga_pahB = siga_pahB * xnl 
                       siga_d    = siga_d    * xnd
                       sigs_d    = sigs_d    * xnd
                       sigt_d    = sigt_d    * xnd
              
              
                       sigp_ac   = sigp_ac   * xnl
                       sigp_si   = sigp_si   * xnl
                       sigcp_ac  = sigcp_ac  * xnl
                       sigcp_si  = sigcp_si  * xnl
                       sigp_d    = sigp_d    * xnd
                       sigcp_d   = sigcp_d   * xnd
              
              
                       
                       sigt      = sigt_l    + sigt_d         
                       
                       fdark     = (xnd/(xnl + xnd)  + fdark_old) / 2.d0
              
                       fdark     = max(fdark,0.001)
                       fdark     = min(fdark,0.999)
                       del       = abs(fdark/fdark_old - 1.)
              
                       if(fdark .le. 0.009 .and. iter .ge. 2) del =0.
                       if(ibug .ge. 2) &
                       write(6,'(a12,i3,a27, 4f9.2)') '   --- iter=',iter,' : Mmrn, Mdark, fdark, del', xnl/(xnl+xnd), xnd/(xnl+xnd), fdark, del
                    endif
                    enddo   ! end of iterative solution of Eq.A2 and Eq.A3
              
            !   c
            !   c -------------------------------------
            !   c
            !   c  total-to-slective extinction Rv_mod of the dust model:
            !   c
                     sigtV  = sigt(kvis)
                     Rv_mod = sigt(kvis) / (sigt(kblue) - sigt(kvis))
                     Ebv    = 2.5/alog(10.) * (sigt(kblue) - sigt(kvis))
              
            !   c
            !   c     check precission in using nl, nd in renormalization of sig*
            !   c
                     if (abs(Ebv_obs-Ebv) .gt. 0.01) &
                         write(6,'(a35, 0p3f10.3)') ' Precision: Ebv_obs, Ebv_mod, err(obs/mod) = ', &
                                   Ebv_obs, Ebv, Ebv_obs /Ebv
              
                            
                      if (abs((sigt_l(kvis)+sigt_d(kvis)) / tauV -1.) .gt. 0.001) &
                        write(6,'(a30, 1p3e12.3)') '  tauV, xnl*sl+xnd*sd, err = ',  &
                          tauV, sigt_l(kvis)+sigt_d(kvis), &
                           abs((sigt_l(kvis)+sigt_d(kvis)) / tauV -1.)
              
                     if(abs(tauV/tbv/Rv_mod -1.) .gt. 1e-3) then 
                      write(6,'(a40, 2f8.2)')    ' Rv = Rv_mod ?               = ', tauV/tbv, Rv_mod
                     endif 
              
              
              
                        write(6,*)       '    abusi    abuc_tot abucaC     abugr   abuvsi  abucpahs  abucpahb '
                        write(6,'(7f9.1)') abusi, abuc_tot,   abuc,    abucvgr,   abuvsi, abucpahs, abucpahb
                        write(6,*)        ' acmass,   Simass,   vgrmass,  vsimass,  pahmass,  tmassDark '
                        write(6,'(1p6e10.2)') acmass,   Simass,   vgrmass,  vsimass,  pahmass, tmassDark    
                        if(abs(tmassmrn+tmassDark-1.) .gt. 0.001) stop 'tot Mass MRN=Drak ne 1 '
                        write(6,'(a40,4f7.2)')  '  Mass of MRN, Dark, Mtot, fDark : ', &
                                                 tmassmrn, tmassDark, tmassmrn+tmassDark, fDark
                     write(6,'(a40, 1p2e12.3)') ' N_large+vsg,   N_dark       = ', xnl, xnd
                     write(6,'(a40, 0p3f8.2)')    ' E(B-V), tauV_tot, tauV_dark = ', ebv, sigtV, sigt_d(kvis)
                     write(6,'(a40, 1p3e12.3)') ' Mass_mrn, Mass_dark, fdark  = ', xnl/(xnl + xnd), xnd/(xnl + xnd), fdark
                     write(6,'(a40)') '     '
              
              
                     return
                     end