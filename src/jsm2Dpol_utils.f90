MODULE JSM_UTILS
    IMPLICIT NONE
  CONTAINS
    
    SUBROUTINE PRINT_DUST_PARAMS(VVAC, POR, DEL0, AB)
        IMPLICIT NONE
        REAL(KIND=8), INTENT(IN) :: VVAC, POR, DEL0, AB
        CHARACTER(LEN=128) :: MSG

        WRITE(MSG, '(A, F4.2, A, F4.2, A, F4.1, A, F4.1)') &
            'Currently: vvac = ', VVAC, ', por = ', POR, ', del0 = ', DEL0, ', ab = ', AB
        PRINT *, TRIM(MSG)
    END SUBROUTINE PRINT_DUST_PARAMS

    SUBROUTINE READ_PARAMETERS_FROM_QFILES(VVAC, POR, DEL0, AB, STATUS)
        IMPLICIT NONE
        REAL(8), INTENT(OUT) :: VVAC, POR, DEL0, AB
        LOGICAL, INTENT(OUT) :: STATUS
    
        CHARACTER(LEN=80) :: lineSi, lineaC, lineDark
        CHARACTER(LEN=80) :: dummy
        REAL(8) :: porSi, poraC, vvacD
        INTEGER :: i, unitSi, unitAC, unitDark
        INTEGER :: kprol, idg
        CHARACTER(LEN=10) :: comp
    
        ! File units and paths
        unitSi   = 3
        unitAC   = 4
        unitDark = 7
    
        STATUS = .FALSE.
    
        OPEN(unitSi,   FILE='./Input/d.QellipSi',   FORM='formatted', STATUS='old', ERR=100)
        OPEN(unitAC,   FILE='./Input/d.QellipaC',   FORM='formatted', STATUS='old', ERR=100)
        OPEN(unitDark, FILE='./Input/d.QellipDark', FORM='formatted', STATUS='old', ERR=100)
    
        ! Read 3rd line from each file
        DO i = 1, 3
        READ(unitSi,   '(A)', ERR=200) lineSi
        READ(unitAC,   '(A)', ERR=200) lineaC
        READ(unitDark, '(A)', ERR=200) lineDark
        END DO
    
        ! Extract por and vvac from strings (look for "* por =" and "* vvac =")
        READ(lineSi(10:),  *, ERR=201) porSi
        READ(lineaC(10:),  *, ERR=201) poraC
        READ(lineDark(10:),*, ERR=201) vvacD
    
        IF (ABS(porSi - poraC) > 1e-6) THEN
        PRINT *, '❌ POR mismatch in d.QellipSi and d.QellipaC: ', porSi, poraC
        STATUS = .TRUE.
        RETURN
        END IF
    
        ! Set output values
        POR  = porSi
        VVAC = vvacD
    
        ! Skip to line 13 in d.QellipSi to read ab and del0
        DO i = 4, 12
        READ(unitSi, '(A)', ERR=202) dummy
        END DO
        READ(unitSi, '(I4, F4.2, A10, I2, F7.1)', ERR=203) kprol, AB, comp, idg, DEL0
    
        CLOSE(unitSi)
        CLOSE(unitAC)
        CLOSE(unitDark)
    
        RETURN
    
    100 CONTINUE
        PRINT *, '❌ Error: Could not open one of the Q files.'
        STATUS = .TRUE.
        RETURN
    
    200 CONTINUE
        PRINT *, '❌ Error reading header lines from Q files.'
        STATUS = .TRUE.
        RETURN
    
    201 CONTINUE
        PRINT *, '❌ Failed to parse POR or VVAC from line 3.'
        STATUS = .TRUE.
        RETURN
    
    202 CONTINUE
        PRINT *, '❌ Failed skipping lines to reach line 13.'
        STATUS = .TRUE.
        RETURN
    
    203 CONTINUE
        PRINT *, '❌ Failed to read AB/DEL0 on line 13 of d.QellipSi.'
        STATUS = .TRUE.
        RETURN
    
    END SUBROUTINE READ_PARAMETERS_FROM_QFILES

    SUBROUTINE INITIALIZE_DUST_PARAMETERS(VVAC, POR, DEL0, AB, STATUS)
        IMPLICIT NONE
        REAL(8), INTENT(OUT) :: VVAC, POR, DEL0, AB
        LOGICAL, INTENT(OUT) :: STATUS
        LOGICAL :: FILES_EXIST
        CHARACTER(LEN=256) :: QFILES_PATH
        CHARACTER(LEN=16) :: D_TAG
    
        STATUS = .FALSE.
    
        ! Check if all required files are present
        CALL CHECK_QFILE_EXISTENCE(FILES_EXIST)
    
        IF (FILES_EXIST) THEN
        PRINT *, '✅ Q files already present. Reading parameters from them.'
        CALL READ_PARAMETERS_FROM_QFILES(VVAC, POR, DEL0, AB, STATUS)
        IF (STATUS) THEN
            PRINT *, '❌ Failed reading parameters from Q files.'
            RETURN
        END IF
        ELSE
        PRINT *, '🔍 Q files not found. Detecting folder context and fetching files.'
    
        ! Try detecting folder type and extract parameters
        CALL DETECT_DARKFORS_FOLDER(VVAC, POR, DEL0, AB, STATUS, QFILES_PATH, D_TAG)
        IF (STATUS) CALL DETECT_MODEL_FOLDER(VVAC, POR, DEL0, AB, STATUS, QFILES_PATH, D_TAG)
        IF (STATUS) THEN
            PRINT *, '❌ Failed to detect folder structure.'
            RETURN
        END IF
    
        ! Fetch Q files based on detected values
        CALL COPY_QFILES(VVAC, POR, DEL0, AB, QFILES_PATH, D_TAG)
        ! CALL ENSURE_QFILES(VVAC, POR, DEL0, AB, QFILES_PATH)
        END IF
    
        ! Final check: validate Q file headers match parameters
        CALL VERIFY_POR_VVAC(POR, VVAC)
        CALL VERIFY_AB_DEL0(AB, DEL0)
    
    END SUBROUTINE INITIALIZE_DUST_PARAMETERS

    SUBROUTINE CHECK_QFILE_EXISTENCE(FILES_EXIST)
        IMPLICIT NONE
        LOGICAL, INTENT(OUT) :: FILES_EXIST
        LOGICAL :: EXISTS_SI, EXISTS_AC, EXISTS_DARK
        CHARACTER(LEN=*), PARAMETER :: INPUTS_DIR = './Input/'
        CHARACTER(LEN=256) :: FILE_SI, FILE_AC, FILE_DARK

        FILES_EXIST = .FALSE.
        FILE_SI   = INPUTS_DIR // 'd.QellipSi'
        FILE_AC   = INPUTS_DIR // 'd.QellipaC'
        FILE_DARK = INPUTS_DIR // 'd.QellipDark'
        
        ! Check if files already exist
        INQUIRE(FILE=FILE_SI,   EXIST=EXISTS_SI)
        INQUIRE(FILE=FILE_AC,   EXIST=EXISTS_AC)
        INQUIRE(FILE=FILE_DARK, EXIST=EXISTS_DARK)
        
        IF (.NOT. (EXISTS_SI .AND. EXISTS_AC .AND. EXISTS_DARK)) THEN
            PRINT *, 'Some d.Q files are missing.'
        ELSE    
            FILES_EXIST = .TRUE.
        END IF
    END SUBROUTINE CHECK_QFILE_EXISTENCE

    SUBROUTINE VERIFY_AB_DEL0(AB, DEL0)
        IMPLICIT NONE
        REAL(8), INTENT(IN) :: AB, DEL0
        INTEGER :: J, KPROL, IDG, IOS
        REAL(8) :: AB_FILE, DEL0_FILE
        CHARACTER(LEN=10) :: COMP
        CHARACTER(LEN=80) :: CDUM
        CHARACTER(LEN=*), PARAMETER :: FNAME = './Input/d.QellipSi'
        INTEGER :: UNIT
    
        UNIT = 3
        OPEN(UNIT=UNIT, FILE=FNAME, FORM='formatted', STATUS='old', IOSTAT=IOS)
        IF (IOS /= 0) THEN
        PRINT *, '❌ Cannot open file:', FNAME
        RETURN
        END IF
    
        ! Skip first 12 lines
        DO J = 1, 12
        READ(UNIT, '(A)', IOSTAT=IOS) CDUM
        IF (IOS /= 0) THEN
            PRINT *, '❌ Error reading header line ', J
            CLOSE(UNIT)
            RETURN
        END IF
        END DO
    
        ! Line 13: read shape, ab, comp, idg, del0
        READ(UNIT, '(I4, F4.2, A10, I2, F7.1)', IOSTAT=IOS) KPROL, AB_FILE, COMP, IDG, DEL0_FILE
        IF (IOS /= 0) THEN
        PRINT *, '❌ Failed to read ab/del0 from line 13 of d.QellipSi'
        CLOSE(UNIT)
        RETURN
        END IF
    
        CLOSE(UNIT)
    
        ! Compare and report
        IF (ABS(AB - AB_FILE) > 1.0D-6) THEN
        PRINT *, '❌ AB mismatch:', 'expected=', AB, ' found=', AB_FILE
        ELSE
        PRINT *, '✅ AB check passed:', AB
        END IF
    
        IF (ABS(DEL0 - DEL0_FILE) > 1.0D-6) THEN
        PRINT *, '❌ DEL0 mismatch:', 'expected=', DEL0, ' found=', DEL0_FILE
        ELSE
        PRINT *, '✅ DEL0 check passed:', DEL0
        END IF
    
    END SUBROUTINE VERIFY_AB_DEL0

    SUBROUTINE VERIFY_POR_VVAC(POR, VVAC)
        IMPLICIT NONE
        REAL(8), INTENT(IN) :: POR, VVAC
        CHARACTER(LEN=200) :: LINE
        REAL(8) :: POR_SI, POR_AC, VVAC_DARK
        INTEGER :: I, IOS
        CHARACTER(LEN=*), PARAMETER :: INPUTS_DIR = './Input/'
    
        ! Open and check d.QellipSi
        OPEN(UNIT=3, FILE=INPUTS_DIR // 'd.QellipSi', FORM='formatted', STATUS='old', IOSTAT=IOS)
        IF (IOS /= 0) THEN
        PRINT *, '❌ Cannot open d.QellipSi'
        RETURN
        END IF
        DO I = 1, 3
        READ(3, '(A)', IOSTAT=IOS) LINE
        END DO
        READ(LINE(11:), *) POR_SI
        CLOSE(3)
    
        ! Open and check d.QellipaC
        OPEN(UNIT=4, FILE=INPUTS_DIR // 'd.QellipaC', FORM='formatted', STATUS='old', IOSTAT=IOS)
        IF (IOS /= 0) THEN
        PRINT *, '❌ Cannot open d.QellipaC'
        RETURN
        END IF
        DO I = 1, 3
        READ(4, '(A)', IOSTAT=IOS) LINE
        END DO
        READ(LINE(11:), *) POR_AC
        CLOSE(4)
    
        ! Open and check d.QellipDark
        OPEN(UNIT=7, FILE=INPUTS_DIR // 'd.QellipDark', FORM='formatted', STATUS='old', IOSTAT=IOS)
        IF (IOS /= 0) THEN
        PRINT *, '❌ Cannot open d.QellipDark'
        RETURN
        END IF
        DO I = 1, 3
        READ(7, '(A)', IOSTAT=IOS) LINE
        END DO
        READ(LINE(11:), *) VVAC_DARK
        CLOSE(7)
    
        ! Compare values
        IF (ABS(POR_SI - POR_AC) > 1.0D-6) THEN
        PRINT *, '❌ POR mismatch between Si and aC files:', POR_SI, POR_AC
        ELSE IF (ABS(POR - POR_SI) > 1.0D-6) THEN
        PRINT *, '❌ Input POR does not match files:', POR, POR_SI
        ELSE
        PRINT *, '✅ POR check passed:', POR
        END IF
    
        IF (ABS(VVAC - VVAC_DARK) > 1.0D-6) THEN
        PRINT *, '❌ Input VVAC does not match file:', VVAC, VVAC_DARK
        ELSE
        PRINT *, '✅ VVAC check passed:', VVAC
        END IF
    
    END SUBROUTINE VERIFY_POR_VVAC


    SUBROUTINE ENSURE_QFILES(VVAC, POR, DEL0, AB, QFILES_PATH)
        IMPLICIT NONE
        REAL(KIND=8), INTENT(OUT) :: VVAC, POR, DEL0, AB
        CHARACTER(LEN=256) :: QFILES_PATH
        CHARACTER(LEN=16) :: D_TAG
        LOGICAL :: STATUS
    
        CALL DETECT_DARKFORS_FOLDER(VVAC, POR, DEL0, AB, STATUS, QFILES_PATH, D_TAG)
        IF (.NOT. STATUS) THEN
            CALL COPY_QFILES(VVAC, POR, DEL0, AB, QFILES_PATH, D_TAG)
            RETURN
        ENDIF

        CALL DETECT_MODEL_FOLDER(VVAC, POR, DEL0, AB, STATUS, QFILES_PATH, D_TAG)
        IF (.NOT. STATUS) THEN
            CALL COPY_QFILES(VVAC, POR, DEL0, AB, QFILES_PATH, D_TAG)
            RETURN
        ENDIF

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

    SUBROUTINE COPY_QFILES(VVAC, POR, DEL0, AB, QFILES_PATH, D_TAG)
        IMPLICIT NONE
        REAL(KIND=8), INTENT(IN) :: VVAC, POR, DEL0, AB
        ! LOGICAL, INTENT(IN)      :: MODELFOLDER

        CHARACTER(LEN=512) :: SRCDIR, DESTDIR, THISDIR
        CHARACTER(LEN=128) :: SRCNAME(3), DESTNAME(3)
        CHARACTER(LEN=512) :: SRCPATH, DESTPATH
        INTEGER :: I, DVAL
        CHARACTER(LEN=4) :: VSTR, PSTR, DSTR
        CHARACTER(LEN=5) :: ABSTR
        CHARACTER(*), INTENT(IN), OPTIONAL :: D_TAG
        CHARACTER(*), INTENT(IN) :: QFILES_PATH
        LOGICAL :: STATUS
        CHARACTER(LEN=16) :: TAG_USED
        CALL GETCWD(THISDIR)
        
        WRITE(VSTR, '(I2.2)') INT(VVAC * 100.0D0)
        WRITE(PSTR, '(I2.2)') INT(POR * 100.0D0)
        WRITE(ABSTR, '(F3.1)') AB
        
        IF (PRESENT(D_TAG) .AND. LEN_TRIM(D_TAG) > 0) THEN
            TAG_USED = TRIM(D_TAG)
        ELSE
            DVAL = INT(DEL0)
            WRITE(DSTR, '(I2.2)') DVAL
            TAG_USED = 'd' // TRIM(DSTR)
            ! WRITE(DSTR, '(I2.2)') INT(DEL0)
            ! TAG_USED = 'd' // TRIM(DSTR)
        ENDIF
        
        ! PRINT *, 'DEL0', DEL0
        ! PRINT *, 'D_TAG', D_TAG
        ! PRINT *, 'TAG_USED', TAG_USED
        SRCNAME(1) = 'd.Dk' // TRIM(VSTR) // 'ab' // TRIM(ABSTR) // TAG_USED
        SRCNAME(2) = 'd.Si' // TRIM(PSTR)  // 'ab' // TRIM(ABSTR) // TAG_USED
        SRCNAME(3) = 'd.aC' // TRIM(PSTR)  // 'ab' // TRIM(ABSTR) // TAG_USED

        DESTNAME(1) = 'd.QellipDark'
        DESTNAME(2) = 'd.QellipSi'
        DESTNAME(3) = 'd.QellipaC'

        SRCDIR = TRIM(THISDIR) // TRIM(QFILES_PATH)
        ! IF (MODELFOLDER) THEN
            ! SRCDIR = TRIM(THISDIR) // '/../Qfile_tv'
        ! ELSE
            ! SRCDIR = TRIM(THISDIR) // '/../../Qfile_tv'
        ! END IF

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


    SUBROUTINE DETECT_DARKFORS_FOLDER(VVAC, POR, DEL0, AB, STATUS, QFILES_PATH, D_TAG)
        IMPLICIT NONE
        REAL(KIND=8), INTENT(OUT) :: VVAC, POR, DEL0, AB
        LOGICAL, INTENT(OUT) :: STATUS
        CHARACTER(LEN=256), INTENT(OUT) :: QFILES_PATH
        CHARACTER(LEN=512) :: FULLPATH
        CHARACTER(LEN=256) :: LAST, PARENT, DEL0_STR, AB_STR, POR_STR
        INTEGER :: POSD, POSAB, POSPDG, LAST_SLASH, STAT
        REAL(KIND=8) :: TMP
        CHARACTER(LEN=16), INTENT(OUT) :: D_TAG
        LOGICAL :: STATUS_TAG

        VVAC = 0.0D0
        POR = 0.0D0
        DEL0 = 0.0D0
        AB   = 0.0D0
        STATUS = .FALSE.
        QFILES_PATH = '/../../Qfile_tv/'

        CALL GETCWD(FULLPATH)

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


        CALL EXTRACT_D_TAG(LAST, D_TAG, STATUS_TAG)
        ! PRINT *, D_TAG
        ! Extract DEL0
        IF (D_TAG == 'pdg') THEN
            POSPDG = INDEX(LAST, 'pdg')
            POSAB  = INDEX(LAST, 'ab')
            DEL0_STR = LAST(POSPDG+3:POSAB-1)
            READ(DEL0_STR, *, IOSTAT=STAT) TMP
            IF (STAT /= 0) THEN
                PRINT *, '⚠️  Failed to parse DEL0 from "' // TRIM(DEL0_STR) // '"'
                STATUS = .TRUE.
                RETURN
            END IF
            DEL0 = TMP
            ! D_TAG = TRIM(LAST(POSPDG:POSAB-1))
        ELSE IF (D_TAG(1:1) == 'd') THEN
            READ(D_TAG(2:), *, IOSTAT=STAT) TMP
            IF (STAT /= 0) THEN
                PRINT *, '⚠️  Failed to parse DEL0 from "' // TRIM(D_TAG) // '"'
                STATUS = .TRUE.
                RETURN
            END IF
            DEL0 = TMP
            D_TAG = TRIM(LAST(POSD:POSAB-1))
            PRINT *, D_TAG
        END IF



        ! Extract AB_STR based on "ab"
        POSAB = INDEX(LAST, 'ab')
        IF (POSAB == 0) THEN
            PRINT *, '⚠️  Warning: Missing "ab" in folder name.'
            STATUS = .TRUE.
            RETURN
        END IF
        AB_STR = LAST(POSAB+2:)
        
        ! Parse AB
        READ(AB_STR, *, IOSTAT=STAT) TMP
        IF (STAT /= 0) THEN
            PRINT *, '⚠️  Warning: Failed to read AB value.'
            STATUS = .TRUE.
            RETURN
        END IF
        AB = TMP
        
        PRINT *, 'DarkFors folder structure detected: ', TRIM(FULLPATH)
        ! PRINT *, TRIM(FULLPATH)
    END SUBROUTINE DETECT_DARKFORS_FOLDER


    SUBROUTINE EXTRACT_D_TAG(FOLDER_NAME, D_TAG, STATUS)
        CHARACTER(LEN=*), INTENT(IN)  :: FOLDER_NAME
        CHARACTER(LEN=*), INTENT(OUT) :: D_TAG
        LOGICAL, INTENT(OUT)          :: STATUS

        INTEGER :: POSPDG, POSD, POSAB

        STATUS = .FALSE.
        D_TAG = ''

        POSAB = INDEX(FOLDER_NAME, 'ab')
        IF (POSAB == 0) THEN
            STATUS = .TRUE.
            RETURN
        END IF

        POSPDG = INDEX(FOLDER_NAME, 'pdg')
        POSD   = INDEX(FOLDER_NAME, 'd')

        IF (POSPDG > 0 .AND. POSPDG < POSAB) THEN
            D_TAG = 'pdg'
        ELSE IF (POSD > 0 .AND. POSD < POSAB) THEN
            D_TAG = FOLDER_NAME(POSD:POSAB-1)  ! e.g., 'd00.2'
        ELSE
            STATUS = .TRUE.
        END IF
    END SUBROUTINE


    SUBROUTINE DETECT_MODEL_FOLDER(VVAC, POR, DEL0, AB, STATUS, QFILES_PATH, D_TAG)
        IMPLICIT NONE
        REAL(KIND=8), INTENT(OUT) :: VVAC, POR, DEL0, AB
        LOGICAL, INTENT(OUT) :: STATUS
        CHARACTER(LEN=256), INTENT(OUT) :: QFILES_PATH
        CHARACTER(LEN=512) :: FULLPATH
        CHARACTER(LEN=16), INTENT(OUT) :: D_TAG
        INTEGER :: STAT
        CHARACTER(LEN=128) :: MSG
        STATUS = .FALSE.
        ! D_TAG = ''
        CALL GET_ENVIRONMENT_VARIABLE("PWD", FULLPATH, STATUS=STAT)
    
        IF (STAT /= 0) RETURN
    
        IF (INDEX(FULLPATH, 'Model24stars') > 0) THEN
            VVAC = 0.10D0
            POR  = 0.10D0
            DEL0 = 10.0D0
            AB   = 2.5D0
            D_TAG = 'd10'
            QFILES_PATH = '/../Qfile_tv/'
            PRINT *, "Model folder detected: ", TRIM(FULLPATH)
            WRITE(MSG, '(A, F4.2, A, F4.2, A, F4.1, A, A)') &
            'Setting vvac = ', VVAC, ', por = ', POR, ', ab = ', AB, ', d_tag = ', TRIM(D_TAG)
            PRINT *, TRIM(MSG)
            RETURN
        END IF

        STATUS = .TRUE.
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




    ! c
    ! c --------------------------------------------------------
    ! c






    ! c
    ! c --------------------
    ! c

    ! c  *********************************************************************

    ! c  *********************************************************************
    

END MODULE JSM_UTILS