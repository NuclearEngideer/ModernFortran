PROGRAM ECHO_ROBOT
    IMPLICIT NONE
    CHARACTER(LEN=1000) :: text
    READ '(a)', TEXT
    PRINT '(a)', TRIM(TEXT)
END PROGRAM ECHO_ROBOT