PROGRAM DASHBOARD
    USE ISO_FORTRAN_ENV, ONLY: DASH => OUTPUT_UNIT
    IMPLICIT NONE

    REAL :: LAT = 59.329444, LON = 18.068611, ALT = 11678.3
    INTEGER :: ENG(4) = [96,96, 95, 97]
    LOGICAL :: AIRBORNE = .TRUE.

    CHARACTER(LEN=:), ALLOCATABLE :: DASHFMT

    DASHFMT = '(2(F9.5, 2X), F7.1, 2X, 4(I3.3, 2X), L)'
    WRITE (DASH, DASHFMT) LAT, LON, ALT, ENG, AIRBORNE
END PROGRAM DASHBOARD
