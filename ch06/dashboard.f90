PROGRAM DASHBOARD
    USE ISO_FORTRAN_ENV, ONLY: DASH => OUTPUT_UNIT
    IMPLICIT NONE

    REAL :: LAT = 59.329444, LON = 18.068611, ALT = 11678.3
    ! Aircraft latitude, longitude, and altitude
    INTEGER :: ENG(4) = [96,96, 95, 97]
    ! engine load 0-100
    LOGICAL :: AIRBORNE = .TRUE.
    ! crashed?

    CHARACTER(LEN=:), ALLOCATABLE :: DASHFMT

    ! DASHFMT is the formatting string we give to the write statement
    ! formatting string has comma-separated substrings
    ! letters are data types, numbers instruct spaces provided for output
    ! or how many times to repeat the formatting instructions
    DASHFMT = '(2(F9.5, 2X), F7.1, 2X, 4(I3.3, 2X), L)'
    WRITE (DASH, DASHFMT) LAT, LON, ALT, ENG, AIRBORNE

    ! REAL breakdown: lattitude can be -90 to 90 deg and -180, 180 deg
    ! so need 3 characters left of decimal place. Decimal counts as a character,
    ! so F9.5 is 5 digits after decimal place and 3 integers left of decimal 
    ! (or a negative sign for lat i guess)
    ! REAL should be formatted with f or e (for 5.23e3 sort of stuff)
    ! e also has an en and es format
    ! seems like e does 0.123E+3
    !           es does 1.23E+2
    !           en does 12.3E+1
    ! Maybe

    ! the 2X is two space characters so 2(F9.5, 2X) is two sets of reals with 9 total chars
    ! with 5 characters left of the decimal with that real followed by 2 spaces

    ! INTEGER for an integer like 34, applying i5 would be '   34' (three leading spaces) and i5.5 would be
    ! 00034. For engine load it's 0-100 and we always want 3 digits, hence i3.3. Then we've got the
    ! same deal as the reals with the 2x and then it's done 4 times cause of the eng(4) array

    ! Characters are formatted with "a" and logicals with "l"
    ! if a7 with "hello world" you'd have "hello w"

    ! CHECK TABLE 6.1 FOR FORMATTING STRING SYNTAX
    
END PROGRAM DASHBOARD
