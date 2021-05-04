MODULE MOD_INITIAL
    
    ! This module defines the initial conditions of the system, namely the gaussian profile
    ! of the water height in this example

    ! See comments in MOD_DIFF.f90 for explanation on the standard below
    USE ISO_FORTRAN_ENV, ONLY: INT32, REAL32
    IMPLICIT NONE

CONTAINS

    PURE SUBROUTINE SET_GAUSSIAN(X, ICENTER, DECAY)
        REAL(REAL32), INTENT(INOUT) :: X(:)
        INTEGER(INT32), INTENT(IN) :: ICENTER
        REAL(REAL32), INTENT(IN) :: DECAY
        INTEGER(INT32) :: I
        
        ! do concurrent parallizes this generation
        ! note different syntax
        DO CONCURRENT(I=1:SIZE(X))
            X(I) = EXP(-DECAY * (I-ICENTER)**2)
        ENDDO
    END SUBROUTINE SET_GAUSSIAN
    
END MODULE MOD_INITIAL
