MODULE MOD_DIFF

    ! This module returns the finite difference between values of some quantity "X"
    ! The righthand boundary condition is periodic with the last righthand value becoming
    ! the next leftmost value

    ! Import the standards for INT32 and REAL32 from this module
    USE ISO_FORTRAN_ENV, ONLY: INT32, REAL32
    IMPLICIT NONE

CONTAINS
    
    ! Same function from the ch03 example has been placed here.
    ! I guess the variables don't need to be defined prior to declaring function
    PURE FUNCTION DIFF(x) RESULT(DX)
        
        ! Basically REAL(4) might not be the same size across compilers, 
        ! so using REAL(REAL32) lets us use a standard we know won't change
        ! This allows us to have better portability and makes code more "watertight"
        REAL(REAL32), INTENT(IN) :: x(:)
        REAL(REAL32) :: DX(SIZE(X))
        INTEGER(INT32) :: IM
        IM = SIZE(X)
        DX(1)=X(1)-X(IM)
        DX(2:IM) = X(2:IM) - X(1:IM-1)
    END FUNCTION DIFF    

END MODULE MOD_DIFF
