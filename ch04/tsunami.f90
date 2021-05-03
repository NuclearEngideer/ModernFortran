PROGRAM TSUNAMI
    USE ISO_FORTRAN_ENV, ONLY: INT16, REAL32
    IMPLICIT NONE

    INTEGER(INT16) :: N
    INTEGER(INT16), PARAMETER :: GRID_SIZE = 100
    INTEGER(INT16), PARAMETER :: NUM_TIME_STEPS = 100
    REAL(REAL32), PARAMETER :: dt = 1, dx = 1, c = 1 
   
    ! the "parameter" identifies constants that cannot be changed
    ! by the program -- also requires assignment on declaration line
   
    REAL(REAL32) :: h(GRID_SIZE)  ! Array of length grid size (100) 
                         ! representing water height

    !initializing gaussian water height stuff
    integer(INT16),parameter :: icenter = 25
    real(REAL32), parameter :: decay = 0.02
   
    
    ! provide checks for various input parameters
    if (GRID_SIZE <= 0) stop 'GRID_SIZE MUST BE > 0'
    if (dt <= 0) STOP 'TIME STEP dt MUST BE > 0'
    if (dx <= 0) STOP 'GRID SPACING dx MUST BE > 0'
    if (c <= 0) STOP 'BACKGROUND FLOW SPEED c MUST BE > 0'
    if (c >= 3e8) STOP "YOU CAN'T JUST GO FASTER THAN THE SPEED OF &
          &LIGHT!!!"
    
    CALL SET_GAUSSIAN(H, ICENTER, DECAY)

    !Done w/ initial gaussian water height

    print *, 0, h  


    ! iterate water solution forward in time
    time_loop: do n=1, num_time_steps
        
        h = h-c*diff(h)/dx*dt
        print *, n, h

    enddo time_loop

    CONTAINS
    
    PURE SUBROUTINE SET_GAUSSIAN(X, ICENTER, DECAY)
        REAL(REAL32), INTENT(INOUT) :: X(:)
        INTEGER(INT16), INTENT(IN) :: ICENTER
        REAL(REAL32), INTENT(IN) :: DECAY
        INTEGER(INT16) :: I
        
        ! do concurrent parallizes this generation
        ! note different syntax
        DO CONCURRENT(I=1:SIZE(X))
            X(I) = EXP(-DECAY * (I-ICENTER)**2)
        ENDDO
    END SUBROUTINE SET_GAUSSIAN

    PURE FUNCTION DIFF(x) RESULT(DX)
        REAL(REAL32), INTENT(IN) :: x(:)
        REAL(REAL32) :: DX(SIZE(X))
        INTEGER(INT16) :: IM
        IM = SIZE(X)
        DX(1)=X(1)-X(IM)
        DX(2:IM) = X(2:IM) - X(1:IM-1)
    END FUNCTION DIFF    
    
END PROGRAM TSUNAMI
