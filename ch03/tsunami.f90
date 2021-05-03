PROGRAM TSUNAMI
    IMPLICIT NONE

    INTEGER :: N
    INTEGER, PARAMETER :: GRID_SIZE = 100
    INTEGER, PARAMETER :: NUM_TIME_STEPS = 100
    REAL, PARAMETER :: dt = 1, dx = 1, c = 1 
   
    ! the "parameter" identifies constants that cannot be changed
    ! by the program -- also requires assignment on declaration line
   
    REAL :: h(GRID_SIZE)  ! Array of length grid size (100) 
                         ! representing water height

    !initializing gaussian water height stuff
    integer,parameter :: icenter = 25
    real, parameter :: decay = 0.02
   
    
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
        REAL, INTENT(INOUT) :: X(:)
        INTEGER, INTENT(IN) :: ICENTER
        REAL, INTENT(IN) :: DECAY
        INTEGER :: I
        
        ! do concurrent parallizes this generation
        ! note different syntax
        DO CONCURRENT(I=1:SIZE(X))
            X(I) = EXP(-DECAY * (I-ICENTER)**2)
        ENDDO
    END SUBROUTINE SET_GAUSSIAN

    PURE FUNCTION DIFF(x) RESULT(DX)
        REAL, INTENT(IN) :: x(:)
        REAL :: DX(SIZE(X))
        INTEGER :: IM
        IM = SIZE(X)
        DX(1)=X(1)-X(IM)
        DX(2:IM) = X(2:IM) - X(1:IM-1)
    END FUNCTION DIFF    
    
END PROGRAM TSUNAMI
