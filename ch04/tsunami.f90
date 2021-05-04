PROGRAM TSUNAMI

    USE ISO_FORTRAN_ENV, ONLY: INT32, REAL32
    USE MOD_DIFF, ONLY: DIFF => DIFF_CENTERED 
    USE MOD_INITIAL, ONLY: SET_GAUSSIAN
    
    IMPLICIT NONE

    INTEGER(INT32) :: N
    INTEGER(INT32), PARAMETER :: GRID_SIZE = 100
    INTEGER(INT32), PARAMETER :: NUM_TIME_STEPS = 5000
    
    REAL(REAL32), PARAMETER :: dt = 0.02, dx = 1, g = 9.8
    REAL(REAL32), PARAMETER :: hmean = 10
   
    ! the "parameter" identifies constants that cannot be changed
    ! by the program -- also requires assignment on declaration line
   
    REAL(REAL32) :: h(GRID_SIZE), u(GRID_SIZE)  ! Array of length grid size (100) 
                                                ! representing water height
                                                ! and velocity

    !initializing gaussian water height stuff
    integer(INT32), parameter :: icenter = 25
    real(REAL32), parameter :: decay = 0.02
    
    ! provide checks for various input parameters
    if (GRID_SIZE <= 0) stop 'GRID_SIZE MUST BE > 0'
    if (dt <= 0) STOP 'TIME STEP dt MUST BE > 0'
    if (dx <= 0) STOP 'GRID SPACING dx MUST BE > 0'
    
    ! We can now call this subroutine out of the module we imported above
    CALL SET_GAUSSIAN(H, ICENTER, DECAY)
    u = 0 ! Initializes velocity vector to 0s
    
    !Done w/ initial gaussian water height

    print *, 0, h  

    ! iterate water solution forward in time
    time_loop: do n=1, num_time_steps
        
        ! diff now comes from the module imported above
        u = u - (u*diff(u) + g*diff(h))/dx*dt
        h = h-diff(u*(hmean+h))/dx*dt
        print *, n, h

    enddo time_loop    
 
END PROGRAM TSUNAMI
