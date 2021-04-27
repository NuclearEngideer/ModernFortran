PROGRAM TSUNAMI
      IMPLICIT NONE

      INTEGER :: I, N
      INTEGER, PARAMETER :: GRID_SIZE = 100
      INTEGER, PARAMETER :: NUM_TIME_STEPS = 100
      REAL, PARAMETER :: dt = 1, dx = 1, c = 1 
      
      ! the "parameter" identifies constants that cannot be changed
      ! by the program -- also requires assignment on declaration line
     
      REAL :: h(GRID_SIZE)  ! Array of length grid size (100) 
                            ! representing water height

      REAL :: dh(GRID_SIZE) ! change in water height over a step


      !initializing gaussian water height stuff
      integer,parameter :: icenter = 25
      real, parameter :: decay = 0.02
      
      ! do concurrent parallizes this generation
      ! note different syntax
      do concurrent (i=1:grid_size)
         h(i) = exp(-decay * (i-icenter)**2)
      end do
      !Done w/ initial gaussian water height

      print *, 0, h  

      ! provide checks for various input parameters
      if (GRID_SIZE <= 0) stop 'GRID_SIZE MUST BE > 0'
      if (dt <= 0) STOP 'TIME STEP dt MUST BE > 0'
      if (dx <= 0) STOP 'GRID SPACING dx MUST BE > 0'
      if (c <= 0) STOP 'BACKGROUND FLOW SPEED c MUST BE > 0'
      if (c >= 3e8) STOP "YOU CAN'T JUST GO FASTER THAN THE SPEED OF &
          &LIGHT!!!"

      ! iterate water solution forward in time
      time_loop: do n=1, num_time_steps
        dh(1) = h(1) - h(grid_size)   ! periodic boundary condition on left side
        
        do concurrent (i=2:grid_size) ! finite difference of h in space
          dh(i) = h(i)-h(i-1)
        end do

        do concurrent (i=1:grid_size)             ! solves h at next time step
          h(i) = h(i)-c*dh(i)/dx *dt
        end do

        print *, n, h

      end do time_loop


END PROGRAM TSUNAMI
