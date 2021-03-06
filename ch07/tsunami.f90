PROGRAM TSUNAMI

    USE ISO_FORTRAN_ENV, ONLY: INT32, REAL32
    USE MOD_DIFF, ONLY: DIFF => DIFF_CENTERED 
    USE MOD_PARALLEL, ONLY: TILE_INDICES, TILE_NEIGHBORS

    IMPLICIT NONE

    INTEGER(INT32) :: N, I

    INTEGER(INT32), PARAMETER :: GRID_SIZE = 100
    INTEGER(INT32), PARAMETER :: NUM_TIME_STEPS = 5000
    
    REAL(REAL32), PARAMETER :: dt = 0.02, dx = 1, g = 9.8
    ! the "parameter" identifies constants that cannot be changed
    ! by the program -- also requires assignment on declaration line
   
    integer(INT32), parameter :: icenter = 25
    real(REAL32), parameter :: decay = 0.02
    
    ! declare water height and velocity as allocatable coarrays
    real(real32), allocatable :: h(:)[:], u(:)[:]
    ! declare the gather variable to write data
    real(real32), allocatable :: gather(:)[:]
    ! initializing mean water height stuff
    REAL(REAL32), allocatable :: hmean(:)
    
    integer(int32) :: indices(2), neighbors(2)
    integer(int32) :: left, right
    ! global indices
    integer(int32) :: is, ie
    ! local indices (not halo)
    integer(int32) :: ils, ile
    ! local memory indices, including halo
    integer(int32) :: ims, ime
    integer(int32) :: tile_size

    if (mod(grid_size, num_images())>0) then
        error stop 'Error: grid_size must be divisible by number of images'
    endif

    neighbors = tile_neighbors()
    left = neighbors(1)
    right = neighbors(2)
    
    indices = tile_indices(grid_size)
    is = indices(1)
    ie = indices(2)

    tile_size = grid_size / num_images()
    ils=1
    ile=tile_size
    ims=ils-1
    ime=ile+1

    ! allocates water and velocity array over the local memory indices
    allocate(h(ims:ime)[*])
    allocate(u(ims:ime)[*])
    allocate(hmean(ims:ime))

    ! allocates gather array over the entire domain size
    allocate(gather(grid_size)[*])
    
    ! Above, the ils/ile variables are the local calculated values. But these values require
    ! the halo cell values in ims and ime.
    ! need to get those values added like h(ime)[left neighbor number] = h(ils)
    ! Pattern for the parallel time loop is:
    ! Update halo cells
    ! Synchronize images
    ! Solve the step
    
    ! provide checks for various input parameters
    if (GRID_SIZE <= 0) stop 'GRID_SIZE MUST BE > 0'
    if (dt <= 0) STOP 'TIME STEP dt MUST BE > 0'
    if (dx <= 0) STOP 'GRID SPACING dx MUST BE > 0'
    
    ! Initialize guassian blob centered @ i=25
    do i=is-1, ie+1
        h(i-is+1) = exp(-decay  * (i-icenter)**2)
    enddo

    u = 0 ! Initializes velocity vector to 0s
    hmean=10 ! initializes height array to 10
    !Done w/ initial gaussian water height

    gather(is:ie)[1] = h(ils:ile)
    sync all
    if (this_image() == 1) print *, 0, gather

    ! iterate water solution forward in time
    time_loop: do n=1, num_time_steps
        
        ! Send this image's halo cells to its neighbors and sync
        h(ime)[left] = h(ils)
        h(ims)[right] = h(ile)
        sync all
        ! solve and then sync again 
        ! velocity initial guess is all 0, so don't need to pass neighbors around
        ! during first pass thru loop. After first pass, neighbors are accounted for
        u = u - (u*diff(u) + g*diff(h))/dx*dt
        sync all
        ! sync the new velocity guesses and then solve height
        u(ime)[left] = u(ils)
        u(ims)[right] = u(ile)
        sync all

        h = h-diff(u*(hmean+h))/dx*dt
        
        gather(is:ie)[1] = h(ils:ile)
        sync all
        if (this_image() == 1) print *, n, gather

    enddo time_loop    
 
END PROGRAM TSUNAMI
