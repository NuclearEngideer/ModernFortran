program tsunami

    use iso_fortran_env, only: int32, real32
    use mod_diff, only: diff => diff_centered 
    use mod_parallel, only: tile_indices, tile_neighbors

    implicit none

    integer(int32) :: n, i

    integer(int32), parameter :: grid_size = 100
    integer(int32), parameter :: num_time_steps = 5000
    
    real(real32), parameter :: dt = 0.02, dx = 1, g = 9.8
    ! the "parameter" identifies constants that cannot be changed
    ! by the program -- also requires assignment on declaration line
   
    integer(int32), parameter :: icenter = 25
    real(real32), parameter :: decay = 0.02
    
    ! declare water height and velocity as allocatable coarrays
    real(real32), allocatable :: h(:)[:], u(:)[:]
    ! declare the gather variable to write data
    real(real32), allocatable :: gather(:)[:]
    ! initializing mean water height stuff
    real(real32), allocatable :: hmean(:)
    
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
        error stop 'error: grid_size must be divisible by number of images'
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
    
    ! above, the ils/ile variables are the local calculated values. but these values require
    ! the halo cell values in ims and ime.
    ! need to get those values added like h(ime)[left neighbor number] = h(ils)
    ! pattern for the parallel time loop is:
    ! update halo cells
    ! synchronize images
    ! solve the step
    
    ! provide checks for various input parameters
    if (grid_size <= 0) stop 'grid_size must be > 0'
    if (dt <= 0) stop 'time step dt must be > 0'
    if (dx <= 0) stop 'grid spacing dx must be > 0'
    
    ! initialize guassian blob centered @ i=25
    do i=is-1, ie+1
        h(i-is+1) = exp(-decay  * (i-icenter)**2)
    enddo

    u = 0 ! initializes velocity vector to 0s
    hmean=10 ! initializes height array to 10
    !done w/ initial gaussian water height

    gather(is:ie)[1] = h(ils:ile)
    sync all
    if (this_image() == 1) print *, 0, gather

    ! iterate water solution forward in time
    time_loop: do n=1, num_time_steps
        
        ! send this image's halo cells to its neighbors and sync
        h(ime)[left] = h(ils)
        h(ims)[right] = h(ile)
        sync all
        ! solve and then sync again 
        ! velocity initial guess is all 0, so don't need to pass neighbors around
        ! during first pass thru loop. after first pass, neighbors are accounted for
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
 
end program tsunami
