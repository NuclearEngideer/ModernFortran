program weather_stats_serial
    
    use arrayops, only: denan, mean
    use mod_io, only: read_buoy
    use mod_parallel, only: tile_indices

    implicit none

    character(5), allocatable :: ids(:)
    character(20), allocatable :: time(:)
    real, allocatable :: wind_speed(:)
    real, allocatable :: max_wind(:), mean_wind(:)
    integer :: i, is, ie, indices(2)
    real, allocatable :: gather(:)[:]

    real :: min_mean_wind, max_mean_wind, max_max_wind
    
    ids = ['42001','42002','42003','42020','42035',&
           '42036', '42039','42040','42055']

    if (num_images() > size(ids)) error stop 'Error: Too many images'

    indices = tile_indices(size(ids))
    is = indices(1)
    ie = indices(2)

    allocate(max_wind(is:ie), mean_wind(is:ie))

    do i=is, ie
        call read_buoy('data/buoy_' // ids(i) // '.csv',&
                        time, wind_speed)
        wind_speed=denan(wind_speed)
        max_wind(i)=maxval(wind_speed)
        mean_wind(i) = mean(wind_speed)
    enddo
    
    allocate(gather(size(ids))[*])

    gather(is:ie)[1] = max_wind
    sync all
    if (this_image()==1) then
        print *, 'maximum wind speed measured is ',&
                  maxval(gather), 'at station ', ids(maxloc(gather))
        print *, 'lowest mean wind speed is', minval(gather),&
                 'at station ', ids(minloc(gather))
    endif

end program weather_stats_serial
