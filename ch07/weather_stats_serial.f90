program weather_stats_serial
    
    use arrayops, only: denan, mean
    use mod_io, only: read_buoy

    implicit none

    character(5), allocatable :: ids(:)
    character(20), allocatable :: time(:)
    real, allocatable :: wind_speed(:)
    real, allocatable :: max_wind(:), mean_wind(:)
    integer :: i

    ids = ['42001','42002','42003','42020','42035',&
           '42036', '42039','42040','42055']

    allocate(max_wind(size(ids)),mean_wind(size(ids)))

    do i=1, size(ids)
        call read_buoy('weather-buoys-master/data/buoy_' // ids(i) // '.csv',&
                        time, wind_speed)
        wind_speed=denan(wind_speed)
        max_wind(i)=maxval(wind_speed)
        mean_wind(i) = mean(wind_speed)
    enddo

    print *, 'maximum wind speed measured is ',&
              maxval(max_wind), 'at station ', ids(maxloc(max_wind))
    print *, 'highest mean wind speed is ',&
              maxval(mean_wind), 'at station ', ids(maxloc(mean_wind))
    print *, 'lowest mean wind speed is ',&
              minval(mean_wind), 'at station ', ids(minloc(mean_wind))

end program weather_stats_serial
