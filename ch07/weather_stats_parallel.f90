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
        call read_buoy('data/buoy_' // ids(i) // '.csv',&
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

    contains

    pure function tile_indices(x)
        integer, intent(in) :: x
        integer :: offset, tile_size
        integer :: tile_indices(2)
        
        ! if the size of the array is divisible by the number of images, this guess is correct
        ! fortran rounds down to the nearest integer (or just takes the integer part of the real)
        ! ex: 9/2 = 4
        tile_size = x/num_images()

        ! each image gets its own array constructed ON the image
        ! this is the mindset i need to understand... not distributed from 
        ! "master" image to worker images, but calculated directly ON image

        ! if this_image=1, the tile starts at 1 (good!)
        tile_indices(1) = (this_image()-1)*tile_size+1
        tile_indices(2) = tile_indices(1) + tile_size - 1

        ! offset is calculated with modulus.. if there's a remainder, the offset is number of images
        ! minus that modulus
        ! ex: 2 images with x=9 like in the ids:
        ! 2 - mod(9,2) = 2-1 = 1
        offset = num_images() - mod(x, num_images())
        if (this_image() > offset) then ! continuing above example..
            ! this_image=1, tile_indice(1)=1, tile_indice(2)=4
            ! this_image=2, tile_indice(1)=5, tile_indice(2)=8
            tile_indices(1) = tile_indices(1) + this_image() - offset - 1
            tile_indices(2) = tile_indices(2) + this_image() - offset 
            ! after above 2 lines..
            ! this_image=1: does not change
            ! this_image=2: tile_indice(1)=5, tile_indice(2)=9
            ! now our tiling is correct
        endif
    end function tile_indices
        

end program weather_stats_serial
