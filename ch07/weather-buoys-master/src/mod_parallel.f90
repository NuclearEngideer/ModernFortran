module mod_parallel

  ! A module to provide parallel facilities 
  ! for the weather data analysis exercise.

  implicit none

  private
  public :: tile_indices

contains

  pure function tile_indices(dims)
    ! Given input global array size, return start and end index
    ! of a parallel 1-d tile that correspond to this image.
    integer, intent(in) :: dims
    integer :: tile_indices(2)
    integer :: offset, tile_size

    tile_size = dims / num_images()

    ! start and end indices assuming equal tile sizes
    tile_indices(1) = (this_image() - 1) * tile_size + 1
    tile_indices(2) = tile_indices(1) + tile_size - 1

    ! if we have any remainder, distribute it to the tiles at the end 
    offset = num_images() - mod(dims, num_images())
    if (this_image() > offset) then
      tile_indices(1) = tile_indices(1) + this_image() - offset - 1
      tile_indices(2) = tile_indices(2) + this_image() - offset
    end if

  end function tile_indices

end module mod_parallel
