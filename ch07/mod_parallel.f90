module mod_parallel

    implicit none
    
    private
    public :: tile_indices

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
        
end module mod_parallel
