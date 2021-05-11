program images_ordered
    
    implicit none
    
    integer :: n
    do n=1, num_images()
        if(this_image() == n) print *, 'Hello from image', this_image(), 'of', num_images()
        sync all
    end do
end program images_ordered
