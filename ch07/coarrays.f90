program coarrays
    
    implicit none

    integer :: a[*]

    if (num_images () /= 2) error stop 'Error: program must run on 2 images'

    a = 0 ! initializes to 0 on all images
    
    ! give image 1 its value and send a value to image 2
    if (this_image() == 1) then
        a=1
        print *, 'Image ', this_image(), ' has value ', a
        print *, 'Image ', this_image(), ' sending new value to 2nd image..'
        a[2] = 2 * a
    endif

    ! update all images
    sync all

    ! update image 1's new value
    if (this_image() == 2) then
        print *, 'Image ', this_image(), ' now has value ', a
        print *, 'Image ', this_image(), ' sending new value to image 1.'
        a[1] = 2*a
    endif

    ! update all images again
    sync all

    ! print final bit
    if (this_image() == 2) print *, 'Image ', this_image(), ' sees that &
                                    &image 1 now has value ', a[1]

end program coarrays
