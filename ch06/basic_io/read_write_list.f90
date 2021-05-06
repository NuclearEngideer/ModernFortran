program read_write_list
    implicit none
    character(len=1000) :: text
    integer :: a
    real :: x
    ! The * default formatting allows input of any type and can take a list as input
    read *, text, a, x
    print*, 'User typed:', trim(text), a, x
end program read_write_list
