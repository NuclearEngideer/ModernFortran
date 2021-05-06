program directo
    use iso_fortran_env, only: stdin => input_unit,&
                               stdout => output_unit,&
                               stderr => error_unit
    implicit none
    integer :: outunit, errunit
    
    ! I didn't have to specify integer values here. Opening these units like I 
    ! did below is correct to not have to do shell redirection 1> and 2> for 
    ! stdout and stderr to write to files
    outunit = 100
    errunit = 999

    write (*,'(a)') 'we will write some output to log.out and error message to log.err'

    open(unit=100, file='log.out')
    write(100, '(a)') 'This goes to log.out'
    open(unit=999, file='log.err')
    write(999, '(a)') 'This goes to log.err'

    close(stdout)
    close(stderr)
end program directo
