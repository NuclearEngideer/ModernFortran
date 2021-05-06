program standard_streams
    use iso_fortran_env, only: stdin => input_unit,&
                               stdout => output_unit,&
                               stderr => error_unit
    implicit none
    character(len=1000) :: text
    read(stdin, '(a)') text
    write(stdout, '(a)') trim(text)
    ! Can check out github.com/cmacmackin/flogging for better fortran logging
    write(stderr, '(a)') 'This is what an error message looks like'
end program standard_streams
