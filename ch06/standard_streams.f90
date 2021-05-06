program standard_streams
    use iso_fortran_env, only: stdin => input_unit,&
                               stdout => output_unit,&
                               stderr => error_unit
    implicit none
    character(len=1000) :: text
    read(stdin, '(a)') text
    write(stdout, '(a)') trim(text)
    ! in macOS the stderr looks exactly the same as stdout w/ no differentiation here..
    ! compiled w/ gfortran 10.20.0
    write(stderr, '(a)') 'This is what an error message looks like'
end program standard_streams
