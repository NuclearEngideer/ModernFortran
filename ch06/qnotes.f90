program qnotes
    use iso_fortran_env, only: stdin => input_unit,&
                               stdout => output_unit,&
                               stderr => error_unit
    implicit none
    integer :: fileunit ! i/o unit number used to connect to a file
                        ! this number is not known ahead of time
    character(9999) :: filename, text
    ! fortran can't allocate dynamic characters in a read statement
    character(10) :: pos
    logical :: file_exists
    integer :: stat


    if (command_argument_count() < 1) stop 'Usage: qnotes <filename>'

    ! reads first CLI argument and stores it in "filename"
    call get_command_argument(1, filename)

    inquire(file=trim(filename), exist=file_exists)
    pos='rewind'

    if (file_exists) then
        write(stdout, '(a)') 'File ' // trim(filename) // ' already exists!'
        do
            write(*, '(a)', advance='no') '[O]verwrite, [A]ppend, [Q]uit: '
            read(stdin, '(a)') text
            if (any(trim(text) == ['O', 'o'])) then
                write(stdout, '(a)') 'Overwriting ' // trim(filename)
                exit
            else if (any(trim(text) == ['A','a'])) then
                pos = 'append'
                write(stdout, '(a)') 'Appending to ' // trim(filename)
                exit
            else if (any(trim(text) == ['Q', 'q'])) then
                stop
            endif
        enddo
    endif

    open(newunit=fileunit, file=trim(filename), action='write', position=pos)

    ! no exit condition, runs until ^C
    do
        read (stdin, '(a)', iostat=stat, err=100) text
        write (fileunit, '(a)', iostat=stat, err=100) trim(text)
        flush (fileunit, iostat=stat, err=100) 
                         ! This is basically the same as writing a newline at the end of the text
                         ! flush writes the current buffer to disk (useful with lots of writing
                         ! to keep the disk from getting "clogged" up)
    enddo
    
    ! code jumps to this label if iostat changes from 0 and trips err card on read/write/flush
    100 close(fileunit)
    ! generally a good idea to close the file so all data is written from buffer to file before program stops
    if (stat>0) then
        write(stderr, '(a, i3)') 'Error encountered, code ', stat
        stop
    endif
end program qnotes
