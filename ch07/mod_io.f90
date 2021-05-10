module mod_io

    implicit none

    private
    public :: read_buoy

    contains

    integer function num_records(filename)
        ! count lines in an input file
        character(*), intent(in) :: filename
        integer :: fileunit
        open(newunit=fileunit, file=filename)
        num_records=0
        do
            read(unit=fileunit, fmt=*, end=1)
            num_records=num_records + 1
        enddo
        1 continue
        close(unit=fileunit)
    end function num_records

    subroutine read_buoy(filename, time, wind_speed)
        ! read time and wind speed data from file & store in array
        character(*),intent(in) :: filename
        character(20), allocatable,intent(inout) :: time(:)
        real, allocatable, intent(inout) :: wind_speed(:)
        integer :: fileunit
        integer :: n,nm
        if (allocated(time)) deallocate(time)
        if (allocated(wind_speed)) deallocate(wind_speed)
        nm = num_records(filename)
        allocate(time(nm), wind_speed(nm))
        open(newunit=fileunit, file=filename)
        do n=1,nm
            ! Fortran is list directed io, so it works well for CSV
            ! For this CSV, the columns are "time, wind speed, pressure..."
            ! We only care about the wind speed and time so those two values 
            ! get passed into the arrays at the positions
            read(unit=fileunit, fmt=*, end=1) time(n), wind_speed(n)
        enddo
        1 continue
        close(unit=fileunit)
    end subroutine read_buoy

end module mod_io
