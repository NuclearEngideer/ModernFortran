MODULE CH05MISC
    
    !this is a very messy module file
    
    USE ARRAYOPS, ONLY: ALLOC

    IMPLICIT NONE
    PRIVATE
    PUBLIC :: READ_STOCK, write_stock

    CONTAINS

    SUBROUTINE READ_STOCK(FILENAME, TIME, OPEN, HIGH, &
                          LOW, CLOSE, ADJCLOSE, VOLUME)
        CHARACTER(len=*), INTENT(IN) :: FILENAME
        CHARACTER(len=:), ALLOCATABLE, INTENT(INOUT) :: TIME(:)
        REAL, ALLOCATABLE, INTENT(INOUT) :: OPEN(:), &
          HIGH(:), LOW(:), CLOSE(:), ADJCLOSE(:), VOLUME(:)
        INTEGER :: FILEUNIT, N, NM

        NM = NUM_RECORDS(FILENAME)-1

        IF (ALLOCATED(TIME)) DEALLOCATE(TIME)
        ALLOCATE(CHARACTER(10) :: TIME(NM))
        ! the alloc subroutine can only allocate reals right now
        ! so time is allocated explicitly
        CALL ALLOC(OPEN, NM)
        CALL ALLOC(HIGH, NM)
        CALL ALLOC(LOW, NM)
        CALL ALLOC(CLOSE, NM)
        CALL ALLOC(ADJCLOSE, NM)
        CALL ALLOC(VOLUME, NM)

        OPEN(NEWUNIT=FILEUNIT, FILE=FILENAME)
        READ(FILEUNIT, FMT=*, END=1)
        DO N=1,NM
            READ(FILEUNIT, FMT=*, END=1) TIME(N), OPEN(N), &
              HIGH(N), LOW(N), CLOSE(N), ADJCLOSE(N), VOLUME(N)
        ENDDO
        1 CLOSE(FILEUNIT)

    END SUBROUTINE READ_STOCK

    INTEGER FUNCTION NUM_RECORDS(FILENAME)
        CHARACTER(len=*), INTENT(IN) :: FILENAME
        INTEGER :: FILEUNIT
        OPEN(NEWUNIT=FILEUNIT, FILE=FILENAME)
        NUM_RECORDS=0
        DO
            READ(UNIT=FILEUNIT, FMT=*, END=1)
            NUM_RECORDS = NUM_RECORDS + 1
        ENDDO
        1 CONTINUE
        CLOSE(UNIT=FILEUNIT)
    END FUNCTION NUM_RECORDS

    subroutine write_stock(filename, time, price, mvavg, mvstd)
        character(*), intent(in) :: filename
        character(:), allocatable, intent(in) :: time(:)
        real, intent(in) :: price(:), mvavg(:), mvstd(:)
        integer :: fileunit, n
        open(newunit=fileunit, file=filename)
        do n=1, size(time)
            write(fileunit, fmt=*) time(n), price(n), mvavg(n), mvstd(n)
        end do
        close(fileunit)
    end subroutine write_Stock

END MODULE
