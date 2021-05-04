PROGRAM STONKS_GAIN
    
    USE ARRAYOPS
    USE CH05MISC


    IMPLICIT NONE
   

    ! note, character(len=4) is the same as character(4). 
    ! Character is character(1) (single character)
    CHARACTER(LEN=4), ALLOCATABLE :: SYMBOLS(:)
    CHARACTER(LEN=:), ALLOCATABLE :: TIME(:)
    REAL, ALLOCATABLE :: OPEN(:), HIGH(:), LOW(:),  CLOSE(:), ADJCLOSE(:), VOLUME(:)
    INTEGER :: I, IM, N
    REAL :: GAIN

    SYMBOLS = ['AAPL', 'AMZN', 'CRAY', 'CSCO', 'HPQ ',&
               'IBM ', 'INTC', 'MSFT', 'NVDA', 'ORCL']

    DO N=1, SIZE(SYMBOLS)

        CALL READ_STOCK('stock-prices-master/data/' // TRIM(SYMBOLS(N)) // '.csv', TIME,&
          OPEN, HIGH, LOW, CLOSE, ADJCLOSE, VOLUME)

        ADJCLOSE = REVERSE(ADJCLOSE)
        GAIN = (ADJCLOSE(SIZE(ADJCLOSE)) - ADJCLOSE(1))
        IF (N==1) THEN
            PRINT *, TIME(SIZE(TIME)) // ' THROUGH ' // TIME(1)
            PRINT *, 'SYMBOL, GAIN (USD), RELATIVE GAIN (%)'
            PRINT *, '-------------------------------------'
        ENDIF

        PRINT *, SYMBOLS(N), GAIN, NINT(GAIN / ADJCLOSE(1) * 100)

    ENDDO

END PROGRAM STONKS_GAIN
