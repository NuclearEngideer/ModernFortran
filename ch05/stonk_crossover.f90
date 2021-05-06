PROGRAM STONK_CROSSOVER
    
    USE CH05MISC, ONLY: READ_STOCK
    USE ARRAYOPS, ONLY: CROSSNEG, CROSSPOS, REVERSE

    IMPLICIT NONE

    CHARACTER(4), ALLOCATABLE :: SYMBOLS(:)
    CHARACTER(:), ALLOCATABLE :: TIME(:)
    REAL, ALLOCATABLE :: OPEN(:), HIGH(:), LOW(:), CLOSE(:), ADJCLOSE(:), VOLUME(:)
    INTEGER :: FILEUNIT, I, IM, N
    INTEGER, ALLOCATABLE :: BUY(:), SELL(:)

    SYMBOLS = ['AAPL', 'AMZN', 'CRAY', 'CSCO', 'HPQ ',&
               'IBM ', 'INTC', 'MSFT', 'NVDA', 'ORCL']

    DO N=1, SIZE(SYMBOLS)
        
        PRINT *, 'Processing moving average crossover for '//symbols(n)

        call read_stock('stock-prices-master/data/'//trim(symbols(n)) // '.csv', time,&
                        open, high, low, close, adjclose,volume)

        TIME=TIME(SIZE(TIME):1:-1)
        ADJCLOSE=REVERSE(ADJCLOSE)

        BUY=CROSSPOS(ADJCLOSE,30)
        SELL=CROSSNEG(ADJCLOSE,30)

        OPEN(NEWUNIT=FILEUNIT, FILE=TRIM(SYMBOLS(N)) // '_crossover.txt')
        DO I=1,SIZE(BUY)
            WRITE(FILEUNIT, FMT=*) 'BUY ', TIME(BUY(I))
        ENDDO
        DO I=1,SIZE(SELL)
            WRITE(FILEUNIT, FMT=*) 'SELL ', TIME(SELL(I))
        ENDDO
        CLOSE(FILEUNIT)
    ENDDO

END PROGRAM STONK_CROSSOVER

