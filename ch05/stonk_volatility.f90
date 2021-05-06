program stonk_volatility
    

    use arrayops, only: average, std, movingAverage, movingSTD, reverse
    use ch05misc, only: read_stock, write_stock 
    
    IMPLICIT NONE

    character(4), allocatable :: symbols(:)
    character(:), allocatable :: time(:)
    real, allocatable :: open(:), high(:), low(:), close(:), adjclose(:), volume(:)
    integer :: i, im, n

    symbols =['AAPL', 'AMZN', 'CRAY', 'CSCO', 'HPQ ',&
              'IBM ', 'INTC', 'MSFT', 'NVDA', 'ORCL']


    do n=1, size(symbols)
        
        call read_stock('stock-prices-master/data/' // trim(symbols(n)) // '.csv', time,&
                        open, high, low, close, adjclose, volume)

        im=size(time)
        adjclose=reverse(adjclose)

        if (n==1) then 
            print *, time(im) // ' through ' // time(1)
            print *, 'Symbol, Average(USD), Volatility(USD), Relative Volatility (%)'
            print *, '--------------------------------------------------------------'
        endif

        print *, symbols(n), average(adjclose), std(adjclose),&
                 nint(std(adjclose) / average(adjclose) * 100)

        time=time(im:1:-1)

        call write_stock(trim(symbols(n)) // '_volatility.txt',&
                         time, adjclose, movingaverage(adjclose, 30),&
                         movingstd(adjclose, 30))

    enddo

end program stonk_volatility
