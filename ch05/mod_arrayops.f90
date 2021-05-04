MODULE ARRAYOPS

    IMPLICIT NONE
    
    PRIVATE
    PUBLIC :: AVERAGE, ALLOC, FREE, REVERSE, STD

    CONTAINS
     
    SUBROUTINE ALLOC(INAR, INTSIZE)
        REAL, ALLOCATABLE, INTENT(INOUT) :: INAR(:)
        INTEGER, INTENT(IN) :: INTSIZE
        INTEGER :: STAT
        CHARACTER(LEN=100) :: ERRMSG='CANNOT ALLOCATE ARRAY'
        IF (ALLOCATED(INAR)) CALL FREE(INAR)
        ALLOCATE(INAR(INTSIZE), STAT=STAT, ERRMSG=ERRMSG)
        IF (STAT > 0) ERROR STOP ERRMSG
    END SUBROUTINE ALLOC

    SUBROUTINE FREE(INAR)
        REAL, ALLOCATABLE, INTENT(INOUT) :: INAR(:)
        INTEGER :: STAT
        CHARACTER(LEN=100) :: ERRMSG='CANNOT DEALLOCATE ARRAY'
        IF (.NOT. ALLOCATED(INAR)) RETURN
        DEALLOCATE(INAR,STAT=STAT, ERRMSG=ERRMSG)
        IF (STAT > 0) ERROR STOP ERRMSG
    END SUBROUTINE FREE

    PURE FUNCTION REVERSE(INAR) RESULT(REVERSED)
        REAL, INTENT(IN) :: INAR(:)
        REAL :: REVERSED(SIZE(INAR))
        REVERSED = INAR(SIZE(INAR):1:-1)
    END FUNCTION REVERSE

    PURE REAL FUNCTION AVERAGE(X)
        REAL, INTENT(IN) :: X(:)
        AVERAGE = SUM(X)/SIZE(X)
    END FUNCTION AVERAGE
    
    pure real function std(x)
        real, intent(in) :: x(:)
        std = sqrt(average((x-average(x))**2))
    end function std

    pure function movingAverage(x,w) result(res)
        real, intent(in) :: x(:)
        integer, intent(in) :: w
        real :: res(size(x))
        integer :: i, j
        do i = 1, size(x)
            j = max(i-w,1)
            res(i)=average(x(j:i))
        enddo
    end function movingAverage

END MODULE
