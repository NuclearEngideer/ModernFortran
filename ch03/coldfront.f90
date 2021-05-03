program coldfront
    IMPLICIT NONE
    INTEGER :: N
    REAL :: NHOURS(8)
    NHOURS = [6,12,18,24,30,36,42,48]

    
    PRINT *, 'Temperature after ', NHOURS, ' hours is', &
    COLD_FRONT_TEMP(12.,24.,20., 960., NHOURS), 'deg. C'

    CONTAINS ! the function/subroutine can be defined above the "program coldfront"
             ! line, outside of the program "loop"

    REAL ELEMENTAL  FUNCTION COLD_FRONT_TEMP(temp1, temp2, c, dx,dt) result(res)
        REAL,INTENT(IN) :: temp1, temp2, c, dx, dt
        res = temp2 - c*(temp2-temp1)/dx*dt
    END FUNCTION COLD_FRONT_TEMP    

end program coldfront 
