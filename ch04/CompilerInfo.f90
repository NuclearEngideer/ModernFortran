PROGRAM COMPILERINFO 
    USE ISO_FORTRAN_ENV
    IMPLICIT NONE
    PRINT *, 'COMPILER VERSION: ', compiler_version()
    PRINT *, 'COMPILER OPTIONS: ', compiler_options()
END PROGRAM COMPILERINFO
