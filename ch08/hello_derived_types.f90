module mod_person
    
    ! General oop practice is types/classes start w/ capital letter
    ! Obviously fortran is case insensitive
    type :: Person
        character(20) :: name ! Declares component variable
    contains   
        procedure, pass(self) :: greet
    end type Person

    contains

        subroutine greet(self)
            class(Person), intent(in) :: self
            print *, 'Hello, my name is ' // trim(self % name) // '!'
        end subroutine greet

end module mod_person

program hello_derived_types
    use mod_person, only: Person
    implicit none
    type(Person) :: some_person = Person('Jill')
    call some_person % greet()
end program hello_derived_types
