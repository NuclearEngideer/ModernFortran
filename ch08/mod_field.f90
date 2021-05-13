module mod_field

    type :: Field
        character(:), allocatable :: name
        integer :: dims(2)
    end type Field

end module mod_field

program field_ex
    
    use mod_field

    type(Field) :: testfieldplzignore
    testfieldplzignore = Field('testing',[1,4])

    print *, testfieldplzignore % name

end program field_ex
