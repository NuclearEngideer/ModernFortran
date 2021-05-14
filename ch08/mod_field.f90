module mod_field

    use mod_parallel, only: tile_indices

    type :: Field
        character(:), allocatable :: name
        integer(int32) :: dims(2), lb(2), ub(2)
        real(real32), allocatable :: data(:,:)
    end type Field

    contains

    type(Field) function field_constrcutor(name, dims) result(res)
        character(*), intent(in) :: name
        integer(int32), intent(in) :: dims(2)
        integer(int32) :: indices(4)
        res % name = name
        res % dims = dims
        indices = tile_indices(dims)
        res % lb = indices([1, 3])
        res % ub = indices([2, 4])
        allocate(res % data(res % lb(1)-1:res % ub(1)+1, &
                            res % lb(2)-1:res % ub(2)+1))
        res % data = 0
    end function field_constrcutor
end module mod_field

