module DimensionsWrapper0D

USE DimensionsWrapper

implicit none
private

    type, extends(DimensionsWrapper_t), abstract :: DimensionsWrapper0D_t
    private
    contains
        procedure(DimensionsWrapper0D_Set),            deferred :: Set
        procedure(DimensionsWrapper0D_Get),            deferred :: Get
        procedure(DimensionsWrapper0D_GetPointer),     deferred :: GetPointer
        procedure(DimensionsWrapper0D_GetPolymorphic), deferred :: GetPolymorphic
    end type

    abstract interface
        subroutine DimensionsWrapper0D_Set(this, Value)
            import DimensionsWrapper0D_t
            class(DimensionsWrapper0D_t), intent(INOUT)  :: this
            class(*),                     intent(IN)     :: Value
        end subroutine

        subroutine DimensionsWrapper0D_Get(this, Value)
            import DimensionsWrapper0D_t
            class(DimensionsWrapper0D_t), intent(IN)  :: this
            class(*),                     intent(OUT) :: Value
        end subroutine

        function DimensionsWrapper0D_GetPointer(this) result(Value)
            import DimensionsWrapper0D_t
            class(DimensionsWrapper0D_t), target, intent(IN)  :: this
            class(*), pointer                                 :: Value
        end function

        subroutine DimensionsWrapper0D_GetPolymorphic(this, Value)
            import DimensionsWrapper0D_t
            class(DimensionsWrapper0D_t), intent(IN)  :: this
            class(*), allocatable,        intent(OUT) :: Value
        end subroutine
    end interface

public :: DimensionsWrapper0D_t

end module DimensionsWrapper0D
