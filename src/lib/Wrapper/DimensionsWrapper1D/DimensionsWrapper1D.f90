module DimensionsWrapper1D

USE DimensionsWrapper

implicit none
private

    type, extends(DimensionsWrapper_t), abstract :: DimensionsWrapper1D_t
    private
    contains
        procedure(DimensionsWrapper1D_Set),            deferred :: Set
        procedure(DimensionsWrapper1D_Get),            deferred :: Get
        procedure(DimensionsWrapper1D_GetPointer),     deferred :: GetPointer
    end type

    abstract interface
        subroutine DimensionsWrapper1D_Set(this, Value)
            import DimensionsWrapper1D_t
            class(DimensionsWrapper1D_t), intent(INOUT) :: this
            class(*),                     intent(IN)    :: Value(:)
        end subroutine

        subroutine DimensionsWrapper1D_Get(this, Value)
            import DimensionsWrapper1D_t
            class(DimensionsWrapper1D_t), intent(IN)  :: this
            class(*),                     intent(OUT) :: Value(:)
        end subroutine

        function DimensionsWrapper1D_GetPointer(this) result(Value)
            import DimensionsWrapper1D_t
            class(DimensionsWrapper1D_t), target, intent(IN)  :: this
            class(*), pointer                                 :: Value(:)
        end function

        subroutine DimensionsWrapper1D_GetPolymorphic(this, Value)
            import DimensionsWrapper1D_t
            class(DimensionsWrapper1D_t), intent(IN)  :: this
            class(*), allocatable,        intent(OUT) :: Value(:)
        end subroutine
    end interface

public :: DimensionsWrapper1D_t

end module DimensionsWrapper1D
