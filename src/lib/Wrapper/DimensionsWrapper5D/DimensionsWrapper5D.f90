module DimensionsWrapper5D

USE DimensionsWrapper

implicit none
private

    type, extends(DimensionsWrapper_t), abstract :: DimensionsWrapper5D_t
    private
    contains
        procedure(DimensionsWrapper5D_Set),            deferred :: Set
        procedure(DimensionsWrapper5D_Get),            deferred :: Get
        procedure(DimensionsWrapper5D_GetPointer),     deferred :: GetPointer
        procedure(DimensionsWrapper5D_GetPolymorphic), deferred :: GetPolymorphic
    end type

    abstract interface
        subroutine DimensionsWrapper5D_Set(this, Value)
            import DimensionsWrapper5D_t
            class(DimensionsWrapper5D_t), intent(INOUT) :: this
            class(*),                     intent(IN)    :: Value(:,:,:,:,:)
        end subroutine

        subroutine DimensionsWrapper5D_Get(this, Value)
            import DimensionsWrapper5D_t
            class(DimensionsWrapper5D_t), intent(IN)  :: this
            class(*),                     intent(OUT) :: Value(:,:,:,:,:)
        end subroutine

        function DimensionsWrapper5D_GetPointer(this) result(Value)
            import DimensionsWrapper5D_t
            class(DimensionsWrapper5D_t), target, intent(IN)  :: this
            class(*), pointer                                 :: Value(:,:,:,:,:)
        end function

        subroutine DimensionsWrapper5D_GetPolymorphic(this, Value)
            import DimensionsWrapper5D_t
            class(DimensionsWrapper5D_t), intent(IN)  :: this
            class(*), allocatable,        intent(OUT) :: Value(:,:,:,:,:)
        end subroutine
    end interface

public :: DimensionsWrapper5D_t

end module DimensionsWrapper5D
