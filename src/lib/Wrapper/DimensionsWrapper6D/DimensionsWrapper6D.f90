module DimensionsWrapper6D

USE DimensionsWrapper

implicit none
private

    type, extends(DimensionsWrapper_t), abstract :: DimensionsWrapper6D_t
    private
    contains
        procedure(DimensionsWrapper6D_Set),            deferred :: Set
        procedure(DimensionsWrapper6D_Get),            deferred :: Get
        procedure(DimensionsWrapper6D_GetPointer),     deferred :: GetPointer
    end type

    abstract interface
        subroutine DimensionsWrapper6D_Set(this, Value)
            import DimensionsWrapper6D_t
            class(DimensionsWrapper6D_t), intent(INOUT) :: this
            class(*),                     intent(IN)    :: Value(:,:,:,:,:,:)
        end subroutine

        subroutine DimensionsWrapper6D_Get(this, Value)
            import DimensionsWrapper6D_t
            class(DimensionsWrapper6D_t), intent(IN)  :: this
            class(*),                     intent(OUT) :: Value(:,:,:,:,:,:)
        end subroutine

        function DimensionsWrapper6D_GetPointer(this) result(Value)
            import DimensionsWrapper6D_t
            class(DimensionsWrapper6D_t), target, intent(IN)  :: this
            class(*), pointer                                 :: Value(:,:,:,:,:,:)
        end function

        subroutine DimensionsWrapper6D_GetPolymorphic(this, Value)
            import DimensionsWrapper6D_t
            class(DimensionsWrapper6D_t), intent(IN)  :: this
            class(*), allocatable,        intent(OUT) :: Value(:,:,:,:,:,:)
        end subroutine
    end interface

public :: DimensionsWrapper6D_t

end module DimensionsWrapper6D
