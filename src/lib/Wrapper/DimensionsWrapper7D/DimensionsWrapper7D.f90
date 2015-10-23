module DimensionsWrapper7D

USE DimensionsWrapper

implicit none
private

    type, extends(DimensionsWrapper_t), abstract :: DimensionsWrapper7D_t
    private
    contains
        procedure(DimensionsWrapper7D_Set),            deferred :: Set
        procedure(DimensionsWrapper7D_Get),            deferred :: Get
        procedure(DimensionsWrapper7D_GetPolymorphic), deferred :: GetPolymorphic
    end type

    abstract interface
        subroutine DimensionsWrapper7D_Set(this, Value)
            import DimensionsWrapper7D_t
            class(DimensionsWrapper7D_t), intent(INOUT) :: this
            class(*),                     intent(IN)    :: Value(:,:,:,:,:,:,:)
        end subroutine

        subroutine DimensionsWrapper7D_Get(this, Value)
            import DimensionsWrapper7D_t
            class(DimensionsWrapper7D_t), intent(IN)    :: this
            class(*),                     intent(INOUT) :: Value(:,:,:,:,:,:,:)
        end subroutine

        subroutine DimensionsWrapper7D_GetPolymorphic(this, Value)
            import DimensionsWrapper7D_t
            class(DimensionsWrapper7D_t), intent(IN)  :: this
            class(*),                     intent(OUT) :: Value(:,:,:,:,:,:,:)
        end subroutine
    end interface

public :: DimensionsWrapper7D_t

end module DimensionsWrapper7D
