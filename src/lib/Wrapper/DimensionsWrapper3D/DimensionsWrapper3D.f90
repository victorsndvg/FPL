module DimensionsWrapper3D

USE DimensionsWrapper

implicit none
private

    type, extends(DimensionsWrapper_t), abstract :: DimensionsWrapper3D_t
    private
    contains
        procedure(DimensionsWrapper3D_Set), deferred :: Set
        procedure(DimensionsWrapper3D_Get), deferred :: Get
    end type

    abstract interface
        subroutine DimensionsWrapper3D_Set(this, Value)
            import DimensionsWrapper3D_t
            class(DimensionsWrapper3D_t), intent(INOUT) :: this
            class(*),                     intent(IN)    :: Value(:,:,:)
        end subroutine

        subroutine DimensionsWrapper3D_Get(this, Value)
            import DimensionsWrapper3D_t
            class(DimensionsWrapper3D_t), intent(IN)  :: this
            class(*),                     intent(OUT) :: Value(:,:,:)
        end subroutine
    end interface

public :: DimensionsWrapper3D_t

end module DimensionsWrapper3D
