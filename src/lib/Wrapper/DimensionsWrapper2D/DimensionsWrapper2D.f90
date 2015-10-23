module DimensionsWrapper2D

USE DimensionsWrapper

implicit none
private

    type, extends(DimensionsWrapper_t), abstract :: DimensionsWrapper2D_t
    private
    contains
        procedure(DimensionsWrapper2D_Set), deferred :: Set
        procedure(DimensionsWrapper2D_Get), deferred :: Get
    end type

    abstract interface
        subroutine DimensionsWrapper2D_Set(this, Value)
            import DimensionsWrapper2D_t
            class(DimensionsWrapper2D_t), intent(INOUT) :: this
            class(*),                     intent(IN)    :: Value(:,:)
        end subroutine

        subroutine DimensionsWrapper2D_Get(this, Value)
            import DimensionsWrapper2D_t
            class(DimensionsWrapper2D_t), intent(IN)  :: this
            class(*),                     intent(OUT) :: Value(:,:)
        end subroutine
    end interface

public :: DimensionsWrapper2D_t

end module DimensionsWrapper2D
