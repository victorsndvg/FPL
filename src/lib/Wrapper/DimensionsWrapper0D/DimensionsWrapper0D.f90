module DimensionsWrapper0D

USE DimensionsWrapper

implicit none
private

    type, extends(DimensionsWrapper_t), abstract :: DimensionsWrapper0D_t
    private
    contains
        procedure(DimensionsWrapper0D_Set), deferred :: Set
        procedure(DimensionsWrapper0D_Get), deferred :: Get
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
    end interface

public :: DimensionsWrapper0D_t

end module DimensionsWrapper0D
