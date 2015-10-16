module DimensionsWrapper2D

USE DimensionsWrapper

implicit none
private

    type, extends(DimensionsWrapper_t), abstract :: DimensionsWrapper2D_t
    private
    contains
        procedure(DimensionsWrapper2D_isOfDataType), deferred :: isOfDataType
        procedure(DimensionsWrapper2D_Free),         deferred :: Free
    end type

    abstract interface
        subroutine DimensionsWrapper2D_Free(this)
            import DimensionsWrapper2D_t
            class(DimensionsWrapper2D_t), intent(INOUT) :: this
        end subroutine
        function DimensionsWrapper2D_isOfDataType(this, Mold) result(isOfDataType)
            import DimensionsWrapper2D_t
            class(DimensionsWrapper2D_t), intent(IN) :: this
            class(*),                   intent(IN) :: Mold
            logical                                :: isOfDataType
        end function
    end interface

public :: DimensionsWrapper2D_t

end module DimensionsWrapper2D
