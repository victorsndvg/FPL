module DimensionsWrapper5D

USE DimensionsWrapper

implicit none
private

    type, extends(DimensionsWrapper_t), abstract :: DimensionsWrapper5D_t
    private
    contains
        procedure(DimensionsWrapper5D_isOfDataType), deferred :: isOfDataType
        procedure(DimensionsWrapper5D_Free),         deferred :: Free
    end type

    abstract interface
        subroutine DimensionsWrapper5D_Free(this)
            import DimensionsWrapper5D_t
            class(DimensionsWrapper5D_t), intent(INOUT) :: this
        end subroutine
        function DimensionsWrapper5D_isOfDataType(this, Mold) result(isOfDataType)
            import DimensionsWrapper5D_t
            class(DimensionsWrapper5D_t), intent(IN) :: this
            class(*),                   intent(IN) :: Mold
            logical                                :: isOfDataType
        end function
    end interface

public :: DimensionsWrapper5D_t

end module DimensionsWrapper5D
