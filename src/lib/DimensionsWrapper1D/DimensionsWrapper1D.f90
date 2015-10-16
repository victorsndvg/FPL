module DimensionsWrapper1D

USE DimensionsWrapper

implicit none
private

    type, extends(DimensionsWrapper_t), abstract :: DimensionsWrapper1D_t
    private
    contains
        procedure(DimensionsWrapper1D_isOfDataType), deferred :: isOfDataType
        procedure(DimensionsWrapper1D_Free),         deferred :: Free
    end type

    abstract interface
        subroutine DimensionsWrapper1D_Free(this)
            import DimensionsWrapper1D_t
            class(DimensionsWrapper1D_t), intent(INOUT) :: this
        end subroutine
        function DimensionsWrapper1D_isOfDataType(this, Mold) result(isOfDataType)
            import DimensionsWrapper1D_t
            class(DimensionsWrapper1D_t), intent(IN) :: this
            class(*),                   intent(IN) :: Mold
            logical                                :: isOfDataType
        end function
    end interface

public :: DimensionsWrapper1D_t

end module DimensionsWrapper1D
