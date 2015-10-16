module DimensionsWrapper0D

USE DimensionsWrapper

implicit none
private

    type, extends(DimensionsWrapper_t), abstract :: DimensionsWrapper0D_t
    private
    contains
        procedure(DimensionsWrapper0D_isOfDataType), deferred :: isOfDataType
        procedure(DimensionsWrapper0D_Free),         deferred :: Free
    end type

    abstract interface
        subroutine DimensionsWrapper0D_Free(this)
            import DimensionsWrapper0D_t
            class(DimensionsWrapper0D_t), intent(INOUT) :: this
        end subroutine
        function DimensionsWrapper0D_isOfDataType(this, Mold) result(isOfDataType)
            import DimensionsWrapper0D_t
            class(DimensionsWrapper0D_t), intent(IN) :: this
            class(*),                   intent(IN) :: Mold
            logical                                :: isOfDataType
        end function
    end interface

public :: DimensionsWrapper0D_t

end module DimensionsWrapper0D
