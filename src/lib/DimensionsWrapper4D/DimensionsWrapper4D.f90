module DimensionsWrapper4D

USE DimensionsWrapper

implicit none
private

    type, extends(DimensionsWrapper_t), abstract :: DimensionsWrapper4D_t
    private
    contains
        procedure(DimensionsWrapper4D_isOfDataType), deferred :: isOfDataType
        procedure(DimensionsWrapper4D_Free),         deferred :: Free
    end type

    abstract interface
        subroutine DimensionsWrapper4D_Free(this)
            import DimensionsWrapper4D_t
            class(DimensionsWrapper4D_t), intent(INOUT) :: this
        end subroutine
        function DimensionsWrapper4D_isOfDataType(this, Mold) result(isOfDataType)
            import DimensionsWrapper4D_t
            class(DimensionsWrapper4D_t), intent(IN) :: this
            class(*),                   intent(IN) :: Mold
            logical                                :: isOfDataType
        end function
    end interface

public :: DimensionsWrapper4D_t

end module DimensionsWrapper4D
