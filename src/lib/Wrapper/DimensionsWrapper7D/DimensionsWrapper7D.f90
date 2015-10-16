module DimensionsWrapper7D

USE DimensionsWrapper

implicit none
private

    type, extends(DimensionsWrapper_t), abstract :: DimensionsWrapper7D_t
    private
    contains
        procedure(DimensionsWrapper7D_isOfDataType), deferred :: isOfDataType
        procedure(DimensionsWrapper7D_Free),         deferred :: Free
    end type

    abstract interface
        subroutine DimensionsWrapper7D_Free(this)
            import DimensionsWrapper7D_t
            class(DimensionsWrapper7D_t), intent(INOUT) :: this
        end subroutine
        function DimensionsWrapper7D_isOfDataType(this, Mold) result(isOfDataType)
            import DimensionsWrapper7D_t
            class(DimensionsWrapper7D_t), intent(IN) :: this
            class(*),                   intent(IN) :: Mold
            logical                                :: isOfDataType
        end function
    end interface

public :: DimensionsWrapper7D_t

end module DimensionsWrapper7D
