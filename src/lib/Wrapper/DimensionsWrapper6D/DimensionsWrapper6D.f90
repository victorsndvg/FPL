module DimensionsWrapper6D

USE DimensionsWrapper

implicit none
private

    type, extends(DimensionsWrapper_t), abstract :: DimensionsWrapper6D_t
    private
    contains
        procedure(DimensionsWrapper6D_isOfDataType), deferred :: isOfDataType
        procedure(DimensionsWrapper6D_Free),         deferred :: Free
    end type

    abstract interface
        subroutine DimensionsWrapper6D_Free(this)
            import DimensionsWrapper6D_t
            class(DimensionsWrapper6D_t), intent(INOUT) :: this
        end subroutine
        function DimensionsWrapper6D_isOfDataType(this, Mold) result(isOfDataType)
            import DimensionsWrapper6D_t
            class(DimensionsWrapper6D_t), intent(IN) :: this
            class(*),                   intent(IN) :: Mold
            logical                                :: isOfDataType
        end function
    end interface

public :: DimensionsWrapper6D_t

end module DimensionsWrapper6D
