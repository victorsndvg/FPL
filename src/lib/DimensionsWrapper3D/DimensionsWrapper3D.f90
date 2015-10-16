module DimensionsWrapper3D

USE DimensionsWrapper

implicit none
private

    type, extends(DimensionsWrapper_t), abstract :: DimensionsWrapper3D_t
    private
    contains
        procedure(DimensionsWrapper3D_isOfDataType), deferred :: isOfDataType
        procedure(DimensionsWrapper3D_Free),         deferred :: Free
    end type

    abstract interface
        subroutine DimensionsWrapper3D_Free(this)
            import DimensionsWrapper3D_t
            class(DimensionsWrapper3D_t), intent(INOUT) :: this
        end subroutine
        function DimensionsWrapper3D_isOfDataType(this, Mold) result(isOfDataType)
            import DimensionsWrapper3D_t
            class(DimensionsWrapper3D_t), intent(IN) :: this
            class(*),                   intent(IN) :: Mold
            logical                                :: isOfDataType
        end function
    end interface

public :: DimensionsWrapper3D_t

end module DimensionsWrapper3D
