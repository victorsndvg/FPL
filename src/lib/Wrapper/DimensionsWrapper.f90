module DimensionsWrapper

USE IR_Precision, only: I1P

implicit none
private

    type, abstract :: DimensionsWrapper_t
    private
        integer(I1P) :: Dimensions = -1
    contains
        procedure :: SetDimensions => DimensionsWrapper_SetDimensions
        procedure :: GetDimensions => DimensionsWrapper_GetDimensions
        procedure(DimensionsWrapper_isOfDataType), deferred :: isOfDataType
        procedure(DimensionsWrapper_Free),         deferred :: Free
    end type

    abstract interface
        subroutine DimensionsWrapper_Free(this)
            import DimensionsWrapper_t
            class(DimensionsWrapper_t), intent(INOUT) :: this
        end subroutine
        function DimensionsWrapper_isOfDataType(this, Mold) result(isOfDataType)
            import DimensionsWrapper_t
            class(DimensionsWrapper_t), intent(IN) :: this
            class(*),                   intent(IN) :: Mold
            logical                                :: isOfDataType
        end function
    end interface

public :: DimensionsWrapper_t


contains

    subroutine DimensionsWrapper_SetDimensions(this, Dimensions)
    !-----------------------------------------------------------------
    !< Set the dimensions of the Value contained in the wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper_t), intent(INOUT) :: this
        integer(I1P),               intent(IN)    :: Dimensions
    !-----------------------------------------------------------------
        this%Dimensions = Dimensions
    end subroutine


    function DimensionsWrapper_GetDimensions(this) result(Dimensions)
    !-----------------------------------------------------------------
    !< Get the dimensions of the Value contained in the wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        integer(I1P)                              :: Dimensions
        Dimensions = this%Dimensions
    end function

end module DimensionsWrapper
