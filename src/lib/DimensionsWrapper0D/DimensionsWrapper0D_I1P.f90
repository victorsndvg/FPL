module DimensionsWrapper0D_I1P

USE DimensionsWrapper0D
USE IR_Precision, only: I1P

implicit none
private

    type, extends(DimensionsWrapper0D_t) :: DimensionsWrapper0D_I1P_t
        integer(I1P), allocatable :: Value
    contains
    private
        procedure, public :: Set          => DimensionsWrapper0D_I1P_Set
        procedure, public :: Get          => DimensionsWrapper0D_I1P_Get
        procedure, public :: isOfDataType => DimensionsWrapper0D_I1P_isOfDataType
        procedure, public :: Free         => DimensionsWrapper0D_I1P_Free
        final             ::                 DimensionsWrapper0D_I1P_Final
    end type           

public :: DimensionsWrapper0D_I1P_t

contains


    subroutine DimensionsWrapper0D_I1P_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper0D
    !-----------------------------------------------------------------
        type(DimensionsWrapper0D_I1P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper0D_I1P_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set I1P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_I1P_t), intent(INOUT) :: this
        class(*),                         intent(IN)    :: Value
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I1P))
                allocate(this%Value, source=Value)
        end select
    end subroutine


    subroutine DimensionsWrapper0D_I1P_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get I1P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_I1P_t), intent(IN)  :: this
        integer(I1P), allocatable,        intent(OUT) :: Value
    !-----------------------------------------------------------------
        allocate(Value, source=this%Value)
    end subroutine


    subroutine DimensionsWrapper0D_I1P_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper0D
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_I1P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        if(allocated(this%Value)) deallocate(this%Value)
    end subroutine


    function DimensionsWrapper0D_I1P_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_I1P_t), intent(IN) :: this          !< Dimensions wrapper 0D
        class(*),                         intent(IN) :: Mold          !< Mold for data type comparison
        logical                                      :: isOfDataType  !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (integer(I1P))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper0D_I1P_isOfDataType

end module DimensionsWrapper0D_I1P
