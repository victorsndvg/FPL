module DimensionsWrapper0D_I4P

USE DimensionsWrapper0D
USE IR_Precision, only: I4P

implicit none
private

    type, extends(DimensionsWrapper0D_t) :: DimensionsWrapper0D_I4P_t
        integer(I4P), allocatable :: Value
    contains
    private
        procedure, public :: Set          => DimensionsWrapper0D_I4P_Set
        procedure, public :: Get          => DimensionsWrapper0D_I4P_Get
        procedure, public :: isOfDataType => DimensionsWrapper0D_I4P_isOfDataType
        procedure, public :: Free         => DimensionsWrapper0D_I4P_Free
        final             ::                 DimensionsWrapper0D_I4P_Final
    end type           

public :: DimensionsWrapper0D_I4P_t

contains


    subroutine DimensionsWrapper0D_I4P_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper0D
    !-----------------------------------------------------------------
        type(DimensionsWrapper0D_I4P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper0D_I4P_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set I4P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_I4P_t), intent(INOUT) :: this
        class(*),                         intent(IN)    :: Value
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I4P))
                allocate(this%Value, source=Value)
        end select
    end subroutine


    subroutine DimensionsWrapper0D_I4P_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get I4P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_I4P_t), intent(IN)  :: this
        integer(I4P), allocatable,        intent(OUT) :: Value
    !-----------------------------------------------------------------
        allocate(Value, source=this%Value)
    end subroutine


    subroutine DimensionsWrapper0D_I4P_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper0D
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_I4P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        if(allocated(this%Value)) deallocate(this%Value)
    end subroutine


    function DimensionsWrapper0D_I4P_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_I4P_t), intent(IN) :: this           !< Dimensions wrapper 0D
        class(*),                         intent(IN) :: Mold           !< Mold for data type comparison
        logical                                      :: isOfDataType   !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (integer(I4P))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper0D_I4P_isOfDataType

end module DimensionsWrapper0D_I4P
