module DimensionsWrapper1D_I4P

USE DimensionsWrapper1D
USE IR_Precision, only: I4P

implicit none
private

    type, extends(DimensionsWrapper1D_t) :: DimensionsWrapper1D_I4P_t
        integer(I4P), allocatable :: Value(:)
    contains
    private
        procedure         ::                 DimensionsWrapper1D_I4P_Set
        procedure         ::                 DimensionsWrapper1D_I4P_Get
        generic,   public :: Set          => DimensionsWrapper1D_I4P_Set
        generic,   public :: Get          => DimensionsWrapper1D_I4P_Get
        procedure, public :: isOfDataType => DimensionsWrapper1D_I4P_isOfDataType
        procedure, public :: Free         => DimensionsWrapper1D_I4P_Free
        final             ::                 DimensionsWrapper1D_I4P_Final
    end type           

public :: DimensionsWrapper1D_I4P_t

contains


    subroutine DimensionsWrapper1D_I4P_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper1D
    !-----------------------------------------------------------------
        type(DimensionsWrapper1D_I4P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper1D_I4P_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set I4P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_I4P_t), intent(INOUT) :: this
        class(*),                         intent(IN)    :: Value(:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I4P))
                allocate(this%Value(size(Value,dim=1)), source=Value)
        end select
    end subroutine


    subroutine DimensionsWrapper1D_I4P_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get I4P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_I4P_t), intent(IN)  :: this
        integer(I4P), allocatable,        intent(OUT) :: Value(:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1)), source=this%Value)
    end subroutine


    subroutine DimensionsWrapper1D_I4P_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper1D
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_I4P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        if(allocated(this%Value)) deallocate(this%Value)
    end subroutine


    function DimensionsWrapper1D_I4P_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_I4P_t), intent(IN) :: this           !< Dimensions wrapper 1D
        class(*),                         intent(IN) :: Mold           !< Mold for data type comparison
        logical                                      :: isOfDataType   !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (integer(I4P))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper1D_I4P_isOfDataType

end module DimensionsWrapper1D_I4P