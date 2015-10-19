module DimensionsWrapper1D_L

USE DimensionsWrapper1D

implicit none
private

    type, extends(DimensionsWrapper1D_t) :: DimensionsWrapper1D_L_t
        logical, allocatable :: Value(:)
    contains
    private
        procedure         ::                 DimensionsWrapper1D_L_Set
        procedure         ::                 DimensionsWrapper1D_L_Get
        generic,   public :: Set          => DimensionsWrapper1D_L_Set
        generic,   public :: Get          => DimensionsWrapper1D_L_Get
        procedure, public :: isOfDataType => DimensionsWrapper1D_L_isOfDataType
        procedure, public :: Free         => DimensionsWrapper1D_L_Free
        final             ::                 DimensionsWrapper1D_L_Final
    end type           

public :: DimensionsWrapper1D_L_t

contains


    subroutine DimensionsWrapper1D_L_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper1D
    !-----------------------------------------------------------------
        type(DimensionsWrapper1D_L_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper1D_L_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set logical Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_L_t), intent(INOUT) :: this
        class(*),                       intent(IN)    :: Value(:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (logical)
                allocate(this%Value(size(Value,dim=1)), source=Value)
        end select
    end subroutine


    subroutine DimensionsWrapper1D_L_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get logical Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_L_t), intent(IN)  :: this
        logical, allocatable,           intent(OUT) :: Value(:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1)), source=this%Value)
    end subroutine


    subroutine DimensionsWrapper1D_L_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper1D
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_L_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        if(allocated(this%Value)) deallocate(this%Value)
    end subroutine


    function DimensionsWrapper1D_L_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_L_t), intent(IN) :: this            !< Dimensions wrapper 1D
        class(*),                       intent(IN) :: Mold            !< Mold for data type comparison
        logical                                    :: isOfDataType    !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (logical)
                isOfDataType = .true.
        end select
    end function DimensionsWrapper1D_L_isOfDataType

end module DimensionsWrapper1D_L
