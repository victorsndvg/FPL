module DimensionsWrapper0D_L

USE DimensionsWrapper0D

implicit none
private

    type, extends(DimensionsWrapper0D_t) :: DimensionsWrapper0D_L_t
        logical, allocatable :: Value
    contains
    private
        procedure         ::                 DimensionsWrapper0D_L_Set
        procedure         ::                 DimensionsWrapper0D_L_Get
        generic,   public :: Set          => DimensionsWrapper0D_L_Set
        generic,   public :: Get          => DimensionsWrapper0D_L_Get
        procedure, public :: isOfDataType => DimensionsWrapper0D_L_isOfDataType
        procedure, public :: Free         => DimensionsWrapper0D_L_Free
        final             ::                 DimensionsWrapper0D_L_Final
    end type           

public :: DimensionsWrapper0D_L_t

contains


    subroutine DimensionsWrapper0D_L_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper0D
    !-----------------------------------------------------------------
        type(DimensionsWrapper0D_L_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper0D_L_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set logical Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_L_t), intent(INOUT) :: this
        class(*),                       intent(IN)    :: Value
    !-----------------------------------------------------------------
        select type (Value)
            type is (logical)
                allocate(this%Value, source=Value)
        end select
    end subroutine


    subroutine DimensionsWrapper0D_L_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get logical Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_L_t), intent(IN)  :: this
        logical, allocatable,           intent(OUT) :: Value
    !-----------------------------------------------------------------
        allocate(Value, source=this%Value)
    end subroutine


    subroutine DimensionsWrapper0D_L_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper0D
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_L_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        if(allocated(this%Value)) deallocate(this%Value)
    end subroutine


    function DimensionsWrapper0D_L_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_L_t), intent(IN) :: this            !< Dimensions wrapper 0D
        class(*),                       intent(IN) :: Mold            !< Mold for data type comparison
        logical                                    :: isOfDataType    !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (logical)
                isOfDataType = .true.
        end select
    end function DimensionsWrapper0D_L_isOfDataType

end module DimensionsWrapper0D_L
