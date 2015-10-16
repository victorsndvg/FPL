module DimensionsWrapper4D_L

USE DimensionsWrapper4D

implicit none
private

    type, extends(DimensionsWrapper4D_t) :: DimensionsWrapper4D_L_t
        logical, allocatable :: Value(:,:,:,:)
    contains
    private
        procedure, public :: Set          => DimensionsWrapper4D_L_Set
        procedure, public :: Get          => DimensionsWrapper4D_L_Get
        procedure, public :: isOfDataType => DimensionsWrapper4D_L_isOfDataType
        procedure, public :: Free         => DimensionsWrapper4D_L_Free
        final             ::                 DimensionsWrapper4D_L_Final
    end type           

public :: DimensionsWrapper4D_L_t

contains


    subroutine DimensionsWrapper4D_L_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper4D
    !-----------------------------------------------------------------
        type(DimensionsWrapper4D_L_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper4D_L_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set logical Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_L_t), intent(INOUT) :: this
        class(*),                       intent(IN)    :: Value(:,:,:,:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (logical)
                allocate(this%Value(size(Value,dim=1),  &
                                    size(Value,dim=2),  &
                                    size(Value,dim=3),  &
                                    size(Value,dim=4)), &
                                    source=Value)
        end select
    end subroutine


    subroutine DimensionsWrapper4D_L_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get logical Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_L_t), intent(IN)  :: this
        logical, allocatable,           intent(OUT) :: Value(:,:,:,:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1),  &
                       size(this%Value,dim=2),  &
                       size(this%Value,dim=3),  &
                       size(this%Value,dim=4)), &
                       source=this%Value)
    end subroutine


    subroutine DimensionsWrapper4D_L_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper4D
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_L_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        if(allocated(this%Value)) deallocate(this%Value)
    end subroutine


    function DimensionsWrapper4D_L_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_L_t), intent(IN) :: this            !< Dimensions wrapper 4D
        class(*),                       intent(IN) :: Mold            !< Mold for data type comparison
        logical                                    :: isOfDataType    !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (logical)
                isOfDataType = .true.
        end select
    end function DimensionsWrapper4D_L_isOfDataType

end module DimensionsWrapper4D_L
