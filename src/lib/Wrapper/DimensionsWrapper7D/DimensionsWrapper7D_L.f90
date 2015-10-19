module DimensionsWrapper7D_L

USE DimensionsWrapper7D

implicit none
private

    type, extends(DimensionsWrapper7D_t) :: DimensionsWrapper7D_L_t
        logical, allocatable :: Value(:,:,:,:,:,:,:)
    contains
    private
        procedure         ::                 DimensionsWrapper7D_L_Set
        procedure         ::                 DimensionsWrapper7D_L_Get
        generic,   public :: Set          => DimensionsWrapper7D_L_Set
        generic,   public :: Get          => DimensionsWrapper7D_L_Get
        procedure, public :: isOfDataType => DimensionsWrapper7D_L_isOfDataType
        procedure, public :: Free         => DimensionsWrapper7D_L_Free
        final             ::                 DimensionsWrapper7D_L_Final
    end type           

public :: DimensionsWrapper7D_L_t

contains


    subroutine DimensionsWrapper7D_L_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper7D
    !-----------------------------------------------------------------
        type(DimensionsWrapper7D_L_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper7D_L_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set logical Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_L_t), intent(INOUT) :: this
        class(*),                       intent(IN)    :: Value(:,:,:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (logical)
                allocate(this%Value(size(Value,dim=1),  &
                                    size(Value,dim=2),  &
                                    size(Value,dim=3),  &
                                    size(Value,dim=4),  &
                                    size(Value,dim=5),  &
                                    size(Value,dim=6),  &
                                    size(Value,dim=7)), &
                                    source=Value)
        end select
    end subroutine


    subroutine DimensionsWrapper7D_L_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get logical Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_L_t), intent(IN)  :: this
        logical, allocatable,           intent(OUT) :: Value(:,:,:,:,:,:,:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1),  &
                       size(this%Value,dim=2),  &
                       size(this%Value,dim=3),  &
                       size(this%Value,dim=4),  &
                       size(this%Value,dim=5),  &
                       size(this%Value,dim=6),  &
                       size(this%Value,dim=7)), &
                       source=this%Value)
    end subroutine


    subroutine DimensionsWrapper7D_L_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper7D
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_L_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        if(allocated(this%Value)) deallocate(this%Value)
    end subroutine


    function DimensionsWrapper7D_L_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_L_t), intent(IN) :: this            !< Dimensions wrapper 7D
        class(*),                       intent(IN) :: Mold            !< Mold for data type comparison
        logical                                    :: isOfDataType    !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (logical)
                isOfDataType = .true.
        end select
    end function DimensionsWrapper7D_L_isOfDataType

end module DimensionsWrapper7D_L
