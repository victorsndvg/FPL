module DimensionsWrapper6D_L

USE DimensionsWrapper6D

implicit none
private

    type, extends(DimensionsWrapper6D_t) :: DimensionsWrapper6D_L_t
        logical, allocatable :: Value(:,:,:,:,:,:)
    contains
    private
        procedure, public :: Set          => DimensionsWrapper6D_L_Set
        procedure, public :: Get          => DimensionsWrapper6D_L_Get
        procedure, public :: isOfDataType => DimensionsWrapper6D_L_isOfDataType
        procedure, public :: Free         => DimensionsWrapper6D_L_Free
        final             ::                 DimensionsWrapper6D_L_Final
    end type           

public :: DimensionsWrapper6D_L_t

contains


    subroutine DimensionsWrapper6D_L_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper6D
    !-----------------------------------------------------------------
        type(DimensionsWrapper6D_L_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper6D_L_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set logical Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_L_t), intent(INOUT) :: this
        class(*),                       intent(IN)    :: Value(:,:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (logical)
                allocate(this%Value(size(Value,dim=1),  &
                                    size(Value,dim=2),  &
                                    size(Value,dim=3),  &
                                    size(Value,dim=4),  &
                                    size(Value,dim=5),  &
                                    size(Value,dim=6)), &
                                    source=Value)
        end select
    end subroutine


    subroutine DimensionsWrapper6D_L_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get logical Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_L_t), intent(IN)  :: this
        logical, allocatable,           intent(OUT) :: Value(:,:,:,:,:,:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1),  &
                       size(this%Value,dim=2),  &
                       size(this%Value,dim=3),  &
                       size(this%Value,dim=4),  &
                       size(this%Value,dim=5),  &
                       size(this%Value,dim=6)), &
                       source=this%Value)
    end subroutine


    subroutine DimensionsWrapper6D_L_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper6D
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_L_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        if(allocated(this%Value)) deallocate(this%Value)
    end subroutine


    function DimensionsWrapper6D_L_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_L_t), intent(IN) :: this            !< Dimensions wrapper 6D
        class(*),                       intent(IN) :: Mold            !< Mold for data type comparison
        logical                                    :: isOfDataType    !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (logical)
                isOfDataType = .true.
        end select
    end function DimensionsWrapper6D_L_isOfDataType

end module DimensionsWrapper6D_L
