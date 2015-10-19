module DimensionsWrapper6D_UP

USE DimensionsWrapper6D

implicit none
private

    type, extends(DimensionsWrapper6D_t) :: DimensionsWrapper6D_UP_t
        class(*), allocatable :: Value(:,:,:,:,:,:)
    contains
    private
        procedure         ::                 DimensionsWrapper6D_UP_Set
        procedure         ::                 DimensionsWrapper6D_UP_Get
        generic,   public :: Set          => DimensionsWrapper6D_UP_Set
        generic,   public :: Get          => DimensionsWrapper6D_UP_Get
        procedure, public :: isOfDataType => DimensionsWrapper6D_UP_isOfDataType
        procedure, public :: Free         => DimensionsWrapper6D_UP_Free
        final             ::                 DimensionsWrapper6D_UP_Final
    end type           

public :: DimensionsWrapper6D_UP_t

contains


    subroutine DimensionsWrapper6D_UP_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper6D
    !-----------------------------------------------------------------
        type(DimensionsWrapper6D_UP_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper6D_UP_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_UP_t), intent(INOUT) :: this
        class(*),                         intent(IN)    :: Value(:,:,:,:,:,:)
    !-----------------------------------------------------------------
        allocate(this%Value(size(Value,dim=1),  &
                            size(Value,dim=2),  &
                            size(Value,dim=3),  &
                            size(Value,dim=4),  &
                            size(Value,dim=5),  &
                            size(Value,dim=6)), &
                            source=Value)
    end subroutine


    subroutine DimensionsWrapper6D_UP_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_UP_t), intent(IN)  :: this
        class(*), allocatable,           intent(OUT) :: Value(:,:,:,:,:,:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1),  &
                       size(this%Value,dim=2),  &
                       size(this%Value,dim=3),  &
                       size(this%Value,dim=4),  &
                       size(this%Value,dim=5),  &
                       size(this%Value,dim=6)), &
                       source=this%Value)
    end subroutine


    subroutine DimensionsWrapper6D_UP_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper6D
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_UP_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        if(allocated(this%Value)) deallocate(this%Value)
    end subroutine


    function DimensionsWrapper6D_UP_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_UP_t), intent(IN) :: this              !< Dimensions wrapper 6D
        class(*),                     intent(IN) :: Mold              !< Mold for data type comparison
        logical                                  :: isOfDataType      !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = same_type_as(this%Value, Mold)
    end function DimensionsWrapper6D_UP_isOfDataType

end module DimensionsWrapper6D_UP
