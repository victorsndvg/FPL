module DimensionsWrapper5D_UP

USE DimensionsWrapper5D

implicit none
private

    type, extends(DimensionsWrapper5D_t) :: DimensionsWrapper5D_UP_t
        class(*), allocatable :: Value(:,:,:,:,:)
    contains
    private
        procedure, public :: Set          => DimensionsWrapper5D_UP_Set
        procedure, public :: Get          => DimensionsWrapper5D_UP_Get
        procedure, public :: isOfDataType => DimensionsWrapper5D_UP_isOfDataType
        procedure, public :: Free         => DimensionsWrapper5D_UP_Free
        final             ::                 DimensionsWrapper5D_UP_Final
    end type           

public :: DimensionsWrapper5D_UP_t

contains


    subroutine DimensionsWrapper5D_UP_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper5D
    !-----------------------------------------------------------------
        type(DimensionsWrapper5D_UP_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper5D_UP_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_UP_t), intent(INOUT) :: this
        class(*),                         intent(IN)    :: Value(:,:,:,:,:)
    !-----------------------------------------------------------------
        allocate(this%Value(size(Value,dim=1),  &
                            size(Value,dim=2),  &
                            size(Value,dim=3),  &
                            size(Value,dim=4),  &
                            size(Value,dim=5)), &
                            source=Value)
    end subroutine


    subroutine DimensionsWrapper5D_UP_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_UP_t), intent(IN)  :: this
        class(*), allocatable,           intent(OUT) :: Value(:,:,:,:,:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1),  &
                       size(this%Value,dim=2),  &
                       size(this%Value,dim=3),  &
                       size(this%Value,dim=4),  &
                       size(this%Value,dim=5)), &
                       source=this%Value)
    end subroutine


    subroutine DimensionsWrapper5D_UP_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper5D
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_UP_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        if(allocated(this%Value)) deallocate(this%Value)
    end subroutine


    function DimensionsWrapper5D_UP_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_UP_t), intent(IN) :: this              !< Dimensions wrapper 5D
        class(*),                     intent(IN) :: Mold              !< Mold for data type comparison
        logical                                  :: isOfDataType      !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = same_type_as(this%Value, Mold)
    end function DimensionsWrapper5D_UP_isOfDataType

end module DimensionsWrapper5D_UP
