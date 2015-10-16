module DimensionsWrapper4D_UP

USE DimensionsWrapper4D

implicit none
private

    type, extends(DimensionsWrapper4D_t) :: DimensionsWrapper4D_UP_t
        class(*), allocatable :: Value(:,:,:,:)
    contains
    private
        procedure, public :: Set          => DimensionsWrapper4D_UP_Set
        procedure, public :: Get          => DimensionsWrapper4D_UP_Get
        procedure, public :: isOfDataType => DimensionsWrapper4D_UP_isOfDataType
        procedure, public :: Free         => DimensionsWrapper4D_UP_Free
        final             ::                 DimensionsWrapper4D_UP_Final
    end type           

public :: DimensionsWrapper4D_UP_t

contains


    subroutine DimensionsWrapper4D_UP_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper4D
    !-----------------------------------------------------------------
        type(DimensionsWrapper4D_UP_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper4D_UP_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_UP_t), intent(INOUT) :: this
        class(*),                         intent(IN)    :: Value(:,:,:,:)
    !-----------------------------------------------------------------
        allocate(this%Value(size(Value,dim=1),  &
                            size(Value,dim=2),  &
                            size(Value,dim=3),  &
                            size(Value,dim=4)), &
                            source=Value)
    end subroutine


    subroutine DimensionsWrapper4D_UP_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_UP_t), intent(IN)  :: this
        class(*), allocatable,           intent(OUT) :: Value(:,:,:,:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1),  &
                       size(this%Value,dim=2),  &
                       size(this%Value,dim=3),  &
                       size(this%Value,dim=4)), &
                       source=this%Value)
    end subroutine


    subroutine DimensionsWrapper4D_UP_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper4D
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_UP_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        if(allocated(this%Value)) deallocate(this%Value)
    end subroutine


    function DimensionsWrapper4D_UP_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_UP_t), intent(IN) :: this              !< Dimensions wrapper 4D
        class(*),                     intent(IN) :: Mold              !< Mold for data type comparison
        logical                                  :: isOfDataType      !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = same_type_as(this%Value, Mold)
    end function DimensionsWrapper4D_UP_isOfDataType

end module DimensionsWrapper4D_UP
