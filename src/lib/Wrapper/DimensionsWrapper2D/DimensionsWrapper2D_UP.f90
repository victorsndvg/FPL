module DimensionsWrapper2D_UP

USE DimensionsWrapper2D

implicit none
private

    type, extends(DimensionsWrapper2D_t) :: DimensionsWrapper2D_UP_t
        class(*), allocatable :: Value(:,:)
    contains
    private
        procedure         ::                 DimensionsWrapper2D_UP_Set
        procedure         ::                 DimensionsWrapper2D_UP_Get
        generic,   public :: Set          => DimensionsWrapper2D_UP_Set
        generic,   public :: Get          => DimensionsWrapper2D_UP_Get
        procedure, public :: isOfDataType => DimensionsWrapper2D_UP_isOfDataType
        procedure, public :: Free         => DimensionsWrapper2D_UP_Free
        final             ::                 DimensionsWrapper2D_UP_Final
    end type           

public :: DimensionsWrapper2D_UP_t

contains


    subroutine DimensionsWrapper2D_UP_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper2D
    !-----------------------------------------------------------------
        type(DimensionsWrapper2D_UP_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper2D_UP_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_UP_t), intent(INOUT) :: this
        class(*),                        intent(IN)    :: Value(:,:)
    !-----------------------------------------------------------------
        allocate(this%Value(size(Value,dim=1),  &
                            size(Value,dim=2)), &
                            source=Value)
    end subroutine


    subroutine DimensionsWrapper2D_UP_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_UP_t), intent(IN)  :: this
        class(*), allocatable,           intent(OUT) :: Value(:,:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1),  &
                       size(this%Value,dim=2)), &
                       source=this%Value)
    end subroutine


    subroutine DimensionsWrapper2D_UP_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper2D
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_UP_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        if(allocated(this%Value)) deallocate(this%Value)
    end subroutine


    function DimensionsWrapper2D_UP_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_UP_t), intent(IN) :: this              !< Dimensions wrapper 2D
        class(*),                     intent(IN) :: Mold              !< Mold for data type comparison
        logical                                  :: isOfDataType      !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = same_type_as(this%Value, Mold)
    end function DimensionsWrapper2D_UP_isOfDataType

end module DimensionsWrapper2D_UP
