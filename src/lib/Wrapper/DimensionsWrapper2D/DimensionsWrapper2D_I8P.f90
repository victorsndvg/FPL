module DimensionsWrapper2D_I8P

USE DimensionsWrapper2D
USE IR_Precision, only: I8P

implicit none
private

    type, extends(DimensionsWrapper2D_t) :: DimensionsWrapper2D_I8P_t
        integer(I8P), allocatable :: Value(:,:)
    contains
    private
        procedure, public :: Set          => DimensionsWrapper2D_I8P_Set
        procedure, public :: Get          => DimensionsWrapper2D_I8P_Get
        procedure, public :: isOfDataType => DimensionsWrapper2D_I8P_isOfDataType
        procedure, public :: Free         => DimensionsWrapper2D_I8P_Free
        final             ::                 DimensionsWrapper2D_I8P_Final
    end type           

public :: DimensionsWrapper2D_I8P_t

contains


    subroutine DimensionsWrapper2D_I8P_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper2D
    !-----------------------------------------------------------------
        type(DimensionsWrapper2D_I8P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper2D_I8P_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set I8P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_I8P_t), intent(INOUT) :: this
        class(*),                         intent(IN)    :: Value(:,:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I8P))
                allocate(this%Value(size(Value,dim=1),  &
                                    size(Value,dim=2)), &
                                    source=Value)
        end select
    end subroutine


    subroutine DimensionsWrapper2D_I8P_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get I8P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_I8P_t), intent(IN)  :: this
        integer(I8P), allocatable,        intent(OUT) :: Value(:,:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1),  &
                       size(this%Value,dim=2)), &
                       source=this%Value)
    end subroutine


    subroutine DimensionsWrapper2D_I8P_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper2D
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_I8P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        if(allocated(this%Value)) deallocate(this%Value)
    end subroutine


    function DimensionsWrapper2D_I8P_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_I8P_t), intent(IN) :: this          !< Dimensions wrapper 2D
        class(*),                         intent(IN) :: Mold          !< Mold for data type comparison
        logical                                      :: isOfDataType  !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (integer(I8P))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper2D_I8P_isOfDataType

end module DimensionsWrapper2D_I8P
