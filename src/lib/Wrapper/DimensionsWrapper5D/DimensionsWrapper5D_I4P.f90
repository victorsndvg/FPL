module DimensionsWrapper5D_I4P

USE DimensionsWrapper5D
USE IR_Precision, only: I4P

implicit none
private

    type, extends(DimensionsWrapper5D_t) :: DimensionsWrapper5D_I4P_t
        integer(I4P), allocatable :: Value(:,:,:,:,:)
    contains
    private
        procedure, public :: Set          => DimensionsWrapper5D_I4P_Set
        procedure, public :: Get          => DimensionsWrapper5D_I4P_Get
        procedure, public :: isOfDataType => DimensionsWrapper5D_I4P_isOfDataType
        procedure, public :: Free         => DimensionsWrapper5D_I4P_Free
        final             ::                 DimensionsWrapper5D_I4P_Final
    end type           

public :: DimensionsWrapper5D_I4P_t

contains


    subroutine DimensionsWrapper5D_I4P_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper5D
    !-----------------------------------------------------------------
        type(DimensionsWrapper5D_I4P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper5D_I4P_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set I4P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_I4P_t), intent(INOUT) :: this
        class(*),                         intent(IN)    :: Value(:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I4P))
                allocate(this%Value(size(Value,dim=1),  &
                                    size(Value,dim=2),  &
                                    size(Value,dim=3),  &
                                    size(Value,dim=4),  &
                                    size(Value,dim=5)), &
                                    source=Value)
        end select
    end subroutine


    subroutine DimensionsWrapper5D_I4P_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get I4P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_I4P_t), intent(IN)  :: this
        integer(I4P), allocatable,        intent(OUT) :: Value(:,:,:,:,:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1),  &
                       size(this%Value,dim=2),  &
                       size(this%Value,dim=3),  &
                       size(this%Value,dim=4),  &
                       size(this%Value,dim=5)), &
                       source=this%Value)
    end subroutine


    subroutine DimensionsWrapper5D_I4P_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper5D
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_I4P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        if(allocated(this%Value)) deallocate(this%Value)
    end subroutine


    function DimensionsWrapper5D_I4P_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_I4P_t), intent(IN) :: this           !< Dimensions wrapper 5D
        class(*),                         intent(IN) :: Mold           !< Mold for data type comparison
        logical                                      :: isOfDataType   !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (integer(I4P))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper5D_I4P_isOfDataType

end module DimensionsWrapper5D_I4P
