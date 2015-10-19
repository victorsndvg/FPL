module DimensionsWrapper4D_DLCA

USE DimensionsWrapper4D

implicit none
private

    type, extends(DimensionsWrapper4D_t) :: DimensionsWrapper4D_DLCA_t
        character(len=:), allocatable :: Value(:,:,:,:)
    contains
    private
        procedure         ::                 DimensionsWrapper4D_DLCA_Set
        procedure         ::                 DimensionsWrapper4D_DLCA_Get
        generic,   public :: Set          => DimensionsWrapper4D_DLCA_Set
        generic,   public :: Get          => DimensionsWrapper4D_DLCA_Get
        procedure, public :: isOfDataType => DimensionsWrapper4D_DLCA_isOfDataType
        procedure, public :: Free         => DimensionsWrapper4D_DLCA_Free
        final             ::                 DimensionsWrapper4D_DLCA_Final
    end type           

public :: DimensionsWrapper4D_DLCA_t

contains


    subroutine DimensionsWrapper4D_DLCA_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper4D
    !-----------------------------------------------------------------
        type(DimensionsWrapper4D_DLCA_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper4D_DLCA_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set DLCA Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_DLCA_t), intent(INOUT) :: this
        class(*),                         intent(IN)    :: Value(:,:,:,:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (character(len=*))
!                allocate(this%Value(size(Value,dim=1),  &
!                                    size(Value,dim=2),  &
!                                    size(Value,dim=3),  &
!                                    size(Value,dim=4)), &
!                                    source=Value)
        end select
    end subroutine


    subroutine DimensionsWrapper4D_DLCA_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get deferred length character array Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_DLCA_t), intent(IN)  :: this
        character(len=:), allocatable,     intent(OUT) :: Value(:,:,:,:)
    !-----------------------------------------------------------------
!        allocate(Value(size(this%Value,dim=1),  &
!                       size(this%Value,dim=2),  &
!                       size(this%Value,dim=3),  &
!                       size(this%Value,dim=4)), &
!                       source=this%Value)
    end subroutine


    subroutine DimensionsWrapper4D_DLCA_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper4D
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_DLCA_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        if(allocated(this%Value)) deallocate(this%Value)
    end subroutine


    function DimensionsWrapper4D_DLCA_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_DLCA_t), intent(IN) :: this         !< Dimensions wrapper 4D
        class(*),                          intent(IN) :: Mold         !< Mold for data type comparison
        logical                                  :: isOfDataType      !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (character(len=*))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper4D_DLCA_isOfDataType

end module DimensionsWrapper4D_DLCA
