module DimensionsWrapper1D_DLCA

USE DimensionsWrapper1D

implicit none
private

    type, extends(DimensionsWrapper1D_t) :: DimensionsWrapper1D_DLCA_t
        character(len=:), allocatable :: Value(:)
    contains
    private
        procedure         ::                 DimensionsWrapper1D_DLCA_Set
        procedure         ::                 DimensionsWrapper1D_DLCA_Get
        generic,   public :: Set          => DimensionsWrapper1D_DLCA_Set
        generic,   public :: Get          => DimensionsWrapper1D_DLCA_Get
        procedure, public :: isOfDataType => DimensionsWrapper1D_DLCA_isOfDataType
        procedure, public :: Free         => DimensionsWrapper1D_DLCA_Free
        final             ::                 DimensionsWrapper1D_DLCA_Final
    end type           

public :: DimensionsWrapper1D_DLCA_t

contains


    subroutine DimensionsWrapper1D_DLCA_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper1D
    !-----------------------------------------------------------------
        type(DimensionsWrapper1D_DLCA_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper1D_DLCA_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set DLCA Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_DLCA_t), intent(INOUT) :: this
        class(*),                         intent(IN)    :: Value(:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (character(len=*))
!                allocate(this%Value(size(Value,dim=1)), &
!                                    source=Value)
        end select
    end subroutine


    subroutine DimensionsWrapper1D_DLCA_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get deferred length character array Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_DLCA_t), intent(IN)  :: this
        character(len=:), allocatable,     intent(OUT) :: Value(:)
    !-----------------------------------------------------------------
!        allocate(Value(size(this%Value,dim=1)), &
!                       source=this%Value)
    end subroutine


    subroutine DimensionsWrapper1D_DLCA_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper1D
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_DLCA_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        if(allocated(this%Value)) deallocate(this%Value)
    end subroutine


    function DimensionsWrapper1D_DLCA_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_DLCA_t), intent(IN) :: this         !< Dimensions wrapper 1D
        class(*),                          intent(IN) :: Mold         !< Mold for data type comparison
        logical                                  :: isOfDataType      !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (character(len=*))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper1D_DLCA_isOfDataType

end module DimensionsWrapper1D_DLCA
