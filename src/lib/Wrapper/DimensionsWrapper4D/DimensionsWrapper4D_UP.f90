module DimensionsWrapper4D_UP

USE DimensionsWrapper4D
USE IR_Precision, only: I4P, str

implicit none
private

    type, extends(DimensionsWrapper4D_t) :: DimensionsWrapper4D_UP_t
        class(*), allocatable :: Value(:,:,:,:)
    contains
    private
        procedure         ::                 DimensionsWrapper4D_UP_Set
        procedure         ::                 DimensionsWrapper4D_UP_Get
        generic,   public :: Set          => DimensionsWrapper4D_UP_Set
        generic,   public :: Get          => DimensionsWrapper4D_UP_Get
        procedure, public :: isOfDataType => DimensionsWrapper4D_UP_isOfDataType
        procedure, public :: Free         => DimensionsWrapper4D_UP_Free
        procedure, public :: Print        => DimensionsWrapper4D_UP_Print
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


    subroutine DimensionsWrapper4D_UP_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_UP_t), intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = UP, '//&
                        ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper4D_UP_Print

end module DimensionsWrapper4D_UP
