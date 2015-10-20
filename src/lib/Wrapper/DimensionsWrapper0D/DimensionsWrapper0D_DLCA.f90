module DimensionsWrapper0D_DLCA

USE DimensionsWrapper0D
USE IR_Precision, only: I4P, str

implicit none
private

    type, extends(DimensionsWrapper0D_t) :: DimensionsWrapper0D_DLCA_t
        character(len=:), allocatable :: Value
    contains
    private
        procedure         ::                 DimensionsWrapper0D_DLCA_Set
        procedure         ::                 DimensionsWrapper0D_DLCA_Get
        generic,   public :: Set          => DimensionsWrapper0D_DLCA_Set
        generic,   public :: Get          => DimensionsWrapper0D_DLCA_Get
        procedure, public :: isOfDataType => DimensionsWrapper0D_DLCA_isOfDataType
        procedure, public :: Free         => DimensionsWrapper0D_DLCA_Free
        procedure, public :: Print        => DimensionsWrapper0D_DLCA_Print
        final             ::                 DimensionsWrapper0D_DLCA_Final
    end type           

public :: DimensionsWrapper0D_DLCA_t

contains


    subroutine DimensionsWrapper0D_DLCA_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper0D
    !-----------------------------------------------------------------
        type(DimensionsWrapper0D_DLCA_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper0D_DLCA_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set DLCA Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_DLCA_t), intent(INOUT) :: this
        class(*),                          intent(IN)    :: Value
    !-----------------------------------------------------------------
        select type (Value)
            type is (character(len=*))
                allocate(this%Value, source=Value)
        end select
    end subroutine


    subroutine DimensionsWrapper0D_DLCA_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get deferred length character array Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_DLCA_t), intent(IN)  :: this
        character(len=:), allocatable,     intent(OUT) :: Value
    !-----------------------------------------------------------------
        allocate(Value, source=this%Value)
    end subroutine


    subroutine DimensionsWrapper0D_DLCA_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper0D
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_DLCA_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        if(allocated(this%Value)) deallocate(this%Value)
    end subroutine


    subroutine DimensionsWrapper0D_DLCA_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_DLCA_t), intent(IN)  :: this        !< DimensionsWrapper
        integer(I4P),                      intent(IN)  :: unit        !< Logic unit.
        character(*), optional,            intent(IN)  :: prefix      !< Prefixing string.
        integer(I4P), optional,            intent(OUT) :: iostat      !< IO error.
        character(*), optional,            intent(OUT) :: iomsg       !< IO error message.
        character(len=:), allocatable                  :: prefd       !< Prefixing string.
        integer(I4P)                                   :: iostatd     !< IO error.
        character(500)                                 :: iomsgd      !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = DLCA'//&
                            ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                            ', Value = '//trim(this%Value)
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper0D_DLCA_Print


    function DimensionsWrapper0D_DLCA_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_DLCA_t), intent(IN) :: this         !< Dimensions wrapper 0D
        class(*),                          intent(IN) :: Mold         !< Mold for data type comparison
        logical                                  :: isOfDataType      !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (character(len=*))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper0D_DLCA_isOfDataType

end module DimensionsWrapper0D_DLCA
