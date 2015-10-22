module DimensionsWrapper

USE IR_Precision, only: I1P, I4P, str

implicit none
private

    type, abstract :: DimensionsWrapper_t
    private
        integer(I1P) :: Dimensions = -1
    contains
    private
        procedure, public :: SetDimensions => DimensionsWrapper_SetDimensions
        procedure, public :: GetDimensions => DimensionsWrapper_GetDimensions
        procedure, public :: Print         => DimensionsWrapper_Print
        procedure(DimensionsWrapper_isOfDataType), public, deferred :: isOfDataType
        procedure(DimensionsWrapper_Free),         public, deferred :: Free
    end type

    abstract interface
        subroutine DimensionsWrapper_Free(this)
            import DimensionsWrapper_t
            class(DimensionsWrapper_t), intent(INOUT) :: this
        end subroutine

        function DimensionsWrapper_isOfDataType(this, Mold) result(isOfDataType)
            import DimensionsWrapper_t
            class(DimensionsWrapper_t), intent(IN) :: this
            class(*),                   intent(IN) :: Mold
            logical                                :: isOfDataType
        end function
    end interface

public :: DimensionsWrapper_t


contains

    subroutine DimensionsWrapper_SetDimensions(this, Dimensions)
    !-----------------------------------------------------------------
    !< Set the dimensions of the Value contained in the wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper_t), intent(INOUT) :: this
        integer(I1P),               intent(IN)    :: Dimensions
    !-----------------------------------------------------------------
        this%Dimensions = Dimensions
    end subroutine


    function DimensionsWrapper_GetDimensions(this) result(Dimensions)
    !-----------------------------------------------------------------
    !< Get the dimensions of the Value contained in the wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper_t), intent(IN) :: this
    !-----------------------------------------------------------------
        integer(I1P)                              :: Dimensions
        Dimensions = this%Dimensions
    end function


    subroutine DimensionsWrapper_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Generic Wrapper Print
    !-----------------------------------------------------------------
        class(DimensionsWrapper_t),       intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = -, '//&
                            ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper_Print

end module DimensionsWrapper
