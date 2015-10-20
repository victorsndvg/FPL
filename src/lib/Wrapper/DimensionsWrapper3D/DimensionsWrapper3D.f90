module DimensionsWrapper3D

USE DimensionsWrapper
USE IR_Precision, only: I4P, str

implicit none
private

    type, extends(DimensionsWrapper_t), abstract :: DimensionsWrapper3D_t
    private
    contains
        procedure :: Print => DimensionsWrapper3D_Print
        procedure(DimensionsWrapper3D_isOfDataType), deferred :: isOfDataType
        procedure(DimensionsWrapper3D_Free),         deferred :: Free
    end type

    abstract interface
        subroutine DimensionsWrapper3D_Free(this)
            import DimensionsWrapper3D_t
            class(DimensionsWrapper3D_t), intent(INOUT) :: this
        end subroutine
        function DimensionsWrapper3D_isOfDataType(this, Mold) result(isOfDataType)
            import DimensionsWrapper3D_t
            class(DimensionsWrapper3D_t), intent(IN) :: this
            class(*),                   intent(IN) :: Mold
            logical                                :: isOfDataType
        end function
    end interface

public :: DimensionsWrapper3D_t

contains

    subroutine DimensionsWrapper3D_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Generic Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_t),     intent(IN)  :: this         !< DimensionsWrapper
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
    end subroutine DimensionsWrapper3D_Print

end module DimensionsWrapper3D
