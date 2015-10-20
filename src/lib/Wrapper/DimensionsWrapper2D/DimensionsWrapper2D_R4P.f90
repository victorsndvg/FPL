module DimensionsWrapper2D_R4P

USE DimensionsWrapper2D
USE IR_Precision, only: I4P, R4P, str

implicit none
private

    type, extends(DimensionsWrapper2D_t) :: DimensionsWrapper2D_R4P_t
        real(R4P), allocatable :: Value(:,:)
    contains
    private
        procedure         ::                 DimensionsWrapper2D_R4P_Set
        procedure         ::                 DimensionsWrapper2D_R4P_Get
        generic,   public :: Set          => DimensionsWrapper2D_R4P_Set
        generic,   public :: Get          => DimensionsWrapper2D_R4P_Get
        procedure, public :: isOfDataType => DimensionsWrapper2D_R4P_isOfDataType
        procedure, public :: Free         => DimensionsWrapper2D_R4P_Free
        procedure, public :: Print        => DimensionsWrapper2D_R4P_Print
        final             ::                 DimensionsWrapper2D_R4P_Final
    end type           

public :: DimensionsWrapper2D_R4P_t

contains


    subroutine DimensionsWrapper2D_R4P_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper2D
    !-----------------------------------------------------------------
        type(DimensionsWrapper2D_R4P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper2D_r4P_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set R4P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_R4P_t), intent(INOUT) :: this
        class(*),                         intent(IN)    :: Value(:,:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (real(R4P))
                allocate(this%Value(size(Value,dim=1),  &
                                    size(Value,dim=2)), &
                                    source=Value)
        end select
    end subroutine


    subroutine DimensionsWrapper2D_R4P_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get R4P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_R4P_t), intent(IN)  :: this
        real(R4P), allocatable,           intent(OUT) :: Value(:,:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1),  &
                       size(this%Value,dim=2)), &
                       source=this%Value)
    end subroutine


    subroutine DimensionsWrapper2D_R4P_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper2D
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_R4P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        if(allocated(this%Value)) deallocate(this%Value)
    end subroutine


    function DimensionsWrapper2D_R4P_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_R4P_t), intent(IN) :: this          !< Dimensions wrapper 2D
        class(*),                         intent(IN) :: Mold          !< Mold for data type comparison
        logical                                      :: isOfDataType  !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (real(R4P))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper2D_R4P_isOfDataType


    subroutine DimensionsWrapper2D_R4P_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_R4P_t), intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A,$)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = R4P'//&
                        ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                        ', Value = '
        write(unit=unit,fmt=*,iostat=iostatd,iomsg=iomsgd) str(no_sign=.true., n=this%Value)

        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper2D_R4P_Print

end module DimensionsWrapper2D_R4P
