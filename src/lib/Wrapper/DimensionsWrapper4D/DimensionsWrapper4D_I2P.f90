module DimensionsWrapper4D_I2P

USE DimensionsWrapper4D
USE IR_Precision, only: I2P, I4P, str

implicit none
private

    type, extends(DimensionsWrapper4D_t) :: DimensionsWrapper4D_I2P_t
        integer(I2P), allocatable :: Value(:,:,:,:)
    contains
    private
        procedure         ::                 DimensionsWrapper4D_I2P_Set
        procedure         ::                 DimensionsWrapper4D_I2P_Get
        generic,   public :: Set          => DimensionsWrapper4D_I2P_Set
        generic,   public :: Get          => DimensionsWrapper4D_I2P_Get
        procedure, public :: isOfDataType => DimensionsWrapper4D_I2P_isOfDataType
        procedure, public :: Print        => DimensionsWrapper4D_I2P_Print
        procedure, public :: Free         => DimensionsWrapper4D_I2P_Free
        final             ::                 DimensionsWrapper4D_I2P_Final
    end type           

public :: DimensionsWrapper4D_I2P_t

contains


    subroutine DimensionsWrapper4D_I2P_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper4D
    !-----------------------------------------------------------------
        type(DimensionsWrapper4D_I2P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper4D_I2P_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set I2P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_I2P_t), intent(INOUT) :: this
        class(*),                         intent(IN)    :: Value(:,:,:,:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I2P))
                allocate(this%Value(size(Value,dim=1),  &
                                    size(Value,dim=2),  &
                                    size(Value,dim=3),  &
                                    size(Value,dim=4)), &
                                    source=Value)
        end select
    end subroutine


    subroutine DimensionsWrapper4D_I2P_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get I2P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_I2P_t), intent(IN)  :: this
        integer(I2P), allocatable,        intent(OUT) :: Value(:,:,:,:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1),  &
                       size(this%Value,dim=2),  &
                       size(this%Value,dim=3),  &
                       size(this%Value,dim=4)), &
                       source=this%Value)
    end subroutine


    subroutine DimensionsWrapper4D_I2P_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper4D
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_I2P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        if(allocated(this%Value)) deallocate(this%Value)
    end subroutine


    function DimensionsWrapper4D_I2P_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_I2P_t), intent(IN) :: this          !< Dimensions wrapper 4D
        class(*),                         intent(IN) :: Mold          !< Mold for data type comparison
        logical                                      :: isOfDataType  !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (integer(I2P))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper4D_I2P_isOfDataType


    subroutine DimensionsWrapper4D_I2P_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_I2P_t), intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A,$)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = I2P'//&
                        ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                        ', Value = '
        write(unit=unit,fmt=*,iostat=iostatd,iomsg=iomsgd) str(no_sign=.true., n=this%Value)

        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper4D_I2P_Print

end module DimensionsWrapper4D_I2P
