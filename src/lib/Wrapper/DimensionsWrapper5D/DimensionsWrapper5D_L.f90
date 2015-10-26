module DimensionsWrapper5D_L

USE DimensionsWrapper5D
USE IR_Precision, only: I4P, str

implicit none
private

    type, extends(DimensionsWrapper5D_t) :: DimensionsWrapper5D_L_t
        logical, allocatable :: Value(:,:,:,:,:)
    contains
    private
        procedure, public :: Set            => DimensionsWrapper5D_L_Set
        procedure, public :: Get            => DimensionsWrapper5D_L_Get
        procedure, public :: GetShape       => DimensionsWrapper5D_L_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper5D_L_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper5D_L_GetPolymorphic
        procedure, public :: isOfDataType   => DimensionsWrapper5D_L_isOfDataType
        procedure, public :: Print          => DimensionsWrapper5D_L_Print
        procedure, public :: Free           => DimensionsWrapper5D_L_Free
        final             ::                   DimensionsWrapper5D_L_Final
    end type           

public :: DimensionsWrapper5D_L_t

contains


    subroutine DimensionsWrapper5D_L_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper5D
    !-----------------------------------------------------------------
        type(DimensionsWrapper5D_L_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper5D_L_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set logical Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_L_t), intent(INOUT) :: this
        class(*),                       intent(IN)    :: Value(:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (logical)
                allocate(this%Value(size(Value,dim=1),  &
                                    size(Value,dim=2),  &
                                    size(Value,dim=3),  &
                                    size(Value,dim=4),  &
                                    size(Value,dim=5)), &
                                    source=Value)
        end select
    end subroutine


    subroutine DimensionsWrapper5D_L_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get logical Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_L_t), intent(IN)  :: this
        class(*),                       intent(OUT) :: Value(:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (logical)
                Value = this%Value
        end select
    end subroutine


    function DimensionsWrapper5D_L_GetShape(this) result(ValueShape) 
    !-----------------------------------------------------------------
    !< Get Wrapper Value Shape
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_L_t), intent(IN)  :: this
        integer(I4P), allocatable                   :: ValueShape(:)
    !-----------------------------------------------------------------
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper5D_L_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic pointer to Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_L_t), target, intent(IN) :: this
        class(*), pointer                                  :: Value(:,:,:,:,:)
    !-----------------------------------------------------------------
        Value => this%Value
    end function


    subroutine DimensionsWrapper5D_L_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_L_t), intent(IN)  :: this
        class(*), allocatable,          intent(OUT) :: Value(:,:,:,:,:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1),  &
                       size(this%Value,dim=2),  &
                       size(this%Value,dim=3),  &
                       size(this%Value,dim=4),  &
                       size(this%Value,dim=5)), &
                       source=this%Value)
    end subroutine


    subroutine DimensionsWrapper5D_L_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper5D
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_L_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        if(allocated(this%Value)) deallocate(this%Value)
    end subroutine


    function DimensionsWrapper5D_L_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_L_t), intent(IN) :: this            !< Dimensions wrapper 5D
        class(*),                       intent(IN) :: Mold            !< Mold for data type comparison
        logical                                    :: isOfDataType    !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (logical)
                isOfDataType = .true.
        end select
    end function DimensionsWrapper5D_L_isOfDataType


    subroutine DimensionsWrapper5D_L_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_L_t),   intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A,$)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = L'//&
                        ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                        ', Value = '
        write(unit=unit,fmt=*,iostat=iostatd,iomsg=iomsgd) str(n=this%Value)
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper5D_L_Print

end module DimensionsWrapper5D_L
