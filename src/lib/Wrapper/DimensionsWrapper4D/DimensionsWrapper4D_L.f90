module DimensionsWrapper4D_L

USE DimensionsWrapper4D
USE IR_Precision, only: I4P, str
USE ErrorMessages

implicit none
private

    type, extends(DimensionsWrapper4D_t) :: DimensionsWrapper4D_L_t
        logical, allocatable :: Value(:,:,:,:)
    contains
    private
        procedure, public :: Set            => DimensionsWrapper4D_L_Set
        procedure, public :: Get            => DimensionsWrapper4D_L_Get
        procedure, public :: GetShape       => DimensionsWrapper4D_L_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper4D_L_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper4D_L_GetPolymorphic
        procedure, public :: isOfDataType   => DimensionsWrapper4D_L_isOfDataType
        procedure, public :: Print          => DimensionsWrapper4D_L_Print
        procedure, public :: Free           => DimensionsWrapper4D_L_Free
        final             ::                   DimensionsWrapper4D_L_Final
    end type           

public :: DimensionsWrapper4D_L_t

contains


    subroutine DimensionsWrapper4D_L_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper4D
    !-----------------------------------------------------------------
        type(DimensionsWrapper4D_L_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper4D_L_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set logical Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_L_t), intent(INOUT) :: this
        class(*),                       intent(IN)    :: Value(:,:,:,:)
        integer                                       :: err
    !-----------------------------------------------------------------
        select type (Value)
            type is (logical)
                allocate(this%Value(size(Value,dim=1),  &
                                    size(Value,dim=2),  &
                                    size(Value,dim=3),  &
                                    size(Value,dim=4)), &
                                    source=Value, stat=err)
                if(err/=0) &
                    call msg%Error( txt='Setting Value: Allocation error ('//&
                                    str(no_sign=.true.,n=err)//')', &
                                    file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn( txt='Setting value: Expected data type (logical)', &
                               file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    subroutine DimensionsWrapper4D_L_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get logical Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_L_t), intent(IN)   :: this
        class(*),                       intent(OUT) :: Value(:,:,:,:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (logical)
                if(all(this%GetShape() == shape(Value))) then
                    Value = this%Value
                else
                    call msg%Warn(txt='Getting value: Expected shape ('//    &
                                  str(no_sign=.true.,n=this%GetShape())//')',&
                                  file=__FILE__, line=__LINE__ )
                endif
            class Default
                call msg%Warn(txt='Getting value: Expected data type (logical)',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    function DimensionsWrapper4D_L_GetShape(this) result(ValueShape) 
    !-----------------------------------------------------------------
    !< Get Wrapper Value Shape
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_L_t), intent(IN)  :: this
        integer(I4P), allocatable                   :: ValueShape(:)
    !-----------------------------------------------------------------
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper4D_L_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic pointer to Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_L_t), target, intent(IN)  :: this
        class(*), pointer                                   :: Value(:,:,:,:)
    !-----------------------------------------------------------------
        Value => this%Value
    end function


    subroutine DimensionsWrapper4D_L_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_L_t), intent(IN)  :: this
        class(*), allocatable,          intent(OUT) :: Value(:,:,:,:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1),  &
                       size(this%Value,dim=2),  &
                       size(this%Value,dim=3),  &
                       size(this%Value,dim=4)), &
                       source=this%Value)
    end subroutine


    subroutine DimensionsWrapper4D_L_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper4D
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_L_t), intent(INOUT) :: this
        integer                                       :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) deallocate(this%Value, stat=err)
        if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                  str(no_sign=.true.,n=err)//')',             &
                                  file=__FILE__, line=__LINE__ )
    end subroutine


    function DimensionsWrapper4D_L_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_L_t), intent(IN) :: this            !< Dimensions wrapper 4D
        class(*),                       intent(IN) :: Mold            !< Mold for data type comparison
        logical                                    :: isOfDataType    !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (logical)
                isOfDataType = .true.
        end select
    end function DimensionsWrapper4D_L_isOfDataType


    subroutine DimensionsWrapper4D_L_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_L_t),   intent(IN)  :: this         !< DimensionsWrapper
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
    end subroutine DimensionsWrapper4D_L_Print

end module DimensionsWrapper4D_L
