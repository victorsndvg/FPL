module DimensionsWrapper2D_R8P

USE DimensionsWrapper2D
USE IR_Precision, only: I4P, R8P, str
USE ErrorMessages

implicit none
private

    type, extends(DimensionsWrapper2D_t) :: DimensionsWrapper2D_R8P_t
        real(R8P), allocatable :: Value(:,:)
    contains
    private
        procedure, public :: Set            => DimensionsWrapper2D_R8P_Set
        procedure, public :: Get            => DimensionsWrapper2D_R8P_Get
        procedure, public :: GetShape       => DimensionsWrapper2D_R8P_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper2D_R8P_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper2D_R8P_GetPolymorphic
        procedure, public :: isOfDataType   => DimensionsWrapper2D_R8P_isOfDataType
        procedure, public :: Free           => DimensionsWrapper2D_R8P_Free
        procedure, public :: Print          => DimensionsWrapper2D_R8P_Print
        final             ::                   DimensionsWrapper2D_R8P_Final
    end type           

public :: DimensionsWrapper2D_R8P_t

contains


    subroutine DimensionsWrapper2D_R8P_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper2D
    !-----------------------------------------------------------------
        type(DimensionsWrapper2D_R8P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper2D_R8P_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set R8P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_R8P_t), intent(INOUT) :: this
        class(*),                         intent(IN)    :: Value(:,:)
        integer                                         :: err
    !-----------------------------------------------------------------
        select type (Value)
            type is (real(R8P))
                allocate(this%Value(size(Value,dim=1),  &
                                    size(Value,dim=2)), &
                                    source=Value, stat=err)
                if(err/=0) &
                    call msg%Error( txt='Setting Value: Allocation error ('//&
                                    str(no_sign=.true.,n=err)//')', &
                                    file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn( txt='Setting value: Expected data type (R8P)', &
                               file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    subroutine DimensionsWrapper2D_R8P_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get R8P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_R8P_t), intent(IN)  :: this
        class(*),                         intent(OUT) :: Value(:,:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (real(R8P))
                if(all(this%GetShape() == shape(Value))) then
                    Value = this%Value
                else
                    call msg%Warn(txt='Getting value: Expected shape ('//    &
                                  str(no_sign=.true.,n=this%GetShape())//')',&
                                  file=__FILE__, line=__LINE__ )
                endif
            class Default
                call msg%Warn(txt='Getting value: Expected data type (R8P)',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    function DimensionsWrapper2D_R8P_GetShape(this) result(ValueShape) 
    !-----------------------------------------------------------------
    !< Get Wrapper Value Shape
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_R8P_t), intent(IN)  :: this
        integer(I4P), allocatable                     :: ValueShape(:)
    !-----------------------------------------------------------------
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper2D_R8P_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic pointer to Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_R8P_t), target, intent(IN) :: this
        class(*), pointer                                    :: Value(:,:)
    !-----------------------------------------------------------------
        Value => this%value
    end function


    subroutine DimensionsWrapper2D_R8P_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_R8P_t), intent(IN)  :: this
        class(*), allocatable,            intent(OUT) :: Value(:,:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1),  &
                       size(this%Value,dim=2)), &
                       source=this%Value)
    end subroutine


    subroutine DimensionsWrapper2D_R8P_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper2D
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_R8P_t), intent(INOUT) :: this
        integer                                         :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) deallocate(this%Value, stat=err)
        if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                  str(no_sign=.true.,n=err)//')',             &
                                  file=__FILE__, line=__LINE__ )
    end subroutine


    function DimensionsWrapper2D_R8P_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_R8P_t), intent(IN) :: this          !< Dimensions wrapper 2D
        class(*),                         intent(IN) :: Mold          !< Mold for data type comparison
        logical                                      :: isOfDataType  !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (real(R8P))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper2D_R8P_isOfDataType


    subroutine DimensionsWrapper2D_R8P_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_R8P_t), intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A,$)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = R8P'//&
                        ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                        ', Value = '
        write(unit=unit,fmt=*,iostat=iostatd,iomsg=iomsgd) str(no_sign=.true., n=this%Value)

        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper2D_R8P_Print

end module DimensionsWrapper2D_R8P
