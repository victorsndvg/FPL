module CircleWrapper

USE Circle                          !< USE the data type to store
USE DimensionsWrapper0D             !< USE the DimensionsWrapper0D abstract class
USE ErrorMessages                   !< USE the ErrorMessages for printing error messages
USE PENF, only: I4P, str            !< USE I4P data type and str for string conversion

implicit none
private

    type, extends(DimensionsWrapper0D_t) :: CircleWrapper_t                 !< Extends from DimensionsWrapper0D_t (scalar value)
        type(Circle_T), allocatable :: Value                                !< Value stores a copy of the input data by assignment
    contains
    private
        procedure, public :: Set            => CircleWrapper_Set            !< Sets the Value into the Wrapper
        procedure, public :: Get            => CircleWrapper_Get            !< Gets the Value from the Wrapper
        procedure, public :: GetShape       => CircleWrapper_GetShape       !< Return the shape of the stored Value (0, scalar value)
        procedure, public :: GetPointer     => CircleWrapper_GetPointer     !< Return an unlimited polymorphic pointer to the Value
        procedure, public :: DataSizeInBytes=> CircleWrapper_DataSizeInBytes!< Return the size of the stored data in bytes
        procedure, public :: isOfDataType   => CircleWrapper_isOfDataType   !< Check if the data type of a input Mold is Circle_t
        procedure, public :: toString       => CircleWrapper_toString       !< Return the value as a string
        procedure, public :: Free           => CircleWrapper_Free           !< Free the Wrapper
        procedure, public :: Print          => CircleWrapper_Print          !< Print the Wrapper content
    end type

public :: CircleWrapper_t

contains

    subroutine CircleWrapper_Set(this, Value)
    !-----------------------------------------------------------------
    !< Set Circle Wrapper Value
    !-----------------------------------------------------------------
        class(CircleWrapper_t), intent(INOUT) :: this
        class(*),               intent(IN)    :: Value
        integer                               :: err
    !-----------------------------------------------------------------
        select type (Value)
            type is (Circle_t)
                allocate(this%Value, stat=err)
                this%Value = Value
                if(err/=0) &
                    call msg%Error(txt='Setting Value: Allocation error ('//&
                                   str(no_sign=.true.,n=err)//')', &
                                   file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn(txt='Setting value: Expected data type (Circle)',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    subroutine CircleWrapper_Get(this, Value)
    !-----------------------------------------------------------------
    !< Get Circle Wrapper Value
    !-----------------------------------------------------------------
        class(CircleWrapper_t), intent(IN)  :: this
        class(*),               intent(OUT) :: Value
    !-----------------------------------------------------------------
        select type (Value)
            type is (Circle_t)
                Value = this%Value
            class Default
                call msg%Warn(txt='Getting value: Expected data type (Circle)',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine

    subroutine CircleWrapper_GetShape(this, ValueShape)
    !-----------------------------------------------------------------
    !< Return the shape of the Wrapper Value
    !-----------------------------------------------------------------
        class(CircleWrapper_t),    intent(IN)    :: this
        integer(I4P), allocatable, intent(INOUT) :: ValueShape(:)
    !-----------------------------------------------------------------
        if(allocated(ValueShape)) deallocate(ValueShape)
        allocate(ValueShape(1))
        ValueShape = 0
    end subroutine


    function CircleWrapper_GetPointer(this) result(Value)
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic pointer to Wrapper Value
    !-----------------------------------------------------------------
        class(CircleWrapper_t), target, intent(IN)  :: this
        class(*), pointer                           :: Value
    !-----------------------------------------------------------------
        Value => this%Value
    end function


    subroutine CircleWrapper_Free(this)
    !-----------------------------------------------------------------
    !< Free a CircleWrapper0D
    !-----------------------------------------------------------------
        class(CircleWrapper_t), intent(INOUT) :: this
        integer                               :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    function CircleWrapper_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype
    !-----------------------------------------------------------------
        class(CircleWrapper_t),           intent(IN) :: this          !< Circle wrapper 0D
        integer(I4P)                                 :: DataSizeInBytes  !< Data size of the stored data in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = this%value%DataSizeInBytes()
    end function CircleWrapper_DataSizeInBytes


    function CircleWrapper_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype
    !-----------------------------------------------------------------
        class(CircleWrapper_t),           intent(IN) :: this          !< Circle wrapper 0D
        class(*),                         intent(IN) :: Mold          !< Mold for data type comparison
        logical                                      :: isOfDataType  !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (Circle_t)
                isOfDataType = .true.
        end select
    end function CircleWrapper_isOfDataType


    subroutine CircleWrapper_toString(this, String, Separator)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype
    !-----------------------------------------------------------------
        class(CircleWrapper_t),           intent(IN)    :: this          !< Circle wrapper 0D
        character(len=:), allocatable,    intent(INOUT) :: String        !< Return the Value as a string
        character(len=1), optional,       intent(IN)    :: Separator     !< Value separator for multidimensional variables
        real                                            :: Radius        !< Circle radius
    !-----------------------------------------------------------------
        String = ''
        if(allocated(this%Value)) then
            call this%Value%GetRadius(Radius=Radius)
            String = 'Radius = '//str(no_sign=.true., n=Radius)
        endif
    end subroutine CircleWrapper_toString


    subroutine CircleWrapper_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(CircleWrapper_t),           intent(IN)  :: this         !< CircleWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        character(len=:), allocatable                 :: strvalue     !< String value
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
        real                                          :: Radius       !< Circle radius
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        call this%toString(strvalue)
        write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = Circle'//&
                            ', '//strvalue
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine CircleWrapper_Print


end module CircleWrapper
