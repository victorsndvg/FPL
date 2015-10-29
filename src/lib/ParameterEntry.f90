
module ParameterEntry

USE IR_Precision 
USE DimensionsWrapper

implicit none
private

    type :: ParameterEntry_t
    private
        character(len=:), allocatable         :: Key
        class(*),                    pointer  :: Value
        class(ParameterEntry_t),     pointer  :: Next   => null()
    contains
    private
        procedure, public :: Free             => ParameterEntry_Free
        procedure, public :: Print            => ParameterEntry_Print
        procedure, public :: HasNext          => ParameterEntry_HasNext
        procedure, public :: SetNext          => ParameterEntry_SetNext
        procedure, public :: GetNext          => ParameterEntry_GetNext
        procedure, public :: NullifyNext      => ParameterEntry_NullifyNext
        procedure, public :: HasKey           => ParameterEntry_HasKey
        procedure, public :: SetKey           => ParameterEntry_SetKey
        procedure, public :: GetKey           => ParameterEntry_GetKey
        procedure, public :: DeallocateKey    => ParameterEntry_DeallocateKey
        procedure, public :: HasValue         => ParameterEntry_HasValue
        procedure, public :: SetValue         => ParameterEntry_SetValue
        procedure, public :: GetValue         => ParameterEntry_GetValue
        procedure, public :: DeallocateValue  => ParameterEntry_DeallocateValue
        procedure, public :: PointToValue     => ParameterEntry_PointToValue
        final             ::                     ParameterEntry_Finalize 
    end type ParameterEntry_t

public :: ParameterEntry_t

contains


    function ParameterEntry_HasNext(this) result(hasNext)
    !-----------------------------------------------------------------
    !< Check if Next is associated for the current Node
    !-----------------------------------------------------------------
        class(ParameterEntry_t), intent(IN) :: this               !< Parameter List 
        logical                             :: hasNext            !< Check if Next is associated
    !-----------------------------------------------------------------
        hasNext = associated(this%Next)
    end function ParameterEntry_HasNext


    subroutine ParameterEntry_SetNext(this, Next)
    !-----------------------------------------------------------------
    !< Set the pointer to the Next node
    !-----------------------------------------------------------------
        class(ParameterEntry_t),         intent(INOUT) :: this        !< Parameter List 
        class(ParameterEntry_t), target, intent(IN)    :: Next        !< Pointer to Next 
    !-----------------------------------------------------------------
        this%Next => Next
    end subroutine ParameterEntry_SetNext


    function ParameterEntry_GetNext(this) result(Next)
    !-----------------------------------------------------------------
    !< Return a pointer to the Next node
    !-----------------------------------------------------------------
        class(ParameterEntry_t), intent(IN) :: this                   !< Parameter List 
        class(ParameterEntry_t), pointer    :: Next                   !< Pointer to Next
    !-----------------------------------------------------------------
        nullify(Next)
        if(this%HasNext()) Next => this%Next
    end function ParameterEntry_GetNext


    subroutine ParameterEntry_NullifyNext(this)
    !-----------------------------------------------------------------
    !< Nullify Next
    !-----------------------------------------------------------------
        class(ParameterEntry_t), intent(INOUT) :: this                !< Parameter List 
    !-----------------------------------------------------------------
        nullify(this%Next)
    end subroutine ParameterEntry_NullifyNext


    function ParameterEntry_HasKey(this) result(hasKey)
    !-----------------------------------------------------------------
    !< Check if Key is allocated for the current Node
    !-----------------------------------------------------------------
        class(ParameterEntry_t),     intent(IN) :: this               !< Parameter List 
        logical                                 :: hasKey             !< Check if Key is associated
    !-----------------------------------------------------------------
        hasKey = allocated(this%Key)
    end function ParameterEntry_HasKey


    subroutine ParameterEntry_SetKey(this, Key) 
    !-----------------------------------------------------------------
    !< Check if Next is associated for the current Node
    !-----------------------------------------------------------------
        class(ParameterEntry_t),               intent(INOUT) :: this  !< Parameter List 
        character(len=*),                      intent(IN)    :: Key   !< Key
    !-----------------------------------------------------------------
        this%Key = Key
    end subroutine ParameterEntry_SetKey


    function ParameterEntry_GetKey(this) result(Key)
    !-----------------------------------------------------------------
    !< Check if Next is associated for the current Node
    !-----------------------------------------------------------------
        class(ParameterEntry_t),     intent(IN) :: this               !< Parameter List 
        character(len=:), allocatable           :: Key                !< Key
    !-----------------------------------------------------------------
        Key = this%Key
    end function ParameterEntry_GetKey


    subroutine ParameterEntry_DeallocateKey(this)
    !-----------------------------------------------------------------
    !< Deallocate Key if allocated
    !-----------------------------------------------------------------
        class(ParameterEntry_t), intent(INOUT) :: this                !< Parameter List 
    !-----------------------------------------------------------------
        if(this%HasKey()) deallocate(this%Key)
    end subroutine ParameterEntry_DeallocateKey


    recursive subroutine ParameterEntry_Free(this)
    !-----------------------------------------------------------------
    !< Free the list
    !-----------------------------------------------------------------
        class(ParameterEntry_t), intent(INOUT) :: this                !< Parameter List 
        class(ParameterEntry_t),  pointer      :: Next                !< Parameter List Node
    !-----------------------------------------------------------------
        if (this%HasNext()) then
            Next => this%GetNext()
            call Next%Free()
            deallocate(Next)
            nullify(Next)
        endif
        call this%DeallocateKey()
        call this%DeallocateValue()
        call this%NullifyNext()
    end subroutine ParameterEntry_Free


    function ParameterEntry_HasValue(this) result(hasValue)
    !-----------------------------------------------------------------
    !< Check if Value is allocated for the current Node
    !-----------------------------------------------------------------
        class(ParameterEntry_t), intent(IN) :: this                   !< Parameter List 
        logical                             :: hasValue               !< Check if Value is allocated
    !-----------------------------------------------------------------
        hasValue = associated(this%Value)
    end function ParameterEntry_HasValue


    subroutine ParameterEntry_SetValue(this, Value)
    !-----------------------------------------------------------------
    !< Set a concrete Wrapper
    !-----------------------------------------------------------------
        class(ParameterEntry_t), intent(INOUT)  :: this               !< Parameter List
        class(*), pointer,       intent(IN)     :: Value              !< Concrete Wrapper
    !-----------------------------------------------------------------
        this%Value => Value
    end subroutine ParameterEntry_SetValue


    subroutine ParameterEntry_GetValue(this, Value)
    !-----------------------------------------------------------------
    !< Return a concrete WrapperFactory
    !-----------------------------------------------------------------
        class(ParameterEntry_t),             intent(IN)  :: this      !< Parameter List
        class(*), allocatable,               intent(OUT) :: Value     !< Concrete Wrapper
    !-----------------------------------------------------------------
        if(this%HasValue()) allocate(Value, source=this%Value)
    end subroutine ParameterEntry_GetValue


    function ParameterEntry_PointToValue(this) result(Value)
    !-----------------------------------------------------------------
    !< Return a pointer to a concrete WrapperFactory
    !-----------------------------------------------------------------
        class(ParameterEntry_t),         intent(IN)  :: this          !< Parameter List
        class(*), pointer                            :: Value         !< Concrete Wrapper
    !-----------------------------------------------------------------
        Value => this%Value
    end function ParameterEntry_PointToValue


    subroutine ParameterEntry_DeallocateValue(this)
    !-----------------------------------------------------------------
    !< Deallocate Key if allocated
    !-----------------------------------------------------------------
        class(ParameterEntry_t), intent(INOUT) :: this                !< Parameter List 
    !-----------------------------------------------------------------
        if(this%HasValue()) deallocate(this%Value)
    end subroutine ParameterEntry_DeallocateValue


    recursive subroutine ParameterEntry_Finalize(this)
    !-----------------------------------------------------------------
    !< Finalize procedure
    !-----------------------------------------------------------------
        type(ParameterEntry_t), intent(INOUT):: this                  !< Parameter List 
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine ParameterEntry_Finalize


    recursive subroutine ParameterEntry_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print the keys/value pair contained in the parameter list
    !-----------------------------------------------------------------
        class(ParameterEntry_t), target,  intent(IN)  :: this         !< Parameter list
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:),       allocatable           :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        iostatd = 0 ; iomsgd = ''; prefd = '';if (present(prefix)) prefd = prefix
        if(this%HasKey()) then
            write(unit=unit,fmt='(A,$)',iostat=iostatd,iomsg=iomsgd)prefd//' Key = "'//this%GetKey()//'", '
            select type (Wrapper =>this%Value)
                class is (DimensionsWrapper_t)
                    call Wrapper%Print(unit=unit)
                class Default
                    write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd) ' is not a Wrapper'
            end select
        endif
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine ParameterEntry_Print


end module ParameterEntry
