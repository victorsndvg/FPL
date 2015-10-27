
module ParameterEntry

USE IR_Precision 
USE DimensionsWrapper

implicit none
private

    type :: ParameterEntry_t
    private
        character(len=:), allocatable                 :: Key
        class(*),         allocatable                 :: Value
        class(ParameterEntry_t), public, pointer  :: Next   => null()
    contains
    private
        procedure         ::                     ParameterEntry_AddNode
        procedure, public :: GetEntry         => ParameterEntry_GetEntry
        procedure, public :: GetPreviousEntry => ParameterEntry_GetPreviousEntry
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
        procedure, public :: isPresent        => ParameterEntry_isPresent
        procedure, public :: GetLength        => ParameterEntry_GetLength
        procedure, public :: RemoveEntry      => ParameterEntry_RemoveEntry
        generic,   public :: AddNode          => ParameterEntry_AddNode     
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
        if(this%HasKey()) deallocate(this%Key)
        allocate(this%Key, source=Key)
    end subroutine ParameterEntry_SetKey


    function ParameterEntry_GetKey(this) result(Key)
    !-----------------------------------------------------------------
    !< Check if Next is associated for the current Node
    !-----------------------------------------------------------------
        class(ParameterEntry_t),     intent(IN) :: this               !< Parameter List 
        character(len=:), allocatable           :: Key                !< Key
    !-----------------------------------------------------------------
        if(this%HasKey()) allocate(Key, source=this%Key)
    end function ParameterEntry_GetKey


    subroutine ParameterEntry_DeallocateKey(this)
    !-----------------------------------------------------------------
    !< Deallocate Key if allocated
    !-----------------------------------------------------------------
        class(ParameterEntry_t), intent(INOUT) :: this                !< Parameter List 
    !-----------------------------------------------------------------
        if(this%HasKey()) deallocate(this%Key)
    end subroutine ParameterEntry_DeallocateKey


    function ParameterEntry_IsPresent(this, Key) result(isPresent)
    !-----------------------------------------------------------------
    !< Check if a Key is present in the DataBase
    !-----------------------------------------------------------------
        class(ParameterEntry_t),       intent(IN)  :: this            !< Parameter List
        character(len=*),              intent(IN)  :: Key             !< String Key
        logical                                    :: isPresent       !< Boolean flag to check if a Key is present
    !-----------------------------------------------------------------
        isPresent = associated(this%GetEntry(Key))
    end function ParameterEntry_IsPresent


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



    function ParameterEntry_GetEntry(this,Key) result(Entry)
    !-----------------------------------------------------------------
    !< Return a pointer to a ParameterList given a Key
    !-----------------------------------------------------------------
        class(ParameterEntry_t), target,     intent(IN) :: this       !< Parameter List
        character(len=*),                    intent(IN) :: Key        !< String Key
        class(ParameterEntry_t), pointer                :: Entry      !< Parameter List Node
    !-----------------------------------------------------------------
        Entry => this
        do while(associated(Entry))
            if (Entry%HasKey()) then
                if (Entry%GetKey()==Key) exit
                Entry => Entry%GetNext()
            elseif (Entry%HasNext()) then
                Entry => Entry%GetNext()
            else
                nullify(Entry)
                exit
            endif
        enddo
    end function ParameterEntry_GetEntry


    function ParameterEntry_GetPreviousEntry(this,Key) result(PreviousEntry)
    !-----------------------------------------------------------------
    !< Return a pointer to the provious node of a Parameter List given a Key
    !-----------------------------------------------------------------
        class(ParameterEntry_t), target, intent(IN) :: this          !< Parameter List
        character(len=*),                intent(IN) :: Key           !< String Key
        class(ParameterEntry_t), pointer            :: PreviousEntry !< Parameter List Node
        class(ParameterEntry_t), pointer            :: Next          !< Parameter List Next Node
    !-----------------------------------------------------------------
        PreviousEntry => this
        do while(associated(PreviousEntry))
            if (PreviousEntry%HasNext()) then
                Next => PreviousEntry%GetNext()
                if(Next%HasKey()) then
                    if (Next%GetKey()==Key) then
                        exit
                    else
                        PreviousEntry => Next
                    endif
                endif
            else
                nullify(PreviousEntry)
                exit
            endif
        enddo    
    end function ParameterEntry_GetPreviousEntry


    function ParameterEntry_HasValue(this) result(hasValue)
    !-----------------------------------------------------------------
    !< Check if Value is allocated for the current Node
    !-----------------------------------------------------------------
        class(ParameterEntry_t), intent(IN) :: this                   !< Parameter List 
        logical                             :: hasValue               !< Check if Value is allocated
    !-----------------------------------------------------------------
        hasValue = allocated(this%Value)
    end function ParameterEntry_HasValue


    subroutine ParameterEntry_SetValue(this, Value)
    !-----------------------------------------------------------------
    !< Set a concrete Wrapper
    !-----------------------------------------------------------------
        class(ParameterEntry_t), intent(INOUT)  :: this               !< Parameter List
        class(*),                intent(IN)     :: Value              !< Concrete Wrapper
    !-----------------------------------------------------------------
        if(this%HasValue()) deallocate(this%Value)
        allocate(this%Value, source=Value)
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
        class(ParameterEntry_t), target, intent(IN)  :: this          !< Parameter List
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


    recursive subroutine ParameterEntry_AddNode(this,Key, Value)
    !-----------------------------------------------------------------
    !< Add a new Node if key does not Exist
    !-----------------------------------------------------------------
        class(ParameterEntry_t),  intent(INOUT) :: this               !< Linked List
        character(len=*),         intent(IN)    :: Key                !< Key (unique) of the current node.
        class(*),                 intent(IN)    :: Value              !< Wrapper Factory
    !-----------------------------------------------------------------
        if (this%HasKey()) then
            if (this%GetKey()/=Key) then
                if (.not. this%hasNext()) then 
                    ! I reached the end of the list
                    allocate(ParameterEntry_t::this%Next)
                    select type (Next => this%Next)
                    type is (ParameterEntry_t)
                        call Next%AddNode(Key=Key, Value=Value)
                    end select
                else
                    select type (Next => this%Next)
                    type is (ParameterEntry_t)
                        call Next%AddNode(Key=Key, Value=Value)
                    end select
                endif
            else
                call this%SetValue(Value=Value)
            endif
        else
            call this%SetKey(Key=Key)
            call this%SetValue(Value=Value)
        endif
    end subroutine ParameterEntry_AddNode


    subroutine ParameterEntry_RemoveEntry(this, Key, Root)
    !-----------------------------------------------------------------
    !< Remove an Entry given a Key
    !-----------------------------------------------------------------
        class(ParameterEntry_t), target,  intent(INOUT) :: this          !< Parameter List Entry type
        character(len=*),                 intent(IN)    :: Key           !< String Key
        class(ParameterEntry_t), pointer, intent(INOUT) :: Root          !< The Previous Entry of a given key
        class(ParameterEntry_t), pointer                :: PreviousEntry !< The Previous Entry of a given key
        class(ParameterEntry_t), pointer                :: CurrentEntry  !< Entry of a given key
        class(ParameterEntry_t), pointer                :: NextEntry     !< The Next Node of a given key
    !-----------------------------------------------------------------
        if(associated(Root,this)) then
            if(Root%HasKey()) then
                if(Root%GetKey() == Key) then
                    CurrentEntry => Root
                    NextEntry    => CurrentEntry%GetNext()
                    call CurrentEntry%DeallocateKey()    
                    call CurrentEntry%DeallocateValue()
                    if(CurrentEntry%HasNext()) then
                        if(NextEntry%HasKey()) then
                            call CurrentEntry%NullifyNext()
                            Root => NextEntry
                            deallocate(CurrentEntry)
                        endif
                    endif
                else
                    PreviousEntry => Root%GetPreviousEntry(Key=Key)
                    if(associated(PreviousEntry)) then
                        CurrentEntry  => PreviousEntry%GetNext()
                        NextEntry     => CurrentEntry%GetNext()
                        call CurrentEntry%DeallocateKey()    
                        call CurrentEntry%DeallocateValue()
                        if(CurrentEntry%HasNext()) then
                            if(NextEntry%HasKey()) then
                                call CurrentEntry%NullifyNext()
                                call PreviousEntry%SetNext(Next=NextEntry)
                                deallocate(CurrentEntry)
                            endif
                        endif
                    endif   
                endif
            endif
        endif
    end subroutine ParameterEntry_RemoveEntry


    function ParameterEntry_GetLength(this) result(Length)
    !-----------------------------------------------------------------
    !< Return the length of the list
    !-----------------------------------------------------------------
        class(ParameterEntry_t), intent(IN) :: this                   !< Parameter List
        integer(I4P)                        :: Length                 !< Length of the list
        type(ParameterEntry_t),  pointer    :: NextEntry               !< Next Parameter List Entry
    !-----------------------------------------------------------------
        Length = 0 ; if (this%HasKey()) Length = 1
        NextEntry => this%GetNext()
        do while (associated(NextEntry))
            if (NextEntry%HasKey()) then
                Length = Length + 1
            endif
            NextEntry => NextEntry%GetNext()
        enddo
        nullify(NextEntry)
    end function ParameterEntry_GetLength


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
        class(ParameterEntry_t), pointer              :: Node         !< Pointer for scanning the list.
        class(*), pointer                             :: Next         !< Pointer for scanning the list.
    !-----------------------------------------------------------------
        iostatd = 0 ; iomsgd = ''; prefd = '';if (present(prefix)) prefd = prefix
        Node => this
        if(Node%HasKey()) then
            write(unit=unit,fmt='(A,$)',iostat=iostatd,iomsg=iomsgd)prefd//' Key = "'//Node%GetKey()//'", '
            select type (Wrapper =>Node%Value)
                class is (DimensionsWrapper_t)
                    call Wrapper%Print(unit=unit)
                class Default
                    write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd) ' is a SubList'
            end select
            if (Node%HasNext()) then
                Next => Node%GetNext()
                select type (Next)
                    class is (ParameterEntry_t)
                        call Next%Print(unit=unit,prefix=prefd,iostat=iostatd,iomsg=iomsgd)
                end select
            endif
        endif
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine ParameterEntry_Print


end module ParameterEntry
