
module ParameterListEntry

USE IR_Precision 
USE DimensionsWrapper

implicit none
private

    type :: ParameterListEntry_t
    private
        character(len=:), allocatable                 :: Key
        class(*),         allocatable                 :: Value
        class(ParameterListEntry_t), public, pointer  :: Next   => null()
    contains
    private
        procedure         ::                     ParameterListEntry_AddNode
        procedure, public :: GetEntry         => ParameterListEntry_GetEntry
        procedure, public :: GetPreviousEntry => ParameterListEntry_GetPreviousEntry
        procedure, public :: Free             => ParameterListEntry_Free
        procedure, public :: Print            => ParameterListEntry_Print
        procedure, public :: HasNext          => ParameterListEntry_HasNext
        procedure, public :: SetNext          => ParameterListEntry_SetNext
        procedure, public :: GetNext          => ParameterListEntry_GetNext
        procedure, public :: NullifyNext      => ParameterListEntry_NullifyNext
        procedure, public :: HasKey           => ParameterListEntry_HasKey
        procedure, public :: SetKey           => ParameterListEntry_SetKey
        procedure, public :: GetKey           => ParameterListEntry_GetKey
        procedure, public :: DeallocateKey    => ParameterListEntry_DeallocateKey
        procedure, public :: HasValue         => ParameterListEntry_HasValue
        procedure, public :: SetValue         => ParameterListEntry_SetValue
        procedure, public :: GetValue         => ParameterListEntry_GetValue
        procedure, public :: DeallocateValue  => ParameterListEntry_DeallocateValue
        procedure, public :: PointToValue     => ParameterListEntry_PointToValue
        procedure, public :: isPresent        => ParameterListEntry_isPresent
        procedure, public :: GetLength        => ParameterListEntry_GetLength
        procedure, public :: RemoveEntry      => ParameterListEntry_RemoveEntry
        generic,   public :: AddNode          => ParameterListEntry_AddNode     
        final             ::                     ParameterListEntry_Finalize 
    end type ParameterListEntry_t

public :: ParameterListEntry_t

contains


    function ParameterListEntry_HasNext(this) result(hasNext)
    !-----------------------------------------------------------------
    !< Check if Next is associated for the current Node
    !-----------------------------------------------------------------
        class(ParameterListEntry_t), intent(IN) :: this               !< Parameter List 
        logical                                 :: hasNext            !< Check if Next is associated
    !-----------------------------------------------------------------
        hasNext = associated(this%Next)
    end function ParameterListEntry_HasNext


    subroutine ParameterListEntry_SetNext(this, Next)
    !-----------------------------------------------------------------
    !< Set the pointer to the Next node
    !-----------------------------------------------------------------
        class(ParameterListEntry_t),         intent(INOUT) :: this    !< Parameter List 
        class(ParameterListEntry_t), target, intent(IN)    :: Next    !< Pointer to Next 
    !-----------------------------------------------------------------
        this%Next => Next
    end subroutine ParameterListEntry_SetNext


    function ParameterListEntry_GetNext(this) result(Next)
    !-----------------------------------------------------------------
    !< Return a pointer to the Next node
    !-----------------------------------------------------------------
        class(ParameterListEntry_t), intent(IN) :: this               !< Parameter List 
        class(ParameterListEntry_t), pointer    :: Next               !< Pointer to Next
    !-----------------------------------------------------------------
        nullify(Next)
        if(this%HasNext()) Next => this%Next
    end function ParameterListEntry_GetNext


    subroutine ParameterListEntry_NullifyNext(this)
    !-----------------------------------------------------------------
    !< Nullify Next
    !-----------------------------------------------------------------
        class(ParameterListEntry_t), intent(INOUT) :: this            !< Parameter List 
    !-----------------------------------------------------------------
        nullify(this%Next)
    end subroutine ParameterListEntry_NullifyNext


    function ParameterListEntry_HasKey(this) result(hasKey)
    !-----------------------------------------------------------------
    !< Check if Key is allocated for the current Node
    !-----------------------------------------------------------------
        class(ParameterListEntry_t), intent(IN) :: this               !< Parameter List 
        logical                                 :: hasKey             !< Check if Key is associated
    !-----------------------------------------------------------------
        hasKey = allocated(this%Key)
    end function ParameterListEntry_HasKey


    subroutine ParameterListEntry_SetKey(this, Key) 
    !-----------------------------------------------------------------
    !< Check if Next is associated for the current Node
    !-----------------------------------------------------------------
        class(ParameterListEntry_t),           intent(INOUT) :: this  !< Parameter List 
        character(len=*),                      intent(IN)    :: Key   !< Key
    !-----------------------------------------------------------------
        if(this%HasKey()) deallocate(this%Key)
        allocate(this%Key, source=Key)
    end subroutine ParameterListEntry_SetKey


    function ParameterListEntry_GetKey(this) result(Key)
    !-----------------------------------------------------------------
    !< Check if Next is associated for the current Node
    !-----------------------------------------------------------------
        class(ParameterListEntry_t), intent(IN) :: this               !< Parameter List 
        character(len=:), allocatable           :: Key                !< Key
    !-----------------------------------------------------------------
        if(this%HasKey()) allocate(Key, source=this%Key)
    end function ParameterListEntry_GetKey


    subroutine ParameterListEntry_DeallocateKey(this)
    !-----------------------------------------------------------------
    !< Deallocate Key if allocated
    !-----------------------------------------------------------------
        class(ParameterListEntry_t), intent(INOUT) :: this            !< Parameter List 
    !-----------------------------------------------------------------
        if(this%HasKey()) deallocate(this%Key)
    end subroutine ParameterListEntry_DeallocateKey


    function ParameterListEntry_IsPresent(this, Key) result(isPresent)
    !-----------------------------------------------------------------
    !< Check if a Key is present in the DataBase
    !-----------------------------------------------------------------
        class(ParameterListEntry_t),   intent(IN)  :: this            !< Parameter List
        character(len=*),              intent(IN)  :: Key             !< String Key
        logical                                    :: isPresent       !< Boolean flag to check if a Key is present
    !-----------------------------------------------------------------
        isPresent = associated(this%GetEntry(Key))
    end function ParameterListEntry_IsPresent


    recursive subroutine ParameterListEntry_Free(this)
    !-----------------------------------------------------------------
    !< Free the list
    !-----------------------------------------------------------------
        class(ParameterListEntry_t), intent(INOUT) :: this            !< Parameter List 
        class(ParameterListEntry_t),  pointer      :: Next            !< Parameter List Node
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
    end subroutine ParameterListEntry_Free



    function ParameterListEntry_GetEntry(this,Key) result(Entry)
    !-----------------------------------------------------------------
    !< Return a pointer to a ParameterList given a Key
    !-----------------------------------------------------------------
        class(ParameterListEntry_t), target, intent(IN) :: this       !< Parameter List
        character(len=*),                    intent(IN) :: Key        !< String Key
        class(ParameterListEntry_t), pointer            :: Entry      !< Parameter List Node
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
    end function ParameterListEntry_GetEntry


    function ParameterListEntry_GetPreviousEntry(this,Key) result(PreviousEntry)
    !-----------------------------------------------------------------
    !< Return a pointer to the provious node of a Parameter List given a Key
    !-----------------------------------------------------------------
        class(ParameterListEntry_t), target, intent(IN) :: this          !< Parameter List
        character(len=*),                    intent(IN) :: Key           !< String Key
        class(ParameterListEntry_t), pointer            :: PreviousEntry !< Parameter List Node
        class(ParameterListEntry_t), pointer            :: Next          !< Parameter List Next Node
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
    end function ParameterListEntry_GetPreviousEntry


    function ParameterListEntry_HasValue(this) result(hasValue)
    !-----------------------------------------------------------------
    !< Check if Value is allocated for the current Node
    !-----------------------------------------------------------------
        class(ParameterListEntry_t), intent(IN) :: this               !< Parameter List 
        logical                                 :: hasValue           !< Check if Value is allocated
    !-----------------------------------------------------------------
        hasValue = allocated(this%Value)
    end function ParameterListEntry_HasValue


    subroutine ParameterListEntry_SetValue(this, Value)
    !-----------------------------------------------------------------
    !< Set a concrete Wrapper
    !-----------------------------------------------------------------
        class(ParameterListEntry_t), intent(INOUT)  :: this           !< Parameter List
        class(*),                    intent(IN)     :: Value          !< Concrete Wrapper
    !-----------------------------------------------------------------
        if(this%HasValue()) deallocate(this%Value)
        allocate(this%Value, source=Value)
    end subroutine ParameterListEntry_SetValue


    subroutine ParameterListEntry_GetValue(this, Value)
    !-----------------------------------------------------------------
    !< Return a concrete WrapperFactory
    !-----------------------------------------------------------------
        class(ParameterListEntry_t),             intent(IN)  :: this  !< Parameter List
        class(*), allocatable,                   intent(OUT) :: Value !< Concrete Wrapper
    !-----------------------------------------------------------------
        if(this%HasValue()) allocate(Value, source=this%Value)
    end subroutine ParameterListEntry_GetValue


    function ParameterListEntry_PointToValue(this) result(Value)
    !-----------------------------------------------------------------
    !< Return a pointer to a concrete WrapperFactory
    !-----------------------------------------------------------------
        class(ParameterListEntry_t), target, intent(IN)  :: this      !< Parameter List
        class(*), pointer                                :: Value     !< Concrete Wrapper
    !-----------------------------------------------------------------
        Value => this%Value
    end function ParameterListEntry_PointToValue


    subroutine ParameterListEntry_DeallocateValue(this)
    !-----------------------------------------------------------------
    !< Deallocate Key if allocated
    !-----------------------------------------------------------------
        class(ParameterListEntry_t), intent(INOUT) :: this            !< Parameter List 
    !-----------------------------------------------------------------
        if(this%HasValue()) deallocate(this%Value)
    end subroutine ParameterListEntry_DeallocateValue



    recursive subroutine ParameterListEntry_Finalize(this)
    !-----------------------------------------------------------------
    !< Finalize procedure
    !-----------------------------------------------------------------
        type(ParameterListEntry_t), intent(INOUT):: this              !< Parameter List 
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine ParameterListEntry_Finalize


    recursive subroutine ParameterListEntry_AddNode(this,Key, Value)
    !-----------------------------------------------------------------
    !< Add a new Node if key does not Exist
    !-----------------------------------------------------------------
        class(ParameterListEntry_T),          intent(INOUT) :: this   !< Linked List
        character(len=*),                     intent(IN)    :: Key    !< Key (unique) of the current node.
        class(*),                             intent(IN)    :: Value  !< Wrapper Factory
    !-----------------------------------------------------------------
        if (this%HasKey()) then
            if (this%GetKey()/=Key) then
                if (.not. this%hasNext()) then 
                    ! I reached the end of the list
                    allocate(ParameterListEntry_t::this%Next)
                    select type (Next => this%Next)
                    type is (ParameterListEntry_t)
                        call Next%AddNode(Key=Key, Value=Value)
                    end select
                else
                    select type (Next => this%Next)
                    type is (ParameterListEntry_t)
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
    end subroutine ParameterListEntry_AddNode


    subroutine ParameterListEntry_RemoveEntry(this, Key, Root)
    !-----------------------------------------------------------------
    !< Remove an Entry given a Key
    !-----------------------------------------------------------------
        class(ParameterListEntry_t), target,  intent(INOUT) :: this          !< Parameter List Entry type
        character(len=*),                     intent(IN)    :: Key           !< String Key
        class(ParameterListEntry_t), pointer, intent(INOUT) :: Root          !< The Previous Entry of a given key
        class(ParameterListEntry_t), pointer                :: PreviousEntry !< The Previous Entry of a given key
        class(ParameterListEntry_t), pointer                :: CurrentEntry  !< Entry of a given key
        class(ParameterListEntry_t), pointer                :: NextEntry     !< The Next Node of a given key
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
    end subroutine ParameterListEntry_RemoveEntry


    function ParameterListEntry_GetLength(this) result(Length)
    !-----------------------------------------------------------------
    !< Return the length of the list
    !-----------------------------------------------------------------
        class(ParameterListEntry_t), intent(IN) :: this               !< Parameter List
        integer(I4P)                            :: Length             !< Length of the list
        type(ParameterListEntry_t),  pointer    :: NextEntry           !< Next Parameter List Entry
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
    end function ParameterListEntry_GetLength


    recursive subroutine ParameterListEntry_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print the keys/value pair contained in the parameter list
    !-----------------------------------------------------------------
        class(ParameterListEntry_t),      intent(IN), target  :: this !< Parameter list
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
        class(ParameterListEntry_t), pointer          :: Node         !< Pointer for scanning the list.
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
                    class is (ParameterListEntry_t)
                        call Next%Print(unit=unit,prefix=prefd,iostat=iostatd,iomsg=iomsgd)
                end select
            endif
        endif
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine ParameterListEntry_Print


end module ParameterListEntry
