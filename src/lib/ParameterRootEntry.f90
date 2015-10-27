module ParameterRootEntry

USE ParameterEntry
USE IR_Precision, only: I4P, str

implicit none
private

    type :: ParameterRootEntry_t
        class(ParameterEntry_t), pointer :: Root => null()
    contains
    private
        procedure, public :: init             => ParameterRootEntry_Init
        procedure, public :: HasRoot          => ParameterRootEntry_HasRoot
        procedure, public :: SetRoot          => ParameterRootEntry_SetRoot
        procedure, public :: GetRoot          => ParameterRootEntry_GetRoot
        procedure, public :: GetEntry         => ParameterRootEntry_GetEntry
        procedure, public :: GetPreviousEntry => ParameterRootEntry_GetPreviousEntry
        procedure, public :: Print            => ParameterRootEntry_Print
        procedure, public :: isPresent        => ParameterRootEntry_isPresent
        procedure, public :: GetLength        => ParameterRootEntry_GetLength
        procedure, public :: RemoveEntry      => ParameterRootEntry_RemoveEntry
        procedure, public :: AddEntry         => ParameterRootEntry_AddEntry
        procedure, public :: Free             => ParameterRootEntry_Free
        final             ::                     ParameterRootEntry_Finalize 
    end type


public :: ParameterRootEntry_T

contains


    subroutine ParameterRootEntry_SetRoot(this, Root)
    !-----------------------------------------------------------------
    !< Set the Root of the list
    !-----------------------------------------------------------------
        class(ParameterRootEntry_t), target,  intent(INOUT) :: this       !< Parameter Root Entry
        class(ParameterEntry_t),     pointer, intent(IN)    :: Root       !< Parameter Entry correspoing to the head of the list
    !-----------------------------------------------------------------
        this%Root => Root
    end subroutine ParameterRootEntry_SetRoot


    function ParameterRootEntry_GetRoot(this) result(Root)
    !-----------------------------------------------------------------
    !< Return a pointer to the Root of the list
    !-----------------------------------------------------------------
        class(ParameterRootEntry_t), target, intent(IN) :: this       !< Parameter Root Entry
        class(ParameterEntry_t),     pointer            :: Root       !< Parameter Entry correspoing to the head of the list
    !-----------------------------------------------------------------
        Root => this%Root
    end function ParameterRootEntry_GetRoot


    function ParameterRootEntry_HasRoot(this) result(HasRoot)
    !-----------------------------------------------------------------
    !< Return a pointer to the Root of the list
    !-----------------------------------------------------------------
        class(ParameterRootEntry_t), target, intent(IN) :: this       !< Parameter Root Entry
        logical                                         :: hasRoot    !< Check if Root is associated
    !-----------------------------------------------------------------
        hasRoot = associated(this%GetRoot())
    end function ParameterRootEntry_HasRoot


    subroutine ParameterRootEntry_Init(this)
    !-----------------------------------------------------------------
    !< Initialize the Root of the list
    !-----------------------------------------------------------------
        class(ParameterRootEntry_t), target,  intent(INOUT) :: this       !< Parameter Root Entry
    !-----------------------------------------------------------------
        if(.not. this%HasRoot()) allocate(ParameterEntry_t::this%Root)
    end subroutine ParameterRootEntry_Init


    function ParameterRootEntry_IsPresent(this, Key) result(isPresent)
    !-----------------------------------------------------------------
    !< Check if a Key is present in the List
    !-----------------------------------------------------------------
        class(ParameterRootEntry_t),   intent(IN)  :: this            !< Parameter Root Entry
        character(len=*),              intent(IN)  :: Key             !< String Key
        logical                                    :: isPresent       !< Boolean flag to check if a Key is present
    !-----------------------------------------------------------------
        isPresent = associated(this%GetEntry(Key))
    end function ParameterRootEntry_IsPresent


    subroutine ParameterRootEntry_AddEntry(this,Key, Value)
    !-----------------------------------------------------------------
    !< Add a new Node if key does not Exist
    !-----------------------------------------------------------------
        class(ParameterRootEntry_t),  intent(INOUT) :: this           !< Parameter Root Entry
        character(len=*),             intent(IN)    :: Key            !< Key (unique) of the current node.
        class(*),                     intent(IN)    :: Value          !< Parameter Entry Value
        class(ParameterEntry_t),      pointer       :: NextEntry      !< Parameter Entry
    !-----------------------------------------------------------------
        if(.not. this%HasRoot()) then
            call this%Init()
        endif
        NextEntry => this%GetRoot()
        do while(associated(NextEntry))
            if (NextEntry%HasKey()) then
                if (NextEntry%GetKey()/=Key) then
                    if (.not. NextEntry%hasNext()) then 
                        ! I reached the end of the list
                        allocate(ParameterEntry_t::NextEntry%Next)
                        call NextEntry%Next%SetKey(Key=Key)
                        call NextEntry%Next%SetValue(Value=Value)
                        exit
                    else
                        NextEntry => NextEntry%GetNext()
                    endif
                else
                    call NextEntry%SetValue(Value=Value)
                    exit
                endif
            else
                call NextEntry%SetKey(Key=Key)
                call NExtEntry%SetValue(Value=Value)
                exit
            endif
        enddo
    end subroutine ParameterRootEntry_AddEntry


    subroutine ParameterRootEntry_RemoveEntry(this, Key)
    !-----------------------------------------------------------------
    !< Remove an Entry given a Key
    !-----------------------------------------------------------------
        class(ParameterRootEntry_t), target,  intent(INOUT) :: this          !< Parameter Root Entry
        character(len=*),                     intent(IN)    :: Key           !< String Key
        class(ParameterEntry_t),     pointer                :: PreviousEntry !< The Previous Entry of a given key
        class(ParameterEntry_t),     pointer                :: CurrentEntry  !< Entry of a given key
        class(ParameterEntry_t),     pointer                :: NextEntry     !< The Next Entry of a given key
    !-----------------------------------------------------------------
        if(this%HasRoot()) then
            CurrentEntry => this%GetRoot()
            if(CurrentEntry%HasKey()) then
                if(CurrentEntry%GetKey() == Key) then
                    NextEntry    => CurrentEntry%GetNext()
                    call CurrentEntry%DeallocateKey()    
                    call CurrentEntry%DeallocateValue()
                    if(CurrentEntry%HasNext()) then
                        if(NextEntry%HasKey()) then
                            call CurrentEntry%NullifyNext()
                            call this%SetRoot(Root = NextEntry)
                            deallocate(CurrentEntry)
                        endif
                    endif
                else
                    PreviousEntry => this%GetPreviousEntry(Key=Key)
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
    end subroutine ParameterRootEntry_RemoveEntry



    function ParameterRootEntry_GetEntry(this,Key) result(Entry)
    !-----------------------------------------------------------------
    !< Return a pointer to a ParameterEntry given a Key
    !-----------------------------------------------------------------
        class(ParameterRootEntry_t), target, intent(IN) :: this       !< Parameter Root Entry
        character(len=*),                    intent(IN) :: Key        !< String Key
        class(ParameterEntry_t),     pointer            :: Entry      !< Parameter Entry
    !-----------------------------------------------------------------
        Entry => this%GetRoot()
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
    end function ParameterrootEntry_GetEntry


    function ParameterRootEntry_GetPreviousEntry(this,Key) result(PreviousEntry)
    !-----------------------------------------------------------------
    !< Return a pointer to the provious node of a Parameter List given a Key
    !-----------------------------------------------------------------
        class(ParameterRootEntry_t), target, intent(IN) :: this          !< Parameter List
        character(len=*),                    intent(IN) :: Key           !< String Key
        class(ParameterEntry_t),     pointer            :: PreviousEntry !< Parameter List Node
        class(ParameterEntry_t),     pointer            :: Next          !< Parameter List Next Node
    !-----------------------------------------------------------------
        PreviousEntry => this%GetRoot()
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
    end function ParameterRootEntry_GetPreviousEntry


    function ParameterRootEntry_GetLength(this) result(Length)
    !-----------------------------------------------------------------
    !< Return the length of the list
    !-----------------------------------------------------------------
        class(ParameterRootEntry_t), intent(IN) :: this               !< Parameter Root Entry
        integer(I4P)                            :: Length             !< Length of the list
        type(ParameterEntry_t), pointer         :: NextEntry          !< Next Parameter Entry
    !-----------------------------------------------------------------
        Length = 0
        NextEntry => this%GetRoot()
        do while (associated(NextEntry))
            if (NextEntry%HasKey()) then
                Length = Length + 1
            endif
            NextEntry => NextEntry%GetNext()
        enddo
        nullify(NextEntry)
    end function ParameterRootEntry_GetLength



    subroutine ParameterRootEntry_Free(this)
    !-----------------------------------------------------------------
    !< Free the list
    !-----------------------------------------------------------------
        class(ParameterRootEntry_t), intent(INOUT) :: this            !< Parameter Root Entry
        class(ParameterEntry_t), pointer           :: Root            !< Parameter Entry
        class(ParameterEntry_t), pointer           :: Next            !< Parameter Entry
    !-----------------------------------------------------------------
        if(this%HasRoot()) then
            Root => this%GetRoot()
            call Root%Free()
            deallocate(Root)
        endif
    end subroutine ParameterRootEntry_Free



    subroutine ParameterRootEntry_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print the keys/value pair contained in the parameter list
    !-----------------------------------------------------------------
        class(ParameterRootEntry_t), target, intent(IN)  :: this      !< Parameter Root Entry
        integer(I4P),                        intent(IN)  :: unit      !< Logic unit.
        character(*), optional,              intent(IN)  :: prefix    !< Prefixing string.
        integer(I4P), optional,              intent(OUT) :: iostat    !< IO error.
        character(*), optional,              intent(OUT) :: iomsg     !< IO error message.
        character(len=:),       allocatable              :: prefd     !< Prefixing string.
        integer(I4P)                                     :: iostatd   !< IO error.
        character(500)                                   :: iomsgd    !< Temporary variable for IO error message.
        class(ParameterEntry_t), pointer                 :: NextEntry !< Pointer for scanning the list.
    !-----------------------------------------------------------------
        iostatd = 0 ; iomsgd = ''; prefd = '';if (present(prefix)) prefd = prefix
        if(this%HasRoot()) then
            NextEntry => this%GetRoot()
            do while(associated(NextEntry))
                if(.not. NextEntry%HasKey()) exit
                call NextEntry%Print(unit=unit, prefix=prefix, iostat=iostatd, iomsg=iomsgd )
                NextEntry => NextEntry%GetNext()
            enddo
        endif
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine ParameterRootEntry_Print


    recursive subroutine ParameterRootEntry_Finalize(this)
    !-----------------------------------------------------------------
    !< Finalize procedure
    !-----------------------------------------------------------------
        type(ParameterRootEntry_t), intent(INOUT):: this              !< Parameter List 
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine ParameterRootEntry_Finalize


end module ParameterRootEntry
