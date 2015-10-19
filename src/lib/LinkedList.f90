    !-----------------------------------------------------------------
    ! LinkedList is a datatype that implements a generic
    ! linked list. Each node contains a Key. Key is a
    ! deferred length character array
    !
    ! This work takes as a starting point the previou work of
    ! Stefano Zaghi (@szaghi, https://github.com/szaghi).
    !
    ! You can find the original source at:
    ! https://github.com/szaghi/OFF/blob/95691ca15e6d68128ba016e40df74e42123f1c54/src/Data_Type_Generic_List.f90
    !-----------------------------------------------------------------

module LinkedList

USE IR_Precision 

implicit none
private

    type, public :: LinkedList_t
    private
        character(len=:), allocatable :: Key
        type(LinkedList_t), pointer  :: Next   => null()
    contains
    private
        procedure         ::                  LinkedList_AddNode
        procedure, public :: Free          => LinkedList_Free
        procedure, public :: HasNext       => LinkedList_HasNext
        procedure, public :: SetNext       => LinkedList_SetNext
        procedure, public :: GetNext       => LinkedList_GetNext
        procedure, public :: NullifyNext   => LinkedList_NullifyNext
        procedure, public :: HasKey        => LinkedList_HasKey
        procedure, public :: SetKey        => LinkedList_SetKey
        procedure, public :: GetKey        => LinkedList_GetKey
        procedure, public :: GetNode       => LinkedList_GetNode
        procedure, public :: DeallocateKey => LinkedList_DeallocateKey
        procedure, public :: isPresent     => LinkedList_isPresent
        procedure, public :: RemoveNode    => LinkedList_RemoveNode
        procedure, public :: GetLength     => LinkedList_GetLength
        procedure, public :: Print         => LinkedList_Print
        generic, public   :: AddNode       => LinkedList_AddNode
        final             ::                  LinkedList_Finalize
    end type LinkedList_t

contains

    function LinkedList_HasNext(this) result(hasNext)
    !-----------------------------------------------------------------
    !< Check if Next is associated for the current Node
    !-----------------------------------------------------------------
        class(LinkedList_t), intent(IN) :: this                       !< Linked List 
        logical                         :: hasNext                    !< Check if Next is associated
    !-----------------------------------------------------------------
        hasNext = associated(this%Next)
    end function LinkedList_HasNext


    subroutine LinkedList_SetNext(this, Next)
    !-----------------------------------------------------------------
    !< Set the pointer to the Next node
    !-----------------------------------------------------------------
        class(LinkedList_t),         intent(INOUT) :: this               !< Linked List 
        class(LinkedList_t), target, intent(IN)    :: Next               !< Pointer to Next 
    !-----------------------------------------------------------------
        this%Next => Next
    end subroutine LinkedList_SetNext


    function LinkedList_GetNext(this) result(Next)
    !-----------------------------------------------------------------
    !< Return a pointer to the Next node
    !-----------------------------------------------------------------
        class(LinkedList_t), intent(IN) :: this                       !< Linked List 
        class(LinkedList_t), pointer    :: Next                       !< Pointer to Next
    !-----------------------------------------------------------------
        nullify(Next)
        if(this%HasNext()) Next => this%Next
    end function LinkedList_GetNext


    subroutine LinkedList_NullifyNext(this)
    !-----------------------------------------------------------------
    !< Nullify Next
    !-----------------------------------------------------------------
        class(LinkedList_t), intent(INOUT) :: this                       !< Linked List 
    !-----------------------------------------------------------------
        nullify(this%Next)
    end subroutine LinkedList_NullifyNext


    function LinkedList_HasKey(this) result(hasKey)
    !-----------------------------------------------------------------
    !< Check if Key is allocated for the current Node
    !-----------------------------------------------------------------
        class(LinkedList_t), intent(IN) :: this                       !< Linked List 
        logical                         :: hasKey                     !< Check if Key is associated
    !-----------------------------------------------------------------
        hasKey = allocated(this%Key)
    end function LinkedList_HasKey


    subroutine LinkedList_SetKey(this, Key) 
    !-----------------------------------------------------------------
    !< Check if Next is associated for the current Node
    !-----------------------------------------------------------------
        class(LinkedList_t),           intent(INOUT) :: this          !< Linked List 
        character(len=*),              intent(IN)    :: Key           !< Key
    !-----------------------------------------------------------------
        if(this%HasKey()) deallocate(this%Key)
        allocate(this%Key, source=Key)
    end subroutine LinkedList_SetKey


    function LinkedList_GetKey(this) result(Key)
    !-----------------------------------------------------------------
    !< Check if Next is associated for the current Node
    !-----------------------------------------------------------------
        class(LinkedList_t), intent(IN) :: this                       !< Linked List 
        character(len=:), allocatable   :: Key                        !< Key
    !-----------------------------------------------------------------
        if(this%HasKey()) allocate(Key, source=this%Key)
    end function LinkedList_GetKey


    subroutine LinkedList_DeallocateKey(this)
    !-----------------------------------------------------------------
    !< Deallocate Key if allocated
    !-----------------------------------------------------------------
        class(LinkedList_t), intent(INOUT) :: this                    !< Linked List 
    !-----------------------------------------------------------------
        if(this%HasKey()) deallocate(this%Key)
    end subroutine LinkedList_DeallocateKey


    recursive subroutine LinkedList_Free(this)
    !-----------------------------------------------------------------
    !< Free the list
    !-----------------------------------------------------------------
        class(LinkedList_t), intent(INOUT) :: this                    !< Linked List 
        class(LinkedList_t),  pointer      :: Next                    !< Linked List Node
    !-----------------------------------------------------------------
        if (this%HasNext()) then
            Next => this%GetNext()
            call Next%Free()
            deallocate(Next)
            nullify(Next)
        endif
        if (this%HasKey())   deallocate(this%Key)
        nullify(this%Next)
    end subroutine LinkedList_Free


    subroutine LinkedList_Finalize(this)
    !-----------------------------------------------------------------
    !< Destructor procedure
    !-----------------------------------------------------------------
        type(LinkedList_t), intent(INOUT):: this                      !< Linked List
    !-----------------------------------------------------------------
        call this%Free()
        return
    end subroutine LinkedList_Finalize


    function LinkedList_GetNode(this,Key) result(Node)
    !-----------------------------------------------------------------
    !< Return a pointer to a LinkedList given a Key
    !-----------------------------------------------------------------
        class(LinkedList_t), target, intent(IN) :: this               !< Linked List
        character(len=*),            intent(IN) :: Key                !< String Key
        type(LinkedList_t),  pointer            :: Node               !< Linked List Node
    !-----------------------------------------------------------------
        Node => this
        do while(associated(Node))
            if (Node%HasKey()) then
                if (Node%GetKey()==Key) exit
                Node => Node%GetNext()
            elseif (Node%HasNext()) then
                Node => Node%GetNext()
            else
                nullify(Node)
                exit
            endif
            
        enddo
    end function LinkedList_GetNode


    recursive function LinkedList_IsPresent(this, Key) result(isPresent)
    !-----------------------------------------------------------------
    !< Check if a Key is present in the DataBase
    !-----------------------------------------------------------------
        class(LinkedList_t),   intent(IN)  :: this                    !< Linked List
        character(len=*),      intent(IN)  :: Key                     !< String Key
        logical                            :: isPresent               !< Boolean flag to check if a Key is present
    !-----------------------------------------------------------------
        isPresent = associated(this%GetNode(Key))
    end function LinkedList_IsPresent


    recursive subroutine LinkedList_AddNode(this,Key)
    !-----------------------------------------------------------------
    !< Add a new Node if key does not Exist
    !-----------------------------------------------------------------
        class(LinkedList_T), intent(INOUT) :: this                    !< Linked List
        character(len=*),    intent(IN)    :: Key                     !< Key (unique) of the current node.
        class(LinkedList_t),  pointer      :: Next                    !< Pointer to a next LinkedList
    !-----------------------------------------------------------------
        if (this%HasKey()) then
            if (this%GetKey()/=Key) then
                if (.not. this%hasNext()) allocate(this%Next)
                Next => this%GetNext()
                call Next%AddNode(Key=Key)
            endif
        else
            call this%SetKey(Key=Key)
        endif
    end subroutine LinkedList_AddNode


    subroutine LinkedList_RemoveNode(this, Key)
    !-----------------------------------------------------------------
    !< Remove an LinkedList given a Key

    !-----------------------------------------------------------------
    class(LinkedList_t), target, intent(INOUT) :: this                !< Linked List
    character(len=*),            intent(IN)    :: Key                 !< String Key
    type(LinkedList_t),  pointer               :: CurrentNode         !< Pointer to the current LinkedList
    type(LinkedList_t),  pointer               :: NextNode            !< Pointer to a next LinkedList
    !-----------------------------------------------------------------
    CurrentNode => this
    do while(associated(CurrentNode))
        NextNode => CurrentNode%GetNext()
        if (CurrentNode%HasKey()) then
            if (CurrentNode%GetKey()==Key) then
                if (associated(NextNode)) then
                    if (NextNode%HasKey()) then
                        call CurrentNode%SetKey(Key=NextNode%GetKey())
                    else
                        call CurrentNode%DeallocateKey()
                    endif
                    CurrentNode%Next => NextNode%GetNext()
                    deallocate(NextNode)
                else
                    call CurrentNode%DeallocateKey()
                    nullify(CurrentNode%Next)
                endif
                exit
            endif
        endif
        CurrentNode => NextNode
    enddo
    end subroutine LinkedList_RemoveNode


    function LinkedList_GetLength(this) result(Length)
    !-----------------------------------------------------------------
    !< Return the length of the list
    !-----------------------------------------------------------------
        class(LinkedList_t), intent(IN) :: this                       !< Linked List
        integer(I4P)                    :: Length                     !< Length of the list
        type(LinkedList_t), pointer     :: NextNode                   !< Next Linked List Node
    !-----------------------------------------------------------------
        Length = 0 ; if (this%HasKey()) Length = 1
        NextNode => this%GetNext()
        do while (associated(NextNode))
            if (NextNode%HasKey()) then
                Length = Length + 1
            endif
            NextNode => NextNode%GetNext()
        enddo
        nullify(NextNode)
    end function LinkedList_GetLength


    subroutine LinkedList_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Return the length of the list
    !-----------------------------------------------------------------
        class(LinkedList_t), target,      intent(IN)  :: this    !< Linked List
        integer(I4P),                     intent(IN)  :: unit    !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix  !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat  !< IO error.
        character(*), optional,           intent(OUT) :: iomsg   !< IO error message.
        character(len=:), allocatable                 :: prefd   !< Prefixing string.
        integer(I4P)                                  :: iostatd !< IO error.
        character(500)                                :: iomsgd  !< Temporary variable for IO error message.
        class(LinkedList_T), pointer                  :: Node    !< Pointer for scanning the list.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        Node => this
        write(*,fmt='(A)') prefd//' LINKED LIST KEYS:'
        do while(Node%HasKey())
            write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)prefd//'   Key = '//Node%GetKey()
            if (Node%HasNExt()) then
                Node => Node%GetNext()
            else
                exit
            endif
        enddo
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine LinkedList_Print


end module LinkedList
