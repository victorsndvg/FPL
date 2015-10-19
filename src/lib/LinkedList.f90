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
        character(len=:),        allocatable :: Key
        type(LinkedList_t), pointer  :: Next   => null()
    contains
    private
        procedure, public :: Free        => LinkedList_Free
        procedure, public :: GetNode     => LinkedList_GetNode
        procedure, public :: HasNext     => LinkedList_HasNext
        procedure, public :: GetNext     => LinkedList_GetNext
        procedure, public :: HasKey      => LinkedList_HasKey
        procedure, public :: GetKey      => LinkedList_GetKey
        procedure, public :: isPresent   => LinkedList_isPresent
        procedure, public :: RemoveNode  => LinkedList_RemoveNode
        procedure, public :: GetLength   => LinkedList_GetLength
        final             ::                LinkedList_Finalize
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


    function LinkedList_GetNext(this) result(Next)
    !-----------------------------------------------------------------
    !< Check if Next is associated for the current Node
    !-----------------------------------------------------------------
        class(LinkedList_t), intent(IN) :: this                       !< Linked List 
        class(LinkedList_t), pointer    :: Next                       !< Pointer to Next
    !-----------------------------------------------------------------
        nullify(Next)
        if(this%HasNext()) Next => this%Next
    end function LinkedList_GetNext


    function LinkedList_HasKey(this) result(hasKey)
    !-----------------------------------------------------------------
    !< Check if Key is allocated for the current Node
    !-----------------------------------------------------------------
        class(LinkedList_t), intent(IN) :: this                       !< Linked List 
        logical                         :: hasKey                     !< Check if Key is associated
    !-----------------------------------------------------------------
        hasKey = allocated(this%Key)
    end function LinkedList_HasKey


    function LinkedList_GetKey(this) result(Key)
    !-----------------------------------------------------------------
    !< Check if Next is associated for the current Node
    !-----------------------------------------------------------------
        class(LinkedList_t), intent(IN) :: this                       !< Linked List 
        character(len=:), allocatable   :: Key                        !< Key
    !-----------------------------------------------------------------
        if(this%HasKey()) allocate(Key, source=this%Key)
    end function LinkedList_GetKey


    recursive subroutine LinkedList_Free(this)
    !-----------------------------------------------------------------
    !< Free the list
    !-----------------------------------------------------------------
        class(LinkedList_t), intent(INOUT):: this                     !< Linked List 
    !-----------------------------------------------------------------
        if (this%HasNext()) then
            call this%Next%Free()
            deallocate(this%Next)
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
        do
            if (Node%HasKey()) then
                if (Node%Key==Key) exit
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
        if (CurrentNode%HasKey()) then
            if (CurrentNode%Key==Key) then
                if (CurrentNode%HasNext()) then
                    NextNode => CurrentNode%GetNext()
                    if (NextNode%HasKey()) then
                        CurrentNode%Key = NextNode%Key
                    else
                        deallocate(CurrentNode%Key)
                    endif
                    CurrentNode%Next => NextNode%GetNext()
                    deallocate(NextNode)
                else
                    deallocate(CurrentNode%Key)
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


end module LinkedList
