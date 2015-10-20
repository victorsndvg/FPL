
module ParameterListEntry

USE IR_Precision 
USE LinkedList
USE DimensionsWrapper

implicit none
private

    type, extends(LinkedList_t) :: ParameterListEntry_t
    private
        class(DimensionsWrapper_t), allocatable :: Value
    contains
    private
        procedure         ::                ParameterListEntry_AddNode
        procedure, public :: Free        => ParameterListEntry_Free
        procedure, public :: HasValue    => ParameterListEntry_HasValue
        procedure, public :: SetValue    => ParameterListEntry_SetValue
        procedure, public :: GetValue    => ParameterListEntry_GetValue
        procedure, public :: RemoveNode  => ParameterListEntry_RemoveNode
        generic,   public :: AddNode     => ParameterListEntry_AddNode     
        final             ::                ParameterListEntry_Finalize 
    end type ParameterListEntry_t

public :: ParameterListEntry_t

contains


    function ParameterListEntry_HasValue(this) result(hasValue)
    !-----------------------------------------------------------------
    !< Check if Value is allocated for the current Node
    !-----------------------------------------------------------------
        class(ParameterListEntry_t), intent(IN) :: this               !< Wrapper Factory List 
        logical                                 :: hasValue           !< Check if Value is allocated
    !-----------------------------------------------------------------
        hasValue = allocated(this%Value)
    end function ParameterListEntry_HasValue


    subroutine ParameterListEntry_SetValue(this, Value)
    !-----------------------------------------------------------------
    !< Return a concrete WrapperFactory
    !-----------------------------------------------------------------
        class(ParameterListEntry_t),          intent(INOUT)  :: this     !< Wrapper Factory List
        class(DimensionsWrapper_t),           intent(IN)     :: Value    !< Concrete WrapperFactory
    !-----------------------------------------------------------------
        if(this%HasValue()) deallocate(this%Value)
        allocate(this%Value, source=Value)
    end subroutine ParameterListEntry_SetValue


    subroutine ParameterListEntry_GetValue(this, Value)
    !-----------------------------------------------------------------
    !< Return a concrete WrapperFactory
    !-----------------------------------------------------------------
        class(ParameterListEntry_t),             intent(IN)  :: this  !< Wrapper Factory List
        class(DimensionsWrapper_t), allocatable, intent(OUT) :: Value !< Concrete WrapperFactory
    !-----------------------------------------------------------------
        if(this%HasValue()) allocate(Value, source=this%Value)
    end subroutine ParameterListEntry_GetValue


    recursive subroutine ParameterListEntry_Free(this)
    !-----------------------------------------------------------------
    !< Free the list
    !-----------------------------------------------------------------
        class(ParameterListEntry_t), intent(INOUT):: this             !< Wrapper Factory List 
    !-----------------------------------------------------------------
        call this%LinkedList_t%Free()
        if (this%HasValue())   deallocate(this%Value)
    end subroutine ParameterListEntry_Free


    recursive subroutine ParameterListEntry_Finalize(this)
    !-----------------------------------------------------------------
    !< Finalize procedure
    !-----------------------------------------------------------------
        type(ParameterListEntry_t), intent(INOUT):: this              !< Wrapper Factory List 
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine ParameterListEntry_Finalize


    recursive subroutine ParameterListEntry_AddNode(this,Key, Value)
    !-----------------------------------------------------------------
    !< Add a new Node if key does not Exist
    !-----------------------------------------------------------------
        class(ParameterListEntry_T),          intent(INOUT) :: this   !< Linked List
        character(len=*),                     intent(IN)    :: Key    !< Key (unique) of the current node.
        class(DimensionsWrapper_t),           intent(IN)    :: Value  !< Wrapper Factory
    !-----------------------------------------------------------------
        if (this%HasKey()) then
            if (this%GetKey()/=Key) then
                if (.not. this%hasNext()) then
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
            endif
        else
            call this%SetKey(Key=Key)
            call this%SetValue(Value=Value)
        endif
    end subroutine ParameterListEntry_AddNode


    subroutine ParameterListEntry_RemoveNode(this, Key)
    !-----------------------------------------------------------------
    !< Remove an LinkedList given a Key
    !-----------------------------------------------------------------
    class(ParameterListEntry_t), target, intent(INOUT) :: this        !< Wrapper Factory List
    character(len=*),                    intent(IN)    :: Key         !< String Key
    class(ParameterListEntry_t),  pointer              :: CurrentNode !< Pointer to the current Wrapper Factory List
    class(ParameterListEntry_t),  pointer              :: NextNode    !< Pointer to a next Wrapper Factory List
    !-----------------------------------------------------------------
    CurrentNode => this
    do while(associated(CurrentNode))
        if (CurrentNode%HasKey()) then
            if (CurrentNode%GetKey()==Key) then
                if (CurrentNode%HasNext()) then
                    if (NextNode%HasKey()) then
                        call CurrentNode%SetKey(Key=NextNode%GetKey())
                    else
                        call CurrentNode%DeallocateKey()
                    endif
                    if (NextNode%HasValue()) then
                        allocate(CurrentNode%Value, source=NextNode%Value)
                    else
                        deallocate(CurrentNode%Value)
                    endif
                    call CurrentNode%SetNext(Next=NextNode%GetNext())
                else
                    call CurrentNode%DeallocateKey()
                    if (CurrentNode%HasValue()) deallocate(CurrentNode%Value)
                    call CurrentNode%NullifyNext()
                endif
                exit
            endif
        endif
        CurrentNode => NextNode
    enddo
    end subroutine ParameterListEntry_RemoveNode


end module ParameterListEntry
