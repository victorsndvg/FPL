module WrapperFactoryList

USE LinkedList
USE WrapperFactory
USE DLCAWrapperFactory
USE I1PWrapperFactory
USE I2PWrapperFactory
USE I4PWrapperFactory
USE I8PWrapperFactory
USE LWrapperFactory
USE R4PWrapperFactory
USE R8PWrapperFactory
USE UPWrapperFactory


implicit none
private

    type, extends(LinkedList_t), public :: WrapperFactoryList_t
    private
        class(WrapperFactory_t), allocatable :: Value
    contains
    private
        procedure         ::                WrapperFactoryList_AddNode
        procedure, public :: Init        => WrapperFactoryList_Init
        procedure, public :: Free        => WrapperFactoryList_Free
        procedure, public :: HasValue    => WrapperFactoryList_HasValue
        procedure, public :: SetValue    => WrapperFactoryList_SetValue
        procedure, public :: GetValue    => WrapperFactoryList_GetValue
        procedure, public :: RemoveNode  => WrapperFactoryList_RemoveNode
        generic,   public :: AddNode     => WrapperFactoryList_AddNode
!        final             ::                LinkedList_Finalize
    end type WrapperFactoryList_t

contains

    subroutine WrapperFactoryList_Init(this)
    !-----------------------------------------------------------------
    !< WrapperFactory default initialization
    !-----------------------------------------------------------------
        class(WrapperFactoryList_t),          intent(INOUT)  :: this     !< Wrapper Factory List
        type(DLCAWrapperFactory_t)                           :: DLCAwf
        type(I1PWrapperFactory_t)                            :: I1Pwf
        type(I2PWrapperFactory_t)                            :: I2Pwf
        type(I4PWrapperFactory_t)                            :: I4Pwf
        type(I8PWrapperFactory_t)                            :: I8Pwf
        type(LWrapperFactory_t)                              :: Lwf
        type(R4PWrapperFactory_t)                            :: R4Pwf
        type(R8PWrapperFactory_t)                            :: R8Pwf
!        type(UPWrapperFactory_t)                             :: UPwf
    !-----------------------------------------------------------------
        call this%AddNode(key='DLCA', WrapperFactory=DLCAwf)
        call this%AddNode(key='I1P', WrapperFactory=I1Pwf)
        call this%AddNode(key='I2P', WrapperFactory=I2Pwf)
        call this%AddNode(key='I4P', WrapperFactory=I4Pwf)
        call this%AddNode(key='I8P', WrapperFactory=I8Pwf)
        call this%AddNode(key='L', WrapperFactory=Lwf)
        call this%AddNode(key='R4P', WrapperFactory=R4Pwf)
        call this%AddNode(key='R8P', WrapperFactory=R8Pwf)
!        call this%AddNode(key='UP', WrapperFactory=UPwf)
    end subroutine WrapperFactoryList_Init


    function WrapperFactoryList_HasValue(this) result(hasValue)
    !-----------------------------------------------------------------
    !< Check if Value is allocated for the current Node
    !-----------------------------------------------------------------
        class(WrapperFactoryList_t), intent(IN) :: this               !< Wrapper Factory List 
        logical                                 :: hasValue           !< Check if Value is allocated
    !-----------------------------------------------------------------
        hasValue = allocated(this%Value)
    end function WrapperFactoryList_HasValue


    subroutine WrapperFactoryList_SetValue(this, Value)
    !-----------------------------------------------------------------
    !< Return a concrete WrapperFactory
    !-----------------------------------------------------------------
        class(WrapperFactoryList_t),          intent(INOUT)  :: this     !< Wrapper Factory List
        class(WrapperFactory_t),              intent(IN)     :: Value    !< Concrete WrapperFactory
    !-----------------------------------------------------------------
        if(this%HasValue()) deallocate(this%Value)
        allocate(this%Value, source=Value)
    end subroutine WrapperFactoryList_SetValue


    subroutine WrapperFactoryList_GetValue(this, Value)
    !-----------------------------------------------------------------
    !< Return a concrete WrapperFactory
    !-----------------------------------------------------------------
        class(WrapperFactoryList_t),          intent(IN)  :: this     !< Wrapper Factory List
        class(WrapperFactory_t), allocatable, intent(OUT) :: Value    !< Concrete WrapperFactory
    !-----------------------------------------------------------------
        if(this%HasValue()) allocate(Value, source=this%Value)
    end subroutine WrapperFactoryList_GetValue


    recursive subroutine WrapperFactoryList_Free(this)
    !-----------------------------------------------------------------
    !< Free the list
    !-----------------------------------------------------------------
        class(WrapperFactoryList_t), intent(INOUT):: this             !< Wrapper Factory List 
    !-----------------------------------------------------------------
        call this%LinkedList_t%Free()
        if (this%HasValue())   deallocate(this%Value)
    end subroutine WrapperFactoryList_Free


    recursive subroutine WrapperFactoryList_AddNode(this,Key, WrapperFactory)
    !-----------------------------------------------------------------
    !< Add a new Node if key does not Exist
    !-----------------------------------------------------------------
        class(WrapperFactoryList_T),          intent(INOUT) :: this           !< Linked List
        character(len=*),                     intent(IN)    :: Key            !< Key (unique) of the current node.
        class(WrapperFactory_t),              intent(IN)    :: WrapperFactory !< Wrapper Factory
    !-----------------------------------------------------------------
        if (this%HasKey()) then
            if (this%GetKey()/=Key) then
                if (.not. this%hasNext()) then
                    allocate(WrapperFactoryList_t::this%Next)
                    select type (Next => this%Next)
                    type is (WrapperFactoryList_t)
                        call Next%AddNode(Key=Key, WrapperFactory=WrapperFactory)
                    end select
                else
                    select type (Next => this%Next)
                    type is (WrapperFactoryList_t)
                        call Next%AddNode(Key=Key, WrapperFactory=WrapperFactory)
                    end select
                endif
            endif
        else
            call this%SetKey(Key=Key)
            call this%SetValue(Value=WrapperFactory)
        endif
    end subroutine WrapperFactoryList_AddNode


    subroutine WrapperFactoryList_RemoveNode(this, Key)
    !-----------------------------------------------------------------
    !< Remove an LinkedList given a Key
    !-----------------------------------------------------------------
    class(WrapperFactoryList_t), target, intent(INOUT) :: this        !< Wrapper Factory List
    character(len=*),                    intent(IN)    :: Key         !< String Key
    type(WrapperFactoryList_t),  pointer               :: CurrentNode !< Pointer to the current Wrapper Factory List
    type(WrapperFactoryList_t),  pointer               :: NextNode    !< Pointer to a next Wrapper Factory List
    !-----------------------------------------------------------------
    CurrentNode => this
    do while(associated(CurrentNode))
        if (CurrentNode%HasKey()) then
            if (CurrentNode%GetKey()==Key) then
                if (CurrentNode%HasNext()) then
                    select type (NextNode => CurrentNode%GetNext())
                        type is (WrapperFactoryList_T)
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
                    end select
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
    end subroutine WrapperFactoryList_RemoveNode


end module WrapperFactoryList
