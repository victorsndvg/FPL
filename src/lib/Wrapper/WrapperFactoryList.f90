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
        class(WrapperFactory_t), pointer :: Value
    contains
    private
        procedure         ::                WrapperFactoryList_AddNode
        procedure         ::                WrapperFactoryList_GetFactory0D
        procedure         ::                WrapperFactoryList_GetFactory1D
        procedure         ::                WrapperFactoryList_GetFactory2D
        procedure         ::                WrapperFactoryList_GetFactory3D
        procedure         ::                WrapperFactoryList_GetFactory4D
        procedure         ::                WrapperFactoryList_GetFactory5D
        procedure         ::                WrapperFactoryList_GetFactory6D
        procedure         ::                WrapperFactoryList_GetFactory7D
        procedure, public :: Init        => WrapperFactoryList_Init
        procedure, public :: Free        => WrapperFactoryList_Free
        procedure, public :: HasValue    => WrapperFactoryList_HasValue
        procedure, public :: SetValue    => WrapperFactoryList_SetValue
        procedure, public :: GetValue    => WrapperFactoryList_GetValue
        procedure, public :: RemoveNode  => WrapperFactoryList_RemoveNode
        generic,   public :: GetFactory  => WrapperFactoryList_GetFactory0D, &
                                            WrapperFactoryList_GetFactory1D, &
                                            WrapperFactoryList_GetFactory2D, &
                                            WrapperFactoryList_GetFactory3D, &
                                            WrapperFactoryList_GetFactory4D, &
                                            WrapperFactoryList_GetFactory5D, &
                                            WrapperFactoryList_GetFactory6D, &
                                            WrapperFactoryList_GetFactory7D
        generic,   public :: AddNode     => WrapperFactoryList_AddNode
        final             ::                WrapperFactoryList_Finalize
    end type WrapperFactoryList_t

contains

    subroutine WrapperFactoryList_Init(this)
    !-----------------------------------------------------------------
    !< WrapperFactory default initialization
    !-----------------------------------------------------------------
        class(WrapperFactoryList_t),          intent(INOUT)  :: this     !< Wrapper Factory List

    !-----------------------------------------------------------------
        call this%AddNode(key='I1P', WrapperFactory=WrapperFactoryI1P)
        call this%AddNode(key='I2P', WrapperFactory=WrapperFactoryI2P)
        call this%AddNode(key='I4P', WrapperFactory=WrapperFactoryI4P)
        call this%AddNode(key='I8P', WrapperFactory=WrapperFactoryI8P)
        call this%AddNode(key='R4P', WrapperFactory=WrapperFactoryR4P)
        call this%AddNode(key='R8P', WrapperFactory=WrapperFactoryR8P)
        call this%AddNode(key='L', WrapperFactory=WrapperFactoryL)
        call this%AddNode(key='DLCA', WrapperFactory=WrapperFactoryDLCA)
    end subroutine WrapperFactoryList_Init


    function WrapperFactoryList_HasValue(this) result(hasValue)
    !-----------------------------------------------------------------
    !< Check if Value is allocated for the current Node
    !-----------------------------------------------------------------
        class(WrapperFactoryList_t), intent(IN) :: this               !< Wrapper Factory List 
        logical                                 :: hasValue           !< Check if Value is allocated
    !-----------------------------------------------------------------
        hasValue = associated(this%Value)
    end function WrapperFactoryList_HasValue


    subroutine WrapperFactoryList_SetValue(this, Value)
    !-----------------------------------------------------------------
    !< Return a concrete WrapperFactory
    !-----------------------------------------------------------------
        class(WrapperFactoryList_t),          intent(INOUT)  :: this     !< Wrapper Factory List
        class(WrapperFactory_t), target,      intent(IN)     :: Value    !< Concrete WrapperFactory
    !-----------------------------------------------------------------
        this%Value => Value
    end subroutine WrapperFactoryList_SetValue


    subroutine WrapperFactoryList_GetValue(this, Value)
    !-----------------------------------------------------------------
    !< Return a concrete WrapperFactory
    !-----------------------------------------------------------------
        class(WrapperFactoryList_t),          intent(IN)  :: this     !< Wrapper Factory List
        class(WrapperFactory_t), pointer,     intent(OUT) :: Value    !< Concrete WrapperFactory pointer
    !-----------------------------------------------------------------
        nullify(Value)
        if(this%HasValue()) Value => this%Value
    end subroutine WrapperFactoryList_GetValue


    recursive subroutine WrapperFactoryList_Free(this)
    !-----------------------------------------------------------------
    !< Free the list
    !-----------------------------------------------------------------
        class(WrapperFactoryList_t), intent(INOUT):: this             !< Wrapper Factory List 
    !-----------------------------------------------------------------
        call this%LinkedList_t%Free()
        nullify(this%Value)
    end subroutine WrapperFactoryList_Free


    recursive subroutine WrapperFactoryList_Finalize(this)
    !-----------------------------------------------------------------
    !< Finalize procedure
    !-----------------------------------------------------------------
        type(WrapperFactoryList_t), intent(INOUT):: this             !< Wrapper Factory List 
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine WrapperFactoryList_Finalize


    recursive subroutine WrapperFactoryList_AddNode(this,Key, WrapperFactory)
    !-----------------------------------------------------------------
    !< Add a new Node if key does not Exist
    !-----------------------------------------------------------------
        class(WrapperFactoryList_T),          intent(INOUT) :: this           !< Linked List
        character(len=*),                     intent(IN)    :: Key            !< Key (unique) of the current node.
        class(WrapperFactory_t), target,      intent(IN)    :: WrapperFactory !< Wrapper Factory
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
            else
                call this%SetValue(Value=WrapperFactory)
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
        class(WrapperFactoryList_t), target,  intent(INOUT) :: this        !< Wrapper Factory List
        character(len=*),                     intent(IN)    :: Key         !< String Key
        class(WrapperFactoryList_t),  pointer               :: CurrentNode !< Pointer to the current Wrapper Factory List
        class(WrapperFactoryList_t),  pointer               :: NextNode    !< Pointer to a next Wrapper Factory List
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
                            CurrentNode%Value => NextNode%Value
                        else
                            deallocate(CurrentNode%Value)
                        endif
                        call CurrentNode%SetNext(Next=NextNode%GetNext())
                    else
                        call CurrentNode%DeallocateKey()
                        nullify(CurrentNode%Value)
                        call CurrentNode%NullifyNext()
                    endif
                    exit
                endif
            endif
            CurrentNode => NextNode
        enddo
    end subroutine WrapperFactoryList_RemoveNode


    recursive function WrapperFactoryList_GetFactory0D(this, Value) result(WrapperFactory)
    !-----------------------------------------------------------------
    !< Return a WrapperFactory given a value
    !-----------------------------------------------------------------
        class(WrapperFactoryList_t),          intent(IN)  :: this            !< Linked List
        class(*),                             intent(IN)  :: Value           !< Polymorphic Mold
        class(WrapperFactory_t), pointer                  :: WrapperFactory  !< Wrapper Factory
    !-----------------------------------------------------------------
        if (this%HasKey() .and. this%HasValue()) then
            if(this%Value%HasSameType(Value=Value)) then
                WrapperFactory => this%Value
            elseif(this%HasNext()) then
                select type (Next => this%Next)
                    type is (WrapperFactoryList_T)
                        WrapperFactory => Next%GetFactory(Value=Value)
                end select
            else
                ! Default case: Return an Unlimited Polymorphic Wrapper Factory
                WrapperFactory => WrapperFactoryUP
            endif
        endif
    end function WrapperFactoryList_GetFactory0D


    recursive function WrapperFactoryList_GetFactory1D(this, Value) result(WrapperFactory)
    !-----------------------------------------------------------------
    !< Return a WrapperFactory given a value
    !-----------------------------------------------------------------
        class(WrapperFactoryList_t),      intent(IN)  :: this            !< Linked List
        class(*),                         intent(IN)  :: Value(1:)       !< Polymorphic Mold
        class(WrapperFactory_t), pointer              :: WrapperFactory  !< Wrapper Factory
    !-----------------------------------------------------------------
        if (this%HasKey() .and. this%HasValue()) then
            if(this%Value%HasSameType(Value=Value(1))) then
                WrapperFactory => this%Value
            elseif(this%HasNext()) then
                select type (Next => this%Next)
                    type is (WrapperFactoryList_T)
                        WrapperFactory => Next%GetFactory(Value=Value)
                end select
            else
                ! Default case: Return an Unlimited Polymorphic Wrapper Factory
                WrapperFactory => WrapperFactoryUP
            endif
        endif
    end function WrapperFactoryList_GetFactory1D


    recursive function WrapperFactoryList_GetFactory2D(this, Value) result(WrapperFactory)
    !-----------------------------------------------------------------
    !< Return a WrapperFactory given a value
    !-----------------------------------------------------------------
        class(WrapperFactoryList_t),      intent(IN)  :: this            !< Linked List
        class(*),                         intent(IN)  :: Value(1:,1:)    !< Polymorphic Mold
        class(WrapperFactory_t), pointer              :: WrapperFactory  !< Wrapper Factory
    !-----------------------------------------------------------------
        if (this%HasKey() .and. this%HasValue()) then
            if(this%Value%HasSameType(Value=Value(1,1))) then
                WrapperFactory => this%Value
            elseif(this%HasNext()) then
                select type (Next => this%Next)
                    type is (WrapperFactoryList_T)
                        WrapperFactory => Next%GetFactory(Value=Value)
                end select
            else
                ! Default case: Return an Unlimited Polymorphic Wrapper Factory
                WrapperFactory => WrapperFactoryUP
            endif
        endif
    end function WrapperFactoryList_GetFactory2D


    recursive function WrapperFactoryList_GetFactory3D(this, Value) result(WrapperFactory)
    !-----------------------------------------------------------------
    !< Return a WrapperFactory given a value
    !-----------------------------------------------------------------
        class(WrapperFactoryList_t),      intent(IN)  :: this            !< Linked List
        class(*),                         intent(IN)  :: Value(1:,1:,1:) !< Polymorphic Mold
        class(WrapperFactory_t), pointer              :: WrapperFactory  !< Wrapper Factory
    !-----------------------------------------------------------------
        if (this%HasKey() .and. this%HasValue()) then
            if(this%Value%HasSameType(Value=Value(1,1,1))) then
                WrapperFactory => this%Value
            elseif(this%HasNext()) then
                select type (Next => this%Next)
                    type is (WrapperFactoryList_T)
                        WrapperFactory => Next%GetFactory(Value=Value)
                end select
            else
                ! Default case: Return an Unlimited Polymorphic Wrapper Factory
                WrapperFactory => WrapperFactoryUP
            endif
        endif
    end function WrapperFactoryList_GetFactory3D


    recursive function WrapperFactoryList_GetFactory4D(this, Value) result(WrapperFactory)
    !-----------------------------------------------------------------
    !< Return a WrapperFactory given a value
    !-----------------------------------------------------------------
        class(WrapperFactoryList_t),      intent(IN)  :: this               !< Linked List
        class(*),                         intent(IN)  :: Value(1:,1:,1:,1:) !< Polymorphic Mold
        class(WrapperFactory_t), pointer              :: WrapperFactory     !< Wrapper Factory
    !-----------------------------------------------------------------
        if (this%HasKey() .and. this%HasValue()) then
            if(this%Value%HasSameType(Value=Value(1,1,1,1))) then
                WrapperFactory => this%Value
            elseif(this%HasNext()) then
                select type (Next => this%Next)
                    type is (WrapperFactoryList_T)
                        WrapperFactory => Next%GetFactory(Value=Value)
                end select
            endif
            else
                ! Default case: Return an Unlimited Polymorphic Wrapper Factory
                WrapperFactory => WrapperFactoryUP
        endif
    end function WrapperFactoryList_GetFactory4D


    recursive function WrapperFactoryList_GetFactory5D(this, Value) result(WrapperFactory)
    !-----------------------------------------------------------------
    !< Return a WrapperFactory given a value
    !-----------------------------------------------------------------
        class(WrapperFactoryList_t),      intent(IN)  :: this                  !< Linked List
        class(*),                         intent(IN)  :: Value(1:,1:,1:,1:,1:) !< Polymorphic Mold
        class(WrapperFactory_t), pointer              :: WrapperFactory        !< Wrapper Factory
    !-----------------------------------------------------------------
        if (this%HasKey() .and. this%HasValue()) then
            if(this%Value%HasSameType(Value=Value(1,1,1,1,1))) then
                WrapperFactory => this%Value
            elseif(this%HasNext()) then
                select type (Next => this%Next)
                    type is (WrapperFactoryList_T)
                        WrapperFactory => Next%GetFactory(Value=Value)
                end select
            else
                ! Default case: Return an Unlimited Polymorphic Wrapper Factory
                WrapperFactory => WrapperFactoryUP
            endif
        endif
    end function WrapperFactoryList_GetFactory5D


    recursive function WrapperFactoryList_GetFactory6D(this, Value) result(WrapperFactory)
    !-----------------------------------------------------------------
    !< Return a WrapperFactory given a value
    !-----------------------------------------------------------------
        class(WrapperFactoryList_t),      intent(IN)  :: this                     !< Linked List
        class(*),                         intent(IN)  :: Value(1:,1:,1:,1:,1:,1:) !< Polymorphic Mold
        class(WrapperFactory_t), pointer              :: WrapperFactory           !< Wrapper Factory
    !-----------------------------------------------------------------
        if (this%HasKey() .and. this%HasValue()) then
            if(this%Value%HasSameType(Value=Value(1,1,1,1,1,1))) then
                allocate(WrapperFactory, source=this%Value)
            elseif(this%HasNext()) then
                select type (Next => this%Next)
                    type is (WrapperFactoryList_T)
                        WrapperFactory => Next%GetFactory(Value=Value)
                end select
            else
                ! Default case: Return an Unlimited Polymorphic Wrapper Factory
                WrapperFactory => WrapperFactoryUP
            endif
        endif
    end function WrapperFactoryList_GetFactory6D


    recursive function WrapperFactoryList_GetFactory7D(this, Value) result(WrapperFactory)
    !-----------------------------------------------------------------
    !< Return a WrapperFactory given a value
    !-----------------------------------------------------------------
        class(WrapperFactoryList_t),      intent(IN)  :: this                        !< Linked List
        class(*),                         intent(IN)  :: Value(1:,1:,1:,1:,1:,1:,1:) !< Polymorphic Mold
        class(WrapperFactory_t), pointer              :: WrapperFactory              !< Wrapper Factory
    !-----------------------------------------------------------------
        if (this%HasKey() .and. this%HasValue()) then
            if(this%Value%HasSameType(Value=Value(1,1,1,1,1,1,1))) then
                WrapperFactory => this%Value
            elseif(this%HasNext()) then
                select type (Next => this%Next)
                    type is (WrapperFactoryList_T)
                        WrapperFactory => Next%GetFactory(Value=Value)
                end select
            else
                ! Default case: Return an Unlimited Polymorphic Wrapper Factory
                WrapperFactory => WrapperFactoryUP
            endif
        endif
    end function WrapperFactoryList_GetFactory7D


end module WrapperFactoryList
