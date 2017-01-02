!-----------------------------------------------------------------
! FPL (Fortran Parameter List)
! Copyright (c) 2015 Santiago Badia, Alberto F. Martín, 
! Javier Principe and Víctor Sande.
! All rights reserved.
!
! This library is free software; you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public
! License as published by the Free Software Foundation; either
! version 3.0 of the License, or (at your option) any later version.
!
! This library is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public
! License along with this library.
!-----------------------------------------------------------------

module ParameterRootEntry

USE ParameterEntry
USE PENF, only: I4P, str

implicit none
private

    type :: ParameterRootEntry_t
    private
        type(ParameterEntry_t),    pointer :: Root => null()
    contains
    private
        procedure, non_overridable         :: Init             => ParameterRootEntry_Init
        procedure, non_overridable, public :: HasRoot          => ParameterRootEntry_HasRoot
        procedure, non_overridable         :: SetRoot          => ParameterRootEntry_SetRoot
        procedure, non_overridable, public :: GetRoot          => ParameterRootEntry_GetRoot
        procedure, non_overridable         :: NullifyRoot      => ParameterRootEntry_NullifyRoot
        procedure, non_overridable         :: DeallocateRoot   => ParameterRootEntry_DeallocateRoot
        procedure, non_overridable, public :: GetEntry         => ParameterRootEntry_GetEntry
        procedure, non_overridable, public :: GetPreviousEntry => ParameterRootEntry_GetPreviousEntry
        procedure, non_overridable, public :: Print            => ParameterRootEntry_Print
        procedure, non_overridable, public :: isPresent        => ParameterRootEntry_isPresent
        procedure, non_overridable, public :: Length           => ParameterRootEntry_Length
        procedure, non_overridable, public :: RemoveEntry      => ParameterRootEntry_RemoveEntry
        procedure, non_overridable, public :: AddEntry         => ParameterRootEntry_AddEntry
        procedure, non_overridable, public :: GetIterator      => ParameterRootEntry_GetIterator
        procedure, non_overridable, public :: Free             => ParameterRootEntry_Free
        final                              ::                     ParameterRootEntry_Finalize 
    end type


public :: ParameterRootEntry_T

contains


    subroutine ParameterRootEntry_SetRoot(this, Root)
    !-----------------------------------------------------------------
    !< Set the Root of the list
    !-----------------------------------------------------------------
        class(ParameterRootEntry_t),          intent(INOUT) :: this       !< Parameter Root Entry
        class(ParameterEntry_t),     pointer, intent(IN)    :: Root       !< Parameter Entry correspoing to the head of the list
    !-----------------------------------------------------------------
        this%Root => Root
    end subroutine ParameterRootEntry_SetRoot


    function ParameterRootEntry_GetRoot(this) result(Root)
    !-----------------------------------------------------------------
    !< Return a pointer to the Root of the list
    !-----------------------------------------------------------------
        class(ParameterRootEntry_t),         intent(IN) :: this       !< Parameter Root Entry
        class(ParameterEntry_t),     pointer            :: Root       !< Parameter Entry correspoing to the head of the list
    !-----------------------------------------------------------------
        Root => this%Root
    end function ParameterRootEntry_GetRoot


    function ParameterRootEntry_HasRoot(this) result(HasRoot)
    !-----------------------------------------------------------------
    !< Return a pointer to the Root of the list
    !-----------------------------------------------------------------
        class(ParameterRootEntry_t),         intent(IN) :: this       !< Parameter Root Entry
        logical                                         :: hasRoot    !< Check if Root is associated
    !-----------------------------------------------------------------
        hasRoot = associated(this%GetRoot())
    end function ParameterRootEntry_HasRoot


    subroutine ParameterRootEntry_NullifyRoot(this)
    !-----------------------------------------------------------------
    !< Set the Root of the list
    !-----------------------------------------------------------------
        class(ParameterRootEntry_t),   intent(INOUT) :: this       !< Parameter Root Entry
    !-----------------------------------------------------------------
        nullify(this%Root)
    end subroutine ParameterRootEntry_NullifyRoot


    subroutine ParameterRootEntry_DeallocateRoot(this)
    !-----------------------------------------------------------------
    !< Set the Root of the list
    !-----------------------------------------------------------------
        class(ParameterRootEntry_t),   intent(INOUT) :: this       !< Parameter Root Entry
    !-----------------------------------------------------------------
        if(this%HasRoot()) then
            call this%Root%Free()
            deallocate(this%Root)
        endif
    end subroutine ParameterRootEntry_DeallocateRoot


    subroutine ParameterRootEntry_Init(this, Key, Value)
    !-----------------------------------------------------------------
    !< Initialize the Root of the list
    !-----------------------------------------------------------------
        class(ParameterRootEntry_t),       intent(INOUT) :: this       !< Parameter Root Entry
        character(len=*),                  intent(IN)    :: Key        !< Key (unique) of the current node.
        class(*), pointer,                 intent(IN)    :: Value      !< Parameter Entry Value
    !-----------------------------------------------------------------
        if(.not. this%HasRoot()) allocate(ParameterEntry_t::this%Root)
        call this%Root%SetKey(Key=Key)
        call this%Root%SetValue(Value=Value)
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
        class(*), pointer,            intent(IN)    :: Value          !< Parameter Entry Value
        class(ParameterEntry_t),      pointer       :: NextEntry      !< Parameter Entry
        class(ParameterEntry_t),      pointer       :: NewEntry       !< New Parameter Entry
        character(len=:), allocatable               :: NextEntryKey   !< Key of the NextEntry
    !-----------------------------------------------------------------
        if(.not. this%HasRoot()) then
            call this%Init(Key=Key, Value=Value)
        else
            NextEntry => this%GetRoot()
            do while(associated(NextEntry))
                call NextEntry%GetKey(NExtEntryKey)
                if (NextEntryKey/=Key) then
                    if (.not. NextEntry%hasNext()) then 
                        ! I reached the end of the list
                        allocate(ParameterEntry_t::NewEntry)
                        call NewEntry%SetKey(Key=Key)
                        call NewEntry%SetValue(Value=Value)
                        call NextEntry%SetNext(NExt=NewEntry)
                        exit
                    else
                        NextEntry => NextEntry%GetNext()
                    endif
                else
                    call NextEntry%SetValue(Value=Value)
                    exit
                endif
            enddo
            if(allocated(NextEntryKey)) deallocate(NextEntryKey)
        endif
    end subroutine ParameterRootEntry_AddEntry


    subroutine ParameterRootEntry_RemoveEntry(this, Key)
    !-----------------------------------------------------------------
    !< Remove an Entry given a Key
    !-----------------------------------------------------------------
        class(ParameterRootEntry_t),          intent(INOUT) :: this            !< Parameter Root Entry
        character(len=*),                     intent(IN)    :: Key             !< String Key
        character(len=:), allocatable                       :: CurrentEntryKey !< Current Entry Key
        class(ParameterEntry_t),     pointer                :: PreviousEntry   !< The Previous Entry of a given key
        class(ParameterEntry_t),     pointer                :: CurrentEntry    !< Entry of a given key
        class(ParameterEntry_t),     pointer                :: NextEntry       !< The Next Entry of a given key
    !-----------------------------------------------------------------
        if(this%HasRoot()) then
            CurrentEntry => this%GetRoot()
            call CurrentEntry%GetKey(CurrentEntryKey)
            if(CurrentEntryKey == Key) then
                NextEntry => CurrentEntry%GetNext()
                call CurrentEntry%DeallocateKey()    
                call CurrentEntry%DeallocateValue()
                call CurrentEntry%NullifyNext()
                deallocate(CurrentEntry)
                call this%NullifyRoot()
                if(allocated(CurrentEntryKey)) deallocate(CurrentEntryKey)
            else
                PreviousEntry     => this%GetPreviousEntry(Key=Key)
                if(associated(PreviousEntry)) then
                    CurrentEntry  => PreviousEntry%GetNext()
                    NextEntry     => CurrentEntry%GetNext()
                    call CurrentEntry%DeallocateKey()    
                    call CurrentEntry%DeallocateValue()
                    call CurrentEntry%NullifyNext()
                    deallocate(CurrentEntry)
                    call PreviousEntry%NullifyNext()
                    if(associated(NextEntry)) call PreviousEntry%SetNext(Next=NextEntry)
                endif   
            endif
            if(associated(NextEntry)) call this%SetRoot(Root = NextEntry)
        endif
    end subroutine ParameterRootEntry_RemoveEntry



    function ParameterRootEntry_GetEntry(this,Key) result(Entry)
    !-----------------------------------------------------------------
    !< Return a pointer to a ParameterEntry given a Key
    !-----------------------------------------------------------------
        class(ParameterRootEntry_t),         intent(IN) :: this       !< Parameter Root Entry
        character(len=*),                    intent(IN) :: Key        !< String Key
        class(ParameterEntry_t),     pointer            :: Entry      !< Parameter Entry
        character(len=:), allocatable                   :: EntryKey   !< Entry Key
    !-----------------------------------------------------------------
        Entry => this%GetRoot()
        do while(associated(Entry))
            call Entry%GetKey(EntryKey)
            if (EntryKey==Key) exit
            Entry => Entry%GetNext()
        enddo
        if(allocated(EntryKey)) deallocate(EntryKey)
    end function ParameterrootEntry_GetEntry


    function ParameterRootEntry_GetPreviousEntry(this,Key) result(PreviousEntry)
    !-----------------------------------------------------------------
    !< Return a pointer to the provious node of a Parameter List given a Key
    !-----------------------------------------------------------------
        class(ParameterRootEntry_t),         intent(IN) :: this          !< Parameter List
        character(len=*),                    intent(IN) :: Key           !< String Key
        class(ParameterEntry_t),     pointer            :: PreviousEntry !< Parameter List Entry
        class(ParameterEntry_t),     pointer            :: NextEntry     !< Parameter List Next Entry
        character(len=:), allocatable                   :: NExtEntryKey  !< NextEntry Key
    !-----------------------------------------------------------------
        PreviousEntry => this%GetRoot()
        do while(associated(PreviousEntry))
            if (PreviousEntry%HasNext()) then
                NextEntry => PreviousEntry%GetNext()
                call NextEntry%GetKey(NextEntryKey)
                if (NextEntryKey==Key) then
                    exit
                else
                    PreviousEntry => NextEntry
                endif
            else
                nullify(PreviousEntry)
                exit
            endif
        enddo    
        if(allocated(NextEntryKey)) deallocate(NextEntryKey)
    end function ParameterRootEntry_GetPreviousEntry


    function ParameterRootEntry_Length(this) result(Length)
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
            Length = Length + 1
            NextEntry => NextEntry%GetNext()
        enddo
        nullify(NextEntry)
    end function ParameterRootEntry_Length


    subroutine ParameterRootEntry_Free(this)
    !-----------------------------------------------------------------
    !< Free the list
    !-----------------------------------------------------------------
        class(ParameterRootEntry_t), intent(INOUT) :: this            !< Parameter Root Entry
        class(ParameterEntry_t),     pointer       :: Current         !< Current Parameter List Node
        class(ParameterEntry_t),     pointer       :: Next            !< Next Parameter List Node
    !-----------------------------------------------------------------
        do while(this%HasRoot()) 
            Next => this%Root%GetNext()
            call this%Root%Free()
            call this%DeallocateRoot()
            call this%SetRoot(Root=Next)
        enddo
    end subroutine ParameterRootEntry_Free


    function ParameterRootEntry_GetIterator(this) result(Iterator)
    !-----------------------------------------------------------------
    !< Free the list
    !-----------------------------------------------------------------
        class(ParameterRootEntry_t),  intent(INOUT) :: this           !< Parameter Root Entry
        type(EntryListIterator_t)                   :: Iterator       !< List iterator
    !-----------------------------------------------------------------
        call Iterator%Init(Entry=this%Root)    
    end function ParameterRootEntry_GetIterator


    subroutine ParameterRootEntry_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print the keys/value pair contained in the parameter list
    !-----------------------------------------------------------------
        class(ParameterRootEntry_t),         intent(IN)  :: this      !< Parameter Root Entry
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
                call NextEntry%Print(unit=unit, prefix=prefix, iostat=iostatd, iomsg=iomsgd )
                NextEntry => NextEntry%GetNext()
            enddo
        endif
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine ParameterRootEntry_Print


    subroutine ParameterRootEntry_Finalize(this)
    !-----------------------------------------------------------------
    !< Finalize procedure
    !-----------------------------------------------------------------
        type(ParameterRootEntry_t), intent(INOUT):: this              !< Parameter List 
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine ParameterRootEntry_Finalize


end module ParameterRootEntry
