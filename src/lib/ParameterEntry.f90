!-----------------------------------------------------------------
! FPL (Fortran Parameter Entry)
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

module ParameterEntry

USE PENF
USE DimensionsWrapper

implicit none
private

    type :: EntryListIterator_t
    private
        type(ParameterEntry_t),     pointer :: CurrentEntry => NULL()
    contains
    private
        procedure,         non_overridable ::                    EntryListIterator_Assignment
        procedure, public, non_overridable :: Init            => EntryListIterator_Init
        procedure, public, non_overridable :: Next            => EntryListIterator_Next
        procedure, public, non_overridable :: HasFinished     => EntryListIterator_HasFinished
        procedure, public, non_overridable :: GetEntry        => EntryListIterator_GetEntry
        procedure, public, non_overridable :: GetKey          => EntryListIterator_GetKey
        procedure, public, non_overridable :: PointToValue    => EntryListIterator_PointToValue
        procedure, public, non_overridable :: Free            => EntryListIterator_Free
        generic,   public                  :: Assignment(=)   => EntryListIterator_Assignment
        final                              ::                    EntryListIterator_Final
    end type


    type :: ParameterEntry_t
    private
        character(len=:), allocatable         :: Key
        class(*),                    pointer  :: Value  => NULL()
        class(ParameterEntry_t),     pointer  :: Next   => NULL()
    contains
    private
        procedure, non_overridable, public :: Free             => ParameterEntry_Free
        procedure, non_overridable, public :: Print            => ParameterEntry_Print
        procedure, non_overridable, public :: HasNext          => ParameterEntry_HasNext
        procedure, non_overridable, public :: SetNext          => ParameterEntry_SetNext
        procedure, non_overridable, public :: GetNext          => ParameterEntry_GetNext
        procedure, non_overridable, public :: NullifyNext      => ParameterEntry_NullifyNext
        procedure, non_overridable, public :: HasKey           => ParameterEntry_HasKey
        procedure, non_overridable, public :: SetKey           => ParameterEntry_SetKey
        procedure, non_overridable, public :: GetKey           => ParameterEntry_GetKey
        procedure, non_overridable, public :: DeallocateKey    => ParameterEntry_DeallocateKey
        procedure, non_overridable, public :: HasValue         => ParameterEntry_HasValue
        procedure, non_overridable, public :: SetValue         => ParameterEntry_SetValue
        procedure, non_overridable, public :: GetValue         => ParameterEntry_GetValue
        procedure, non_overridable, public :: DeallocateValue  => ParameterEntry_DeallocateValue
        procedure, non_overridable, public :: PointToValue     => ParameterEntry_PointToValue
        procedure, non_overridable, public :: GetIterator      => ParameterEntry_GetIterator
        final                              ::                     ParameterEntry_Finalize
    end type ParameterEntry_t

public :: ParameterEntry_t
public :: EntryListIterator_t

contains


    function ParameterEntry_HasNext(this) result(hasNext)
    !-----------------------------------------------------------------
    !< Check if Next is associated for the current Node
    !-----------------------------------------------------------------
        class(ParameterEntry_t), intent(IN) :: this               !< Parameter Entry
        logical                             :: hasNext            !< Check if Next is associated
    !-----------------------------------------------------------------
        hasNext = associated(this%Next)
    end function ParameterEntry_HasNext


    subroutine ParameterEntry_SetNext(this, Next)
    !-----------------------------------------------------------------
    !< Set the pointer to the Next node
    !-----------------------------------------------------------------
        class(ParameterEntry_t),          intent(INOUT) :: this        !< Parameter Entry
        class(ParameterEntry_t), pointer, intent(IN)    :: Next        !< Pointer to Next
    !-----------------------------------------------------------------
        this%Next => Next
    end subroutine ParameterEntry_SetNext


    function ParameterEntry_GetNext(this) result(Next)
    !-----------------------------------------------------------------
    !< Return a pointer to the Next node
    !-----------------------------------------------------------------
        class(ParameterEntry_t), intent(IN) :: this                   !< Parameter Entry
        class(ParameterEntry_t), pointer    :: Next                   !< Pointer to Next
    !-----------------------------------------------------------------
        nullify(Next)
        if(this%HasNext()) Next => this%Next
    end function ParameterEntry_GetNext


    subroutine ParameterEntry_NullifyNext(this)
    !-----------------------------------------------------------------
    !< Nullify Next
    !-----------------------------------------------------------------
        class(ParameterEntry_t), intent(INOUT) :: this                !< Parameter Entry
    !-----------------------------------------------------------------
        nullify(this%Next)
    end subroutine ParameterEntry_NullifyNext


    function ParameterEntry_HasKey(this) result(hasKey)
    !-----------------------------------------------------------------
    !< Check if Key is allocated for the current Node
    !-----------------------------------------------------------------
        class(ParameterEntry_t),     intent(IN) :: this               !< Parameter Entry
        logical                                 :: hasKey             !< Check if Key is associated
    !-----------------------------------------------------------------
        hasKey = allocated(this%Key)
    end function ParameterEntry_HasKey


    subroutine ParameterEntry_SetKey(this, Key)
    !-----------------------------------------------------------------
    !< Check if Next is associated for the current Node
    !-----------------------------------------------------------------
        class(ParameterEntry_t),               intent(INOUT) :: this  !< Parameter Entry
        character(len=*),                      intent(IN)    :: Key   !< Key
    !-----------------------------------------------------------------
        this%Key = Key
    end subroutine ParameterEntry_SetKey


    subroutine ParameterEntry_GetKey(this, Key)
    !-----------------------------------------------------------------
    !< Return entry key
    !-----------------------------------------------------------------
        class(ParameterEntry_t),       intent(IN)    :: this          !< Parameter Entry
        character(len=:), allocatable, intent(INOUT) :: Key           !< Key
    !-----------------------------------------------------------------
        Key = this%Key
    end subroutine ParameterEntry_GetKey


    subroutine ParameterEntry_DeallocateKey(this)
    !-----------------------------------------------------------------
    !< Deallocate Key if allocated
    !-----------------------------------------------------------------
        class(ParameterEntry_t), intent(INOUT) :: this                !< Parameter Entry
    !-----------------------------------------------------------------
        if(this%HasKey()) deallocate(this%Key)
    end subroutine ParameterEntry_DeallocateKey


    subroutine ParameterEntry_Free(this)
    !-----------------------------------------------------------------
    !< Free the Entry
    !-----------------------------------------------------------------
        class(ParameterEntry_t), intent(INOUT) :: this                !< Parameter Entry
    !-----------------------------------------------------------------
        call this%DeallocateKey()
        call this%DeallocateValue()
        call this%NullifyNext()
    end subroutine ParameterEntry_Free


    function ParameterEntry_HasValue(this) result(hasValue)
    !-----------------------------------------------------------------
    !< Check if Value is allocated for the current Node
    !-----------------------------------------------------------------
        class(ParameterEntry_t), intent(IN) :: this                   !< Parameter Entry
        logical                             :: hasValue               !< Check if Value is allocated
    !-----------------------------------------------------------------
        hasValue = associated(this%Value)
    end function ParameterEntry_HasValue


    subroutine ParameterEntry_SetValue(this, Value)
    !-----------------------------------------------------------------
    !< Set a concrete Wrapper
    !-----------------------------------------------------------------
        class(ParameterEntry_t), intent(INOUT)  :: this               !< Parameter Entry
        class(*), pointer,       intent(IN)     :: Value              !< Concrete Wrapper
    !-----------------------------------------------------------------
        if(this%HasValue()) deallocate(this%Value)
        this%Value => Value
    end subroutine ParameterEntry_SetValue


    subroutine ParameterEntry_GetValue(this, Value)
    !-----------------------------------------------------------------
    !< Return a concrete WrapperFactory
    !-----------------------------------------------------------------
        class(ParameterEntry_t),             intent(IN)  :: this      !< Parameter Entry
        class(*), allocatable,               intent(OUT) :: Value     !< Concrete Wrapper
    !-----------------------------------------------------------------
        if(this%HasValue()) allocate(Value, source=this%Value)
    end subroutine ParameterEntry_GetValue


    function ParameterEntry_PointToValue(this) result(Value)
    !-----------------------------------------------------------------
    !< Return a pointer to a concrete WrapperFactory
    !-----------------------------------------------------------------
        class(ParameterEntry_t),         intent(IN)  :: this          !< Parameter Entry
        class(*), pointer                            :: Value         !< Concrete Wrapper
    !-----------------------------------------------------------------
        Value => this%Value
    end function ParameterEntry_PointToValue


    subroutine ParameterEntry_DeallocateValue(this)
    !-----------------------------------------------------------------
    !< Deallocate Key if allocated
    !-----------------------------------------------------------------
        class(ParameterEntry_t), intent(INOUT) :: this                !< Parameter Entry
    !-----------------------------------------------------------------
        if(this%HasValue()) deallocate(this%Value)
    end subroutine ParameterEntry_DeallocateValue


    subroutine ParameterEntry_Finalize(this)
    !-----------------------------------------------------------------
    !< Finalize procedure
    !-----------------------------------------------------------------
        type(ParameterEntry_t), intent(INOUT):: this                  !< Parameter Entry
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine ParameterEntry_Finalize


    function ParameterEntry_GetIterator(this) result(Iterator)
    !-----------------------------------------------------------------
    !< Free the list
    !-----------------------------------------------------------------
        class(ParameterEntry_t),  target, intent(INOUT) :: this       !< Parameter Entry
        type(EntryListIterator_t)                       :: Iterator   !< List iterator
    !-----------------------------------------------------------------
        call Iterator%Init(Entry=this)
    end function ParameterEntry_GetIterator


    subroutine ParameterEntry_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print the keys/value pair contained in the Parameter Entry
    !-----------------------------------------------------------------
        class(ParameterEntry_t),          intent(IN)  :: this         !< Parameter Entry
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:),       allocatable           :: prefd        !< Prefixing string.
        character(len=:),       allocatable           :: Key          !< Entry Key
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        iostatd = 0 ; iomsgd = ''; prefd = '';if (present(prefix)) prefd = prefix
        if(this%HasKey()) then
            call this%GetKey(Key)
            write(unit=unit,fmt='(A)', advance="NO", iostat=iostatd,iomsg=iomsgd) prefd // ' Key = "' // Key // '", '
            select type (Wrapper =>this%Value)
                class is (DimensionsWrapper_t)
                    call Wrapper%Print(unit=unit)
                class Default
                    write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd) ' is a Parameter SubList'
            end select
        endif
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine ParameterEntry_Print


!---------------------------------------------------------------------
!< Entry List Iterator Procedures
!---------------------------------------------------------------------

    subroutine EntryListIterator_Assignment(this, ListIterator)
    !-----------------------------------------------------------------
    !< Assignment operator
    !-----------------------------------------------------------------
        class(EntryListIterator_t), intent(INOUT) :: this             ! Output List iterator
        type(EntryListIterator_t),  intent(IN)    :: ListIterator     ! Input List iterator
    !-----------------------------------------------------------------
        this%CurrentEntry => ListIterator%CurrentEntry
    end subroutine EntryListIterator_Assignment


    subroutine EntryListIterator_Free(this)
    !-----------------------------------------------------------------
    !< Free the List iterator
    !-----------------------------------------------------------------
        class(EntryListIterator_t), intent(INOUT) :: this             ! List iterator
    !-----------------------------------------------------------------
        nullify(this%CurrentEntry)
    end subroutine EntryListIterator_Free


    subroutine EntryListIterator_Final(this)
    !-----------------------------------------------------------------
    !< Free the List iterator
    !-----------------------------------------------------------------
        type(EntryListIterator_t), intent(INOUT) :: this              ! List iterator
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine EntryListIterator_Final


    subroutine EntryListIterator_Init(this, Entry)
    !-----------------------------------------------------------------
    !< Associate the iterator with an entry
    !-----------------------------------------------------------------
        class(EntryListIterator_t),      intent(INOUT) :: this        ! List iterator
        type(ParameterEntry_t), target,  intent(IN)    :: Entry       ! List entry
    !-----------------------------------------------------------------
        call this%Free()
        this%CurrentEntry => Entry
    end subroutine EntryListIterator_Init


    subroutine EntryListIterator_Next(this)
    !-----------------------------------------------------------------
    !< The iterator points to the next associated entry
    !-----------------------------------------------------------------
        class(EntryListIterator_t),     intent(INOUT) :: this         ! List iterator
    !-----------------------------------------------------------------
        if(.not. this%HasFinished()) this%CurrentEntry => this%CurrentEntry%GetNext()
    end subroutine EntryListIterator_Next


    function EntryListIterator_GetEntry(this) result(CurrentEntry)
    !-----------------------------------------------------------------
    !< Return the current Entry
    !-----------------------------------------------------------------
        class(EntryListIterator_t),  intent(IN) :: this               ! List iterator
        type(ParameterEntry_t),  pointer        :: CurrentEntry       ! Current entry
    !-----------------------------------------------------------------
        nullify(CurrentEntry)
        CurrentEntry => this%CurrentEntry
    end function EntryListIterator_GetEntry


    subroutine EntryListIterator_GetKey(this, Key)
    !-----------------------------------------------------------------
    !< Return the current Key
    !-----------------------------------------------------------------
        class(EntryListIterator_t),    intent(IN)    :: this          ! List iterator
        character(len=:), allocatable, intent(INOUT) :: Key           ! Entry Key
        type(ParameterEntry_t),  pointer             :: CurrentEntry  ! Current entry
    !-----------------------------------------------------------------
        if(associated(this%CurrentEntry)) then
            if(this%CurrentEntry%HasKey()) call this%CurrentEntry%GetKey(Key)
        endif
    end subroutine EntryListIterator_GetKey


    function EntryListIterator_PointToValue(this) result(Value)
    !-----------------------------------------------------------------
    !< Return the current Value
    !-----------------------------------------------------------------
        class(EntryListIterator_t),  intent(IN) :: this               ! List iterator
        type(ParameterEntry_t),  pointer        :: CurrentEntry       ! Current entry
        class(*), pointer                       :: Value              ! Entry Value
    !-----------------------------------------------------------------
        nullify(Value)
        if(associated(this%CurrentEntry)) then
            if(this%CurrentEntry%HasValue()) Value => this%CurrentEntry%PointToValue()
        endif
    end function EntryListIterator_PointToValue


    function EntryListIterator_HasFinished(this) result(HasFinished)
    !-----------------------------------------------------------------
    !< Check if Iterator has reached the end of the dictionary
    !-----------------------------------------------------------------
        class(EntryListIterator_t),   intent(IN) :: this              ! List iterator
        logical                                  :: HasFinished       ! Check if has reached the end of the list
    !-----------------------------------------------------------------
        HasFinished = .false.
        if(.not. associated(this%CurrentEntry)) then
            HasFinished = .true.
        elseif(.not. this%CurrentEntry%HasNext()) then
            HasFinished = .true.
        endif
    end function EntryListIterator_HasFinished

end module ParameterEntry
