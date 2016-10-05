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

!-----------------------------------------------------------------
! ParameterEntryDictionary is a datatype containing a Database
! array of ParameterListEntries made to store diferent Entries
! depending on the hash of its Key.
!
! This work takes as a starting point the previou work of
! Stefano Zaghi (@szaghi, https://github.com/szaghi).
!
! You can find the original source at:
! https://github.com/szaghi/OFF/blob/95691ca15e6d68128ba016e40df74e42123f1c54/src/Data_Type_Hash_Table.f90
!-----------------------------------------------------------------

module ParameterEntryDictionary

USE ParameterEntry
USE ParameterRootEntry
USE PENF, only: I4P, str

implicit None
private

    integer(I4P), parameter:: DefaultDataBaseSize = 100_I4P

    type :: ParameterEntryDictionary_t
    private
        type(ParameterRootEntry_t), allocatable :: DataBase(:)
        integer(I4P)                            :: Size = 0_I4P
    contains
    private
        procedure, non_overridable         :: Hash       => ParameterEntryDictionary_Hash
        procedure, non_overridable, public :: Init       => ParameterEntryDictionary_Init
        procedure, non_overridable, public :: Set        => ParameterEntryDictionary_Set
        procedure, non_overridable, public :: Get        => ParameterEntryDictionary_Get
        procedure, non_overridable, public :: GetPointer => ParameterEntryDictionary_GetPointer
        procedure, non_overridable, public :: GetDatabase=> ParameterEntryDictionary_GetDataBase
        procedure, non_overridable, public :: Del        => ParameterEntryDictionary_Delete
        procedure, non_overridable, public :: IsPresent  => ParameterEntryDictionary_IsPresent
        procedure, non_overridable, public :: Length     => ParameterEntryDictionary_Length
        procedure, non_overridable, public :: Print      => ParameterEntryDictionary_Print
        procedure, non_overridable, public :: Free       => ParameterEntryDictionary_Free
        final                              ::               ParameterEntryDictionary_Finalize
    end type

public :: ParameterEntryDictionary_t

contains


    function ParameterEntryDictionary_Hash(this,Key) result(Hash)
    !-----------------------------------------------------------------
    !< String hash function
    !-----------------------------------------------------------------
        class(ParameterEntryDictionary_t),    intent(IN) :: this        !< Parameter Entry Dictionary
        character(len=*),                     intent(IN) :: Key         !< String Key
        integer(I4P)                                     :: Hash        !< Hash code
        character, dimension(len(Key))                   :: CharArray   !< Character array containing the Key
        integer(I4P)                                     :: CharIterator!< Char iterator index
    !-----------------------------------------------------------------
        forall (CharIterator=1:LEN(Key))
            CharArray(CharIterator) = Key(CharIterator:CharIterator)
        end forall
        Hash = MOD(SUM(ICHAR(CharArray)), this%Size)
    end function ParameterEntryDictionary_Hash


    subroutine ParameterEntryDictionary_Init(this,Size)
    !-----------------------------------------------------------------
    !< Allocate the database with a given Szie of DefaultDataBaseSize
    !-----------------------------------------------------------------
        class(ParameterEntryDictionary_t),    intent(INOUT) :: this   !< Parameter Entry Dictionary
        integer(I4P), optional,               intent(IN)    :: Size   !< DataBase Size
    !-----------------------------------------------------------------
        call this%Free()
        if (present(Size)) then
            this%Size = Size
        else
            this%Size = DefaultDataBaseSize
        endif
        allocate(this%DataBase(0:this%Size-1))
    end subroutine ParameterEntryDictionary_Init


    function ParameterEntryDictionary_isPresent(this,Key) result(isPresent)
    !-----------------------------------------------------------------
    !< Check if a Key is present in the DataBase
    !-----------------------------------------------------------------
        class(ParameterEntryDictionary_t),    intent(IN) :: this      !< Parameter Entry Dictionary
        character(len=*),                     intent(IN) :: Key       !< String Key
        logical                                          :: isPresent !< Boolean flag to check if a Key is present
    !-----------------------------------------------------------------
        isPresent = this%DataBase(this%Hash(Key=Key))%isPresent(Key=Key)
    end function ParameterEntryDictionary_isPresent


    subroutine ParameterEntryDictionary_Set(this,Key,Value)
    !-----------------------------------------------------------------
    !< Set a Key/Value pair into the DataBase
    !-----------------------------------------------------------------
        class(ParameterEntryDictionary_t),       intent(INOUT) :: this    !< Parameter Entry Dictionary
        character(len=*),                        intent(IN)    :: Key     !< String Key
        class(*), pointer,                       intent(IN)    :: Value   !< Value
    !-----------------------------------------------------------------
        call this%DataBase(this%Hash(Key=Key))%AddEntry(Key=Key,Value=Value)
    end subroutine ParameterEntryDictionary_Set


    subroutine ParameterEntryDictionary_Get(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a Value given the Key
    !-----------------------------------------------------------------
        class(ParameterEntryDictionary_t),    intent(IN)    :: this   !< Parameter Entry Dictionary
        character(len=*),                     intent(IN)    :: Key    !< String Key
        class(*),                allocatable, intent(INOUT) :: Value  !< Returned value
        class(ParameterEntry_t), pointer                    :: Entry  !< Pointer to a Parameter List
    !-----------------------------------------------------------------
        Entry => this%DataBase(this%Hash(Key=Key))%GetEntry(Key=Key)
        if(associated(Entry)) call Entry%GetValue(Value=Value)
    end subroutine ParameterEntryDictionary_Get


    subroutine ParameterEntryDictionary_GetPointer(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a Value given the Key
    !-----------------------------------------------------------------
        class(ParameterEntryDictionary_t),    intent(IN)    :: this   !< Parameter Entry Dictionary
        character(len=*),                     intent(IN)    :: Key    !< String Key
        class(*),                pointer,     intent(INOUT) :: Value  !< Returned value
        class(ParameterEntry_t), pointer                    :: Entry  !< Pointer to a Parameter List
        integer(I4P)                                        :: Hash   !< Hash code corresponding to Key
    !-----------------------------------------------------------------
        Entry => this%DataBase(this%Hash(Key=Key))%GetEntry(Key=Key)
        if(associated(Entry)) Value => Entry%PointToValue()
    end subroutine ParameterEntryDictionary_GetPointer


    function ParameterEntryDictionary_GetDataBase(this) result(Database)
    !-----------------------------------------------------------------
    !< Return a pointer to a Dictionary Database
    !-----------------------------------------------------------------
        class(ParameterEntryDictionary_t), target, intent(IN) :: this        !< Parameter Entry Dictionary
        type(ParameterRootEntry_t),        pointer            :: Database(:) !< Dictionary Database
    !-----------------------------------------------------------------
        DataBase => this%Database
    end function ParameterEntryDictionary_GetDataBase


    subroutine ParameterEntryDictionary_Delete(this, Key)
    !-----------------------------------------------------------------
    !< Remove an Entry given a Key
    !-----------------------------------------------------------------
        class(ParameterEntryDictionary_t),    intent(INOUT) :: this   !< Parameter Entry Dictionary
        character(len=*),                     intent(IN)    :: Key    !< String Key
    !-----------------------------------------------------------------
        call this%DataBase(this%Hash(Key=Key))%RemoveEntry(Key=Key)
    end subroutine ParameterEntryDictionary_Delete


    function ParameterEntryDictionary_Length(this) result(Length)
    !-----------------------------------------------------------------
    !< Return the number of ParameterListEntries contained in the DataBase
    !-----------------------------------------------------------------
        class(ParameterEntryDictionary_t),    intent(IN) :: this       !< Parameter Entry Dictionary
        integer(I4P)                                     :: Length     !< Number of parameters in database
        integer(I4P)                                     :: DBIterator !< Database Iterator index 
    !-----------------------------------------------------------------
        Length = 0
        if (allocated(this%DataBase)) THEN
            do DBIterator=lbound(this%DataBase,dim=1),ubound(this%DataBase,dim=1)
                    Length = Length + this%DataBase(DBIterator)%Length()
            enddo
        endif
    end function ParameterEntryDictionary_Length


    subroutine ParameterentryDictionary_Free(this)
    !-----------------------------------------------------------------
    !< Free ParameterListEntries and the DataBase
    !-----------------------------------------------------------------
        class(ParameterEntryDictionary_t),    intent(INOUT) :: this       !< Parameter Entry Dictionary
        integer(I4P)                                        :: DBIterator !< Database Iterator index 
    !-----------------------------------------------------------------
        if (allocated(this%DataBase)) THEN
            do DBIterator=lbound(this%DataBase,dim=1),ubound(this%DataBase,dim=1)
                call this%DataBase(DBIterator)%Free()
            enddo
            deallocate(this%DataBase)
        endif
        this%Size = 0_I4P
    end subroutine ParameterEntryDictionary_Free


    subroutine ParameterEntryDictionary_Finalize(this)
    !-----------------------------------------------------------------
    !< Destructor procedure
    !-----------------------------------------------------------------
        type(ParameterEntryDictionary_t),    intent(INOUT) :: this    !< Parameter Entry Dictionary
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine ParameterEntryDictionary_Finalize


    subroutine ParameterEntryDictionary_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print the content of the DataBase
    !-----------------------------------------------------------------
        class(ParameterEntryDictionary_t),    intent(IN)  :: this    !< Linked List
        integer(I4P),                         intent(IN)  :: unit    !< Logic unit.
        character(*), optional,               intent(IN)  :: prefix  !< Prefixing string.
        integer(I4P), optional,               intent(OUT) :: iostat  !< IO error.
        character(*), optional,               intent(OUT) :: iomsg   !< IO error message.
        character(len=:), allocatable                     :: prefd   !< Prefixing string.
        integer(I4P)                                      :: iostatd !< IO error.
        character(500)                                    :: iomsgd  !< Temporary variable for IO error message.
        integer(I4P)                                      :: DBIter  !< Database iterator
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        if (allocated(this%DataBase)) then
            do DBIter=lbound(this%DataBase,dim=1), ubound(this%DataBase,dim=1)
                call this%DataBase(DBIter)%Print(unit=unit,                         &
                    prefix=prefd//'  ['//trim(str(no_sign=.true., n=DBIter))//'] ', &
                    iostat=iostatd,iomsg=iomsgd)
            enddo
        endif
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine ParameterEntryDictionary_Print


end module ParameterEntryDictionary
