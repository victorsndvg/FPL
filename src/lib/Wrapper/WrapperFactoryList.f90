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

module WrapperFactoryList

USE PENF, only: I4P
USE WrapperFactory

implicit none
private

    type, public :: WrapperFactoryList_t
    private
        character(len=:),            allocatable :: Key
        class(WrapperFactory_t),     pointer     :: Value  => null()
        type(WrapperFactoryList_t),  pointer     :: Next   => null()
    contains
    private
        procedure, non_overridable, public :: Init              => WrapperFactoryList_Init
        procedure, non_overridable, public :: HasNext           => WrapperFactoryList_HasNext
        procedure, non_overridable, public :: SetNext           => WrapperFactoryList_SetNext
        procedure, non_overridable, public :: GetNext           => WrapperFactoryList_GetNext
        procedure, non_overridable, public :: NullifyNext       => WrapperFactoryList_NullifyNext
        procedure, non_overridable, public :: HasKey            => WrapperFactoryList_HasKey
        procedure, non_overridable, public :: SetKey            => WrapperFactoryList_SetKey
        procedure, non_overridable, public :: GetKey            => WrapperFactoryList_GetKey
        procedure, non_overridable, public :: DeallocateKey     => WrapperFactoryList_DeallocateKey
        procedure, non_overridable, public :: HasValue          => WrapperFactoryList_HasValue
        procedure, non_overridable, public :: SetValue          => WrapperFactoryList_SetValue
        procedure, non_overridable, public :: GetValue          => WrapperFactoryList_GetValue
        procedure, non_overridable, public :: Free              => WrapperFactoryList_Free
        procedure, non_overridable, public :: AddWrapperFactory => WrapperFactoryList_AddWrapperFactory
        procedure, non_overridable, public :: Print             => WrapperFactoryList_Print
        procedure, non_overridable         ::                      WrapperFactoryList_GetFactory0D
        procedure, non_overridable         ::                      WrapperFactoryList_GetFactory1D
        procedure, non_overridable         ::                      WrapperFactoryList_GetFactory2D
        procedure, non_overridable         ::                      WrapperFactoryList_GetFactory3D
        procedure, non_overridable         ::                      WrapperFactoryList_GetFactory4D
        procedure, non_overridable         ::                      WrapperFactoryList_GetFactory5D
        procedure, non_overridable         ::                      WrapperFactoryList_GetFactory6D
        procedure, non_overridable         ::                      WrapperFactoryList_GetFactory7D
        generic,   public :: GetFactory        => WrapperFactoryList_GetFactory0D, &
                                                  WrapperFactoryList_GetFactory1D, &
                                                  WrapperFactoryList_GetFactory2D, &
                                                  WrapperFactoryList_GetFactory3D, &
                                                  WrapperFactoryList_GetFactory4D, &
                                                  WrapperFactoryList_GetFactory5D, &
                                                  WrapperFactoryList_GetFactory6D, &
                                                  WrapperFactoryList_GetFactory7D
        final             ::                      WrapperFactoryList_Finalize
    end type WrapperFactoryList_t

contains

    subroutine WrapperFactoryList_Init(this)
    !-----------------------------------------------------------------
    !< Initialize the node
    !-----------------------------------------------------------------
        class(WrapperFactoryList_t),         intent(INOUT) :: this    !< Wrapper Factory List
    !-----------------------------------------------------------------
        if(allocated(this%Key)) deallocate(this%Key)
        nullify(this%Value)
        nullify(this%Next)
    end subroutine WrapperFactoryList_Init


    function WrapperFactoryList_HasNext(this) result(hasNext)
    !-----------------------------------------------------------------
    !< Check if Next is associated for the current Node
    !-----------------------------------------------------------------
        class(WrapperFactoryList_t), intent(IN) :: this               !< Wrapper Factory List
        logical                                 :: hasNext            !< Check if Next is associated
    !-----------------------------------------------------------------
        hasNext = associated(this%Next)
    end function WrapperFactoryList_HasNext


    subroutine WrapperFactoryList_SetNext(this, Next)
    !-----------------------------------------------------------------
    !< Set the pointer to the Next node
    !-----------------------------------------------------------------
        class(WrapperFactoryList_t),         intent(INOUT) :: this    !< Wrapper Factory List
        class(WrapperFactoryList_t), target, intent(IN)    :: Next    !< Pointer to Next
    !-----------------------------------------------------------------
        this%Next => Next
    end subroutine WrapperFactoryList_SetNext


    function WrapperFactoryList_GetNext(this) result(Next)
    !-----------------------------------------------------------------
    !< Return a pointer to the Next node
    !-----------------------------------------------------------------
        class(WrapperFactoryList_t), intent(IN) :: this               !< Wrapper Factory List
        class(WrapperFactoryList_t), pointer    :: Next               !< Pointer to Next
    !-----------------------------------------------------------------
        nullify(Next)
        if(this%HasNext()) Next => this%Next
    end function WrapperFactoryList_GetNext


    subroutine WrapperFactoryList_NullifyNext(this)
    !-----------------------------------------------------------------
    !< Nullify Next
    !-----------------------------------------------------------------
        class(WrapperFactoryList_t), intent(INOUT) :: this            !< Wrapper Factory List
    !-----------------------------------------------------------------
        nullify(this%Next)
    end subroutine WrapperFactoryList_NullifyNext


    function WrapperFactoryList_HasKey(this) result(hasKey)
    !-----------------------------------------------------------------
    !< Check if Key is allocated for the current Node
    !-----------------------------------------------------------------
        class(WrapperFactoryList_t), intent(IN) :: this               !< Wrapper Factory List
        logical                         :: hasKey                     !< Check if Key is associated
    !-----------------------------------------------------------------
        hasKey = allocated(this%Key)
    end function WrapperFactoryList_HasKey


    subroutine WrapperFactoryList_SetKey(this, Key)
    !-----------------------------------------------------------------
    !< Check if Next is associated for the current Node
    !-----------------------------------------------------------------
        class(WrapperFactoryList_t),   intent(INOUT) :: this          !< Wrapper Factory List
        character(len=*),              intent(IN)    :: Key           !< Key
    !-----------------------------------------------------------------
        this%Key = Key
    end subroutine WrapperFactoryList_SetKey


    function WrapperFactoryList_GetKey(this) result(Key)
    !-----------------------------------------------------------------
    !< Check if Next is associated for the current Node
    !-----------------------------------------------------------------
        class(WrapperFactoryList_t), intent(IN) :: this               !< Wrapper Factory List
        character(len=:), allocatable           :: Key                !< Key
    !-----------------------------------------------------------------
        if(this%HasKey()) Key = this%Key
    end function WrapperFactoryList_GetKey


    subroutine WrapperFactoryList_DeallocateKey(this)
    !-----------------------------------------------------------------
    !< Deallocate Key if allocated
    !-----------------------------------------------------------------
        class(WrapperFactoryList_t), intent(INOUT) :: this            !< Wrapper Factory List
    !-----------------------------------------------------------------
        if(this%HasKey()) deallocate(this%Key)
    end subroutine WrapperFactoryList_DeallocateKey


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
        class(WrapperFactoryList_t),          intent(INOUT)  :: this  !< Wrapper Factory List
        class(WrapperFactory_t), target,      intent(IN)     :: Value !< Concrete WrapperFactory
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
        class(WrapperFactoryList_t),  pointer     :: Next             !< Wrapper Factory List Node
    !-----------------------------------------------------------------
        if (this%HasNext()) then
            Next => this%GetNext()
            call Next%Free()
            deallocate(Next)
            nullify(Next)
        endif
        if (this%HasKey())   deallocate(this%Key)
        nullify(this%Next)
        nullify(this%Value)
    end subroutine WrapperFactoryList_Free


    recursive subroutine WrapperFactoryList_Finalize(this)
    !-----------------------------------------------------------------
    !< Finalize procedure
    !-----------------------------------------------------------------
        type(WrapperFactoryList_t), intent(INOUT):: this              !< Wrapper Factory List
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine WrapperFactoryList_Finalize


    recursive subroutine WrapperFactoryList_AddWrapperFactory(this,Key, WrapperFactory)
    !-----------------------------------------------------------------
    !< Add a new Node if key does not Exist
    !-----------------------------------------------------------------
        class(WrapperFactoryList_T),          intent(INOUT) :: this           !< Wrapper Factory List
        character(len=*),                     intent(IN)    :: Key            !< Key (unique) of the current node.
        class(WrapperFactory_t), target,      intent(IN)    :: WrapperFactory !< Wrapper Factory
    !-----------------------------------------------------------------
        if (this%HasKey()) then
            if (this%GetKey()/=Key) then
                if (.not. this%hasNext()) then
                    allocate(WrapperFactoryList_t::this%Next)
                    call this%Next%AddWrapperFactory(Key=Key, WrapperFactory=WrapperFactory)
                else
                    call this%Next%AddWrapperFactory(Key=Key, WrapperFactory=WrapperFactory)
                endif
            else
                call this%SetValue(Value=WrapperFactory)
            endif
        else
            call this%SetKey(Key=Key)
            call this%SetValue(Value=WrapperFactory)
        endif
    end subroutine WrapperFactoryList_AddWrapperFactory


    recursive function WrapperFactoryList_GetFactory0D(this, Value) result(WrapperFactory)
    !-----------------------------------------------------------------
    !< Return a WrapperFactory given a value
    !-----------------------------------------------------------------
        class(WrapperFactoryList_t),          intent(IN)  :: this            !< Wrapper Factory List
        class(*),                             intent(IN)  :: Value           !< Polymorphic Mold
        class(WrapperFactory_t), pointer                  :: WrapperFactory  !< Wrapper Factory
    !-----------------------------------------------------------------
        nullify(WrapperFactory)
        if (this%HasKey() .and. this%HasValue()) then
            if(this%Value%HasSameType(Value=Value)) then
                WrapperFactory => this%Value
            elseif(this%HasNext()) then
                WrapperFactory => this%Next%GetFactory(Value=Value)
            endif
        endif
    end function WrapperFactoryList_GetFactory0D


    recursive function WrapperFactoryList_GetFactory1D(this, Value) result(WrapperFactory)
    !-----------------------------------------------------------------
    !< Return a WrapperFactory given a value
    !-----------------------------------------------------------------
        class(WrapperFactoryList_t),      intent(IN)  :: this            !< Wrapper Factory List
        class(*),                         intent(IN)  :: Value(1:)       !< Polymorphic Mold
        class(WrapperFactory_t), pointer              :: WrapperFactory  !< Wrapper Factory
    !-----------------------------------------------------------------
        nullify(WrapperFactory)
        if (this%HasKey() .and. this%HasValue()) then
            if(this%Value%HasSameType(Value=Value(1))) then
                WrapperFactory => this%Value
            elseif(this%HasNext()) then
                WrapperFactory => this%Next%GetFactory(Value=Value)
            endif
        endif
    end function WrapperFactoryList_GetFactory1D


    recursive function WrapperFactoryList_GetFactory2D(this, Value) result(WrapperFactory)
    !-----------------------------------------------------------------
    !< Return a WrapperFactory given a value
    !-----------------------------------------------------------------
        class(WrapperFactoryList_t),      intent(IN)  :: this            !< Wrapper Factory List
        class(*),                         intent(IN)  :: Value(1:,1:)    !< Polymorphic Mold
        class(WrapperFactory_t), pointer              :: WrapperFactory  !< Wrapper Factory
    !-----------------------------------------------------------------
        nullify(WrapperFactory)
        if (this%HasKey() .and. this%HasValue()) then
            if(this%Value%HasSameType(Value=Value(1,1))) then
                WrapperFactory => this%Value
            elseif(this%HasNext()) then
                WrapperFactory => this%Next%GetFactory(Value=Value)
            endif
        endif
    end function WrapperFactoryList_GetFactory2D


    recursive function WrapperFactoryList_GetFactory3D(this, Value) result(WrapperFactory)
    !-----------------------------------------------------------------
    !< Return a WrapperFactory given a value
    !-----------------------------------------------------------------
        class(WrapperFactoryList_t),      intent(IN)  :: this            !< Wrapper Factory List
        class(*),                         intent(IN)  :: Value(1:,1:,1:) !< Polymorphic Mold
        class(WrapperFactory_t), pointer              :: WrapperFactory  !< Wrapper Factory
    !-----------------------------------------------------------------
        nullify(WrapperFactory)
        if (this%HasKey() .and. this%HasValue()) then
            if(this%Value%HasSameType(Value=Value(1,1,1))) then
                WrapperFactory => this%Value
            elseif(this%HasNext()) then
                WrapperFactory => this%Next%GetFactory(Value=Value)
            endif
        endif
    end function WrapperFactoryList_GetFactory3D


    recursive function WrapperFactoryList_GetFactory4D(this, Value) result(WrapperFactory)
    !-----------------------------------------------------------------
    !< Return a WrapperFactory given a value
    !-----------------------------------------------------------------
        class(WrapperFactoryList_t),      intent(IN)  :: this               !< Wrapper Factory List
        class(*),                         intent(IN)  :: Value(1:,1:,1:,1:) !< Polymorphic Mold
        class(WrapperFactory_t), pointer              :: WrapperFactory     !< Wrapper Factory
    !-----------------------------------------------------------------
        nullify(WrapperFactory)
        if (this%HasKey() .and. this%HasValue()) then
            if(this%Value%HasSameType(Value=Value(1,1,1,1))) then
                WrapperFactory => this%Value
            elseif(this%HasNext()) then
                WrapperFactory => this%Next%GetFactory(Value=Value)
            endif
        endif
    end function WrapperFactoryList_GetFactory4D


    recursive function WrapperFactoryList_GetFactory5D(this, Value) result(WrapperFactory)
    !-----------------------------------------------------------------
    !< Return a WrapperFactory given a value
    !-----------------------------------------------------------------
        class(WrapperFactoryList_t),      intent(IN)  :: this                  !< Wrapper Factory List
        class(*),                         intent(IN)  :: Value(1:,1:,1:,1:,1:) !< Polymorphic Mold
        class(WrapperFactory_t), pointer              :: WrapperFactory        !< Wrapper Factory
    !-----------------------------------------------------------------
        nullify(WrapperFactory)
        if (this%HasKey() .and. this%HasValue()) then
            if(this%Value%HasSameType(Value=Value(1,1,1,1,1))) then
                WrapperFactory => this%Value
            elseif(this%HasNext()) then
                WrapperFactory => this%Next%GetFactory(Value=Value)
            endif
        endif
    end function WrapperFactoryList_GetFactory5D


    recursive function WrapperFactoryList_GetFactory6D(this, Value) result(WrapperFactory)
    !-----------------------------------------------------------------
    !< Return a WrapperFactory given a value
    !-----------------------------------------------------------------
        class(WrapperFactoryList_t),      intent(IN)  :: this                     !< Wrapper Factory List
        class(*),                         intent(IN)  :: Value(1:,1:,1:,1:,1:,1:) !< Polymorphic Mold
        class(WrapperFactory_t), pointer              :: WrapperFactory           !< Wrapper Factory
    !-----------------------------------------------------------------
        nullify(WrapperFactory)
        if (this%HasKey() .and. this%HasValue()) then
            if(this%Value%HasSameType(Value=Value(1,1,1,1,1,1))) then
                WrapperFactory => this%Value
            elseif(this%HasNext()) then
                WrapperFactory => this%Next%GetFactory(Value=Value)
            endif
        endif
    end function WrapperFactoryList_GetFactory6D


    recursive function WrapperFactoryList_GetFactory7D(this, Value) result(WrapperFactory)
    !-----------------------------------------------------------------
    !< Return a WrapperFactory given a value
    !-----------------------------------------------------------------
        class(WrapperFactoryList_t),      intent(IN)  :: this                        !< Wrapper Factory List
        class(*),                         intent(IN)  :: Value(1:,1:,1:,1:,1:,1:,1:) !< Polymorphic Mold
        class(WrapperFactory_t), pointer              :: WrapperFactory              !< Wrapper Factory
    !-----------------------------------------------------------------
        nullify(WrapperFactory)
        if (this%HasKey() .and. this%HasValue()) then
            if(this%Value%HasSameType(Value=Value(1,1,1,1,1,1,1))) then
                WrapperFactory => this%Value
            elseif(this%HasNext()) then
                WrapperFactory => this%Next%GetFactory(Value=Value)
            endif
        endif
    end function WrapperFactoryList_GetFactory7D


    subroutine WrapperFactoryList_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print the keys contained in the list
    !-----------------------------------------------------------------
        class(WrapperFactoryList_t), target, intent(IN)  :: this      !< Wrapper Factory List
        integer(I4P),                        intent(IN)  :: unit      !< Logic unit.
        character(*), optional,              intent(IN)  :: prefix    !< Prefixing string.
        integer(I4P), optional,              intent(OUT) :: iostat    !< IO error.
        character(*), optional,              intent(OUT) :: iomsg     !< IO error message.
        character(len=:), allocatable                    :: prefd     !< Prefixing string.
        integer(I4P)                                     :: iostatd   !< IO error.
        character(500)                                   :: iomsgd    !< Temporary variable for IO error message.
        class(WrapperFactoryList_T), pointer             :: Node      !< Pointer for scanning the list.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        Node => this
        write(*,fmt='(A)') prefd//' WRAPPER FACTORY LIST KEYS:'
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
    end subroutine WrapperFactoryList_Print


end module WrapperFactoryList
