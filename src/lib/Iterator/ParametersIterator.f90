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

module ParametersIterator

USE ParameterEntry
USE ParameterRootEntry
USE ListIterator
USE IR_Precision, only: I4P, str

implicit none
private

    type :: ParametersIterator_t
    private
        type(ParameterRootEntry_t), pointer :: DataBase(:)  => NULL()
        type(ListIterator_t),       pointer :: ListIterator => NULL()
        integer(I4P)                        :: Index       = 0
        integer(I4P)                        :: UpperBound  = 0
    contains
    private
        procedure,         non_overridable :: NextNotEmptyListIterator => ParametersIterator_NextNotEmptyListIterator
        procedure, public, non_overridable :: Init => ParametersIterator_Init
        procedure, public, non_overridable :: Next => ParametersIterator_Next
        procedure, public, non_overridable :: HasFinished => ParametersIterator_HasFinished
        procedure, public, non_overridable :: GetCurrentEntry => ParametersIterator_GetCurrentEntry
        procedure, public, non_overridable :: GetCurrentIndex => ParametersIterator_GetCurrentIndex
        procedure, public, non_overridable :: Free => ParametersIterator_Free
        final                              :: ParametersIterator_Final
    end type

public :: ParametersIterator_t

contains

    subroutine ParametersIterator_Free(this)
    !-----------------------------------------------------------------
    !< Free the dictionary iterator
    !-----------------------------------------------------------------
        class(ParametersIterator_t), intent(INOUT) :: this            ! Dictionary iterator
    !-----------------------------------------------------------------
        this%Index      = 0
        this%UpperBound = 0
        nullify(this%DataBase)
        if(associated(this%ListIterator)) deallocate(this%ListIterator)
        nullify(this%ListIterator)
    end subroutine ParametersIterator_Free


    subroutine ParametersIterator_Final(this)
    !-----------------------------------------------------------------
    !< Free the dictionary iterator
    !-----------------------------------------------------------------
        type(ParametersIterator_t), intent(INOUT) :: this            ! Dictionary iterator
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine ParametersIterator_Final


    subroutine ParametersIterator_Init(this, DataBase)
    !-----------------------------------------------------------------
    !< Associate the iterator with a dictionary and rewind 
    !< to the first position
    !-----------------------------------------------------------------
        class(ParametersIterator_t),        intent(INOUT) :: this        ! Dictionary iterator
        type(ParameterRootEntry_t), target, intent(IN)    :: DataBase(:) ! Entries database
    !-----------------------------------------------------------------
        call this%Free()
        this%DataBase(0:) => DataBase(:)
        this%Index = 0
        this%UpperBound = size(this%DataBase)
        call this%Next()
    end subroutine ParametersIterator_Init


    subroutine ParametersIterator_NextNotEmptyListIterator(this)
    !-----------------------------------------------------------------
    !< The iterator points to the next associated entry
    !-----------------------------------------------------------------
        class(ParametersIterator_t),     intent(INOUT) :: this        ! Dictionary iterator
    !-----------------------------------------------------------------
        if(associated(this%ListIterator)) then
            call this%ListIterator%Free()
            deallocate(this%ListIterator)
            this%Index = this%Index + 1
        endif
        do while(this%Index < this%UpperBound)
            if(this%DataBase(this%Index)%HasRoot()) then 
                this%ListIterator => this%Database(this%Index)%GetIterator()
                exit
            endif
            this%Index = this%Index + 1
        enddo
    end subroutine ParametersIterator_NextNotEmptyListIterator


    subroutine ParametersIterator_Next(this)
    !-----------------------------------------------------------------
    !< The iterator points to the next associated entry
    !-----------------------------------------------------------------
        class(ParametersIterator_t),     intent(INOUT) :: this        ! Dictionary iterator
    !-----------------------------------------------------------------
        if(.not. this%HasFinished()) then
            if(associated(this%ListIterator)) then
                if(.not. this%ListIterator%HasFinished()) then
                    call this%ListIterator%Next()
                else
                    call this%NextNotEmptyListIterator()
                endif
            else ! First Entry
                call this%NextNotEmptyListIterator()
            endif 
        endif
    end subroutine ParametersIterator_Next


    function ParametersIterator_GetCurrentEntry(this) result(CurrentEntry)
    !-----------------------------------------------------------------
    !< Return the current Entry
    !-----------------------------------------------------------------
        class(ParametersIterator_t), intent(INOUT) :: this            ! Dictionary iterator
        type(ParameterEntry_t),  pointer           :: CurrentEntry    ! Current entry
    !-----------------------------------------------------------------
        CurrentEntry => this%ListIterator%GetCurrentEntry()
    end function ParametersIterator_GetCurrentEntry


    function ParametersIterator_GetCurrentIndex(this) result(CurrentIndex)
    !-----------------------------------------------------------------
    !< Return the current Index
    !-----------------------------------------------------------------
        class(ParametersIterator_t), intent(INOUT) :: this            ! Dictionary iterator
        integer(I4P)                               :: CurrentIndex    ! Current index
    !-----------------------------------------------------------------
        CurrentIndex = this%Index
    end function ParametersIterator_GetCurrentIndex


    function ParametersIterator_HasFinished(this) result(HasFinished)
    !-----------------------------------------------------------------
    !< Check if Iterator has reached the end of the dictionary
    !-----------------------------------------------------------------
        class(ParametersIterator_t),        intent(INOUT) :: this        ! Dictionary iterator
        logical                                           :: HasFinished
    !-----------------------------------------------------------------
        HasFinished = .false.
        if(this%Index==this%UpperBound) HasFinished = .true.
    end function ParametersIterator_HasFinished

end module ParametersIterator
