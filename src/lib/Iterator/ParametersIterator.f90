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
USE IR_Precision, only: I4P, str

implicit none
private

    type :: ParametersIterator_t
    private
        type(ParameterRootEntry_t), pointer :: DataBase(:) => NULL()
        type(ParameterEntry_t),     pointer :: Entry       => NULL()
        integer(I4P)                        :: Index       = 0
        integer(I4P)                        :: UpperBound  = 0
    contains
    private
        procedure,         non_overridable :: SearchNextAssociatedListRoot => ParametersIterator_SearchNextAssociatedListRoot
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
        nullify(this%Entry)
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
        this%Index = -1
        this%UpperBound = size(this%DataBase)
        call this%Next()
    end subroutine ParametersIterator_Init


    subroutine ParametersIterator_SearchNextAssociatedListRoot(this)
    !-----------------------------------------------------------------
    !< The iterator points to the next associated entry
    !-----------------------------------------------------------------
        class(ParametersIterator_t),     intent(INOUT) :: this        ! Dictionary iterator
    !-----------------------------------------------------------------
        nullify(this%Entry)
        this%Index = this%Index + 1
        do while(this%Index<this%UpperBound)
            if(this%DataBase(this%Index)%HasRoot()) exit
            this%Index = this%Index + 1
        enddo
        if(this%Index<this%UpperBound) this%Entry => this%DataBase(this%Index)%GetRoot() 
    end subroutine ParametersIterator_SearchNextAssociatedListRoot


    subroutine ParametersIterator_Next(this)
    !-----------------------------------------------------------------
    !< The iterator points to the next associated entry
    !-----------------------------------------------------------------
        class(ParametersIterator_t),     intent(INOUT) :: this        ! Dictionary iterator
    !-----------------------------------------------------------------
        if(.not. this%HasFinished()) then
            if(associated(this%Entry)) then
                if(this%Entry%HasNext()) then
                    this%Entry => this%Entry%GetNext()
                else
                    call this%SearchNextAssociatedListRoot()
                endif
            else ! First Entry
                call this%SearchNextAssociatedListRoot()
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
        CurrentEntry => this%Entry
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
