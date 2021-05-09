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

#define ParameterList_t ParameterList_
#define ParameterListIterator_t ParameterListIterator_

module FPL

USE ParameterList
USE WrapperFactoryListSingleton

public :: ParameterList_t, ParameterListIterator_t

contains

    subroutine FPL_Init()
    !-----------------------------------------------------------------
    !< Initialize FPL
    !-----------------------------------------------------------------
        call TheWrapperFactoryList_Init()
    end subroutine FPL_Init


    subroutine FPL_Finalize()
    !-----------------------------------------------------------------
    !< Finalize FPL
    !-----------------------------------------------------------------
        call TheWrapperFactoryList%Free()
    end subroutine FPL_Finalize

end module FPL
