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

module WrapperFactoryListSingleton

USE WrapperFactoryList
USE WrapperFactory

implicit none
private

    type(WrapperFactoryList_t) :: TheWrapperFactoryList

public :: TheWrapperFactoryList
public :: TheWrapperFactoryList_Init

contains

    subroutine TheWrapperFactoryList_Init()
    !-----------------------------------------------------------------
    !< Set the dimensions of the Value contained in the wrapper
    !-----------------------------------------------------------------
        ! Add some Wrapper Factories to the list
        call TheWrapperFactoryList%AddNode(key='I1P',  WrapperFactory=WrapperFactoryI1P)
        call TheWrapperFactoryList%AddNode(key='I2P',  WrapperFactory=WrapperFactoryI2P)
        call TheWrapperFactoryList%AddNode(key='I4P',  WrapperFactory=WrapperFactoryI4P)
        call TheWrapperFactoryList%AddNode(key='I8P',  WrapperFactory=WrapperFactoryI8P)
        call TheWrapperFactoryList%AddNode(key='R4P',  WrapperFactory=WrapperFactoryR4P)
        call TheWrapperFactoryList%AddNode(key='R8P',  WrapperFactory=WrapperFactoryR8P)
        call TheWrapperFactoryList%AddNode(key='L',    WrapperFactory=WrapperFactoryL)
        call TheWrapperFactoryList%AddNode(key='DLCA', WrapperFactory=WrapperFactoryDLCA)
    end subroutine TheWrapperFactoryList_Init

end module WrapperFactoryListSingleton
