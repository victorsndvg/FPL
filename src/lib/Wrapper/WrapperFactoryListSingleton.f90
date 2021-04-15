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
USE DLCAWrapperFactory
USE I1PWrapperFactory
USE I2PWrapperFactory
USE I4PWrapperFactory
USE I8PWrapperFactory
USE LWrapperFactory
USE R4PWrapperFactory
USE R8PWrapperFactory

implicit none
private

    type(WrapperFactoryList_t), save :: TheWrapperFactoryList
    !$OMP THREADPRIVATE(TheWrapperFactoryList)

public :: TheWrapperFactoryList
public :: TheWrapperFactoryList_Init

contains

    subroutine TheWrapperFactoryList_Init()
    !-----------------------------------------------------------------
    !< Set the dimensions of the Value contained in the wrapper
    !-----------------------------------------------------------------
        ! Add some Wrapper Factories to the list
        call TheWrapperFactoryList%Init()
        call TheWrapperFactoryList%AddWrapperFactory(key='I1P',  WrapperFactory=WrapperFactoryI1P)
        call TheWrapperFactoryList%AddWrapperFactory(key='I2P',  WrapperFactory=WrapperFactoryI2P)
        call TheWrapperFactoryList%AddWrapperFactory(key='I4P',  WrapperFactory=WrapperFactoryI4P)
        call TheWrapperFactoryList%AddWrapperFactory(key='I8P',  WrapperFactory=WrapperFactoryI8P)
        call TheWrapperFactoryList%AddWrapperFactory(key='R4P',  WrapperFactory=WrapperFactoryR4P)
        call TheWrapperFactoryList%AddWrapperFactory(key='R8P',  WrapperFactory=WrapperFactoryR8P)
        call TheWrapperFactoryList%AddWrapperFactory(key='L',    WrapperFactory=WrapperFactoryL)
        call TheWrapperFactoryList%AddWrapperFactory(key='DLCA', WrapperFactory=WrapperFactoryDLCA)
    end subroutine TheWrapperFactoryList_Init

end module WrapperFactoryListSingleton
