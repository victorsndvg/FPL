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

module DimensionsWrapper3D

USE DimensionsWrapper

implicit none
private

    type, extends(DimensionsWrapper_t), abstract :: DimensionsWrapper3D_t
    private
    contains
        procedure(DimensionsWrapper3D_Set),            deferred :: Set
        procedure(DimensionsWrapper3D_Get),            deferred :: Get
        procedure(DimensionsWrapper3D_GetPointer),     deferred :: GetPointer
    end type

    abstract interface
        subroutine DimensionsWrapper3D_Set(this, Value)
            import DimensionsWrapper3D_t
            class(DimensionsWrapper3D_t), intent(INOUT) :: this
            class(*),                     intent(IN)    :: Value(:,:,:)
        end subroutine

        subroutine DimensionsWrapper3D_Get(this, Value)
            import DimensionsWrapper3D_t
            class(DimensionsWrapper3D_t), intent(IN)  :: this
            class(*),                     intent(OUT) :: Value(:,:,:)
        end subroutine

        function DimensionsWrapper3D_GetPointer(this) result(Value)
            import DimensionsWrapper3D_t
            class(DimensionsWrapper3D_t), target, intent(IN)  :: this
            class(*), pointer                                 :: Value(:,:,:)
        end function

        subroutine DimensionsWrapper3D_GetPolymorphic(this, Value)
            import DimensionsWrapper3D_t
            class(DimensionsWrapper3D_t), intent(IN)  :: this
            class(*), allocatable,        intent(OUT) :: Value(:,:,:)
        end subroutine
    end interface

public :: DimensionsWrapper3D_t

end module DimensionsWrapper3D
