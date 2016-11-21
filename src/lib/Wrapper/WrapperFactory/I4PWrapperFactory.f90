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

module I4PWrapperFactory

USE WrapperFactory
USE PENF, only: I1P, I4P
USE DimensionsWrapper
USE DimensionsWrapper0D_I4P
USE DimensionsWrapper1D_I4P
USE DimensionsWrapper2D_I4P
USE DimensionsWrapper3D_I4P
USE DimensionsWrapper4D_I4P
USE DimensionsWrapper5D_I4P
USE DimensionsWrapper6D_I4P
USE DimensionsWrapper7D_I4P

implicit none
private

    type, extends(WrapperFactory_t) :: I4PWrapperFactory_t
    private

    contains
        procedure         :: Wrap0D      => I4PWrapperFactory_Wrap0D
        procedure         :: Wrap1D      => I4PWrapperFactory_Wrap1D
        procedure         :: Wrap2D      => I4PWrapperFactory_Wrap2D
        procedure         :: Wrap3D      => I4PWrapperFactory_Wrap3D
        procedure         :: Wrap4D      => I4PWrapperFactory_Wrap4D
        procedure         :: Wrap5D      => I4PWrapperFactory_Wrap5D
        procedure         :: Wrap6D      => I4PWrapperFactory_Wrap6D
        procedure         :: Wrap7D      => I4PWrapperFactory_Wrap7D
        procedure         :: UnWrap0D    => I4PWrapperFactory_UnWrap0D
        procedure         :: UnWrap1D    => I4PWrapperFactory_UnWrap1D
        procedure         :: UnWrap2D    => I4PWrapperFactory_UnWrap2D
        procedure         :: UnWrap3D    => I4PWrapperFactory_UnWrap3D
        procedure         :: UnWrap4D    => I4PWrapperFactory_UnWrap4D
        procedure         :: UnWrap5D    => I4PWrapperFactory_UnWrap5D
        procedure         :: UnWrap6D    => I4PWrapperFactory_UnWrap6D
        procedure         :: UnWrap7D    => I4PWrapperFactory_UnWrap7D
        procedure, public :: hasSameType => I4PWrapperFactory_hasSameType
    end type

    type(I4PWrapperFactory_t), save, public :: WrapperFactoryI4P
   !$OMP THREADPRIVATE(WrapperFactoryI4P)

contains

    function I4PWrapperFactory_hasSameType(this, Value) result(hasSameType)
    !-----------------------------------------------------------------
    !< Check if Value type agrees with wrapper type
    !-----------------------------------------------------------------
        class(I4PWrapperFactory_t), intent(IN) :: this
        class(*),                   intent(IN) :: Value
        logical                                :: hasSameType
    !-----------------------------------------------------------------
        hasSameType = .false.
        select type(Value)
            type is (integer(I4P))
                hasSameType = .true.
        end select
    end function I4PWrapperFactory_hasSameType


    function I4PWrapperFactory_Wrap0D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create I4P 0D Wrapper
    !-----------------------------------------------------------------
        class(I4PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value)) then
            allocate(DimensionsWrapper0D_I4P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=0_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper0D_I4P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function I4PWrapperFactory_Wrap0D


    function I4PWrapperFactory_Wrap1D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create I4P 1D Wrapper
    !-----------------------------------------------------------------
        class(I4PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1))) then
            allocate(DimensionsWrapper1D_I4P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=1_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper1D_I4P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function I4PWrapperFactory_Wrap1D


    function I4PWrapperFactory_Wrap2D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create I4P 2D Wrapper
    !-----------------------------------------------------------------
        class(I4PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1))) then
            allocate(DimensionsWrapper2D_I4P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=2_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper2D_I4P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function I4PWrapperFactory_Wrap2D


    function I4PWrapperFactory_Wrap3D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create I4P 3D Wrapper
    !-----------------------------------------------------------------
        class(I4PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1,1))) then
            allocate(DimensionsWrapper3D_I4P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=3_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper3D_I4P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function I4PWrapperFactory_Wrap3D


    function I4PWrapperFactory_Wrap4D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create I4P 4D Wrapper
    !-----------------------------------------------------------------
        class(I4PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1,1,1))) then
            allocate(DimensionsWrapper4D_I4P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=4_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper4D_I4P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function I4PWrapperFactory_Wrap4D


    function I4PWrapperFactory_Wrap5D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create I4P 5D Wrapper
    !-----------------------------------------------------------------
        class(I4PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:,1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1,1,1,1))) then
            allocate(DimensionsWrapper5D_I4P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=5_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper5D_I4P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function I4PWrapperFactory_Wrap5D


    function I4PWrapperFactory_Wrap6D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create I4P 6D Wrapper
    !-----------------------------------------------------------------
        class(I4PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:,1:,1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1,1,1,1,1))) then
            allocate(DimensionsWrapper6D_I4P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=6_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper6D_I4P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function I4PWrapperFactory_Wrap6D


    function I4PWrapperFactory_Wrap7D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create I4P 7D Wrapper
    !-----------------------------------------------------------------
        class(I4PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:,1:,1:,1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1,1,1,1,1,1))) then
            allocate(DimensionsWrapper7D_I4P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=7_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper7D_I4P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function I4PWrapperFactory_Wrap7D


    subroutine I4PWrapperFactory_UnWrap0D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the I4P 0D Wrapped Value
    !-----------------------------------------------------------------
        class(I4PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper0D_I4P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine I4PWrapperFactory_UnWrap1D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the I4P 1D Wrapped Value
    !-----------------------------------------------------------------
        class(I4PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper1D_I4P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine I4PWrapperFactory_UnWrap2D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the I4P 2D Wrapped Value
    !-----------------------------------------------------------------
        class(I4PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper2D_I4P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine I4PWrapperFactory_UnWrap3D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the I4P 3D Wrapped Value
    !-----------------------------------------------------------------
        class(I4PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper3D_I4P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine I4PWrapperFactory_UnWrap4D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the I4P 4D Wrapped Value
    !-----------------------------------------------------------------
        class(I4PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper4D_I4P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine I4PWrapperFactory_UnWrap5D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the I4P 5D Wrapped Value
    !-----------------------------------------------------------------
        class(I4PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper5D_I4P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine I4PWrapperFactory_UnWrap6D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the I4P 6D Wrapped Value
    !-----------------------------------------------------------------
        class(I4PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper6D_I4P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine I4PWrapperFactory_UnWrap7D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the I4P 7D Wrapped Value
    !-----------------------------------------------------------------
        class(I4PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:,:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper7D_I4P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine

end module I4PWrapperFactory
