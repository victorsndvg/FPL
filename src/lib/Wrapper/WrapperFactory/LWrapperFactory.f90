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

module LWrapperFactory

USE PENF, only: I1P
USE WrapperFactory
USE DimensionsWrapper
USE DimensionsWrapper0D_L
USE DimensionsWrapper1D_L
USE DimensionsWrapper2D_L
USE DimensionsWrapper3D_L
USE DimensionsWrapper4D_L
USE DimensionsWrapper5D_L
USE DimensionsWrapper6D_L
USE DimensionsWrapper7D_L

implicit none
private

    type, extends(WrapperFactory_t) :: LWrapperFactory_t
    private

    contains
        procedure         :: Wrap0D      => LWrapperFactory_Wrap0D
        procedure         :: Wrap1D      => LWrapperFactory_Wrap1D
        procedure         :: Wrap2D      => LWrapperFactory_Wrap2D
        procedure         :: Wrap3D      => LWrapperFactory_Wrap3D
        procedure         :: Wrap4D      => LWrapperFactory_Wrap4D
        procedure         :: Wrap5D      => LWrapperFactory_Wrap5D
        procedure         :: Wrap6D      => LWrapperFactory_Wrap6D
        procedure         :: Wrap7D      => LWrapperFactory_Wrap7D
        procedure         :: UnWrap0D    => LWrapperFactory_UnWrap0D
        procedure         :: UnWrap1D    => LWrapperFactory_UnWrap1D
        procedure         :: UnWrap2D    => LWrapperFactory_UnWrap2D
        procedure         :: UnWrap3D    => LWrapperFactory_UnWrap3D
        procedure         :: UnWrap4D    => LWrapperFactory_UnWrap4D
        procedure         :: UnWrap5D    => LWrapperFactory_UnWrap5D
        procedure         :: UnWrap6D    => LWrapperFactory_UnWrap6D
        procedure         :: UnWrap7D    => LWrapperFactory_UnWrap7D
        procedure, public :: hasSameType => LWrapperFactory_hasSameType
    end type

    type(LWrapperFactory_t), save, public :: WrapperFactoryL
    !$OMP THREADPRIVATE(WrapperFactoryL)

contains

    function LWrapperFactory_hasSameType(this, Value) result(hasSameType)
    !-----------------------------------------------------------------
    !< Check if Value type agrees with wrapper type
    !-----------------------------------------------------------------
        class(LWrapperFactory_t),   intent(IN) :: this
        class(*),                   intent(IN) :: Value
        logical                                :: hasSameType
    !-----------------------------------------------------------------
        hasSameType = .false.
        select type(Value)
            type is (logical)
                hasSameType = .true.
        end select
    end function LWrapperFactory_hasSameType


    function LWrapperFactory_Wrap0D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create L 0D Wrapper
    !-----------------------------------------------------------------
        class(LWrapperFactory_t),                intent(IN)    :: this
        class(*),                                intent(IN)    :: Value
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value)) then
            allocate(DimensionsWrapper0D_L_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=0_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper0D_L_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function LWrapperFactory_Wrap0D


    function LWrapperFactory_Wrap1D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create L 1D Wrapper
    !-----------------------------------------------------------------
        class(LWrapperFactory_t),                intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1))) then
            allocate(DimensionsWrapper1D_L_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=1_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper1D_L_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function LWrapperFactory_Wrap1D


    function LWrapperFactory_Wrap2D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create L 2D Wrapper
    !-----------------------------------------------------------------
        class(LWrapperFactory_t),                intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1))) then
            allocate(DimensionsWrapper2D_L_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=2_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper2D_L_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function LWrapperFactory_Wrap2D


    function LWrapperFactory_Wrap3D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create L 3D Wrapper
    !-----------------------------------------------------------------
        class(LWrapperFactory_t),                intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1,1))) then
            allocate(DimensionsWrapper3D_L_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=3_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper3D_L_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function LWrapperFactory_Wrap3D


    function LWrapperFactory_Wrap4D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create L 4D Wrapper
    !-----------------------------------------------------------------
        class(LWrapperFactory_t),                intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1,1,1))) then
            allocate(DimensionsWrapper4D_L_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=4_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper4D_L_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function LWrapperFactory_Wrap4D


    function LWrapperFactory_Wrap5D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create L 5D Wrapper
    !-----------------------------------------------------------------
        class(LWrapperFactory_t),                intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:,1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1,1,1,1))) then
            allocate(DimensionsWrapper5D_L_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=5_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper5D_L_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function LWrapperFactory_Wrap5D


    function LWrapperFactory_Wrap6D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create L 6D Wrapper
    !-----------------------------------------------------------------
        class(LWrapperFactory_t),                intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:,1:,1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1,1,1,1,1))) then
            allocate(DimensionsWrapper6D_L_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=6_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper6D_L_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function LWrapperFactory_Wrap6D


    function LWrapperFactory_Wrap7D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create L 7D Wrapper
    !-----------------------------------------------------------------
        class(LWrapperFactory_t),                intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:,1:,1:,1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1,1,1,1,1,1))) then
            allocate(DimensionsWrapper7D_L_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=7_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper7D_L_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function LWrapperFactory_Wrap7D


    subroutine LWrapperFactory_UnWrap0D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the L 0D Wrapped Value
    !-----------------------------------------------------------------
        class(LWrapperFactory_t),                intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper0D_L_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine LWrapperFactory_UnWrap1D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the L 1D Wrapped Value
    !-----------------------------------------------------------------
        class(LWrapperFactory_t),                intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper1D_L_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine LWrapperFactory_UnWrap2D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the L 2D Wrapped Value
    !-----------------------------------------------------------------
        class(LWrapperFactory_t),                intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper2D_L_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine LWrapperFactory_UnWrap3D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the L 3D Wrapped Value
    !-----------------------------------------------------------------
        class(LWrapperFactory_t),                intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper3D_L_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine LWrapperFactory_UnWrap4D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the L 4D Wrapped Value
    !-----------------------------------------------------------------
        class(LWrapperFactory_t),                intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper4D_L_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine LWrapperFactory_UnWrap5D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the L 5D Wrapped Value
    !-----------------------------------------------------------------
        class(LWrapperFactory_t),                intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper5D_L_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine LWrapperFactory_UnWrap6D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the L 6D Wrapped Value
    !-----------------------------------------------------------------
        class(LWrapperFactory_t),                intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper6D_L_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine LWrapperFactory_UnWrap7D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the L 7D Wrapped Value
    !-----------------------------------------------------------------
        class(LWrapperFactory_t),                intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:,:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper7D_L_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine

end module LWrapperFactory
