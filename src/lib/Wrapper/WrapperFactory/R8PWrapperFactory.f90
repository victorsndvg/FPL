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

module R8PWrapperFactory

USE WrapperFactory
USE PENF, only: I1P, R8P
USE DimensionsWrapper
USE DimensionsWrapper0D_R8P
USE DimensionsWrapper1D_R8P
USE DimensionsWrapper2D_R8P
USE DimensionsWrapper3D_R8P
USE DimensionsWrapper4D_R8P
USE DimensionsWrapper5D_R8P
USE DimensionsWrapper6D_R8P
USE DimensionsWrapper7D_R8P

implicit none
private

    type, extends(WrapperFactory_t) :: R8PWrapperFactory_t
    private

    contains
        procedure         :: Wrap0D      => R8PWrapperFactory_Wrap0D
        procedure         :: Wrap1D      => R8PWrapperFactory_Wrap1D
        procedure         :: Wrap2D      => R8PWrapperFactory_Wrap2D
        procedure         :: Wrap3D      => R8PWrapperFactory_Wrap3D
        procedure         :: Wrap4D      => R8PWrapperFactory_Wrap4D
        procedure         :: Wrap5D      => R8PWrapperFactory_Wrap5D
        procedure         :: Wrap6D      => R8PWrapperFactory_Wrap6D
        procedure         :: Wrap7D      => R8PWrapperFactory_Wrap7D
        procedure         :: UnWrap0D    => R8PWrapperFactory_UnWrap0D
        procedure         :: UnWrap1D    => R8PWrapperFactory_UnWrap1D
        procedure         :: UnWrap2D    => R8PWrapperFactory_UnWrap2D
        procedure         :: UnWrap3D    => R8PWrapperFactory_UnWrap3D
        procedure         :: UnWrap4D    => R8PWrapperFactory_UnWrap4D
        procedure         :: UnWrap5D    => R8PWrapperFactory_UnWrap5D
        procedure         :: UnWrap6D    => R8PWrapperFactory_UnWrap6D
        procedure         :: UnWrap7D    => R8PWrapperFactory_UnWrap7D
        procedure, public :: hasSameType => R8PWrapperFactory_hasSameType
    end type

    type(R8PWrapperFactory_t), save, public :: WrapperFactoryR8P
   !$OMP THREADPRIVATE(WrapperFactoryR8P)

contains

    function R8PWrapperFactory_hasSameType(this, Value) result(hasSameType)
    !-----------------------------------------------------------------
    !< Check if Value type agrees with wrapper type
    !-----------------------------------------------------------------
        class(R8PWrapperFactory_t), intent(IN) :: this
        class(*),                   intent(IN) :: Value
        logical                                :: hasSameType
    !-----------------------------------------------------------------
        hasSameType = .false.
        select type(Value)
            type is (real(R8P))
                hasSameType = .true.
        end select
    end function R8PWrapperFactory_hasSameType


    function R8PWrapperFactory_Wrap0D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create R8P 0D Wrapper
    !-----------------------------------------------------------------
        class(R8PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value)) then
            allocate(DimensionsWrapper0D_R8P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=0_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper0D_R8P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function R8PWrapperFactory_Wrap0D


    function R8PWrapperFactory_Wrap1D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create R8P 1D Wrapper
    !-----------------------------------------------------------------
        class(R8PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1))) then
            allocate(DimensionsWrapper1D_R8P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=1_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper1D_R8P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function R8PWrapperFactory_Wrap1D


    function R8PWrapperFactory_Wrap2D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create R8P 2D Wrapper
    !-----------------------------------------------------------------
        class(R8PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1))) then
            allocate(DimensionsWrapper2D_R8P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=2_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper2D_R8P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function R8PWrapperFactory_Wrap2D


    function R8PWrapperFactory_Wrap3D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create R8P 3D Wrapper
    !-----------------------------------------------------------------
        class(R8PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1,1))) then
            allocate(DimensionsWrapper3D_R8P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=3_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper3D_R8P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function R8PWrapperFactory_Wrap3D


    function R8PWrapperFactory_Wrap4D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create R8P 4D Wrapper
    !-----------------------------------------------------------------
        class(R8PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1,1,1))) then
            allocate(DimensionsWrapper4D_R8P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=4_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper4D_R8P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function R8PWrapperFactory_Wrap4D


    function R8PWrapperFactory_Wrap5D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create R8P 5D Wrapper
    !-----------------------------------------------------------------
        class(R8PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:,1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1,1,1,1))) then
            allocate(DimensionsWrapper5D_R8P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=5_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper5D_R8P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function R8PWrapperFactory_Wrap5D


    function R8PWrapperFactory_Wrap6D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create R8P 6D Wrapper
    !-----------------------------------------------------------------
        class(R8PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:,1:,1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1,1,1,1,1))) then
            allocate(DimensionsWrapper6D_R8P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=6_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper6D_R8P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function R8PWrapperFactory_Wrap6D


    function R8PWrapperFactory_Wrap7D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create R8P 7D Wrapper
    !-----------------------------------------------------------------
        class(R8PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:,1:,1:,1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1,1,1,1,1,1))) then
            allocate(DimensionsWrapper7D_R8P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=7_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper7D_R8P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function R8PWrapperFactory_Wrap7D


    subroutine R8PWrapperFactory_UnWrap0D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the R8P 0D Wrapped Value
    !-----------------------------------------------------------------
        class(R8PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper0D_R8P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine R8PWrapperFactory_UnWrap1D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the R8P 1D Wrapped Value
    !-----------------------------------------------------------------
        class(R8PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper1D_R8P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine R8PWrapperFactory_UnWrap2D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the R8P 2D Wrapped Value
    !-----------------------------------------------------------------
        class(R8PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper2D_R8P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine R8PWrapperFactory_UnWrap3D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the R8P 3D Wrapped Value
    !-----------------------------------------------------------------
        class(R8PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper3D_R8P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine R8PWrapperFactory_UnWrap4D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the R8P 4D Wrapped Value
    !-----------------------------------------------------------------
        class(R8PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper4D_R8P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine R8PWrapperFactory_UnWrap5D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the R8P 5D Wrapped Value
    !-----------------------------------------------------------------
        class(R8PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper5D_R8P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine R8PWrapperFactory_UnWrap6D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the R8P 6D Wrapped Value
    !-----------------------------------------------------------------
        class(R8PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper6D_R8P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine R8PWrapperFactory_UnWrap7D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the R8P 7D Wrapped Value
    !-----------------------------------------------------------------
        class(R8PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:,:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper7D_R8P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine

end module R8PWrapperFactory
