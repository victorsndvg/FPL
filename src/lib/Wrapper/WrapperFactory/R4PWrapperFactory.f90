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

module R4PWrapperFactory

USE WrapperFactory
USE PENF, only: I1P, R4P
USE DimensionsWrapper
USE DimensionsWrapper0D_R4P
USE DimensionsWrapper1D_R4P
USE DimensionsWrapper2D_R4P
USE DimensionsWrapper3D_R4P
USE DimensionsWrapper4D_R4P
USE DimensionsWrapper5D_R4P
USE DimensionsWrapper6D_R4P
USE DimensionsWrapper7D_R4P

implicit none
private

    type, extends(WrapperFactory_t) :: R4PWrapperFactory_t
    private

    contains
        procedure         :: Wrap0D      => R4PWrapperFactory_Wrap0D
        procedure         :: Wrap1D      => R4PWrapperFactory_Wrap1D
        procedure         :: Wrap2D      => R4PWrapperFactory_Wrap2D
        procedure         :: Wrap3D      => R4PWrapperFactory_Wrap3D
        procedure         :: Wrap4D      => R4PWrapperFactory_Wrap4D
        procedure         :: Wrap5D      => R4PWrapperFactory_Wrap5D
        procedure         :: Wrap6D      => R4PWrapperFactory_Wrap6D
        procedure         :: Wrap7D      => R4PWrapperFactory_Wrap7D
        procedure         :: UnWrap0D    => R4PWrapperFactory_UnWrap0D
        procedure         :: UnWrap1D    => R4PWrapperFactory_UnWrap1D
        procedure         :: UnWrap2D    => R4PWrapperFactory_UnWrap2D
        procedure         :: UnWrap3D    => R4PWrapperFactory_UnWrap3D
        procedure         :: UnWrap4D    => R4PWrapperFactory_UnWrap4D
        procedure         :: UnWrap5D    => R4PWrapperFactory_UnWrap5D
        procedure         :: UnWrap6D    => R4PWrapperFactory_UnWrap6D
        procedure         :: UnWrap7D    => R4PWrapperFactory_UnWrap7D
        procedure, public :: hasSameType => R4PWrapperFactory_hasSameType
    end type

    type(R4PWrapperFactory_t), save, public :: WrapperFactoryR4P
    !$OMP THREADPRIVATE(WrapperFactoryR4P)

contains

    function R4PWrapperFactory_hasSameType(this, Value) result(hasSameType)
    !-----------------------------------------------------------------
    !< Check if Value type agrees with wrapper type
    !-----------------------------------------------------------------
        class(R4PWrapperFactory_t), intent(IN) :: this
        class(*),                   intent(IN) :: Value
        logical                                :: hasSameType
    !-----------------------------------------------------------------
        hasSameType = .false.
        select type(Value)
            type is (real(R4P))
                hasSameType = .true.
        end select
    end function R4PWrapperFactory_hasSameType


    function R4PWrapperFactory_Wrap0D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create R4P 0D Wrapper
    !-----------------------------------------------------------------
        class(R4PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value)) then
            allocate(DimensionsWrapper0D_R4P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=0_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper0D_R4P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function R4PWrapperFactory_Wrap0D


    function R4PWrapperFactory_Wrap1D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create R4P 1D Wrapper
    !-----------------------------------------------------------------
        class(R4PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1))) then
            allocate(DimensionsWrapper1D_R4P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=1_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper1D_R4P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function R4PWrapperFactory_Wrap1D


    function R4PWrapperFactory_Wrap2D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create R4P 2D Wrapper
    !-----------------------------------------------------------------
        class(R4PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1))) then
            allocate(DimensionsWrapper2D_R4P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=2_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper2D_R4P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function R4PWrapperFactory_Wrap2D


    function R4PWrapperFactory_Wrap3D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create R4P 3D Wrapper
    !-----------------------------------------------------------------
        class(R4PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1,1))) then
            allocate(DimensionsWrapper3D_R4P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=3_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper3D_R4P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function R4PWrapperFactory_Wrap3D


    function R4PWrapperFactory_Wrap4D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create R4P 4D Wrapper
    !-----------------------------------------------------------------
        class(R4PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1,1,1))) then
            allocate(DimensionsWrapper4D_R4P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=4_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper4D_R4P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function R4PWrapperFactory_Wrap4D


    function R4PWrapperFactory_Wrap5D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create R4P 5D Wrapper
    !-----------------------------------------------------------------
        class(R4PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:,1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1,1,1,1))) then
            allocate(DimensionsWrapper5D_R4P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=5_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper5D_R4P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function R4PWrapperFactory_Wrap5D


    function R4PWrapperFactory_Wrap6D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create R4P 6D Wrapper
    !-----------------------------------------------------------------
        class(R4PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:,1:,1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1,1,1,1,1))) then
            allocate(DimensionsWrapper6D_R4P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=6_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper6D_R4P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function R4PWrapperFactory_Wrap6D


    function R4PWrapperFactory_Wrap7D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create R4P 7D Wrapper
    !-----------------------------------------------------------------
        class(R4PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:,1:,1:,1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1,1,1,1,1,1))) then
            allocate(DimensionsWrapper7D_R4P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=7_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper7D_R4P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function R4PWrapperFactory_Wrap7D


    subroutine R4PWrapperFactory_UnWrap0D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the R4P 0D Wrapped Value
    !-----------------------------------------------------------------
        class(R4PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper0D_R4P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine R4PWrapperFactory_UnWrap1D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the R4P 1D Wrapped Value
    !-----------------------------------------------------------------
        class(R4PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper1D_R4P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine R4PWrapperFactory_UnWrap2D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the R4P 2D Wrapped Value
    !-----------------------------------------------------------------
        class(R4PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper2D_R4P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine R4PWrapperFactory_UnWrap3D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the R4P 3D Wrapped Value
    !-----------------------------------------------------------------
        class(R4PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper3D_R4P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine R4PWrapperFactory_UnWrap4D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the R4P 4D Wrapped Value
    !-----------------------------------------------------------------
        class(R4PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper4D_R4P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine R4PWrapperFactory_UnWrap5D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the R4P 5D Wrapped Value
    !-----------------------------------------------------------------
        class(R4PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper5D_R4P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine R4PWrapperFactory_UnWrap6D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the R4P 6D Wrapped Value
    !-----------------------------------------------------------------
        class(R4PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper6D_R4P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine R4PWrapperFactory_UnWrap7D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the R4P 7D Wrapped Value
    !-----------------------------------------------------------------
        class(R4PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:,:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper7D_R4P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine

end module R4PWrapperFactory
