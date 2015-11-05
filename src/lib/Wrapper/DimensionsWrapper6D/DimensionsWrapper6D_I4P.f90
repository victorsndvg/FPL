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

module DimensionsWrapper6D_I4P

USE DimensionsWrapper6D
USE IR_Precision, only: I4P, str
USE ErrorMessages

implicit none
private

    type, extends(DimensionsWrapper6D_t) :: DimensionsWrapper6D_I4P_t
        integer(I4P), allocatable :: Value(:,:,:,:,:,:)
    contains
    private
        procedure, public :: Set            => DimensionsWrapper6D_I4P_Set
        procedure, public :: Get            => DimensionsWrapper6D_I4P_Get
        procedure, public :: GetShape       => DimensionsWrapper6D_I4P_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper6D_I4P_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper6D_I4P_GetPolymorphic
        procedure, public :: isOfDataType   => DimensionsWrapper6D_I4P_isOfDataType
        procedure, public :: Print          => DimensionsWrapper6D_I4P_Print
        procedure, public :: Free           => DimensionsWrapper6D_I4P_Free
        final             ::                   DimensionsWrapper6D_I4P_Final
    end type           

public :: DimensionsWrapper6D_I4P_t

contains


    subroutine DimensionsWrapper6D_I4P_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper6D
    !-----------------------------------------------------------------
        type(DimensionsWrapper6D_I4P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper6D_I4P_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set I4P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_I4P_t), intent(INOUT) :: this
        class(*),                         intent(IN)    :: Value(:,:,:,:,:,:)
        integer                                         :: err
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I4P))
                allocate(this%Value(size(Value,dim=1),  &
                                    size(Value,dim=2),  &
                                    size(Value,dim=3),  &
                                    size(Value,dim=4),  &
                                    size(Value,dim=5),  &
                                    size(Value,dim=6)), &
                                    stat=err)
                this%Value = Value
                if(err/=0) &
                    call msg%Error( txt='Setting Value: Allocation error ('//&
                                    str(no_sign=.true.,n=err)//')', &
                                    file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn( txt='Setting value: Expected data type (I4P)', &
                               file=__FILE__, line=__LINE__ )

        end select
    end subroutine


    subroutine DimensionsWrapper6D_I4P_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get I4P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_I4P_t), intent(IN)  :: this
        class(*),                         intent(OUT) :: Value(:,:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I4P))
                if(all(this%GetShape() == shape(Value))) then
                    Value = this%Value
                else
                    call msg%Warn(txt='Getting value: Expected shape ('//    &
                                  str(no_sign=.true.,n=this%GetShape())//')',&
                                  file=__FILE__, line=__LINE__ )
                endif
            class Default
                call msg%Warn(txt='Getting value: Expected data type (I4P)',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    function DimensionsWrapper6D_I4P_GetShape(this) result(ValueShape) 
    !-----------------------------------------------------------------
    !< Get Wrapper Value Shape
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_I4P_t), intent(IN)  :: this
        integer(I4P), allocatable                     :: ValueShape(:)
    !-----------------------------------------------------------------
        allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper6D_I4P_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic pointer to Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_I4P_t), target, intent(IN)  :: this
        class(*), pointer                                     :: Value(:,:,:,:,:,:)
    !-----------------------------------------------------------------
        Value => this%Value
    end function


    subroutine DimensionsWrapper6D_I4P_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_I4P_t), intent(IN)  :: this
        class(*), allocatable,            intent(OUT) :: Value(:,:,:,:,:,:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1),  &
                       size(this%Value,dim=2),  &
                       size(this%Value,dim=3),  &
                       size(this%Value,dim=4),  &
                       size(this%Value,dim=5),  &
                       size(this%Value,dim=6)), &
                       source=this%Value)
    end subroutine


    subroutine DimensionsWrapper6D_I4P_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper6D
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_I4P_t), intent(INOUT) :: this
        integer                                         :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    function DimensionsWrapper6D_I4P_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_I4P_t), intent(IN) :: this           !< Dimensions wrapper 6D
        class(*),                         intent(IN) :: Mold           !< Mold for data type comparison
        logical                                      :: isOfDataType   !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (integer(I4P))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper6D_I4P_isOfDataType


    subroutine DimensionsWrapper6D_I4P_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_I4P_t), intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A,$)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = I4P'//&
                        ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                        ', Value = '
        write(unit=unit,fmt=*,iostat=iostatd,iomsg=iomsgd) str(no_sign=.true., n=this%Value)
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper6D_I4P_Print

end module DimensionsWrapper6D_I4P
