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

module DimensionsWrapper0D_I1P

USE DimensionsWrapper0D
USE PENF, only: I1P, I4P, str, byte_size
USE ErrorMessages

implicit none
private

    type, extends(DimensionsWrapper0D_t) :: DimensionsWrapper0D_I1P_t
        integer(I1P), allocatable :: Value
    contains
    private
        procedure, public :: Set            => DimensionsWrapper0D_I1P_Set
        procedure, public :: Get            => DimensionsWrapper0D_I1P_Get
        procedure, public :: GetShape       => DimensionsWrapper0D_I1P_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper0D_I1P_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper0D_I1P_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper0D_I1P_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper0D_I1P_isOfDataType
        procedure, public :: toString       => DimensionsWrapper0D_I1P_toString
        procedure, public :: Free           => DimensionsWrapper0D_I1P_Free
        procedure, public :: Print          => DimensionsWrapper0D_I1P_Print
        final             ::                   DimensionsWrapper0D_I1P_Final
    end type           

public :: DimensionsWrapper0D_I1P_t

contains


    subroutine DimensionsWrapper0D_I1P_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper0D
    !-----------------------------------------------------------------
        type(DimensionsWrapper0D_I1P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper0D_I1P_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set I1P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_I1P_t), intent(INOUT) :: this
        class(*),                         intent(IN)    :: Value
        integer                                         :: err
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I1P))
                allocate(this%Value, stat=err)
                this%Value = Value
                if(err/=0) &
                    call msg%Error(txt='Setting Value: Allocation error ('//&
                                   str(no_sign=.true.,n=err)//')', &
                                   file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn(txt='Setting value: Expected data type (I1P)',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    subroutine DimensionsWrapper0D_I1P_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get I1P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_I1P_t), intent(IN)  :: this
        class(*),                         intent(OUT) :: Value
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I1P))
                Value = this%Value
            class Default
                call msg%Warn(txt='Getting value: Expected data type (I1P)',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    subroutine DimensionsWrapper0D_I1P_GetShape(this, ValueShape)
    !-----------------------------------------------------------------
    !< Return the shape of the Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_I1P_t), intent(IN)    :: this
        integer(I4P), allocatable,        intent(INOUT) :: ValueShape(:)
    !-----------------------------------------------------------------
        if(allocated(ValueShape)) deallocate(ValueShape)
		allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value, kind=I4P)
    end subroutine


    function DimensionsWrapper0D_I1P_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic pointer to Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_I1P_t), target, intent(IN)  :: this
        class(*), pointer                                     :: Value
    !-----------------------------------------------------------------
        Value => this%Value
    end function


    subroutine DimensionsWrapper0D_I1P_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_I1P_t), intent(IN)  :: this
        class(*), allocatable,            intent(OUT) :: Value
    !-----------------------------------------------------------------
        allocate(Value, source = this%Value)
    end subroutine


    subroutine DimensionsWrapper0D_I1P_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper0D
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_I1P_t), intent(INOUT) :: this
        integer                                         :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    function DimensionsWrapper0D_I1P_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the size in bytes of the stored value
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_I1P_t), intent(IN) :: this            !< Dimensions wrapper 0D
        integer(I4P)                                 :: DataSizeInBytes !< Size in bytes of the stored value
    !-----------------------------------------------------------------
        DataSizeInBytes = byte_size(this%Value)
    end function DimensionsWrapper0D_I1P_DataSizeInBytes


    function DimensionsWrapper0D_I1P_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_I1P_t), intent(IN) :: this          !< Dimensions wrapper 0D
        class(*),                         intent(IN) :: Mold          !< Mold for data type comparison
        logical                                      :: isOfDataType  !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (integer(I1P))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper0D_I1P_isOfDataType


    function DimensionsWrapper0D_I1P_toString(this, Separator) result(String) 
    !-----------------------------------------------------------------
    !< Return the wrapper value as a string
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_I1P_t), intent(IN)  :: this
        character(len=1), optional,       intent(IN)  :: Separator
        character(len=:), allocatable                 :: String
    !-----------------------------------------------------------------
        String = ''
        if(allocated(this%Value)) String = trim(str(n=this%Value))
    end function


    subroutine DimensionsWrapper0D_I1P_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_I1P_t), intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = I1P'//&
                            ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                            ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                            ', Value = '//this%toString()
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper0D_I1P_Print


end module DimensionsWrapper0D_I1P
