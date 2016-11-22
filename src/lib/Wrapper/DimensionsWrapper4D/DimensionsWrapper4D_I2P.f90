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

module DimensionsWrapper4D_I2P

USE DimensionsWrapper4D
USE PENF, only: I2P, I4P, str, byte_size
USE ErrorMessages

implicit none
private

    type, extends(DimensionsWrapper4D_t) :: DimensionsWrapper4D_I2P_t
        integer(I2P), allocatable :: Value(:,:,:,:)
    contains
    private
        procedure, public :: Set            => DimensionsWrapper4D_I2P_Set
        procedure, public :: Get            => DimensionsWrapper4D_I2P_Get
        procedure, public :: GetShape       => DimensionsWrapper4D_I2P_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper4D_I2P_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper4D_I2P_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper4D_I2P_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper4D_I2P_isOfDataType
        procedure, public :: toString       => DimensionsWrapper4D_I2P_toString
        procedure, public :: Print          => DimensionsWrapper4D_I2P_Print
        procedure, public :: Free           => DimensionsWrapper4D_I2P_Free
        final             ::                   DimensionsWrapper4D_I2P_Final
    end type           

public :: DimensionsWrapper4D_I2P_t

contains


    subroutine DimensionsWrapper4D_I2P_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper4D
    !-----------------------------------------------------------------
        type(DimensionsWrapper4D_I2P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper4D_I2P_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set I2P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_I2P_t), intent(INOUT) :: this
        class(*),                         intent(IN)    :: Value(:,:,:,:)
        integer                                         :: err
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I2P))
                allocate(this%Value(size(Value,dim=1),  &
                                    size(Value,dim=2),  &
                                    size(Value,dim=3),  &
                                    size(Value,dim=4)), &
                                    stat=err)
                this%Value = Value
                if(err/=0) &
                    call msg%Error( txt='Setting Value: Allocation error ('//&
                                    str(no_sign=.true.,n=err)//')', &
                                    file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn( txt='Setting value: Expected data type (I2P)', &
                               file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    subroutine DimensionsWrapper4D_I2P_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get I2P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_I2P_t), intent(IN)  :: this
        class(*),                         intent(OUT) :: Value(:,:,:,:)
        integer(I4P), allocatable                     :: ValueShape(:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I2P))
                call this%GetShape(ValueShape)
                if(all(ValueShape == shape(Value))) then
                    Value = this%Value
                else
                    call msg%Warn(txt='Getting value: Wrong shape ('//&
                                  str(no_sign=.true.,n=ValueShape)//'/='//&
                                  str(no_sign=.true.,n=shape(Value))//')',&
                                  file=__FILE__, line=__LINE__ )
                endif
            class Default
                call msg%Warn(txt='Getting value: Expected data type (I2P)',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    subroutine DimensionsWrapper4D_I2P_GetShape(this, ValueShape)
    !-----------------------------------------------------------------
    !< Get Wrapper Value Shape
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_I2P_t), intent(IN)    :: this
        integer(I4P), allocatable,        intent(INOUT) :: ValueShape(:)
    !-----------------------------------------------------------------
        if(allocated(ValueShape)) deallocate(ValueShape)
		allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value, kind=I4P)
    end subroutine


    function DimensionsWrapper4D_I2P_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic pointer to Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_I2P_t), target, intent(IN)  :: this
        class(*), pointer                                     :: Value(:,:,:,:)
    !-----------------------------------------------------------------
        Value => this%Value
    end function


    subroutine DimensionsWrapper4D_I2P_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_I2P_t), intent(IN)  :: this
        class(*), allocatable,            intent(OUT) :: Value(:,:,:,:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1),  &
                       size(this%Value,dim=2),  &
                       size(this%Value,dim=3),  &
                       size(this%Value,dim=4)), &
                       source=this%Value)
    end subroutine


    subroutine DimensionsWrapper4D_I2P_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper4D
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_I2P_t), intent(INOUT) :: this
        integer                                         :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    function DimensionsWrapper4D_I2P_DataSizeInBytes(this) result(DatasizeInBytes)
    !-----------------------------------------------------------------
    !< Return the data size of the stored value in bytes
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_I2P_t), intent(IN) :: this             !< Dimensions wrapper 4D
        integer(I4P)                                  :: DataSizeInBytes !< Data size in bytes of the stored value
    !-----------------------------------------------------------------
        DataSizeInBytes = byte_size(this%value(1,1,1,1))*size(this%value)
    end function DimensionsWrapper4D_I2P_DataSizeInBytes


    function DimensionsWrapper4D_I2P_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_I2P_t), intent(IN) :: this          !< Dimensions wrapper 4D
        class(*),                         intent(IN) :: Mold          !< Mold for data type comparison
        logical                                      :: isOfDataType  !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (integer(I2P))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper4D_I2P_isOfDataType


    subroutine DimensionsWrapper4D_I2P_toString(this, String, Separator) 
    !-----------------------------------------------------------------
    !< Return the wrapper value as a string
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_I2P_t), intent(IN)    :: this
        character(len=:), allocatable,    intent(INOUT) :: String
        character(len=1), optional,       intent(IN)    :: Separator
        character(len=1)                                :: Sep
        integer(I4P)                                    :: idx2,idx3,idx4
    !-----------------------------------------------------------------
        String = ''
        Sep = ','
        if(allocated(this%Value)) then
            if(present(Separator)) Sep = Separator
            do idx4=1, size(this%Value,4)
                do idx3=1, size(this%Value,3)
                    do idx2=1, size(this%Value,2)
                        String = String // trim(str(n=this%Value(:,idx2,idx3,idx4))) // Sep
                    enddo
                enddo
            enddo
            String = trim(adjustl(String(:len(String)-1)))
        endif
    end subroutine


    subroutine DimensionsWrapper4D_I2P_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_I2P_t), intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        character(len=:), allocatable                 :: strvalue     !< String value
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A,$)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = I2P'//&
                        ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                        ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                        ', Value = '
        call this%toString(strvalue)
        write(unit=unit,fmt=*,iostat=iostatd,iomsg=iomsgd) strvalue

        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper4D_I2P_Print

end module DimensionsWrapper4D_I2P
