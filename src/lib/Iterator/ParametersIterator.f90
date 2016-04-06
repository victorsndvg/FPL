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

module ParametersIterator

USE iso_fortran_env, only: OUTPUT_UNIT
USE ParameterEntry
USE ParameterRootEntry
USE ListIterator
USE DimensionsWrapper
USE DimensionsWrapper0D
USE DimensionsWrapper1D
USE DimensionsWrapper2D
USE DimensionsWrapper3D
USE DimensionsWrapper4D
USE DimensionsWrapper5D
USE DimensionsWrapper6D
USE DimensionsWrapper7D
USE ErrorMessages
USE IR_Precision, only: I4P, str

implicit none
private

    type :: ParametersIterator_t
    private
        type(ParameterRootEntry_t), pointer :: DataBase(:)  => NULL()
        type(ListIterator_t),       pointer :: ListIterator => NULL()
        integer(I4P)                        :: Index       = 0
        integer(I4P)                        :: UpperBound  = 0
    contains
    private
        procedure,         non_overridable ::                             ParametersIterator_Get0D
        procedure,         non_overridable ::                             ParametersIterator_Get1D
        procedure,         non_overridable ::                             ParametersIterator_Get2D
        procedure,         non_overridable ::                             ParametersIterator_Get3D
        procedure,         non_overridable ::                             ParametersIterator_Get4D
        procedure,         non_overridable ::                             ParametersIterator_Get5D
        procedure,         non_overridable ::                             ParametersIterator_Get6D
        procedure,         non_overridable ::                             ParametersIterator_Get7D
        procedure,         non_overridable ::                             ParametersIterator_isOfDataType0D
        procedure,         non_overridable ::                             ParametersIterator_isOfDataType1D
        procedure,         non_overridable ::                             ParametersIterator_isOfDataType2D
        procedure,         non_overridable ::                             ParametersIterator_isOfDataType3D
        procedure,         non_overridable ::                             ParametersIterator_isOfDataType4D
        procedure,         non_overridable ::                             ParametersIterator_isOfDataType5D
        procedure,         non_overridable ::                             ParametersIterator_isOfDataType6D
        procedure,         non_overridable ::                             ParametersIterator_isOfDataType7D
        procedure,         non_overridable :: NextNotEmptyListIterator => ParametersIterator_NextNotEmptyListIterator
        procedure, public, non_overridable :: Init                     => ParametersIterator_Init
        procedure, public, non_overridable :: Next                     => ParametersIterator_Next
        procedure, public, non_overridable :: HasFinished              => ParametersIterator_HasFinished
        procedure, public, non_overridable :: GetCurrentEntry          => ParametersIterator_GetCurrentEntry
        procedure, public, non_overridable :: GetCurrentIndex          => ParametersIterator_GetCurrentIndex
        procedure, public, non_overridable :: GetCurrentShape          => ParametersIterator_GetCurrentShape
        procedure, public, non_overridable :: DataSizeInBytes          => ParametersIterator_DataSizeInBytes
        procedure, public, non_overridable :: Print                    => ParametersIterator_Print
        procedure, public, non_overridable :: Free                     => ParametersIterator_Free
        final                              ::                             ParametersIterator_Final
    end type

public :: ParametersIterator_t

contains

    subroutine ParametersIterator_Free(this)
    !-----------------------------------------------------------------
    !< Free the dictionary iterator
    !-----------------------------------------------------------------
        class(ParametersIterator_t), intent(INOUT) :: this            ! Dictionary iterator
    !-----------------------------------------------------------------
        this%Index      = 0
        this%UpperBound = 0
        nullify(this%DataBase)
        if(associated(this%ListIterator)) deallocate(this%ListIterator)
        nullify(this%ListIterator)
    end subroutine ParametersIterator_Free


    subroutine ParametersIterator_Final(this)
    !-----------------------------------------------------------------
    !< Free the dictionary iterator
    !-----------------------------------------------------------------
        type(ParametersIterator_t), intent(INOUT) :: this             ! Dictionary iterator
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine ParametersIterator_Final


    subroutine ParametersIterator_Init(this, DataBase)
    !-----------------------------------------------------------------
    !< Associate the iterator with a dictionary and rewind 
    !< to the first position
    !-----------------------------------------------------------------
        class(ParametersIterator_t),        intent(INOUT) :: this        ! Dictionary iterator
        type(ParameterRootEntry_t), target, intent(IN)    :: DataBase(:) ! Entries database
    !-----------------------------------------------------------------
        call this%Free()
        this%DataBase(0:) => DataBase(:)
        this%Index = 0
        this%UpperBound = size(this%DataBase)
        call this%Next()
    end subroutine ParametersIterator_Init


    subroutine ParametersIterator_NextNotEmptyListIterator(this)
    !-----------------------------------------------------------------
    !< The iterator points to the next associated entry
    !-----------------------------------------------------------------
        class(ParametersIterator_t),     intent(INOUT) :: this        ! Dictionary iterator
    !-----------------------------------------------------------------
        if(associated(this%ListIterator)) then
            call this%ListIterator%Free()
            deallocate(this%ListIterator)
            this%Index = this%Index + 1
        endif
        do while(this%Index < this%UpperBound)
            if(this%DataBase(this%Index)%HasRoot()) then 
                this%ListIterator => this%Database(this%Index)%GetIterator()
                exit
            endif
            this%Index = this%Index + 1
        enddo
    end subroutine ParametersIterator_NextNotEmptyListIterator


    subroutine ParametersIterator_Next(this)
    !-----------------------------------------------------------------
    !< The iterator points to the next associated entry
    !-----------------------------------------------------------------
        class(ParametersIterator_t),     intent(INOUT) :: this        ! Dictionary iterator
    !-----------------------------------------------------------------
        if(.not. this%HasFinished()) then
            if(associated(this%ListIterator)) then
                if(.not. this%ListIterator%HasFinished()) then
                    call this%ListIterator%Next()
                else
                    call this%NextNotEmptyListIterator()
                endif
            else ! First Entry
                call this%NextNotEmptyListIterator()
            endif 
        endif
    end subroutine ParametersIterator_Next


    function ParametersIterator_GetCurrentEntry(this) result(CurrentEntry)
    !-----------------------------------------------------------------
    !< Return the current Entry
    !-----------------------------------------------------------------
        class(ParametersIterator_t),    intent(IN) :: this            ! Dictionary iterator
        type(ParameterEntry_t),  pointer           :: CurrentEntry    ! Current entry
    !-----------------------------------------------------------------
        nullify(CurrentEntry)
        CurrentEntry => this%ListIterator%GetCurrentEntry()
    end function ParametersIterator_GetCurrentEntry


    function ParametersIterator_GetCurrentIndex(this) result(CurrentIndex)
    !-----------------------------------------------------------------
    !< Return the current Index
    !-----------------------------------------------------------------
        class(ParametersIterator_t), intent(IN) :: this               ! Dictionary iterator
        integer(I4P)                            :: CurrentIndex       ! Current index
    !-----------------------------------------------------------------
        CurrentIndex = this%Index
    end function ParametersIterator_GetCurrentIndex


    function ParametersIterator_GetCurrentShape(this, Shape) result(FPLError)
    !-----------------------------------------------------------------
    !< Return an allocatable array with the shape of the contained value
    !-----------------------------------------------------------------
        class(ParametersIterator_t),          intent(IN)    :: this           !< Parameter List
        integer(I4P), allocatable,            intent(INOUT) :: Shape(:)       !< Shape of the stored value
        type(ParameterEntry_t),      pointer                :: CurrentEntry   !< Current entry
        class(*),                    pointer                :: Wrapper        !< Wrapper
        integer(I4P)                                        :: FPLerror       !< Error flag
    !-----------------------------------------------------------------
        FPLerror = FPLSuccess
        nullify(CurrentEntry)
        nullify(Wrapper)
        CurrentEntry => this%GetCurrentEntry()
        if(associated(CurrentEntry)) then
            Wrapper => CurrentEntry%PointToValue()
            if(associated(Wrapper)) then
                select type(Wrapper)
                    class is (DimensionsWrapper_t)
                        call Wrapper%GetShape(Shape)
                    class Default
                        FPLerror = FPLWrapperError
                        call msg%Error(txt='Getting [Key="'//CurrentEntry%GetKey()//'"]: Unknown Wrapper. Shape was not modified.', &
                               file=__FILE__, line=__LINE__ )
                end select
            else
                FPLerror = FPLWrapperFactoryError
                call msg%Error(txt='Getting [Key="'//CurrentEntry%GetKey()//'"]: Not present. Shape was not modified.', &
                               file=__FILE__, line=__LINE__ )
            endif
        else
            FPLerror = FPLParametersIteratorError
            call msg%Error(txt='Current entry not associated. Shape was not modified.', &
                           file=__FILE__, line=__LINE__ )            
        endif
    end function ParametersIterator_GetCurrentShape


    function ParametersIterator_Get0D(this,Value) result(FPLerror)
    !-----------------------------------------------------------------
    !< Return a scalar Value given the Key
    !-----------------------------------------------------------------
        class(ParametersIterator_t),          intent(IN)    :: this           !< Parameter List
        class(*),                             intent(INOUT) :: Value          !< Returned value
        type(ParameterEntry_t),      pointer                :: CurrentEntry   !< Current entry
        class(*),                    pointer                :: Wrapper        !< Wrapper
        integer(I4P)                                        :: FPLerror       !< Error flag
    !-----------------------------------------------------------------
        CurrentEntry => this%GetCurrentEntry()
        if(associated(CurrentEntry)) then
            Wrapper => CurrentEntry%PointToValue()
            if(associated(Wrapper)) then
                select type(Wrapper)
                    class is (DimensionsWrapper0D_t)
                        call Wrapper%Get(Value=Value)
                    class Default
                        FPLerror = FPLWrapperError
                        call msg%Error(txt='Getting [Key="'//CurrentEntry%GetKey()//'"]: Dimensions do not match. Value was not modified.', &
                                   file=__FILE__, line=__LINE__ )
                end select
            else
                FPLerror = FPLWrapperFactoryError
                call msg%Error(txt='Getting [Key="'//CurrentEntry%GetKey()//'"]: Not present. Value was not modified.', &
                               file=__FILE__, line=__LINE__ )
            endif
        else
            FPLerror = FPLParametersIteratorError
            call msg%Error(txt='Current entry not associated. Value was not modified.', &
                           file=__FILE__, line=__LINE__ )            
        endif
    end function ParametersIterator_Get0D


    function ParametersIterator_Get1D(this,Value) result(FPLerror)
    !-----------------------------------------------------------------
    !< Return a scalar Value given the Key
    !-----------------------------------------------------------------
        class(ParametersIterator_t),          intent(IN)    :: this           !< Parameter List
        class(*),                             intent(INOUT) :: Value(:)       !< Returned value
        type(ParameterEntry_t),      pointer                :: CurrentEntry   !< Current entry
        class(*),                    pointer                :: Wrapper        !< Wrapper
        integer(I4P)                                        :: FPLerror       !< Error flag
    !-----------------------------------------------------------------
        CurrentEntry => this%GetCurrentEntry()
        if(associated(CurrentEntry)) then
            Wrapper => CurrentEntry%PointToValue()
            if(associated(Wrapper)) then
                select type(Wrapper)
                    class is (DimensionsWrapper1D_t)
                        call Wrapper%Get(Value=Value)
                    class Default
                        FPLerror = FPLWrapperError
                        call msg%Error(txt='Getting [Key="'//CurrentEntry%GetKey()//'"]: Dimensions do not match. Value was not modified.', &
                                   file=__FILE__, line=__LINE__ )
                end select
            else
                FPLerror = FPLWrapperFactoryError
                call msg%Error(txt='Getting [Key="'//CurrentEntry%GetKey()//'"]: Not present. Value was not modified.', &
                               file=__FILE__, line=__LINE__ )
            endif
        else
            FPLerror = FPLParametersIteratorError
            call msg%Error(txt='Current entry not associated. Value was not modified.', &
                           file=__FILE__, line=__LINE__ )            
        endif
    end function ParametersIterator_Get1D


    function ParametersIterator_Get2D(this,Value) result(FPLerror)
    !-----------------------------------------------------------------
    !< Return a scalar Value given the Key
    !-----------------------------------------------------------------
        class(ParametersIterator_t),          intent(IN)    :: this           !< Parameter List
        class(*),                             intent(INOUT) :: Value(:,:)     !< Returned value
        type(ParameterEntry_t),      pointer                :: CurrentEntry   !< Current entry
        class(*),                    pointer                :: Wrapper        !< Wrapper
        integer(I4P)                                        :: FPLerror       !< Error flag
    !-----------------------------------------------------------------
        CurrentEntry => this%GetCurrentEntry()
        if(associated(CurrentEntry)) then
            Wrapper => CurrentEntry%PointToValue()
            if(associated(Wrapper)) then
                select type(Wrapper)
                    class is (DimensionsWrapper2D_t)
                        call Wrapper%Get(Value=Value)
                    class Default
                        FPLerror = FPLWrapperError
                        call msg%Error(txt='Getting [Key="'//CurrentEntry%GetKey()//'"]: Dimensions do not match. Value was not modified.', &
                                   file=__FILE__, line=__LINE__ )
                end select
            else
                FPLerror = FPLWrapperFactoryError
                call msg%Error(txt='Getting [Key="'//CurrentEntry%GetKey()//'"]: Not present. Value was not modified.', &
                               file=__FILE__, line=__LINE__ )
            endif
        else
            FPLerror = FPLParametersIteratorError
            call msg%Error(txt='Current entry not associated. Value was not modified.', &
                           file=__FILE__, line=__LINE__ )            
        endif
    end function ParametersIterator_Get2D


    function ParametersIterator_Get3D(this,Value) result(FPLerror)
    !-----------------------------------------------------------------
    !< Return a scalar Value given the Key
    !-----------------------------------------------------------------
        class(ParametersIterator_t),          intent(IN)    :: this           !< Parameter List
        class(*),                             intent(INOUT) :: Value(:,:,:)   !< Returned value
        type(ParameterEntry_t),      pointer                :: CurrentEntry   !< Current entry
        class(*),                    pointer                :: Wrapper        !< Wrapper
        integer(I4P)                                        :: FPLerror       !< Error flag
    !-----------------------------------------------------------------
        CurrentEntry => this%GetCurrentEntry()
        if(associated(CurrentEntry)) then
            Wrapper => CurrentEntry%PointToValue()
            if(associated(Wrapper)) then
                select type(Wrapper)
                    class is (DimensionsWrapper3D_t)
                        call Wrapper%Get(Value=Value)
                    class Default
                        FPLerror = FPLWrapperError
                        call msg%Error(txt='Getting [Key="'//CurrentEntry%GetKey()//'"]: Dimensions do not match. Value was not modified.', &
                                   file=__FILE__, line=__LINE__ )
                end select
            else
                FPLerror = FPLWrapperFactoryError
                call msg%Error(txt='Getting [Key="'//CurrentEntry%GetKey()//'"]: Not present. Value was not modified.', &
                               file=__FILE__, line=__LINE__ )
            endif
        else
            FPLerror = FPLParametersIteratorError
            call msg%Error(txt='Current entry not associated. Value was not modified.', &
                           file=__FILE__, line=__LINE__ )            
        endif
    end function ParametersIterator_Get3D


    function ParametersIterator_Get4D(this,Value) result(FPLerror)
    !-----------------------------------------------------------------
    !< Return a scalar Value given the Key
    !-----------------------------------------------------------------
        class(ParametersIterator_t),          intent(IN)    :: this           !< Parameter List
        class(*),                             intent(INOUT) :: Value(:,:,:,:) !< Returned value
        type(ParameterEntry_t),      pointer                :: CurrentEntry   !< Current entry
        class(*),                    pointer                :: Wrapper        !< Wrapper
        integer(I4P)                                        :: FPLerror       !< Error flag
    !-----------------------------------------------------------------
        CurrentEntry => this%GetCurrentEntry()
        if(associated(CurrentEntry)) then
            Wrapper => CurrentEntry%PointToValue()
            if(associated(Wrapper)) then
                select type(Wrapper)
                    class is (DimensionsWrapper4D_t)
                        call Wrapper%Get(Value=Value)
                    class Default
                        FPLerror = FPLWrapperError
                        call msg%Error(txt='Getting [Key="'//CurrentEntry%GetKey()//'"]: Dimensions do not match. Value was not modified.', &
                                   file=__FILE__, line=__LINE__ )
                end select
            else
                FPLerror = FPLWrapperFactoryError
                call msg%Error(txt='Getting [Key="'//CurrentEntry%GetKey()//'"]: Not present. Value was not modified.', &
                               file=__FILE__, line=__LINE__ )
            endif
        else
            FPLerror = FPLParametersIteratorError
            call msg%Error(txt='Current entry not associated. Value was not modified.', &
                           file=__FILE__, line=__LINE__ )            
        endif
    end function ParametersIterator_Get4D


    function ParametersIterator_Get5D(this,Value) result(FPLerror)
    !-----------------------------------------------------------------
    !< Return a scalar Value given the Key
    !-----------------------------------------------------------------
        class(ParametersIterator_t),          intent(IN)    :: this             !< Parameter List
        class(*),                             intent(INOUT) :: Value(:,:,:,:,:) !< Returned value
        type(ParameterEntry_t),      pointer                :: CurrentEntry     !< Current entry
        class(*),                    pointer                :: Wrapper          !< Wrapper
        integer(I4P)                                        :: FPLerror         !< Error flag
    !-----------------------------------------------------------------
        CurrentEntry => this%GetCurrentEntry()
        if(associated(CurrentEntry)) then
            Wrapper => CurrentEntry%PointToValue()
            if(associated(Wrapper)) then
                select type(Wrapper)
                    class is (DimensionsWrapper5D_t)
                        call Wrapper%Get(Value=Value)
                    class Default
                        FPLerror = FPLWrapperError
                        call msg%Error(txt='Getting [Key="'//CurrentEntry%GetKey()//'"]: Dimensions do not match. Value was not modified.', &
                                   file=__FILE__, line=__LINE__ )
                end select
            else
                FPLerror = FPLWrapperFactoryError
                call msg%Error(txt='Getting [Key="'//CurrentEntry%GetKey()//'"]: Not present. Value was not modified.', &
                               file=__FILE__, line=__LINE__ )
            endif
        else
            FPLerror = FPLParametersIteratorError
            call msg%Error(txt='Current entry not associated. Value was not modified.', &
                           file=__FILE__, line=__LINE__ )            
        endif
    end function ParametersIterator_Get5D


    function ParametersIterator_Get6D(this,Value) result(FPLerror)
    !-----------------------------------------------------------------
    !< Return a scalar Value given the Key
    !-----------------------------------------------------------------
        class(ParametersIterator_t),          intent(IN)    :: this               !< Parameter List
        class(*),                             intent(INOUT) :: Value(:,:,:,:,:,:) !< Returned value
        type(ParameterEntry_t),      pointer                :: CurrentEntry       !< Current entry
        class(*),                    pointer                :: Wrapper            !< Wrapper
        integer(I4P)                                        :: FPLerror           !< Error flag
    !-----------------------------------------------------------------
        CurrentEntry => this%GetCurrentEntry()
        if(associated(CurrentEntry)) then
            Wrapper => CurrentEntry%PointToValue()
            if(associated(Wrapper)) then
                select type(Wrapper)
                    class is (DimensionsWrapper6D_t)
                        call Wrapper%Get(Value=Value)
                    class Default
                        FPLerror = FPLWrapperError
                        call msg%Error(txt='Getting [Key="'//CurrentEntry%GetKey()//'"]: Dimensions do not match. Value was not modified.', &
                                   file=__FILE__, line=__LINE__ )
                end select
            else
                FPLerror = FPLWrapperFactoryError
                call msg%Error(txt='Getting [Key="'//CurrentEntry%GetKey()//'"]: Not present. Value was not modified.', &
                               file=__FILE__, line=__LINE__ )
            endif
        else
            FPLerror = FPLParametersIteratorError
            call msg%Error(txt='Current entry not associated. Value was not modified.', &
                           file=__FILE__, line=__LINE__ )            
        endif
    end function ParametersIterator_Get6D


    function ParametersIterator_Get7D(this,Value) result(FPLerror)
    !-----------------------------------------------------------------
    !< Return a scalar Value given the Key
    !-----------------------------------------------------------------
        class(ParametersIterator_t),          intent(IN)    :: this                 !< Parameter List
        class(*),                             intent(INOUT) :: Value(:,:,:,:,:,:,:) !< Returned value
        type(ParameterEntry_t),      pointer                :: CurrentEntry         !< Current entry
        class(*),                    pointer                :: Wrapper              !< Wrapper
        integer(I4P)                                        :: FPLerror             !< Error flag
    !-----------------------------------------------------------------
        CurrentEntry => this%GetCurrentEntry()
        if(associated(CurrentEntry)) then
            Wrapper => CurrentEntry%PointToValue()
            if(associated(Wrapper)) then
                select type(Wrapper)
                    class is (DimensionsWrapper7D_t)
                        call Wrapper%Get(Value=Value)
                    class Default
                        FPLerror = FPLWrapperError
                        call msg%Error(txt='Getting [Key="'//CurrentEntry%GetKey()//'"]: Dimensions do not match. Value was not modified.', &
                                   file=__FILE__, line=__LINE__ )
                end select
            else
                FPLerror = FPLWrapperFactoryError
                call msg%Error(txt='Getting [Key="'//CurrentEntry%GetKey()//'"]: Not present. Value was not modified.', &
                               file=__FILE__, line=__LINE__ )
            endif
        else
            FPLerror = FPLParametersIteratorError
            call msg%Error(txt='Current entry not associated. Value was not modified.', &
                           file=__FILE__, line=__LINE__ )            
        endif
    end function ParametersIterator_Get7D


    function ParametersIterator_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the data size in bytes of the value associated with Key
    !-----------------------------------------------------------------
        class(ParametersIterator_t), intent(IN)    :: this            !< Parameter List
        type(ParameterEntry_t),      pointer       :: CurrentEntry    !< Current entry
        class(*), pointer                          :: Wrapper         !< Wrapper
        integer(I4P)                               :: DataSizeInBytes !< Size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = 0
        CurrentEntry => this%GetCurrentEntry()
        if(associated(CurrentEntry)) then
            Wrapper => CurrentEntry%PointToValue()
            if(associated(Wrapper)) then
                select type (Wrapper)
                    class is (DimensionsWrapper_t)
                        DataSizeInBytes = Wrapper%DataSizeInBytes()
                end select
            endif
        endif
    end function ParametersIterator_DataSizeInBytes


    function ParametersIterator_isOfDataType0D(this, Mold) result(IsOfDataType)
    !-----------------------------------------------------------------
    !< Check if the data type of Mold agrees with the value associated with Key
    !-----------------------------------------------------------------
        class(ParametersIterator_t), intent(IN)    :: this            !< Parameter List
        class(*),                    intent(IN)    :: Mold            !< Mold
        type(ParameterEntry_t),      pointer       :: CurrentEntry    !< Current entry
        class(*), pointer                          :: Wrapper         !< Wrapper
        logical                                    :: isOfDataType    !< Check if has the same type
    !-----------------------------------------------------------------
        isOfDataType = .false.
        CurrentEntry => this%GetCurrentEntry()
        if(associated(CurrentEntry)) then
            Wrapper => CurrentEntry%PointToValue()
            if(associated(Wrapper)) then
                select type (Wrapper)
                    class is (DimensionsWrapper_t)
                        isOfDataType = Wrapper%isOfDataType(Mold=Mold)
                end select
            endif
        endif
    end function ParametersIterator_isOfDataType0D


    function ParametersIterator_isOfDataType1D(this, Mold) result(IsOfDataType)
    !-----------------------------------------------------------------
    !< Check if the data type of Mold agrees with the value associated with Key
    !-----------------------------------------------------------------
        class(ParametersIterator_t), intent(IN)    :: this            !< Parameter List
        class(*),                    intent(IN)    :: Mold(1:)        !< Mold
        type(ParameterEntry_t),      pointer       :: CurrentEntry    !< Current entry
        class(*), pointer                          :: Wrapper         !< Wrapper
        logical                                    :: isOfDataType    !< Check if has the same type
    !-----------------------------------------------------------------
        isOfDataType = .false.
        CurrentEntry => this%GetCurrentEntry()
        if(associated(CurrentEntry)) then
            Wrapper => CurrentEntry%PointToValue()
            if(associated(Wrapper)) then
                select type (Wrapper)
                    class is (DimensionsWrapper_t)
                        isOfDataType = Wrapper%isOfDataType(Mold=Mold(1))
                end select
            endif
        endif
    end function ParametersIterator_isOfDataType1D


    function ParametersIterator_isOfDataType2D(this, Mold) result(IsOfDataType)
    !-----------------------------------------------------------------
    !< Check if the data type of Mold agrees with the value associated with Key
    !-----------------------------------------------------------------
        class(ParametersIterator_t), intent(IN)    :: this            !< Parameter List
        class(*),                    intent(IN)    :: Mold(1:,1:)     !< Mold
        type(ParameterEntry_t),      pointer       :: CurrentEntry    !< Current entry
        class(*), pointer                          :: Wrapper         !< Wrapper
        logical                                    :: isOfDataType    !< Check if has the same type
    !-----------------------------------------------------------------
        isOfDataType = .false.
        CurrentEntry => this%GetCurrentEntry()
        if(associated(CurrentEntry)) then
            Wrapper => CurrentEntry%PointToValue()
            if(associated(Wrapper)) then
                select type (Wrapper)
                    class is (DimensionsWrapper_t)
                        isOfDataType = Wrapper%isOfDataType(Mold=Mold(1,1))
                end select
            endif
        endif
    end function ParametersIterator_isOfDataType2D


    function ParametersIterator_isOfDataType3D(this, Mold) result(IsOfDataType)
    !-----------------------------------------------------------------
    !< Check if the data type of Mold agrees with the value associated with Key
    !-----------------------------------------------------------------
        class(ParametersIterator_t), intent(IN)    :: this            !< Parameter List
        class(*),                    intent(IN)    :: Mold(1:,1:,1:)  !< Mold
        type(ParameterEntry_t),      pointer       :: CurrentEntry    !< Current entry
        class(*), pointer                          :: Wrapper         !< Wrapper
        logical                                    :: isOfDataType    !< Check if has the same type
    !-----------------------------------------------------------------
        isOfDataType = .false.
        CurrentEntry => this%GetCurrentEntry()
        if(associated(CurrentEntry)) then
            Wrapper => CurrentEntry%PointToValue()
            if(associated(Wrapper)) then
                select type (Wrapper)
                    class is (DimensionsWrapper_t)
                        isOfDataType = Wrapper%isOfDataType(Mold=Mold(1,1,1))
                end select
            endif
        endif
    end function ParametersIterator_isOfDataType3D


    function ParametersIterator_isOfDataType4D(this, Mold) result(IsOfDataType)
    !-----------------------------------------------------------------
    !< Check if the data type of Mold agrees with the value associated with Key
    !-----------------------------------------------------------------
        class(ParametersIterator_t), intent(IN)    :: this              !< Parameter List
        class(*),                    intent(IN)    :: Mold(1:,1:,1:,1:) !< Mold
        type(ParameterEntry_t),      pointer       :: CurrentEntry      !< Current entry
        class(*), pointer                          :: Wrapper           !< Wrapper
        logical                                    :: isOfDataType      !< Check if has the same type
    !-----------------------------------------------------------------
        isOfDataType = .false.
        CurrentEntry => this%GetCurrentEntry()
        if(associated(CurrentEntry)) then
            Wrapper => CurrentEntry%PointToValue()
            if(associated(Wrapper)) then
                select type (Wrapper)
                    class is (DimensionsWrapper_t)
                        isOfDataType = Wrapper%isOfDataType(Mold=Mold(1,1,1,1))
                end select
            endif
        endif
    end function ParametersIterator_isOfDataType4D


    function ParametersIterator_isOfDataType5D(this, Mold) result(IsOfDataType)
    !-----------------------------------------------------------------
    !< Check if the data type of Mold agrees with the value associated with Key
    !-----------------------------------------------------------------
        class(ParametersIterator_t), intent(IN)    :: this                 !< Parameter List
        class(*),                    intent(IN)    :: Mold(1:,1:,1:,1:,1:) !< Mold
        type(ParameterEntry_t),      pointer       :: CurrentEntry         !< Current entry
        class(*), pointer                          :: Wrapper              !< Wrapper
        logical                                    :: isOfDataType         !< Check if has the same type
    !-----------------------------------------------------------------
        isOfDataType = .false.
        CurrentEntry => this%GetCurrentEntry()
        if(associated(CurrentEntry)) then
            Wrapper => CurrentEntry%PointToValue()
            if(associated(Wrapper)) then
                select type (Wrapper)
                    class is (DimensionsWrapper_t)
                        isOfDataType = Wrapper%isOfDataType(Mold=Mold(1,1,1,1,1))
                end select
            endif
        endif
    end function ParametersIterator_isOfDataType5D


    function ParametersIterator_isOfDataType6D(this, Mold) result(IsOfDataType)
    !-----------------------------------------------------------------
    !< Check if the data type of Mold agrees with the value associated with Key
    !-----------------------------------------------------------------
        class(ParametersIterator_t), intent(IN)    :: this                    !< Parameter List
        class(*),                    intent(IN)    :: Mold(1:,1:,1:,1:,1:,1:) !< Mold
        type(ParameterEntry_t),      pointer       :: CurrentEntry            !< Current entry
        class(*), pointer                          :: Wrapper                 !< Wrapper
        logical                                    :: isOfDataType            !< Check if has the same type
    !-----------------------------------------------------------------
        isOfDataType = .false.
        CurrentEntry => this%GetCurrentEntry()
        if(associated(CurrentEntry)) then
            Wrapper => CurrentEntry%PointToValue()
            if(associated(Wrapper)) then
                select type (Wrapper)
                    class is (DimensionsWrapper_t)
                        isOfDataType = Wrapper%isOfDataType(Mold=Mold(1,1,1,1,1,1))
                end select
            endif
        endif
    end function ParametersIterator_isOfDataType6D


    function ParametersIterator_isOfDataType7D(this, Mold) result(IsOfDataType)
    !-----------------------------------------------------------------
    !< Check if the data type of Mold agrees with the value associated with Key
    !-----------------------------------------------------------------
        class(ParametersIterator_t), intent(IN)    :: this                       !< Parameter List
        class(*),                    intent(IN)    :: Mold(1:,1:,1:,1:,1:,1:,1:) !< Mold
        type(ParameterEntry_t),      pointer       :: CurrentEntry               !< Current entry
        class(*), pointer                          :: Wrapper                    !< Wrapper
        logical                                    :: isOfDataType               !< Check if has the same type
    !-----------------------------------------------------------------
        isOfDataType = .false.
        CurrentEntry => this%GetCurrentEntry()
        if(associated(CurrentEntry)) then
            Wrapper => CurrentEntry%PointToValue()
            if(associated(Wrapper)) then
                select type (Wrapper)
                    class is (DimensionsWrapper_t)
                        isOfDataType = Wrapper%isOfDataType(Mold=Mold(1,1,1,1,1,1,1))
                end select
            endif
        endif
    end function ParametersIterator_isOfDataType7D


    recursive subroutine ParametersIterator_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print the content of the DataBase
    !-----------------------------------------------------------------
        class(ParametersIterator_t),          intent(IN)  :: this    !< Parameter Iterator
        integer(I4P), optional,               intent(IN)  :: unit    !< Logic unit.
        character(*), optional,               intent(IN)  :: prefix  !< Prefixing string.
        integer(I4P), optional,               intent(OUT) :: iostat  !< IO error.
        character(*), optional,               intent(OUT) :: iomsg   !< IO error message.
        character(len=:), allocatable                     :: prefd   !< Prefixing string.
        integer(I4P)                                      :: unitd   !< Logic unit.
        integer(I4P)                                      :: iostatd !< IO error.
        character(500)                                    :: iomsgd  !< Temporary variable for IO error message.
        type(ParameterEntry_t), pointer                   :: CurrentEntry               !< Current entry
        class(*), pointer                                 :: Value
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        unitd = OUTPUT_UNIT; if(present(unit)) unitd = unit
        CurrentEntry => this%GetCurrentEntry()
        if(associated(CurrentEntry)) then
            Value => CurrentEntry%PointToValue()
            if(associated(Value)) then 
                select type(Value)
                    class is (DimensionsWrapper_t) 
                        call Value%Print(unit=unitd,                                                            &
                                         prefix=prefd//                                                         &
                                                '['//trim(str(no_sign=.true., n=this%GetCurrentIndex()))//']'// &
                                                ' Key = '//CurrentEntry%GetKey()//',',                          &
                                                 iostat=iostatd,                                                &
                                                 iomsg=iomsgd)
                end select
            endif
        endif
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine ParametersIterator_Print


    function ParametersIterator_HasFinished(this) result(HasFinished)
    !-----------------------------------------------------------------
    !< Check if Iterator has reached the end of the dictionary
    !-----------------------------------------------------------------
        class(ParametersIterator_t),        intent(INOUT) :: this        ! Dictionary iterator
        logical                                           :: HasFinished
    !-----------------------------------------------------------------
        HasFinished = .false.
        if(this%Index==this%UpperBound) HasFinished = .true.
    end function ParametersIterator_HasFinished

end module ParametersIterator
