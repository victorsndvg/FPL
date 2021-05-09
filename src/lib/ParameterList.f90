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

#define ParameterList_t ParameterList_
#define ParameterListIterator_t ParameterListIterator_

module ParameterList

USE iso_fortran_env, only: OUTPUT_UNIT
USE ErrorMessages
USE PENF
USE ParameterEntryDictionary
USE ParameterRootEntry
USE ParameterEntry
USE WrapperFactoryListSingleton
USE WrapperFactory
USE DimensionsWrapper
USE DimensionsWrapper0D
USE DimensionsWrapper1D
USE DimensionsWrapper2D
USE DimensionsWrapper3D
USE DimensionsWrapper4D
USE DimensionsWrapper5D
USE DimensionsWrapper6D
USE DimensionsWrapper7D

implicit none
private

    type :: ParameterList_t
    private
        type(ParameterEntryDictionary_t) :: Dictionary
    contains
    private
        procedure, non_overridable         ::                   ParameterList_Set0D
        procedure, non_overridable         ::                   ParameterList_Set1D
        procedure, non_overridable         ::                   ParameterList_Set2D
        procedure, non_overridable         ::                   ParameterList_Set3D
        procedure, non_overridable         ::                   ParameterList_Set4D
        procedure, non_overridable         ::                   ParameterList_Set5D
        procedure, non_overridable         ::                   ParameterList_Set6D
        procedure, non_overridable         ::                   ParameterList_Set7D
        procedure, non_overridable         ::                   ParameterList_Get0D
        procedure, non_overridable         ::                   ParameterList_Get1D
        procedure, non_overridable         ::                   ParameterList_Get2D
        procedure, non_overridable         ::                   ParameterList_Get3D
        procedure, non_overridable         ::                   ParameterList_Get4D
        procedure, non_overridable         ::                   ParameterList_Get5D
        procedure, non_overridable         ::                   ParameterList_Get6D
        procedure, non_overridable         ::                   ParameterList_Get7D
        procedure, non_overridable         ::                   ParameterList_GetPointer0D
        procedure, non_overridable         ::                   ParameterList_GetPointer1D
        procedure, non_overridable         ::                   ParameterList_GetPointer2D
        procedure, non_overridable         ::                   ParameterList_GetPointer3D
        procedure, non_overridable         ::                   ParameterList_GetPointer4D
        procedure, non_overridable         ::                   ParameterList_GetPointer5D
        procedure, non_overridable         ::                   ParameterList_GetPointer6D
        procedure, non_overridable         ::                   ParameterList_GetPointer7D
        procedure, non_overridable         ::                   ParameterList_IsOfDataType0D
        procedure, non_overridable         ::                   ParameterList_IsOfDataType1D
        procedure, non_overridable         ::                   ParameterList_IsOfDataType2D
        procedure, non_overridable         ::                   ParameterList_IsOfDataType3D
        procedure, non_overridable         ::                   ParameterList_IsOfDataType4D
        procedure, non_overridable         ::                   ParameterList_IsOfDataType5D
        procedure, non_overridable         ::                   ParameterList_IsOfDataType6D
        procedure, non_overridable         ::                   ParameterList_IsOfDataType7D
        procedure, non_overridable         ::                   ParameterList_isAssignable0D
        procedure, non_overridable         ::                   ParameterList_isAssignable1D
        procedure, non_overridable         ::                   ParameterList_isAssignable2D
        procedure, non_overridable         ::                   ParameterList_isAssignable3D
        procedure, non_overridable         ::                   ParameterList_isAssignable4D
        procedure, non_overridable         ::                   ParameterList_isAssignable5D
        procedure, non_overridable         ::                   ParameterList_isAssignable6D
        procedure, non_overridable         ::                   ParameterList_isAssignable7D
        generic,   public :: Set           => ParameterList_Set0D, &
                                              ParameterList_Set1D, &
                                              ParameterList_Set2D, &
                                              ParameterList_Set3D, &
                                              ParameterList_Set4D, &
                                              ParameterList_Set5D, &
                                              ParameterList_Set6D, &
                                              ParameterList_Set7D
        generic,   public :: Get           => ParameterList_Get0D, &
                                              ParameterList_Get1D, &
                                              ParameterList_Get2D, &
                                              ParameterList_Get3D, &
                                              ParameterList_Get4D, &
                                              ParameterList_Get5D, &
                                              ParameterList_Get6D, &
                                              ParameterList_Get7D
        generic,   public :: GetPointer    => ParameterList_GetPointer0D, &
                                              ParameterList_GetPointer1D, &
                                              ParameterList_GetPointer2D, &
                                              ParameterList_GetPointer3D, &
                                              ParameterList_GetPointer4D, &
                                              ParameterList_GetPointer5D, &
                                              ParameterList_GetPointer6D, &
                                              ParameterList_GetPointer7D
        generic,   public :: isOfDataType  => ParameterList_IsOfDataType0D, &
                                              ParameterList_IsOfDataType1D, &
                                              ParameterList_IsOfDataType2D, &
                                              ParameterList_IsOfDataType3D, &
                                              ParameterList_IsOfDataType4D, &
                                              ParameterList_IsOfDataType5D, &
                                              ParameterList_IsOfDataType6D, &
                                              ParameterList_IsOfDataType7D
        generic,   public :: isAssignable  => ParameterList_isAssignable0D, &
                                              ParameterList_isAssignable1D, &
                                              ParameterList_isAssignable2D, &
                                              ParameterList_isAssignable3D, &
                                              ParameterList_isAssignable4D, &
                                              ParameterList_isAssignable5D, &
                                              ParameterList_isAssignable6D, &
                                              ParameterList_isAssignable7D
        procedure, non_overridable, public :: DataSizeInBytes=> ParameterList_DataSizeInBytes
        procedure, non_overridable, public :: Del            => ParameterList_RemoveEntry
        generic, public :: Remove => Del
        procedure, non_overridable, public :: Init           => ParameterList_Init
        generic, public :: Initiate => Init
        procedure, non_overridable, public :: GetShape       => ParameterList_GetShape
        procedure, non_overridable, public :: GetDimensions  => ParameterList_GetDimensions
        procedure, non_overridable, public :: NewSubList     => ParameterList_NewSubList
        procedure, non_overridable, public :: GetSubList     => ParameterList_GetSubList
        procedure, non_overridable, public :: isPresent      => ParameterList_isPresent
        procedure, non_overridable, public :: isSubList      => ParameterList_isSubList
        procedure, non_overridable, public :: GetAsString    => ParameterList_GetAsString
        procedure, non_overridable, public :: Free           => ParameterList_Free
        generic, public :: DeallocateData => Free
        procedure, non_overridable, public :: Print          => ParameterList_Print
        procedure, non_overridable, public :: Display        => ParameterList_Display
        procedure, non_overridable, public :: Length         => ParameterList_Length
        procedure, non_overridable, public :: GetIterator    => ParameterList_GetIterator
        final                              ::                   ParameterList_Finalize
    end type ParameterList_t

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

    type :: ParameterListIterator_t
    private
        type(ParameterRootEntry_t), pointer :: DataBase(:)  => NULL()
        type(EntryListIterator_t)           :: EntryListIterator
        integer(I4P)                        :: Index       = 0
        integer(I4P)                        :: UpperBound  = 0
    contains
    private
        procedure,         non_overridable ::                             ParameterListIterator_Assignment
        procedure,         non_overridable ::                             ParameterListIterator_Get0D
        procedure,         non_overridable ::                             ParameterListIterator_Get1D
        procedure,         non_overridable ::                             ParameterListIterator_Get2D
        procedure,         non_overridable ::                             ParameterListIterator_Get3D
        procedure,         non_overridable ::                             ParameterListIterator_Get4D
        procedure,         non_overridable ::                             ParameterListIterator_Get5D
        procedure,         non_overridable ::                             ParameterListIterator_Get6D
        procedure,         non_overridable ::                             ParameterListIterator_Get7D
        procedure,         non_overridable ::                             ParameterListIterator_isOfDataType0D
        procedure,         non_overridable ::                             ParameterListIterator_isOfDataType1D
        procedure,         non_overridable ::                             ParameterListIterator_isOfDataType2D
        procedure,         non_overridable ::                             ParameterListIterator_isOfDataType3D
        procedure,         non_overridable ::                             ParameterListIterator_isOfDataType4D
        procedure,         non_overridable ::                             ParameterListIterator_isOfDataType5D
        procedure,         non_overridable ::                             ParameterListIterator_isOfDataType6D
        procedure,         non_overridable ::                             ParameterListIterator_isOfDataType7D
        procedure,         non_overridable ::                             ParameterListIterator_isAssignable0D
        procedure,         non_overridable ::                             ParameterListIterator_isAssignable1D
        procedure,         non_overridable ::                             ParameterListIterator_isAssignable2D
        procedure,         non_overridable ::                             ParameterListIterator_isAssignable3D
        procedure,         non_overridable ::                             ParameterListIterator_isAssignable4D
        procedure,         non_overridable ::                             ParameterListIterator_isAssignable5D
        procedure,         non_overridable ::                             ParameterListIterator_isAssignable6D
        procedure,         non_overridable ::                             ParameterListIterator_isAssignable7D
        procedure,         non_overridable :: GetEntry                 => ParameterListIterator_GetEntry
        procedure,         non_overridable :: GetIndex                 => ParameterListIterator_GetIndex
        procedure,         non_overridable :: PointToValue             => ParameterListIterator_PointToValue
        procedure,         non_overridable :: NextNotEmptyListIterator => ParameterListIterator_NextNotEmptyListIterator
        procedure, public, non_overridable :: GetKey                   => ParameterListIterator_GetKey
        procedure, public, non_overridable :: Init                     => ParameterListIterator_Init
        procedure, public, non_overridable :: Begin                    => ParameterListIterator_Begin
        procedure, public, non_overridable :: End                      => ParameterListIterator_End
        procedure, public, non_overridable :: Next                     => ParameterListIterator_Next
        procedure, public, non_overridable :: HasFinished              => ParameterListIterator_HasFinished
        procedure, public, non_overridable :: GetShape                 => ParameterListIterator_GetShape
        procedure, public, non_overridable :: GetDimensions            => ParameterListIterator_GetDimensions
        procedure, public, non_overridable :: DataSizeInBytes          => ParameterListIterator_DataSizeInBytes
        procedure, public, non_overridable :: GetAsString              => ParameterListIterator_GetAsString
        procedure, public, non_overridable :: GetSubList               => ParameterListIterator_GetSubList
        procedure, public, non_overridable :: isSubList                => ParameterListIterator_isSubList
        procedure, public, non_overridable :: toString                 => ParameterListIterator_toString
        procedure, public, non_overridable :: Print                    => ParameterListIterator_Print
        procedure, public, non_overridable :: Free                     => ParameterListIterator_Free
        generic,   public                  :: Get                      => ParameterListIterator_Get0D, &
        ParameterListIterator_Get1D, &
        ParameterListIterator_Get2D, &
        ParameterListIterator_Get3D, &
        ParameterListIterator_Get4D, &
        ParameterListIterator_Get5D, &
        ParameterListIterator_Get6D, &
        ParameterListIterator_Get7D
        generic,   public                  :: isOfDataType             => ParameterListIterator_IsOfDataType0D, &
        ParameterListIterator_IsOfDataType1D, &
        ParameterListIterator_IsOfDataType2D, &
        ParameterListIterator_IsOfDataType3D, &
        ParameterListIterator_IsOfDataType4D, &
        ParameterListIterator_IsOfDataType5D, &
        ParameterListIterator_IsOfDataType6D, &
        ParameterListIterator_IsOfDataType7D
        generic,   public                  :: isAssignable             => ParameterListIterator_isAssignable0D, &
        ParameterListIterator_isAssignable1D, &
        ParameterListIterator_isAssignable2D, &
        ParameterListIterator_isAssignable3D, &
        ParameterListIterator_isAssignable4D, &
        ParameterListIterator_isAssignable5D, &
        ParameterListIterator_isAssignable6D, &
        ParameterListIterator_isAssignable7D
        generic,   public                  :: Assignment(=)            => ParameterListIterator_Assignment
        final                              ::                             ParameterListIterator_Final
    end type

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

public :: ParameterList_t
public :: ParameterListIterator_t

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

contains

!---------------------------------------------------------------------
!< Parameter List Procedures
!---------------------------------------------------------------------

    subroutine ParameterList_Init(this,Size)
    !-----------------------------------------------------------------
    !< Initialize the dictionary
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(INOUT) :: this   !< Parameter List
        integer(I4P), optional,               intent(IN)    :: Size   !< Dictionary Size
    !-----------------------------------------------------------------
        call this%Free()
        if (present(Size)) then
            call this%Dictionary%Init(Size = Size)
        else
            call this%Dictionary%Init()
        endif
    end subroutine ParameterList_Init


    function ParameterList_GetShape(this,Key, Shape) result(FPLError)
    !-----------------------------------------------------------------
    !< Return an allocatable array with the shape of the contained value
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN)    :: this           !< Parameter List
        character(len=*),                     intent(IN)    :: Key            !< String Key
        integer(I4P), allocatable,            intent(INOUT) :: Shape(:)       !< Shape of the stored value
        class(*),                    pointer                :: Wrapper        !< Wrapper
        integer(I4P)                                        :: FPLerror       !< Error flag
    !-----------------------------------------------------------------
        FPLerror = FPLSuccess
        nullify(Wrapper)
        call this%Dictionary%GetPointer(Key=Key, Value=Wrapper)
        if(associated(Wrapper)) then
            select type(Wrapper)
                class is (DimensionsWrapper_t)
                    call Wrapper%GetShape(Shape)
                class Default
                    FPLerror = FPLWrapperError
                    call msg%Error(txt='Getting [Key="'//Key//'"]: Unknown Wrapper. Shape was not modified.', &
                           file=__FILE__, line=__LINE__ )
            end select
        else
            FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Getting [Key="'//Key//'"]: Not present. Shape was not modified.', &
                           file=__FILE__, line=__LINE__ )
        endif
    end function ParameterList_GetShape


    function ParameterList_GetDimensions(this,Key) result(Dimensions)
    !-----------------------------------------------------------------
    !< Return an integer with the dimensions of the contained value
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN)    :: this           !< Parameter List
        character(len=*),                     intent(IN)    :: Key            !< String Key
        integer(I4P)                                        :: Dimensions     !< Dimensions of the stored value
        class(*),                    pointer                :: Wrapper        !< Wrapper
        integer(I4P)                                        :: FPLerror       !< Error flag
    !-----------------------------------------------------------------
        FPLerror = FPLSuccess
        Dimensions = 0
        nullify(Wrapper)
        call this%Dictionary%GetPointer(Key=Key, Value=Wrapper)
        if(associated(Wrapper)) then
            select type(Wrapper)
                class is (DimensionsWrapper_t)
                    Dimensions = Wrapper%GetDimensions()
                class Default
                    FPLerror = FPLWrapperError
                    call msg%Error(txt='Getting [Key="'//Key//'"]: Unknown Wrapper. Shape was not modified.', &
                           file=__FILE__, line=__LINE__ )
            end select
        else
            FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Getting [Key="'//Key//'"]: Not present. Shape was not modified.', &
                           file=__FILE__, line=__LINE__ )
        endif
    end function ParameterList_GetDimensions


    subroutine ParameterList_Free(this)
    !-----------------------------------------------------------------
    !< Free the dictionary
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(INOUT) :: this   !< Parameter List
    !-----------------------------------------------------------------
        call this%Dictionary%Free()
    end subroutine ParameterList_Free


    subroutine ParameterList_Finalize(this)
    !-----------------------------------------------------------------
    !< Destructor procedure
    !-----------------------------------------------------------------
        type(ParameterList_t),               intent(INOUT) :: this    !< Parameter List
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine ParameterList_Finalize


    function ParameterList_NewSubList(this,Key, Size) result(SubListPointer)
    !-----------------------------------------------------------------
    !< Set a Key/Value pair into the dictionary
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(INOUT) :: this           !< Parameter List
        character(len=*),                     intent(IN)    :: Key            !< String Key
        integer(I4P), optional,               intent(IN)    :: Size           !< Sublist Size
        class(*),                             pointer       :: Sublist        !< New Sublist
        type(ParameterList_t),                pointer       :: SublistPointer !< New Sublist pointer
    !-----------------------------------------------------------------
        allocate(ParameterList_t :: SubList)
        call this%Dictionary%Set(Key=Key,Value=Sublist)
        select type(SubList)
            class is (ParameterList_t)
                SublistPointer => SubList
                if(present(Size)) then
                    call Sublist%Init(Size=Size)
                else
                    call Sublist%Init(Size=Size)
                endif
        end select
    end function ParameterList_NewSubList


    function ParameterList_GetSublist(this,Key, Sublist) result(FPLerror)
    !-----------------------------------------------------------------
    !< Return a Unlimited polymorphic pointer to a Value given the Key
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN)    :: this    !< Parameter List
        character(len=*),                     intent(IN)    :: Key     !< String Key
        type(ParameterList_t),       pointer, intent(INOUT) :: Sublist !< Wrapper
        class(*),                    pointer                :: Value   !< Returned pointer to value
        integer(I4P)                                        :: FPLerror!< Error flag
    !-----------------------------------------------------------------
        FPLerror = FPLSuccess
        nullify(Value)
        call this%Dictionary%GetPointer(Key=Key, Value=Value)
        if(associated(Value)) then
            select type(Value)
                class is (ParameterList_t)
                    SubList => Value
                class Default
                    FPLerror = FPLSublistError
                    call msg%Error(txt='Getting [Key="'//Key//'"]: Is not a sublist.', &
                           file=__FILE__, line=__LINE__ )
            end select
        else
            FPLerror = FPLSublistError
            call msg%Error(txt='Getting [Key="'//Key//'"]: Not present. Value was not modified.', &
                           file=__FILE__, line=__LINE__ )
        endif
    end function ParameterList_GetSubList


    function ParameterList_Set0D(this,Key,Value) result(FPLerror)
    !-----------------------------------------------------------------
    !< Set a Key/Value pair into the Dictionary
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(INOUT) :: this           !< Parameter List
        character(len=*),                     intent(IN)    :: Key            !< String Key
        class(*),                             intent(IN)    :: Value          !< Unlimited polymorphic Value
        class(WrapperFactory_t),    pointer                 :: WrapperFactory !< WrapperFactory
        class(*),                   pointer                 :: Wrapper        !< Wrapper
        integer(I4P)                                        :: FPLerror       !< Error flag
    !-----------------------------------------------------------------
        FPLerror = FPLSuccess
        nullify(WrapperFactory)
        nullify(Wrapper)
        WrapperFactory => TheWrapperFactoryList%GetFactory(Value=Value)
        if(associated(WrapperFactory)) then
            Wrapper => WrapperFactory%Wrap(Value=Value)
            if(associated(Wrapper)) then
                call this%Dictionary%Set(Key=Key,Value=Wrapper)
            else
                FPLerror = FPLWrapperError
                call msg%Error(txt='Setting [Key="'//Key//'"]: Nonexistent wrapper. Not added to the list.', &
                           file=__FILE__, line=__LINE__ )
            endif
        else
            FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Setting [Key="'//Key//'"]: Unsupported data type. Not added to the list.', &
                           file=__FILE__, line=__LINE__ )
        endif
    end function ParameterList_Set0D


    function ParameterList_Set1D(this,Key,Value) result(FPLerror)
    !-----------------------------------------------------------------
    !< Set a Key/Value pair into the DataBase
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(INOUT) :: this           !< Parameter List
        character(len=*),                     intent(IN)    :: Key            !< String Key
        class(*),                             intent(IN)    :: Value(:)       !< Unlimited polymorphic 1D array Value
        class(WrapperFactory_t),    pointer                 :: WrapperFactory !< WrapperFactory
        class(*),                   pointer                 :: Wrapper        !< Wrapper
        integer(I4P)                                        :: FPLerror       !< Error flag
    !-----------------------------------------------------------------
        FPLerror = FPLSuccess
        nullify(WrapperFactory)
        nullify(Wrapper)
        WrapperFactory => TheWrapperFactoryList%GetFactory(Value=Value)
        if(associated(WrapperFactory)) then
            Wrapper => WrapperFactory%Wrap(Value=Value)
            if(associated(Wrapper)) then
                call this%Dictionary%Set(Key=Key,Value=Wrapper)
            else
                FPLerror = FPLWrapperError
                call msg%Error(txt='Setting [Key="'//Key//'"]: Nonexistent wrapper. Not added to the list.', &
                           file=__FILE__, line=__LINE__ )
            endif
        else
            FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Setting [Key="'//Key//'"]: Unsupported data type. Not added to the list.', &
                           file=__FILE__, line=__LINE__ )
        endif
    end function ParameterList_Set1D


    function ParameterList_Set2D(this,Key,Value) result(FPLerror)
    !-----------------------------------------------------------------
    !< Set a Key/Value pair into the DataBase
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(INOUT) :: this           !< Parameter List
        character(len=*),                     intent(IN)    :: Key            !< String Key
        class(*),                             intent(IN)    :: Value(:,:)     !< Unlimited polymorphic 2D array value
        class(WrapperFactory_t),    pointer                 :: WrapperFactory !< WrapperFactory
        class(*),                   pointer                 :: Wrapper        !< Wrapper
        integer(I4P)                                        :: FPLerror       !< Error flag
    !-----------------------------------------------------------------
        FPLerror = FPLSuccess
        nullify(WrapperFactory)
        nullify(Wrapper)
        WrapperFactory => TheWrapperFactoryList%GetFactory(Value=Value)
        if(associated(WrapperFactory)) then
            Wrapper => WrapperFactory%Wrap(Value=Value)
            if(associated(Wrapper)) then
                call this%Dictionary%Set(Key=Key,Value=Wrapper)
            else
                FPLerror = FPLWrapperError
                call msg%Error(txt='Setting [Key="'//Key//'"]: Nonexistent wrapper. Not added to the list.', &
                           file=__FILE__, line=__LINE__ )
            endif
        else
            FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Setting [Key="'//Key//'"]: Unsupported data type. Not added to the list.', &
                           file=__FILE__, line=__LINE__ )
        endif
    end function ParameterList_Set2D


    function ParameterList_Set3D(this,Key,Value) result(FPLerror)
    !-----------------------------------------------------------------
    !< Set a Key/Value pair into the DataBase
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(INOUT) :: this           !< Parameter List
        character(len=*),                     intent(IN)    :: Key            !< String Key
        class(*),                             intent(IN)    :: Value(:,:,:)   !< Unlimited Polimorphic 3D array Value
        class(WrapperFactory_t),    pointer                 :: WrapperFactory !< WrapperFactory
        class(*),                   pointer                 :: Wrapper        !< Wrapper
        integer(I4P)                                        :: FPLerror       !< Error flag
    !-----------------------------------------------------------------
        FPLerror = FPLSuccess
        nullify(WrapperFactory)
        nullify(Wrapper)
        WrapperFactory => TheWrapperFactoryList%GetFactory(Value=Value)
        if(associated(WrapperFactory)) then
            Wrapper => WrapperFactory%Wrap(Value=Value)
            if(associated(Wrapper)) then
                call this%Dictionary%Set(Key=Key,Value=Wrapper)
            else
                FPLerror = FPLWrapperError
                call msg%Error(txt='Setting [Key="'//Key//'"]: Nonexistent wrapper. Not added to the list.', &
                           file=__FILE__, line=__LINE__ )
            endif
        else
            FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Setting [Key="'//Key//'"]: Unsupported data type. Not added to the list.', &
                           file=__FILE__, line=__LINE__ )
        endif
    end function ParameterList_Set3D


    function ParameterList_Set4D(this,Key,Value) result(FPLerror)
    !-----------------------------------------------------------------
    !< Set a Key/Value pair into the DataBase
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(INOUT) :: this           !< Parameter List
        character(len=*),                     intent(IN)    :: Key            !< String Key
        class(*),                             intent(IN)    :: Value(:,:,:,:) !< Unlimited Polymorphic 4D array Value
        class(WrapperFactory_t),    pointer                 :: WrapperFactory !< WrapperFactory
        class(*),                   pointer                 :: Wrapper        !< Wrapper
        integer(I4P)                                        :: FPLerror       !< Error flag
    !-----------------------------------------------------------------
        FPLerror = FPLSuccess
        nullify(WrapperFactory)
        nullify(Wrapper)
        WrapperFactory => TheWrapperFactoryList%GetFactory(Value=Value)
        if(associated(WrapperFactory)) then
            Wrapper => WrapperFactory%Wrap(Value=Value)
            if(associated(Wrapper)) then
                call this%Dictionary%Set(Key=Key,Value=Wrapper)
            else
                FPLerror = FPLWrapperError
                call msg%Error(txt='Setting [Key="'//Key//'"]: Nonexistent wrapper. Not added to the list.', &
                           file=__FILE__, line=__LINE__ )
            endif
        else
            FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Setting [Key="'//Key//'"]: Unsupported data type. Not added to the list.', &
                           file=__FILE__, line=__LINE__ )
        endif
    end function ParameterList_Set4D


    function ParameterList_Set5D(this,Key,Value) result(FPLerror)
    !-----------------------------------------------------------------
    !< Set a Key/Value pair into the DataBase
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(INOUT) :: this             !< Parameter List
        character(len=*),                     intent(IN)    :: Key              !< String Key
        class(*),                             intent(IN)    :: Value(:,:,:,:,:) !< Unlimited Polymorphic 5D array Value
        class(WrapperFactory_t),    pointer                 :: WrapperFactory   !< WrapperFactory
        class(*),                   pointer                 :: Wrapper          !< Wrapper
        integer(I4P)                                        :: FPLerror         !< Error flag
    !-----------------------------------------------------------------
        FPLerror = FPLSuccess
        nullify(WrapperFactory)
        nullify(Wrapper)
        WrapperFactory => TheWrapperFactoryList%GetFactory(Value=Value)
        if(associated(WrapperFactory)) then
            Wrapper => WrapperFactory%Wrap(Value=Value)
            if(associated(Wrapper)) then
                call this%Dictionary%Set(Key=Key,Value=Wrapper)
            else
                FPLerror = FPLWrapperError
                call msg%Error(txt='Setting [Key="'//Key//'"]: Nonexistent wrapper. Not added to the list.', &
                           file=__FILE__, line=__LINE__ )
            endif
        else
            FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Setting [Key="'//Key//'"]: Unsupported data type. Not added to the list.', &
                           file=__FILE__, line=__LINE__ )
        endif
    end function ParameterList_Set5D


    function ParameterList_Set6D(this,Key,Value) result(FPLerror)
    !-----------------------------------------------------------------
    !< Set a Key/Value pair into the DataBase
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(INOUT) :: this               !< Parameter List
        character(len=*),                     intent(IN)    :: Key                !< String Key
        class(*),                             intent(IN)    :: Value(:,:,:,:,:,:) !< Unlimited Polymorphic 5D array Value
        class(WrapperFactory_t),    pointer                 :: WrapperFactory     !< WrapperFactory
        class(*),                   pointer                 :: Wrapper            !< Wrapper
        integer(I4P)                                        :: FPLerror           !< Error flag
    !-----------------------------------------------------------------
        FPLerror = FPLSuccess
        nullify(WrapperFactory)
        nullify(Wrapper)
        WrapperFactory => TheWrapperFactoryList%GetFactory(Value=Value)
        if(associated(WrapperFactory)) then
            Wrapper => WrapperFactory%Wrap(Value=Value)
            if(associated(Wrapper)) then
                call this%Dictionary%Set(Key=Key,Value=Wrapper)
            else
                FPLerror = FPLWrapperError
                call msg%Error(txt='Setting [Key="'//Key//'"]: Nonexistent wrapper. Not added to the list.', &
                           file=__FILE__, line=__LINE__ )
            endif
        else
            FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Setting [Key="'//Key//'"]: Unsupported data type. Not added to the list.', &
                           file=__FILE__, line=__LINE__ )
        endif
    end function ParameterList_Set6D


    function ParameterList_Set7D(this,Key,Value) result(FPLerror)
    !-----------------------------------------------------------------
    !< Set a Key/Value pair into the DataBase
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(INOUT) :: this                 !< Parameter List
        character(len=*),                     intent(IN)    :: Key                  !< String Key
        class(*),                             intent(IN)    :: Value(:,:,:,:,:,:,:) !< Unlimited Polymorphic 7D array Value
        class(WrapperFactory_t),    pointer                 :: WrapperFactory       !< WrapperFactory
        class(*),                   pointer                 :: Wrapper              !< Wrapper
        integer(I4P)                                        :: FPLerror             !< Error flag
    !-----------------------------------------------------------------
        FPLerror = FPLSuccess
        nullify(WrapperFactory)
        nullify(Wrapper)
        WrapperFactory => TheWrapperFactoryList%GetFactory(Value=Value)
        if(associated(WrapperFactory)) then
            Wrapper => WrapperFactory%Wrap(Value=Value)
            if(associated(Wrapper)) then
                call this%Dictionary%Set(Key=Key,Value=Wrapper)
            else
                FPLerror = FPLWrapperError
                call msg%Error(txt='Setting [Key="'//Key//'"]: Nonexistent wrapper. Not added to the list.', &
                           file=__FILE__, line=__LINE__ )
            endif
        else
            FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Setting [Key="'//Key//'"]: Unsupported data type. Not added to the list.', &
                           file=__FILE__, line=__LINE__ )
        endif
    end function ParameterList_Set7D


    function ParameterList_Get0D(this,Key,Value) result(FPLerror)
    !-----------------------------------------------------------------
    !< Return a scalar Value given the Key
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN)    :: this           !< Parameter List
        character(len=*),                     intent(IN)    :: Key            !< String Key
        class(*),                             intent(INOUT) :: Value          !< Returned value
        class(*),                    pointer                :: Wrapper        !< Wrapper
        integer(I4P)                                        :: FPLerror       !< Error flag
    !-----------------------------------------------------------------
        FPLerror = FPLSuccess
        nullify(Wrapper)
        call this%Dictionary%GetPointer(Key=Key, Value=Wrapper)
        if(associated(Wrapper)) then
            select type(Wrapper)
                class is (DimensionsWrapper0D_t)
                    call Wrapper%Get(Value=Value)
                class Default
                    FPLerror = FPLWrapperError
                    call msg%Error(txt='Getting [Key="'//Key//'"]: Dimensions do not match. Value was not modified.', &
                               file=__FILE__, line=__LINE__ )
            end select
        else
            FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Getting [Key="'//Key//'"]: Not present. Value was not modified.', &
                           file=__FILE__, line=__LINE__ )
        endif
    end function ParameterList_Get0D


    function ParameterList_Get1D(this,Key,Value) result(FPLerror)
    !-----------------------------------------------------------------
    !< Return a vector Value given the Key
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN)    :: this           !< Parameter List
        character(len=*),                     intent(IN)    :: Key            !< String Key
        class(*),                             intent(INOUT) :: Value(:)       !< Returned value
        class(*),                    pointer                :: Wrapper        !< Wrapper
        integer(I4P)                                        :: FPLerror       !< Error flag
    !-----------------------------------------------------------------
        FPLerror = FPLSuccess
        nullify(Wrapper)
        call this%Dictionary%GetPointer(Key=Key, Value=Wrapper)
        if(associated(Wrapper)) then
            select type(Wrapper)
                class is (DimensionsWrapper1D_t)
                call Wrapper%Get(Value=Value)
                class Default
                    FPLerror = FPLWrapperError
                    call msg%Error(txt='Getting [Key="'//Key//'"]: Dimensions do not match. Value was not modified.', &
                               file=__FILE__, line=__LINE__ )
            end select
        else
            FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Getting [Key="'//Key//'"]: Not present. Value was not modified.', &
                           file=__FILE__, line=__LINE__ )
        endif
    end function ParameterList_Get1D


    function ParameterList_Get2D(this,Key,Value) result(FPLerror)
    !-----------------------------------------------------------------
    !< Return a 2D array Value given the Key
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN)    :: this           !< Parameter List
        character(len=*),                     intent(IN)    :: Key            !< String Key
        class(*),                             intent(INOUT) :: Value(:,:)     !< Returned value
        class(*),                    pointer                :: Wrapper        !< Wrapper
        integer(I4P)                                        :: FPLerror       !< Error flag
    !-----------------------------------------------------------------
        FPLerror = FPLSuccess
        nullify(Wrapper)
        call this%Dictionary%GetPointer(Key=Key, Value=Wrapper)
        if(associated(Wrapper)) then
            select type(Wrapper)
                class is (DimensionsWrapper2D_t)
                    call Wrapper%Get(Value=Value)
                class Default
                    FPLerror = FPLWrapperError
                    call msg%Error(txt='Getting [Key="'//Key//'"]: Dimensions do not match. Value was not modified.', &
                               file=__FILE__, line=__LINE__ )
            end select
        else
            FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Getting [Key="'//Key//'"]: Not present. Value was not modified.', &
                           file=__FILE__, line=__LINE__ )
        endif
    end function ParameterList_Get2D


    function ParameterList_Get3D(this,Key,Value) result(FPLerror)
    !-----------------------------------------------------------------
    !< Return a 3D array Value given the Key
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN)    :: this           !< Parameter List
        character(len=*),                     intent(IN)    :: Key            !< String Key
        class(*),                             intent(INOUT) :: Value(:,:,:)   !< Returned value
        class(*),                    pointer                :: Wrapper        !< Wrapper
        integer(I4P)                                        :: FPLerror       !< Error flag
    !-----------------------------------------------------------------
        FPLerror = FPLSuccess
        nullify(Wrapper)
        call this%Dictionary%GetPointer(Key=Key, Value=Wrapper)
        if(associated(Wrapper)) then
            select type(Wrapper)
                class is (DimensionsWrapper3D_t)
                    call Wrapper%Get(Value=Value)
                class Default
                    FPLerror = FPLWrapperError
                    call msg%Error(txt='Getting [Key="'//Key//'"]: Dimensions do not match. Value was not modified.', &
                               file=__FILE__, line=__LINE__ )
            end select
        else
            FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Getting [Key="'//Key//'"]: Not present. Value was not modified.', &
                           file=__FILE__, line=__LINE__ )
        endif
    end function ParameterList_Get3D


    function ParameterList_Get4D(this,Key,Value) result(FPLerror)
    !-----------------------------------------------------------------
    !< Return a 4D array Value given the Key
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN)    :: this           !< Parameter List
        character(len=*),                     intent(IN)    :: Key            !< String Key
        class(*),                             intent(INOUT) :: Value(:,:,:,:) !< Returned value
        class(*),                    pointer                :: Wrapper        !< Wrapper
        integer(I4P)                                        :: FPLerror       !< Error flag
    !-----------------------------------------------------------------
        FPLerror = FPLSuccess
        nullify(Wrapper)
        call this%Dictionary%GetPointer(Key=Key, Value=Wrapper)
        if(associated(Wrapper)) then
            select type(Wrapper)
                class is (DimensionsWrapper4D_t)
                    call Wrapper%Get(Value=Value)
                class Default
                    FPLerror = FPLWrapperError
                    call msg%Error(txt='Getting [Key="'//Key//'"]: Dimensions do not match. Value was not modified.', &
                               file=__FILE__, line=__LINE__ )
            end select
        else
            FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Getting [Key="'//Key//'"]: Not present. Value was not modified.', &
                           file=__FILE__, line=__LINE__ )
        endif
    end function ParameterList_Get4D


    function ParameterList_Get5D(this,Key,Value) result(FPLerror)
    !-----------------------------------------------------------------
    !< Return a 5D array Value given the Key
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN)    :: this             !< Parameter List
        character(len=*),                     intent(IN)    :: Key              !< String Key
        class(*),                             intent(INOUT) :: Value(:,:,:,:,:) !< Returned value
        class(*), pointer                                   :: Node             !< Pointer to a Parameter List
        class(*),                    pointer                :: Wrapper          !< Wrapper
        integer(I4P)                                        :: FPLerror         !< Error flag
    !-----------------------------------------------------------------
        FPLerror = FPLSuccess
        nullify(Wrapper)
        call this%Dictionary%GetPointer(Key=Key, Value=Wrapper)
        if(associated(Wrapper)) then
            select type(Wrapper)
                class is (DimensionsWrapper5D_t)
                    call Wrapper%Get(Value=Value)
                class Default
                    FPLerror = FPLWrapperError
                    call msg%Error(txt='Getting [Key="'//Key//'"]: Dimensions do not match. Value was not modified.', &
                               file=__FILE__, line=__LINE__ )
            end select
        else
            FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Getting [Key="'//Key//'"]: Not present. Value was not modified.', &
                           file=__FILE__, line=__LINE__ )
        endif
    end function ParameterList_Get5D


    function ParameterList_Get6D(this,Key,Value) result(FPLerror)
    !-----------------------------------------------------------------
    !< Return a 6D array Value given the Key
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN)    :: this               !< Parameter List
        character(len=*),                     intent(IN)    :: Key                !< String Key
        class(*),                             intent(INOUT) :: Value(:,:,:,:,:,:) !< Returned value
        class(*),                    pointer                :: Wrapper            !< Wrapper
        integer(I4P)                                        :: FPLerror           !< Error flag
    !-----------------------------------------------------------------
        FPLerror = FPLSuccess
        nullify(Wrapper)
        call this%Dictionary%GetPointer(Key=Key, Value=Wrapper)
        if(associated(Wrapper)) then
            select type(Wrapper)
                class is (DimensionsWrapper6D_t)
                    call Wrapper%Get(Value=Value)
                class Default
                    FPLerror = FPLWrapperError
                    call msg%Error(txt='Getting [Key="'//Key//'"]: Dimensions do not match. Value was not modified.', &
                               file=__FILE__, line=__LINE__ )
            end select
        else
            FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Getting [Key="'//Key//'"]: Not present. Value was not modified.', &
                           file=__FILE__, line=__LINE__ )
        endif
    end function ParameterList_Get6D


    function ParameterList_Get7D(this,Key,Value) result(FPLerror)
    !-----------------------------------------------------------------
    !< Return a 7D array Value given the Key
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN)    :: this                 !< Parameter List
        character(len=*),                     intent(IN)    :: Key                  !< String Key
        class(*),                             intent(INOUT) :: Value(:,:,:,:,:,:,:) !< Returned value
        class(*),                    pointer                :: Wrapper              !< Wrapper
        integer(I4P)                                        :: FPLerror             !< Error flag
    !-----------------------------------------------------------------
        FPLerror = FPLSuccess
        nullify(Wrapper)
        call this%Dictionary%GetPointer(Key=Key, Value=Wrapper)
        if(associated(Wrapper)) then
            select type(Wrapper)
                class is (DimensionsWrapper7D_t)
                    call Wrapper%Get(Value=Value)
                class Default
                    FPLerror = FPLWrapperError
                    call msg%Error(txt='Getting [Key="'//Key//'"]: Dimensions do not match. Value was not modified.', &
                               file=__FILE__, line=__LINE__ )
            end select
        else
            FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Getting [Key="'//Key//'"]: Not present. Value was not modified.', &
                           file=__FILE__, line=__LINE__ )
        endif
    end function ParameterList_Get7D


    function ParameterList_GetPointer0D(this,Key,Value) result(FPLerror)
    !-----------------------------------------------------------------
    !< Return a Unlimited polymorphic pointer to a Value given the Key
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN)    :: this    !< Parameter List
        character(len=*),                     intent(IN)    :: Key     !< String Key
        class(*), pointer,                    intent(INOUT) :: Value   !< Returned pointer to value
        class(*),                    pointer                :: Wrapper !< Wrapper
        integer(I4P)                                        :: FPLerror!< Error flag
    !-----------------------------------------------------------------
        FPLerror = FPLSuccess
        nullify(Wrapper)
        call this%Dictionary%GetPointer(Key=Key, Value=Wrapper)
        if(associated(Wrapper)) then
            select type(Wrapper)
                class is (DimensionsWrapper0D_t)
                    Value => Wrapper%GetPointer()
                class Default
                    FPLerror = FPLWrapperError
                    call msg%Error(txt='Getting [Key="'//Key//'"]: Dimensions do not match. Value was not modified.', &
                               file=__FILE__, line=__LINE__ )
            end select
        else
            FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Getting [Key="'//Key//'"]: Not present. Value was not modified.', &
                           file=__FILE__, line=__LINE__ )
        endif
    end function ParameterList_GetPointer0D


    function ParameterList_GetPointer1D(this,Key,Value) result(FPLerror)
    !-----------------------------------------------------------------
    !< Return a Unlimited polymorphic pointer to a Value given the Key
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN)    :: this     !< Parameter List
        character(len=*),                     intent(IN)    :: Key      !< String Key
        class(*), pointer,                    intent(INOUT) :: Value(:) !< Returned pointer to value
        class(*),                    pointer                :: Wrapper  !< Wrapper
        integer(I4P)                                        :: FPLerror !< Error flag
    !-----------------------------------------------------------------
        FPLerror = FPLSuccess
        nullify(Wrapper)
        call this%Dictionary%GetPointer(Key=Key, Value=Wrapper)
        if(associated(Wrapper)) then
            select type(Wrapper)
                class is (DimensionsWrapper1D_t)
                    Value => Wrapper%GetPointer()
                class Default
                    FPLerror = FPLWrapperError
                    call msg%Error(txt='Getting [Key="'//Key//'"]: Dimensions do not match. Value was not modified.', &
                               file=__FILE__, line=__LINE__ )
            end select
        else
            FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Getting [Key="'//Key//'"]: Not present. Value was not modified.', &
                           file=__FILE__, line=__LINE__ )
        endif
    end function ParameterList_GetPointer1D


    function ParameterList_GetPointer2D(this,Key,Value) result(FPLerror)
    !-----------------------------------------------------------------
    !< Return a Unlimited polymorphic pointer to a Value given the Key
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN)    :: this       !< Parameter List
        character(len=*),                     intent(IN)    :: Key        !< String Key
        class(*), pointer,                    intent(INOUT) :: Value(:,:) !< Returned pointer to value
        class(*),                    pointer                :: Wrapper    !< Wrapper
        integer(I4P)                                        :: FPLerror   !< Error flag
    !-----------------------------------------------------------------
        FPLerror = FPLSuccess
        nullify(Wrapper)
        call this%Dictionary%GetPointer(Key=Key, Value=Wrapper)
        if(associated(Wrapper)) then
            select type(Wrapper)
                class is (DimensionsWrapper2D_t)
                    Value => Wrapper%GetPointer()
                class Default
                    FPLerror = FPLWrapperError
                    call msg%Error(txt='Getting [Key="'//Key//'"]: Dimensions do not match. Value was not modified.', &
                               file=__FILE__, line=__LINE__ )
            end select
        else
            FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Getting [Key="'//Key//'"]: Not present. Value was not modified.', &
                           file=__FILE__, line=__LINE__ )
        endif
    end function ParameterList_GetPointer2D


    function ParameterList_GetPointer3D(this,Key,Value) result(FPLerror)
    !-----------------------------------------------------------------
    !< Return a Unlimited polymorphic pointer to a Value given the Key
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN)    :: this           !< Parameter List
        character(len=*),                     intent(IN)    :: Key            !< String Key
        class(*), pointer,                    intent(INOUT) :: Value(:,:,:)   !< Returned pointer to value
        class(*),                    pointer                :: Wrapper        !< Wrapper
        integer(I4P)                                        :: FPLerror       !< Error flag
    !-----------------------------------------------------------------
        FPLerror = FPLSuccess
        nullify(Wrapper)
        call this%Dictionary%GetPointer(Key=Key, Value=Wrapper)
        if(associated(Wrapper)) then
            select type(Wrapper)
                class is (DimensionsWrapper3D_t)
                    Value => Wrapper%GetPointer()
                class Default
                    FPLerror = FPLWrapperError
                    call msg%Error(txt='Getting [Key="'//Key//'"]: Dimensions do not match. Value was not modified.', &
                               file=__FILE__, line=__LINE__ )
            end select
        else
            FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Getting [Key="'//Key//'"]: Not present. Value was not modified.', &
                           file=__FILE__, line=__LINE__ )
        endif
    end function ParameterList_GetPointer3D


    function ParameterList_GetPointer4D(this,Key,Value) result(FPLerror)
    !-----------------------------------------------------------------
    !< Return a Unlimited polymorphic pointer to a Value given the Key
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN)    :: this           !< Parameter List
        character(len=*),                     intent(IN)    :: Key            !< String Key
        class(*), pointer,                    intent(INOUT) :: Value(:,:,:,:) !< Returned pointer to value
        class(*),                    pointer                :: Wrapper        !< Wrapper
        integer(I4P)                                        :: FPLerror       !< Error flag
    !-----------------------------------------------------------------
        FPLerror = FPLSuccess
        nullify(Wrapper)
        call this%Dictionary%GetPointer(Key=Key, Value=Wrapper)
        if(associated(Wrapper)) then
            select type(Wrapper)
                class is (DimensionsWrapper4D_t)
                    Value => Wrapper%GetPointer()
                class Default
                    FPLerror = FPLWrapperError
                    call msg%Error(txt='Getting [Key="'//Key//'"]: Dimensions do not match. Value was not modified.', &
                               file=__FILE__, line=__LINE__ )
            end select
        else
            FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Getting [Key="'//Key//'"]: Not present. Value was not modified.', &
                           file=__FILE__, line=__LINE__ )
        endif
    end function ParameterList_GetPointer4D


    function ParameterList_GetPointer5D(this,Key,Value) result(FPLerror)
    !-----------------------------------------------------------------
    !< Return a Unlimited polymorphic pointer to a Value given the Key
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN)    :: this             !< Parameter List
        character(len=*),                     intent(IN)    :: Key              !< String Key
        class(*), pointer,                    intent(INOUT) :: Value(:,:,:,:,:) !< Returned pointer to value
        class(*),                    pointer                :: Wrapper          !< Wrapper
        integer(I4P)                                        :: FPLerror         !< Error flag
    !-----------------------------------------------------------------
        FPLerror = FPLSuccess
        nullify(Wrapper)
        call this%Dictionary%GetPointer(Key=Key, Value=Wrapper)
        if(associated(Wrapper)) then
            select type(Wrapper)
                class is (DimensionsWrapper5D_t)
                    Value => Wrapper%GetPointer()
                class Default
                    FPLerror = FPLWrapperError
                    call msg%Error(txt='Getting [Key="'//Key//'"]: Dimensions do not match. Value was not modified.', &
                               file=__FILE__, line=__LINE__ )
            end select
        else
            FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Getting [Key="'//Key//'"]: Not present. Value was not modified.', &
                           file=__FILE__, line=__LINE__ )
        endif
    end function ParameterList_GetPointer5D


    function ParameterList_GetPointer6D(this,Key,Value) result(FPLerror)
    !-----------------------------------------------------------------
    !< Return a Unlimited polymorphic pointer to a Value given the Key
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN)    :: this               !< Parameter List
        character(len=*),                     intent(IN)    :: Key                !< String Key
        class(*), pointer,                    intent(INOUT) :: Value(:,:,:,:,:,:) !< Returned pointer to value
        class(*),                    pointer                :: Wrapper            !< Wrapper
        integer(I4P)                                        :: FPLerror           !< Error flag
    !-----------------------------------------------------------------
        FPLerror = FPLSuccess
        nullify(Wrapper)
        call this%Dictionary%GetPointer(Key=Key, Value=Wrapper)
        if(associated(Wrapper)) then
            select type(Wrapper)
                class is (DimensionsWrapper6D_t)
                    Value => Wrapper%GetPointer()
                class Default
                    FPLerror = FPLWrapperError
                    call msg%Error(txt='Getting [Key="'//Key//'"]: Dimensions do not match. Value was not modified.', &
                               file=__FILE__, line=__LINE__ )
            end select
        else
            FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Getting [Key="'//Key//'"]: Not present. Value was not modified.', &
                           file=__FILE__, line=__LINE__ )
        endif
    end function ParameterList_GetPointer6D


    function ParameterList_GetPointer7D(this,Key,Value) result(FPLerror)
    !-----------------------------------------------------------------
    !< Return a Unlimited polymorphic pointer to a Value given the Key
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN)    :: this                 !< Parameter List
        character(len=*),                     intent(IN)    :: Key                  !< String Key
        class(*), pointer,                    intent(INOUT) :: Value(:,:,:,:,:,:,:) !< Returned pointer to value
        class(*),                    pointer                :: Wrapper              !< Wrapper
        integer(I4P)                                        :: FPLerror             !< Error flag
    !-----------------------------------------------------------------
        FPLerror = FPLSuccess
        nullify(Wrapper)
        call this%Dictionary%GetPointer(Key=Key, Value=Wrapper)
        if(associated(Wrapper)) then
            select type(Wrapper)
                class is (DimensionsWrapper7D_t)
                    Value => Wrapper%GetPointer()
                class Default
                    FPLerror = FPLWrapperError
                    call msg%Error(txt='Getting [Key="'//Key//'"]: Dimensions do not match. Value was not modified.', &
                               file=__FILE__, line=__LINE__ )
            end select
        else
            FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Getting [Key="'//Key//'"]: Not present. Value was not modified.', &
                           file=__FILE__, line=__LINE__ )
        endif
    end function ParameterList_GetPointer7D


    function ParameterList_isPresent(this,Key) result(isPresent)
    !-----------------------------------------------------------------
    !< Check if a Key is present at the DataBase
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN) :: this      !< Parameter List
        character(len=*),                     intent(IN) :: Key       !< String Key
        logical                                          :: isPresent !< Boolean flag to check if a Key is present
    !-----------------------------------------------------------------
        isPresent = this%Dictionary%IsPresent(Key=Key)
    end function ParameterList_isPresent


    function ParameterList_isSubList(this,Key) result(isSubList)
    !-----------------------------------------------------------------
    !< Check if a Key is a SubList
    !-----------------------------------------------------------------
        class(ParameterList_t), intent(IN) :: this                    !< Parameter List
        character(len=*), intent(IN)    :: Key                        !< String Key
        class(*), pointer               :: SubListPointer             !< Pointer to a SubList
        logical                         :: isSubList                  !< Check if is a SubList
    !-----------------------------------------------------------------
        isSubList = .false.
        nullify(SubListPointer)
        call this%Dictionary%GetPointer(Key=Key, Value=SubListPointer)
        if(associated(SubListPointer)) then
            select type (SubListPointer)
                class is (ParameterList_t)
                        isSubList =.true.
            end select
        endif
    end function ParameterList_isSubList


    function ParameterList_DataSizeInBytes(this,Key) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the data size in bytes of the value associated with Key
    !-----------------------------------------------------------------
        class(ParameterList_t), intent(IN) :: this                    !< Parameter List
        character(len=*), intent(IN)    :: Key                        !< String Key
        class(*), pointer               :: Wrapper                    !< Wrapper
        integer(I4P)                    :: DataSizeInBytes            !< Size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = 0
        nullify(Wrapper)
        call this%Dictionary%GetPointer(Key=Key, Value=Wrapper)
        if(associated(Wrapper)) then
            select type (Wrapper)
                class is (DimensionsWrapper_t)
                    DataSizeInBytes = Wrapper%DataSizeInBytes()
            end select
        endif
    end function ParameterList_DataSizeInBytes


    function ParameterList_isOfDataType0D(this,Key, Mold) result(IsOfDataType)
    !-----------------------------------------------------------------
    !< Check if the data type of Mold agrees with the value associated with Key
    !-----------------------------------------------------------------
        class(ParameterList_t), intent(IN) :: this                    !< Parameter List
        character(len=*), intent(IN)    :: Key                        !< String Key
        class(*),         intent(IN)    :: Mold                       !< Mold
        class(*), pointer               :: Wrapper                    !< Wrapper
        logical                         :: isOfDataType               !< Check if has the same type
    !-----------------------------------------------------------------
        isOfDataType = .false.
        nullify(Wrapper)
        call this%Dictionary%GetPointer(Key=Key, Value=Wrapper)
        if(associated(Wrapper)) then
            select type (Wrapper)
                class is (DimensionsWrapper_t)
                    isOfDataType = Wrapper%isOfDataType(Mold=Mold)
                class is (ParameterList_t)
                    select type (Mold)
                        class is (ParameterList_t)
                            isOfDataType = .true.
                    end select
            end select
        endif
    end function ParameterList_isOfDataType0D


    function ParameterList_isOfDataType1D(this,Key, Mold) result(IsOfDataType)
    !-----------------------------------------------------------------
    !< Check if the data type of Mold agrees with the value associated with Key
    !-----------------------------------------------------------------
        class(ParameterList_t), intent(IN) :: this                    !< Parameter List
        character(len=*), intent(IN)    :: Key                        !< String Key
        class(*),         intent(IN)    :: Mold(1:)                   !< Mold
        class(*), pointer               :: Wrapper                    !< Wrapper
        logical                         :: isOfDataType               !< Check if has the same type
    !-----------------------------------------------------------------
        isOfDataType = .false.; nullify(Wrapper)
        call this%Dictionary%GetPointer(Key=Key, Value=Wrapper)
        if(associated(Wrapper)) then
            select type (Wrapper)
                class is (DimensionsWrapper_t)
                    isOfDataType = Wrapper%isOfDataType(Mold=Mold(1))
            end select
        endif
    end function ParameterList_isOfDataType1D


    function ParameterList_isOfDataType2D(this,Key, Mold) result(IsOfDataType)
    !-----------------------------------------------------------------
    !< Check if the data type of Mold agrees with the value associated with Key
    !-----------------------------------------------------------------
        class(ParameterList_t), intent(IN) :: this                    !< Parameter List
        character(len=*), intent(IN)    :: Key                        !< String Key
        class(*),         intent(IN)    :: Mold(1:,1:)                !< Mold
        class(*), pointer               :: Wrapper                    !< Wrapper
        logical                         :: isOfDataType               !< Check if has the same type
    !-----------------------------------------------------------------
        isOfDataType = .false.; nullify(Wrapper)
        call this%Dictionary%GetPointer(Key=Key, Value=Wrapper)
        if(associated(Wrapper)) then
            select type (Wrapper)
                class is (DimensionsWrapper_t)
                    isOfDataType = Wrapper%isOfDataType(Mold=Mold(1,1))
            end select
        endif
    end function ParameterList_isOfDataType2D


    function ParameterList_isOfDataType3D(this,Key, Mold) result(IsOfDataType)
    !-----------------------------------------------------------------
    !< Check if the data type of Mold agrees with the value associated with Key
    !-----------------------------------------------------------------
        class(ParameterList_t), intent(IN) :: this                    !< Parameter List
        character(len=*), intent(IN)    :: Key                        !< String Key
        class(*),         intent(IN)    :: Mold(1:,1:,1:)             !< Mold
        class(*), pointer               :: Wrapper                    !< Wrapper
        logical                         :: isOfDataType               !< Check if has the same type
    !-----------------------------------------------------------------
        isOfDataType = .false.; nullify(Wrapper)
        call this%Dictionary%GetPointer(Key=Key, Value=Wrapper)
        select type (Wrapper)
            class is (DimensionsWrapper_t)
                isOfDataType = Wrapper%isOfDataType(Mold=Mold(1,1,1))
        end select
    end function ParameterList_isOfDataType3D


    function ParameterList_isOfDataType4D(this,Key, Mold) result(IsOfDataType)
    !-----------------------------------------------------------------
    !< Check if the data type of Mold agrees with the value associated with Key
    !-----------------------------------------------------------------
        class(ParameterList_t), intent(IN) :: this                    !< Parameter List
        character(len=*), intent(IN)    :: Key                        !< String Key
        class(*),         intent(IN)    :: Mold(1:,1:,1:,1:)          !< Mold
        class(*), pointer               :: Wrapper                    !< Wrapper
        logical                         :: isOfDataType               !< Check if has the same type
    !-----------------------------------------------------------------
        isOfDataType = .false.; nullify(Wrapper)
        call this%Dictionary%GetPointer(Key=Key, Value=Wrapper)
        if(associated(Wrapper)) then
            select type (Wrapper)
                class is (DimensionsWrapper_t)
                    isOfDataType = Wrapper%isOfDataType(Mold=Mold(1,1,1,1))
            end select
        endif
    end function ParameterList_isOfDataType4D


    function ParameterList_isOfDataType5D(this,Key, Mold) result(IsOfDataType)
    !-----------------------------------------------------------------
    !< Check if the data type of Mold agrees with the value associated with Key
    !-----------------------------------------------------------------
        class(ParameterList_t), intent(IN) :: this                    !< Parameter List
        character(len=*), intent(IN)    :: Key                        !< String Key
        class(*),         intent(IN)    :: Mold(1:,1:,1:,1:,1:)       !< Mold
        class(*),  pointer              :: Wrapper                    !< Wrapper
        logical                         :: isOfDataType               !< Check if has the same type
    !-----------------------------------------------------------------
        isOfDataType = .false.; nullify(Wrapper)
        call this%Dictionary%GetPointer(Key=Key, Value=Wrapper)
        if(associated(Wrapper)) then
            select type (Wrapper)
                class is (DimensionsWrapper_t)
                    isOfDataType = Wrapper%isOfDataType(Mold=Mold(1,1,1,1,1))
            end select
        endif
    end function ParameterList_isOfDataType5D


    function ParameterList_isOfDataType6D(this,Key, Mold) result(IsOfDataType)
    !-----------------------------------------------------------------
    !< Check if the data type of Mold agrees with the value associated with Key
    !-----------------------------------------------------------------
        class(ParameterList_t), intent(IN) :: this                    !< Parameter List
        character(len=*), intent(IN)    :: Key                        !< String Key
        class(*),         intent(IN)    :: Mold(1:,1:,1:,1:,1:,1:)    !< Mold
        class(*), pointer               :: Wrapper                    !< Wrapper
        logical                         :: isOfDataType               !< Check if has the same type
    !-----------------------------------------------------------------
        isOfDataType = .false.; nullify(Wrapper)
        call this%Dictionary%GetPointer(Key=Key, Value=Wrapper)
        if(associated(Wrapper)) then
            select type (Wrapper)
                class is (DimensionsWrapper_t)
                    isOfDataType = Wrapper%isOfDataType(Mold=Mold(1,1,1,1,1,1))
            end select
        endif
    end function ParameterList_isOfDataType6D


    function ParameterList_isOfDataType7D(this,Key, Mold) result(IsOfDataType)
    !-----------------------------------------------------------------
    !< Check if the data type of Mold agrees with the value associated with Key
    !-----------------------------------------------------------------
        class(ParameterList_t), intent(IN) :: this                    !< Parameter List
        character(len=*), intent(IN)    :: Key                        !< String Key
        class(*),         intent(IN)    :: Mold(1:,1:,1:,1:,1:,1:,1:) !< Mold
        class(*), pointer               :: Wrapper                    !< Wrapper
        logical                         :: isOfDataType               !< Check if has the same type
    !-----------------------------------------------------------------
        isOfDataType = .false.; nullify(Wrapper)
        call this%Dictionary%GetPointer(Key=Key, Value=Wrapper)
        if(associated(Wrapper)) then
            select type (Wrapper)
                class is (DimensionsWrapper_t)
                    isOfDataType = Wrapper%isOfDataType(Mold=Mold(1,1,1,1,1,1,1))
            end select
        endif
    end function ParameterList_isOfDataType7D


    function ParameterList_isAssignable0D(this,Key,Value) result(Assignable)
    !-----------------------------------------------------------------
    !< Check if a stored variable is Assignable to Value
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN) :: this         !< Parameter List
        character(len=*),                     intent(IN) :: Key          !< String Key
        class(*),                             intent(IN) :: Value        !< Value to compare with the stored variable
        logical                                          :: Assignable   !< Boolean flag to check compatibility
        class(*),     pointer                            :: Wrapper      !< Wrapper
        integer(I4P), allocatable                        :: ValueShape(:)!< Shape of the stored value
    !-----------------------------------------------------------------
        Assignable = .false.
        nullify(Wrapper)
        ! Check if present
        call this%Dictionary%GetPointer(Key=Key, Value=Wrapper)
        if(associated(Wrapper)) then
            select type (Wrapper)
                class is (DimensionsWrapper0D_t)
                    ! Check same data type
                    if(Wrapper%isOfDataType(Mold=Value)) Assignable = .true.
            end select
        endif
    end function ParameterList_isAssignable0D


    function ParameterList_isAssignable1D(this,Key,Value) result(Assignable)
    !-----------------------------------------------------------------
    !< Check if a stored variable is Assignable to Value
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN) :: this         !< Parameter List
        character(len=*),                     intent(IN) :: Key          !< String Key
        class(*),                             intent(IN) :: Value(1:)    !< Value to check against with the stored variable
        logical                                          :: Assignable   !< Boolean flag to check compatibility
        class(*),     pointer                            :: Wrapper      !< Wrapper
        integer(I4P), allocatable                        :: ValueShape(:)!< Shape of the stored value
    !-----------------------------------------------------------------
        Assignable = .false.
        nullify(Wrapper)
        ! Check if present
        call this%Dictionary%GetPointer(Key=Key,Value=Wrapper)
        if(associated(Wrapper)) then
            select type (Wrapper)
                class is (DimensionsWrapper1D_t)
                    ! Check same data type
                    if(Wrapper%isOfDataType(Mold=Value(1))) then
                        call Wrapper%GetShape(ValueShape)
                        ! Check right shape
                        if(all(ValueShape == shape(Value))) Assignable = .true.
                    endif
            end select
        endif
    end function ParameterList_isAssignable1D


    function ParameterList_isAssignable2D(this,Key,Value) result(Assignable)
    !-----------------------------------------------------------------
    !< Check if a stored variable is Assignable to Value
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN) :: this         !< Parameter List
        character(len=*),                     intent(IN) :: Key          !< String Key
        class(*),                             intent(IN) :: Value(1:,1:) !< Value to check against with the stored variable
        logical                                          :: Assignable   !< Boolean flag to check compatibility
        class(*),     pointer                            :: Wrapper      !< Wrapper
        integer(I4P), allocatable                        :: ValueShape(:)!< Shape of the stored value
    !-----------------------------------------------------------------
        Assignable = .false.
        nullify(Wrapper)
        ! Check if present
        call this%Dictionary%GetPointer(Key=Key, Value=Wrapper)
        if(associated(Wrapper)) then
            select type (Wrapper)
                class is (DimensionsWrapper2D_t)
                    ! Check same data type
                    if(Wrapper%isOfDataType(Mold=Value(1,1))) then
                        call Wrapper%GetShape(ValueShape)
                        ! Check right shape
                        if(all(ValueShape == shape(Value))) Assignable = .true.
                    endif
            end select
        endif
    end function ParameterList_isAssignable2D


    function ParameterList_isAssignable3D(this,Key,Value) result(Assignable)
    !-----------------------------------------------------------------
    !< Check if a stored variable is Assignable to Value
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN) :: this            !< Parameter List
        character(len=*),                     intent(IN) :: Key             !< String Key
        class(*),                             intent(IN) :: Value(1:,1:,1:) !< Value to check against with the stored variable
        logical                                          :: Assignable      !< Boolean flag to check compatibility
        class(*),     pointer                            :: Wrapper         !< Wrapper
        integer(I4P), allocatable                        :: ValueShape(:)   !< Shape of the stored value
    !-----------------------------------------------------------------
        Assignable = .false.
        nullify(Wrapper)
        ! Check if present
        call this%Dictionary%GetPointer(Key=Key, Value=Wrapper)
        if(associated(Wrapper)) then
            select type (Wrapper)
                class is (DimensionsWrapper3D_t)
                    ! Check same data type
                    if(Wrapper%isOfDataType(Mold=Value(1,1,1))) then
                        call Wrapper%GetShape(ValueShape)
                        ! Check right shape
                        if(all(ValueShape == shape(Value))) Assignable = .true.
                    endif
            end select
        endif
    end function ParameterList_isAssignable3D


    function ParameterList_isAssignable4D(this,Key,Value) result(Assignable)
    !-----------------------------------------------------------------
    !< Check if a stored variable is Assignable to Value
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN) :: this               !< Parameter List
        character(len=*),                     intent(IN) :: Key                !< String Key
        class(*),                             intent(IN) :: Value(1:,1:,1:,1:) !< Value to check against the stored variable
        logical                                          :: Assignable         !< Boolean flag to check compatibility
        class(*),     pointer                            :: Wrapper            !< Wrapper
        integer(I4P), allocatable                        :: ValueShape(:)      !< Shape of the stored value
    !-----------------------------------------------------------------
        Assignable = .false.
        nullify(Wrapper)
        ! Check if present
        call this%Dictionary%GetPointer(Key=Key, Value=Wrapper)
        if(associated(Wrapper)) then
            select type (Wrapper)
                class is (DimensionsWrapper4D_t)
                    ! Check same data type
                    if(Wrapper%isOfDataType(Mold=Value(1,1,1,1))) then
                        call Wrapper%GetShape(ValueShape)
                        ! Check right shape
                        if(all(ValueShape == shape(Value))) Assignable = .true.
                    endif
            end select
        endif
    end function ParameterList_isAssignable4D


    function ParameterList_isAssignable5D(this,Key,Value) result(Assignable)
    !-----------------------------------------------------------------
    !< Check if a stored variable is Assignable to Value
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN) :: this                  !< Parameter List
        character(len=*),                     intent(IN) :: Key                   !< String Key
        class(*),                             intent(IN) :: Value(1:,1:,1:,1:,1:) !< Value to check against the stored variable
        logical                                          :: Assignable            !< Boolean flag to check compatibility
        class(*),     pointer                            :: Wrapper               !< Wrapper
        integer(I4P), allocatable                        :: ValueShape(:)         !< Shape of the stored value
    !-----------------------------------------------------------------
        Assignable = .false.
        nullify(Wrapper)
        ! Check if present
        call this%Dictionary%GetPointer(Key=Key, Value=Wrapper)
        if(associated(Wrapper)) then
            select type (Wrapper)
                class is (DimensionsWrapper5D_t)
                    ! Check same data type
                    if(Wrapper%isOfDataType(Mold=Value(1,1,1,1,1))) then
                        call Wrapper%GetShape(ValueShape)
                        ! Check right shape
                        if(all(ValueShape == shape(Value))) Assignable = .true.
                    endif
            end select
        endif
    end function ParameterList_isAssignable5D


    function ParameterList_isAssignable6D(this,Key, Value) result(Assignable)
    !-----------------------------------------------------------------
    !< Check if a stored variable is Assignable to Value
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN) :: this                     !< Parameter List
        character(len=*),                     intent(IN) :: Key                      !< String Key
        class(*),                             intent(IN) :: Value(1:,1:,1:,1:,1:,1:) !< Value to check against the stored variable
        logical                                          :: Assignable               !< Boolean flag to check compatibility
        class(*),     pointer                            :: Wrapper                  !< Wrapper
        integer(I4P), allocatable                        :: ValueShape(:)            !< Shape of the stored value
    !-----------------------------------------------------------------
        Assignable = .false.
        nullify(Wrapper)
        ! Check if present
        call this%Dictionary%GetPointer(Key=Key, Value=Wrapper)
        if(associated(Wrapper)) then
            select type (Wrapper)
                class is (DimensionsWrapper6D_t)
                    ! Check same data type
                    if(Wrapper%isOfDataType(Mold=Value(1,1,1,1,1,1))) then
                        call Wrapper%GetShape(ValueShape)
                        ! Check right shape
                        if(all(ValueShape == shape(Value))) Assignable = .true.
                    endif
            end select
        endif
    end function ParameterList_isAssignable6D


    function ParameterList_isAssignable7D(this,Key,Value) result(Assignable)
    !-----------------------------------------------------------------
    !< Check if a stored variable is Assignable to Value
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN) :: this                        !< Parameter List
        character(len=*),                     intent(IN) :: Key                         !< String Key
        class(*),                             intent(IN) :: Value(1:,1:,1:,1:,1:,1:,1:) !< Value to check against the stored variable
        logical                                          :: Assignable                  !< Boolean flag to check compatibility
        class(*),     pointer                            :: Wrapper                     !< Wrapper
        integer(I4P), allocatable                        :: ValueShape(:)               !< Shape of the stored value
    !-----------------------------------------------------------------
        Assignable = .false.
        nullify(Wrapper)
        ! Check if present
        call this%Dictionary%GetPointer(Key=Key, Value=Wrapper)
        if(associated(Wrapper)) then
            select type (Wrapper)
                class is (DimensionsWrapper7D_t)
                    ! Check same data type
                    if(Wrapper%isOfDataType(Mold=Value(1,1,1,1,1,1,1))) then
                        call Wrapper%GetShape(ValueShape)
                        ! Check right shape
                        if(all(ValueShape == shape(Value))) Assignable = .true.
                    endif
            end select
        endif
    end function ParameterList_isAssignable7D


    subroutine ParameterList_RemoveEntry(this, Key)
    !-----------------------------------------------------------------
    !< Remove an Entry given a Key
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(INOUT) :: this   !< Parameter List
        character(len=*),                     intent(IN)    :: Key    !< String Key
    !-----------------------------------------------------------------
        call this%Dictionary%Del(Key=Key)
    end subroutine ParameterList_RemoveEntry


    function ParameterList_Length(this) result(Length)
    !-----------------------------------------------------------------
    !< Return the number of ParameterListEntries contained in the DataBase
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN) :: this       !< Parameter List
        integer(I4P)                                     :: Length     !< Number of parameters in database
    !-----------------------------------------------------------------
        Length = this%Dictionary%Length()
    end function ParameterList_Length


    function ParameterList_GetIterator(this) result(Iterator)
    !-----------------------------------------------------------------
    !< Return a pointer to a Parameters Iterator
    !-----------------------------------------------------------------
        class(ParameterList_t),     intent(IN) :: this                !< Parameter List Entry Container Type
        type(ParameterListIterator_t)          :: Iterator            !< Parameter List iterator
    !-----------------------------------------------------------------
        call Iterator%Init(DataBase=this%Dictionary%GetDataBase())
    end function ParameterList_GetIterator


    function ParameterList_GetAsString(this,Key,String,Separator) result(FPLerror)
    !-----------------------------------------------------------------
    !< Return a scalar Value given the Key
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN)    :: this           !< Parameter List
        character(len=*),                     intent(IN)    :: Key            !< String Key
        character(len=:), allocatable,        intent(INOUT) :: String         !< Returned value as string
        character(len=1), optional,           intent(IN)    :: Separator      !< Array values separator
        class(*),                    pointer                :: Wrapper        !< Wrapper
        integer(I4P)                                        :: FPLerror       !< Error flag
    !-----------------------------------------------------------------
        FPLerror = FPLSuccess
        nullify(Wrapper)
        call this%Dictionary%GetPointer(Key=Key, Value=Wrapper)
        if(associated(Wrapper)) then
            select type(Wrapper)
                class is (DimensionsWrapper_t)
                    call Wrapper%toString(String=String, Separator=Separator)
                class Default
                    FPLerror = FPLWrapperError
                    call msg%Error(txt='Getting [Key="'//Key//'"]: Unknown Wrapper. Value was not modified.', &
                               file=__FILE__, line=__LINE__ )
            end select
        else
            FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Getting [Key="'//Key//'"]: Not present. Value was not modified.', &
                           file=__FILE__, line=__LINE__ )
        endif
    end function ParameterList_GetAsString

    subroutine ParameterList_Display(this, msg, unitno)
    !-----------------------------------------------------------------
    !< Print the content of the DataBase
    !-----------------------------------------------------------------
        class(ParameterList_t), intent( in ) :: this
        character( len = * ), intent( in ) :: msg
        integer( i4p ), optional, intent( in ) :: unitno
        call this%print(unitno, msg )
    end subroutine ParameterList_Display

    recursive subroutine ParameterList_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print the content of the DataBase
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN)  :: this    !< Linked List
        integer(I4P), optional,               intent(IN)  :: unit    !< Logic unit.
        character(*), optional,               intent(IN)  :: prefix  !< Prefixing string.
        integer(I4P), optional,               intent(OUT) :: iostat  !< IO error.
        character(*), optional,               intent(OUT) :: iomsg   !< IO error message.
        character(len=:), allocatable                     :: prefd   !< Prefixing string.
        integer(I4P)                                      :: unitd   !< Logic unit.
        integer(I4P)                                      :: iostatd !< IO error.
        character(500)                                    :: iomsgd  !< Temporary variable for IO error message.
        type(ParameterListIterator_t)                     :: Iterator!< Dictionary Iterator
        class(*), pointer                                 :: Value
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        unitd = OUTPUT_UNIT; if(present(unit)) unitd = unit
        Iterator = this%GetIterator()
        do while(.not. Iterator%HasFinished())
            Nullify(Value)
            Value => Iterator%PointToValue()
            if(associated(Value)) then
                select type(Value)
                    class is (DimensionsWrapper_t)
                        call Value%Print(unit=unitd,                                                  &
                                         prefix=prefd//                                               &
                                         '['//trim(str(no_sign=.true., n=Iterator%GetIndex()))//']'// &
                                         ' Key = '//Iterator%GetKey()//',',                           &
                                         iostat=iostatd,                                              &
                                         iomsg=iomsgd)
                    type is (ParameterList_t)
                        write(unit=unitd, fmt='(A)') prefd//                                                      &
                                                     '['//trim(str(no_sign=.true., n=Iterator%GetIndex()))//']'// &
                                                     ' Key = '//Iterator%GetKey()//', Data Type = ParameterList'
                        call Value%Print(unit=unitd, prefix=prefd//'['//trim(str(no_sign=.true., n=Iterator%GetIndex()))//'] ', iostat=iostatd, iomsg=iomsgd)
                    class DEFAULT
                        write(unit=unitd, fmt='(A)') prefd//                                                      &
                                                     '['//trim(str(no_sign=.true., n=Iterator%GetIndex()))//']'// &
                                                     ' Key = '//Iterator%GetKey()//', Data Type = Unknown Data Type!'
                end select
            endif
            call Iterator%Next()
        enddo
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine ParameterList_Print


!---------------------------------------------------------------------
!< Parameter List Iterator Procedures
!---------------------------------------------------------------------

    subroutine ParameterListIterator_Assignment(this, ParameterListIterator)
    !-----------------------------------------------------------------
    !< Dictionary iterator Assignment
    !-----------------------------------------------------------------
        class(ParameterListIterator_t), intent(INOUT) :: this                  ! Output Dictionary iterator
        type(ParameterListIterator_t),  intent(IN)    :: ParameterListIterator ! Input Dictionary iterator
    !-----------------------------------------------------------------
        this%DataBase(0:)      => ParameterListIterator%DataBase
        this%EntryListIterator =  ParameterListIterator%EntryListIterator
        this%Index             =  ParameterListIterator%Index
        this%UpperBound        =  ParameterListIterator%UpperBound
    end subroutine ParameterListIterator_Assignment


    subroutine ParameterListIterator_Free(this)
    !-----------------------------------------------------------------
    !< Free the dictionary iterator
    !-----------------------------------------------------------------
        class(ParameterListIterator_t), intent(INOUT) :: this         ! Dictionary iterator
    !-----------------------------------------------------------------
        this%Index      = 0
        this%UpperBound = 0
        nullify(this%DataBase)
        call this%EntryListIterator%Free()
    end subroutine ParameterListIterator_Free


    subroutine ParameterListIterator_Final(this)
    !-----------------------------------------------------------------
    !< Free the dictionary iterator
    !-----------------------------------------------------------------
        type(ParameterListIterator_t), intent(INOUT) :: this          ! Dictionary iterator
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine ParameterListIterator_Final


    subroutine ParameterListIterator_Init(this, DataBase)
    !-----------------------------------------------------------------
    !< Associate the iterator with a dictionary and rewind
    !< to the first position
    !-----------------------------------------------------------------
        class(ParameterListIterator_t),     intent(INOUT) :: this        ! Dictionary iterator
        type(ParameterRootEntry_t), target, intent(IN)    :: DataBase(:) ! Entries database
    !-----------------------------------------------------------------
        call this%Free()
        this%DataBase(0:) => DataBase(:)
        this%Index = -1
        this%UpperBound = size(this%DataBase)
        call this%Next()
    end subroutine ParameterListIterator_Init


    subroutine ParameterListIterator_Begin(this)
    !-----------------------------------------------------------------
    !< Rewind the iterator to the first dictionary position
    !-----------------------------------------------------------------
        class(ParameterListIterator_t),     intent(INOUT) :: this        ! Dictionary iterator
        type(ParameterRootEntry_t), pointer               :: DataBase(:) ! Entries database
    !-----------------------------------------------------------------
        DataBase => this%DataBase
        call this%Init(DataBase)
    end subroutine ParameterListIterator_Begin


    subroutine ParameterListIterator_End(this)
    !-----------------------------------------------------------------
    !< Fast forward to the last dictionary position (HasFinished = .true.)
    !-----------------------------------------------------------------
        class(ParameterListIterator_t),     intent(INOUT) :: this        ! Dictionary iterator
    !-----------------------------------------------------------------
        this%Index = this%UpperBound
        call this%EntryListIterator%Free()
    end subroutine ParameterListIterator_End


    subroutine ParameterListIterator_NextNotEmptyListIterator(this)
    !-----------------------------------------------------------------
    !< The iterator points to the next associated entry
    !-----------------------------------------------------------------
        class(ParameterListIterator_t),  intent(INOUT) :: this        ! Dictionary iterator
    !-----------------------------------------------------------------
        call this%EntryListIterator%Free()
        this%Index = this%Index + 1
        do while(this%Index < this%UpperBound)
            if(this%DataBase(this%Index)%HasRoot()) then
                this%EntryListIterator = this%Database(this%Index)%GetIterator()
                exit
            endif
            this%Index = this%Index + 1
        enddo
    end subroutine ParameterListIterator_NextNotEmptyListIterator


    subroutine ParameterListIterator_Next(this)
    !-----------------------------------------------------------------
    !< The iterator points to the next associated entry
    !-----------------------------------------------------------------
        class(ParameterListIterator_t),  intent(INOUT) :: this        ! Dictionary iterator
    !-----------------------------------------------------------------
        if(.not. this%HasFinished()) then
            if(.not. this%EntryListIterator%HasFinished()) then
                call this%EntryListIterator%Next()
            else
                call this%NextNotEmptyListIterator()
            endif
        endif
    end subroutine ParameterListIterator_Next


    function ParameterListIterator_GetEntry(this) result(CurrentEntry)
    !-----------------------------------------------------------------
    !< Return the current Entry
    !-----------------------------------------------------------------
        class(ParameterListIterator_t), intent(IN) :: this            ! Dictionary iterator
        type(ParameterEntry_t),  pointer           :: CurrentEntry    ! Current entry
        integer(I4P)                               :: FPLerror       !< Error flag
    !-----------------------------------------------------------------
        nullify(CurrentEntry)
        CurrentEntry => this%EntryListIterator%GetEntry()
        if(.not. associated(CurrentEntry)) then
            FPLerror = FPLParameterListIteratorError
            call msg%Error(txt='Current entry not associated. Shape was not modified.', &
                           file=__FILE__, line=__LINE__ )
        endif
    end function ParameterListIterator_GetEntry


    function ParameterListIterator_PointToValue(this) result(Value)
    !-----------------------------------------------------------------
    !< Return a pointer to the value stored in the current Entry
    !-----------------------------------------------------------------
        class(ParameterListIterator_t), intent(IN) :: this            ! Dictionary iterator
        class(*), pointer                          :: Value           ! Unlimited polymorphic pointer
        type(ParameterEntry_t),  pointer           :: CurrentEntry    ! Current entry
    !-----------------------------------------------------------------
        nullify(CurrentEntry)
        nullify(Value)
        CurrentEntry => this%GetEntry()
        if(associated(CurrentEntry)) Value => CurrentEntry%PointToValue()
    end function ParameterListIterator_PointToValue


    function ParameterListIterator_GetKey(this) result(Key)
    !-----------------------------------------------------------------
    !< Return the Key of the current Entry
    !-----------------------------------------------------------------
        class(ParameterListIterator_t), intent(IN) :: this            ! Dictionary iterator
        character(len=:), allocatable              :: Key             ! Key
        type(ParameterEntry_t),  pointer           :: CurrentEntry    ! Current entry
    !-----------------------------------------------------------------
        nullify(CurrentEntry)
        CurrentEntry => this%GetEntry()
        if(associated(CurrentEntry)) call CurrentEntry%GetKey(Key)
    end function ParameterListIterator_GetKey


    function ParameterListIterator_GetIndex(this) result(CurrentIndex)
    !-----------------------------------------------------------------
    !< Return the current Index
    !-----------------------------------------------------------------
        class(ParameterListIterator_t), intent(IN) :: this            ! Dictionary iterator
        integer(I4P)                               :: CurrentIndex    ! Current index
    !-----------------------------------------------------------------
        CurrentIndex = this%Index
    end function ParameterListIterator_GetIndex


    function ParameterListIterator_GetShape(this, Shape) result(FPLError)
    !-----------------------------------------------------------------
    !< Return an allocatable array with the shape of the contained value
    !-----------------------------------------------------------------
        class(ParameterListIterator_t),       intent(IN)    :: this     !< Parameter List Iterator
        integer(I4P), allocatable,            intent(INOUT) :: Shape(:) !< Shape of the stored value
        class(*),                    pointer                :: Wrapper  !< Wrapper
        integer(I4P)                                        :: FPLerror !< Error flag
    !-----------------------------------------------------------------
        FPLerror = FPLSuccess
        nullify(Wrapper)
        Wrapper => this%PointToValue()
        if(associated(Wrapper)) then
            select type(Wrapper)
                class is (DimensionsWrapper_t)
                    call Wrapper%GetShape(Shape)
                class Default
                    FPLerror = FPLWrapperError
                    call msg%Error(txt='Getting [Key="'//this%GetKey()//'"]: Unknown Wrapper. Shape was not modified.', &
                                   file=__FILE__, line=__LINE__ )
            end select
        else
            FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Getting [Key="'//this%GetKey()//'"]: Not present. Shape was not modified.', &
                           file=__FILE__, line=__LINE__ )
        endif
    end function ParameterListIterator_GetShape


    function ParameterListIterator_GetDimensions(this) result(Dimensions)
    !-----------------------------------------------------------------
    !< Return an allocatable array with the shape of the contained value
    !-----------------------------------------------------------------
        class(ParameterListIterator_t),       intent(IN)    :: this       !< Parameter List Iterator
        integer(I4P)                                        :: Dimensions !< Dimensions of the stored value
        class(*),                    pointer                :: Wrapper    !< Wrapper
        integer(I4P)                                        :: FPLerror   !< Error flag
    !-----------------------------------------------------------------
        FPLerror = FPLSuccess
        Dimensions = 0
        nullify(Wrapper)
        Wrapper => this%PointToValue()
        if(associated(Wrapper)) then
            select type(Wrapper)
                class is (DimensionsWrapper_t)
                    Dimensions = Wrapper%GetDimensions()
                class Default
                    FPLerror = FPLWrapperError
                    call msg%Error(txt='Getting [Key="'//this%GetKey()//'"]: Unknown Wrapper. Shape was not modified.', &
                                   file=__FILE__, line=__LINE__ )
            end select
        else
            FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Getting [Key="'//this%GetKey()//'"]: Not present. Shape was not modified.', &
                           file=__FILE__, line=__LINE__ )
        endif
    end function ParameterListIterator_GetDimensions


    function ParameterListIterator_GetAsString(this,String,Separator) result(FPLerror)
    !-----------------------------------------------------------------
    !< Return the current value converted into a string
    !-----------------------------------------------------------------
        class(ParameterListIterator_t),       intent(IN)    :: this           !< Parameter List Iterator
        character(len=:), allocatable,        intent(INOUT) :: String         !< Returned string
        character(len=1), optional,           intent(IN)    :: Separator      !< Array values separator
        class(*),                    pointer                :: Wrapper        !< Wrapper
        integer(I4P)                                        :: FPLerror       !< Error flag
    !-----------------------------------------------------------------
        FPLerror = FPLSuccess
        nullify(Wrapper)
        Wrapper => this%PointToValue()
        if(associated(Wrapper)) then
            select type(Wrapper)
                class is (DimensionsWrapper_t)
                    call Wrapper%ToString(String=String, Separator=Separator)
                class Default
                    FPLerror = FPLWrapperError
                    call msg%Error(txt='Getting [Key="'//this%GetKey()//'"]: Unknown Wrapper. Value was not modified.', &
                               file=__FILE__, line=__LINE__ )
            end select
        else
            FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Getting [Key="'//this%GetKey()//'"]: Not present. Value was not modified.', &
                           file=__FILE__, line=__LINE__ )
        endif
    end function ParameterListIterator_GetAsString


    function ParameterListIterator_Get0D(this,Value) result(FPLerror)
    !-----------------------------------------------------------------
    !< Return the current value
    !-----------------------------------------------------------------
        class(ParameterListIterator_t),       intent(IN)    :: this           !< Parameter List Iterator
        class(*),                             intent(INOUT) :: Value          !< Returned value
        class(*),                    pointer                :: Wrapper        !< Wrapper
        integer(I4P)                                        :: FPLerror       !< Error flag
    !-----------------------------------------------------------------
        FPLerror = FPLSuccess
        nullify(Wrapper)
        Wrapper => this%PointToValue()
        if(associated(Wrapper)) then
            select type(Wrapper)
                class is (DimensionsWrapper0D_t)
                    call Wrapper%Get(Value=Value)
                class Default
                    FPLerror = FPLWrapperError
                    call msg%Error(txt='Getting [Key="'//this%GetKey()//'"]: Dimensions do not match. Value was not modified.', &
                                   file=__FILE__, line=__LINE__ )
            end select
        else
            FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Getting [Key="'//this%GetKey()//'"]: Not present. Value was not modified.', &
                           file=__FILE__, line=__LINE__ )
        endif
    end function ParameterListIterator_Get0D


    function ParameterListIterator_Get1D(this,Value) result(FPLerror)
    !-----------------------------------------------------------------
    !< Return the current value
    !-----------------------------------------------------------------
        class(ParameterListIterator_t),       intent(IN)    :: this           !< Parameter List Iterator
        class(*),                             intent(INOUT) :: Value(:)       !< Returned value
        class(*),                    pointer                :: Wrapper        !< Wrapper
        integer(I4P)                                        :: FPLerror       !< Error flag
    !-----------------------------------------------------------------
        FPLerror = FPLSuccess
        nullify(Wrapper)
        Wrapper => this%PointToValue()
        if(associated(Wrapper)) then
            select type(Wrapper)
                class is (DimensionsWrapper1D_t)
                    call Wrapper%Get(Value=Value)
                class Default
                    FPLerror = FPLWrapperError
                    call msg%Error(txt='Getting [Key="'//this%GetKey()//'"]: Dimensions do not match. Value was not modified.', &
                                   file=__FILE__, line=__LINE__ )
            end select
        else
            FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Getting [Key="'//this%GetKey()//'"]: Not present. Value was not modified.', &
                           file=__FILE__, line=__LINE__ )
        endif
    end function ParameterListIterator_Get1D


    function ParameterListIterator_Get2D(this,Value) result(FPLerror)
    !-----------------------------------------------------------------
    !< Return the current value
    !-----------------------------------------------------------------
        class(ParameterListIterator_t),       intent(IN)    :: this           !< Parameter List Iterator
        class(*),                             intent(INOUT) :: Value(:,:)     !< Returned value
        class(*),                    pointer                :: Wrapper        !< Wrapper
        integer(I4P)                                        :: FPLerror       !< Error flag
    !-----------------------------------------------------------------
        FPLerror = FPLSuccess
        nullify(Wrapper)
        Wrapper => this%PointToValue()
        if(associated(Wrapper)) then
            select type(Wrapper)
                class is (DimensionsWrapper2D_t)
                    call Wrapper%Get(Value=Value)
                class Default
                    FPLerror = FPLWrapperError
                    call msg%Error(txt='Getting [Key="'//this%GetKey()//'"]: Dimensions do not match. Value was not modified.', &
                                   file=__FILE__, line=__LINE__ )
            end select
        else
            FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Getting [Key="'//this%GetKey()//'"]: Not present. Value was not modified.', &
                           file=__FILE__, line=__LINE__ )
        endif
    end function ParameterListIterator_Get2D


    function ParameterListIterator_Get3D(this,Value) result(FPLerror)
    !-----------------------------------------------------------------
    !< Return the current value
    !-----------------------------------------------------------------
        class(ParameterListIterator_t),       intent(IN)    :: this           !< Parameter List Iterator
        class(*),                             intent(INOUT) :: Value(:,:,:)   !< Returned value
        type(ParameterEntry_t),      pointer                :: CurrentEntry   !< Current entry
        class(*),                    pointer                :: Wrapper        !< Wrapper
        integer(I4P)                                        :: FPLerror       !< Error flag
    !-----------------------------------------------------------------
        FPLerror = FPLSuccess
        nullify(Wrapper)
        Wrapper => this%PointToValue()
        if(associated(Wrapper)) then
            select type(Wrapper)
                class is (DimensionsWrapper3D_t)
                    call Wrapper%Get(Value=Value)
                class Default
                    FPLerror = FPLWrapperError
                    call msg%Error(txt='Getting [Key="'//this%GetKey()//'"]: Dimensions do not match. Value was not modified.', &
                                   file=__FILE__, line=__LINE__ )
            end select
        else
            FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Getting [Key="'//this%GetKey()//'"]: Not present. Value was not modified.', &
                           file=__FILE__, line=__LINE__ )
        endif
    end function ParameterListIterator_Get3D


    function ParameterListIterator_Get4D(this,Value) result(FPLerror)
    !-----------------------------------------------------------------
    !< Return the current value
    !-----------------------------------------------------------------
        class(ParameterListIterator_t),       intent(IN)    :: this           !< Parameter List Iterator
        class(*),                             intent(INOUT) :: Value(:,:,:,:) !< Returned value
        class(*),                    pointer                :: Wrapper        !< Wrapper
        integer(I4P)                                        :: FPLerror       !< Error flag
    !-----------------------------------------------------------------
        FPLerror = FPLSuccess
        nullify(Wrapper)
        Wrapper => this%PointToValue()
        if(associated(Wrapper)) then
            select type(Wrapper)
                class is (DimensionsWrapper4D_t)
                    call Wrapper%Get(Value=Value)
                class Default
                    FPLerror = FPLWrapperError
                    call msg%Error(txt='Getting [Key="'//this%GetKey()//'"]: Dimensions do not match. Value was not modified.', &
                                   file=__FILE__, line=__LINE__ )
            end select
        else
            FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Getting [Key="'//this%GetKey()//'"]: Not present. Value was not modified.', &
                           file=__FILE__, line=__LINE__ )
        endif
    end function ParameterListIterator_Get4D


    function ParameterListIterator_Get5D(this,Value) result(FPLerror)
    !-----------------------------------------------------------------
    !< Return the current value
    !-----------------------------------------------------------------
        class(ParameterListIterator_t),       intent(IN)    :: this             !< Parameter List Iterator
        class(*),                             intent(INOUT) :: Value(:,:,:,:,:) !< Returned value
        type(ParameterEntry_t),      pointer                :: CurrentEntry     !< Current entry
        class(*),                    pointer                :: Wrapper          !< Wrapper
        integer(I4P)                                        :: FPLerror         !< Error flag
    !-----------------------------------------------------------------
        FPLerror = FPLSuccess
        nullify(Wrapper)
        Wrapper => this%PointToValue()
        if(associated(Wrapper)) then
            select type(Wrapper)
                class is (DimensionsWrapper5D_t)
                    call Wrapper%Get(Value=Value)
                class Default
                    FPLerror = FPLWrapperError
                    call msg%Error(txt='Getting [Key="'//this%GetKey()//'"]: Dimensions do not match. Value was not modified.', &
                                   file=__FILE__, line=__LINE__ )
            end select
        else
            FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Getting [Key="'//this%GetKey()//'"]: Not present. Value was not modified.', &
                           file=__FILE__, line=__LINE__ )
        endif
    end function ParameterListIterator_Get5D


    function ParameterListIterator_Get6D(this,Value) result(FPLerror)
    !-----------------------------------------------------------------
    !< Return the current value
    !-----------------------------------------------------------------
        class(ParameterListIterator_t),       intent(IN)    :: this               !< Parameter List Iterator
        class(*),                             intent(INOUT) :: Value(:,:,:,:,:,:) !< Returned value
        type(ParameterEntry_t),      pointer                :: CurrentEntry       !< Current entry
        class(*),                    pointer                :: Wrapper            !< Wrapper
        integer(I4P)                                        :: FPLerror           !< Error flag
    !-----------------------------------------------------------------
        FPLerror = FPLSuccess
        nullify(Wrapper)
        Wrapper => this%PointToValue()
        if(associated(Wrapper)) then
            select type(Wrapper)
                class is (DimensionsWrapper6D_t)
                    call Wrapper%Get(Value=Value)
                class Default
                    FPLerror = FPLWrapperError
                    call msg%Error(txt='Getting [Key="'//this%GetKey()//'"]: Dimensions do not match. Value was not modified.', &
                                   file=__FILE__, line=__LINE__ )
            end select
        else
            FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Getting [Key="'//this%GetKey()//'"]: Not present. Value was not modified.', &
                           file=__FILE__, line=__LINE__ )
        endif
    end function ParameterListIterator_Get6D


    function ParameterListIterator_Get7D(this,Value) result(FPLerror)
    !-----------------------------------------------------------------
    !< Return the current value
    !-----------------------------------------------------------------
        class(ParameterListIterator_t),       intent(IN)    :: this                 !< Parameter List Iterator
        class(*),                             intent(INOUT) :: Value(:,:,:,:,:,:,:) !< Returned value
        type(ParameterEntry_t),      pointer                :: CurrentEntry         !< Current entry
        class(*),                    pointer                :: Wrapper              !< Wrapper
        integer(I4P)                                        :: FPLerror             !< Error flag
    !-----------------------------------------------------------------
        FPLerror = FPLSuccess
        nullify(Wrapper)
        Wrapper => this%PointToValue()
        if(associated(Wrapper)) then
            select type(Wrapper)
                class is (DimensionsWrapper7D_t)
                    call Wrapper%Get(Value=Value)
                class Default
                    FPLerror = FPLWrapperError
                    call msg%Error(txt='Getting [Key="'//this%GetKey()//'"]: Dimensions do not match. Value was not modified.', &
                                   file=__FILE__, line=__LINE__ )
            end select
        else
            FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Getting [Key="'//this%GetKey()//'"]: Not present. Value was not modified.', &
                           file=__FILE__, line=__LINE__ )
        endif
    end function ParameterListIterator_Get7D


    function ParameterListIterator_GetSublist(this, Sublist) result(FPLerror)
    !-----------------------------------------------------------------
    !< Return a pointer to the current sublist
    !-----------------------------------------------------------------
        class(ParameterListIterator_t),  intent(IN)    :: this          !< Parameter List
        type(ParameterList_t), pointer,  intent(INOUT) :: Sublist       !< Wrapper
        class(*),               pointer                :: Value         !< Returned pointer to value
        integer(I4P)                                   :: FPLerror      !< Error flag
    !-----------------------------------------------------------------
        FPLerror = FPLSuccess
        nullify(Value)
        nullify(Sublist)
        Value => this%PointToValue()
        if(associated(Value)) then
            select type(Value)
                class is (ParameterList_t)
                    SubList => Value
                class Default
                    FPLerror = FPLSublistError
                    call msg%Error(txt='Getting [Key="'//this%GetKey()//'"]: Is not a sublist.', &
                                   file=__FILE__, line=__LINE__ )
            end select
        else
            FPLerror = FPLSublistError
            call msg%Error(txt='Getting [Key="'//this%GetKey()//'"]: Not present. Sublist was not modified.', &
                           file=__FILE__, line=__LINE__ )
        endif
    end function ParameterListIterator_GetSubList


    function ParameterListIterator_isSubList(this) result(isSubList)
    !-----------------------------------------------------------------
    !< Check if a Key is a SubList
    !-----------------------------------------------------------------
        class(ParameterListIterator_t), intent(IN) :: this            !< Parameter List Iterator
        class(*), pointer                          :: SubList         !< Sublist pointer
        logical                                    :: isSubList       !< Check if is a SubList
    !-----------------------------------------------------------------
        isSubList = .false.
        nullify(Sublist)
        SubList => this%PointToValue()
        if(associated(Sublist)) then
            select type(Sublist)
                class is (ParameterList_t)
                    isSubList =.true.
            end select
        endif
    end function ParameterListIterator_isSubList


    function ParameterListIterator_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the data size in bytes of the current value
    !-----------------------------------------------------------------
        class(ParameterListIterator_t), intent(IN) :: this            !< Parameter List Iterator
        type(ParameterEntry_t),      pointer       :: CurrentEntry    !< Current entry
        class(*), pointer                          :: Wrapper         !< Wrapper
        integer(I4P)                               :: DataSizeInBytes !< Size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = 0
        nullify(Wrapper)
        Wrapper => this%PointToValue()
        if(associated(Wrapper)) then
            select type(Wrapper)
                class is (DimensionsWrapper_t)
                    DataSizeInBytes = Wrapper%DataSizeInBytes()
            end select
        endif
    end function ParameterListIterator_DataSizeInBytes


    function ParameterListIterator_isOfDataType0D(this, Mold) result(IsOfDataType)
    !-----------------------------------------------------------------
    !< Check if the data type of Mold agrees with the current value
    !-----------------------------------------------------------------
        class(ParameterListIterator_t), intent(IN) :: this            !< Parameter List Iterator
        class(*),                    intent(IN)    :: Mold            !< Mold
        class(*), pointer                          :: Wrapper         !< Wrapper
        logical                                    :: isOfDataType    !< Check if has the same type
    !-----------------------------------------------------------------
        isOfDataType = .false.
        nullify(Wrapper)
        Wrapper => this%PointToValue()
        if(associated(Wrapper)) then
            select type(Wrapper)
                class is (DimensionsWrapper_t)
                    isOfDataType = Wrapper%isOfDataType(Mold=Mold)
            end select
        endif
    end function ParameterListIterator_isOfDataType0D


    function ParameterListIterator_isOfDataType1D(this, Mold) result(IsOfDataType)
    !-----------------------------------------------------------------
    !< Check if the data type of Mold agrees with the current value
    !-----------------------------------------------------------------
        class(ParameterListIterator_t), intent(IN) :: this            !< Parameter List Iterator
        class(*),                    intent(IN)    :: Mold(1:)        !< Mold
        class(*), pointer                          :: Wrapper         !< Wrapper
        logical                                    :: isOfDataType    !< Check if has the same type
    !-----------------------------------------------------------------
        isOfDataType = .false.
        nullify(Wrapper)
        Wrapper => this%PointToValue()
        if(associated(Wrapper)) then
            select type(Wrapper)
                class is (DimensionsWrapper_t)
                    isOfDataType = Wrapper%isOfDataType(Mold=Mold(1))
            end select
        endif
    end function ParameterListIterator_isOfDataType1D


    function ParameterListIterator_isOfDataType2D(this, Mold) result(IsOfDataType)
    !-----------------------------------------------------------------
    !< Check if the data type of Mold agrees with the current value
    !-----------------------------------------------------------------
        class(ParameterListIterator_t), intent(IN) :: this            !< Parameter List Iterator
        class(*),                    intent(IN)    :: Mold(1:,1:)     !< Mold
        class(*), pointer                          :: Wrapper         !< Wrapper
        logical                                    :: isOfDataType    !< Check if has the same type
    !-----------------------------------------------------------------
        isOfDataType = .false.
        nullify(Wrapper)
        Wrapper => this%PointToValue()
        if(associated(Wrapper)) then
            select type(Wrapper)
                class is (DimensionsWrapper_t)
                    isOfDataType = Wrapper%isOfDataType(Mold=Mold(1,1))
            end select
        endif
    end function ParameterListIterator_isOfDataType2D


    function ParameterListIterator_isOfDataType3D(this, Mold) result(IsOfDataType)
    !-----------------------------------------------------------------
    !< Check if the data type of Mold agrees with the current value
    !-----------------------------------------------------------------
        class(ParameterListIterator_t), intent(IN) :: this            !< Parameter List Iterator
        class(*),                    intent(IN)    :: Mold(1:,1:,1:)  !< Mold
        class(*), pointer                          :: Wrapper         !< Wrapper
        logical                                    :: isOfDataType    !< Check if has the same type
    !-----------------------------------------------------------------
        isOfDataType = .false.
        nullify(Wrapper)
        Wrapper => this%PointToValue()
        if(associated(Wrapper)) then
            select type(Wrapper)
                class is (DimensionsWrapper_t)
                    isOfDataType = Wrapper%isOfDataType(Mold=Mold(1,1,1))
            end select
        endif
    end function ParameterListIterator_isOfDataType3D


    function ParameterListIterator_isOfDataType4D(this, Mold) result(IsOfDataType)
    !-----------------------------------------------------------------
    !< Check if the data type of Mold agrees with the current value
    !-----------------------------------------------------------------
        class(ParameterListIterator_t), intent(IN) :: this              !< Parameter List Iterator
        class(*),                    intent(IN)    :: Mold(1:,1:,1:,1:) !< Mold
        class(*), pointer                          :: Wrapper           !< Wrapper
        logical                                    :: isOfDataType      !< Check if has the same type
    !-----------------------------------------------------------------
        isOfDataType = .false.
        nullify(Wrapper)
        Wrapper => this%PointToValue()
        if(associated(Wrapper)) then
            select type(Wrapper)
                class is (DimensionsWrapper_t)
                    isOfDataType = Wrapper%isOfDataType(Mold=Mold(1,1,1,1))
            end select
        endif
    end function ParameterListIterator_isOfDataType4D


    function ParameterListIterator_isOfDataType5D(this, Mold) result(IsOfDataType)
    !-----------------------------------------------------------------
    !< Check if the data type of Mold agrees with the current value
    !-----------------------------------------------------------------
        class(ParameterListIterator_t), intent(IN) :: this                 !< Parameter List Iterator
        class(*),                    intent(IN)    :: Mold(1:,1:,1:,1:,1:) !< Mold
        class(*), pointer                          :: Wrapper              !< Wrapper
        logical                                    :: isOfDataType         !< Check if has the same type
    !-----------------------------------------------------------------
        isOfDataType = .false.
        nullify(Wrapper)
        Wrapper => this%PointToValue()
        if(associated(Wrapper)) then
            select type(Wrapper)
                class is (DimensionsWrapper_t)
                    isOfDataType = Wrapper%isOfDataType(Mold=Mold(1,1,1,1,1))
            end select
        endif
    end function ParameterListIterator_isOfDataType5D


    function ParameterListIterator_isOfDataType6D(this, Mold) result(IsOfDataType)
    !-----------------------------------------------------------------
    !< Check if the data type of Mold agrees with the current value
    !-----------------------------------------------------------------
        class(ParameterListIterator_t), intent(IN) :: this                    !< Parameter List Iterator
        class(*),                    intent(IN)    :: Mold(1:,1:,1:,1:,1:,1:) !< Mold
        class(*), pointer                          :: Wrapper                 !< Wrapper
        logical                                    :: isOfDataType            !< Check if has the same type
    !-----------------------------------------------------------------
        isOfDataType = .false.
        nullify(Wrapper)
        Wrapper => this%PointToValue()
        if(associated(Wrapper)) then
            select type(Wrapper)
                class is (DimensionsWrapper_t)
                    isOfDataType = Wrapper%isOfDataType(Mold=Mold(1,1,1,1,1,1))
            end select
        endif
    end function ParameterListIterator_isOfDataType6D


    function ParameterListIterator_isOfDataType7D(this, Mold) result(IsOfDataType)
    !-----------------------------------------------------------------
    !< Check if the data type of Mold agrees with the current value
    !-----------------------------------------------------------------
        class(ParameterListIterator_t), intent(IN) :: this                       !< Parameter List Iterator
        class(*),                    intent(IN)    :: Mold(1:,1:,1:,1:,1:,1:,1:) !< Mold
        class(*), pointer                          :: Wrapper                    !< Wrapper
        logical                                    :: isOfDataType               !< Check if has the same type
    !-----------------------------------------------------------------
        isOfDataType = .false.
        nullify(Wrapper)
        Wrapper => this%PointToValue()
        if(associated(Wrapper)) then
            select type(Wrapper)
                class is (DimensionsWrapper_t)
                    isOfDataType = Wrapper%isOfDataType(Mold=Mold(1,1,1,1,1,1,1))
            end select
        endif
    end function ParameterListIterator_isOfDataType7D


    function ParameterListIterator_isAssignable0D(this,Value) result(isAssignable)
    !-----------------------------------------------------------------
    !< Check if a stored variable is Assignable to Value
    !-----------------------------------------------------------------
        class(ParameterListIterator_t), intent(IN) :: this             !< Parameter List Iterator
        class(*),                    intent(IN)    :: Value            !< Value
        class(*), pointer                          :: Wrapper          !< Wrapper
        logical                                    :: isAssignable     !< Check if is assignable
    !-----------------------------------------------------------------
        isAssignable = .false.
        nullify(Wrapper)
        Wrapper => this%PointToValue()
        if(associated(Wrapper)) then
            select type(Wrapper)
                class is (DimensionsWrapper0D_t)
                    ! Check same data type
                    if(Wrapper%isOfDataType(Mold=Value)) isAssignable = .true.
            end select
        endif
    end function ParameterListIterator_isAssignable0D


    function ParameterListIterator_isAssignable1D(this,Value) result(isAssignable)
    !-----------------------------------------------------------------
    !< Check if a stored variable is Assignable to Value
    !-----------------------------------------------------------------
        class(ParameterListIterator_t), intent(IN) :: this            !< Parameter List Iterator
        class(*),                    intent(IN)    :: Value(1:)       !< Value
        class(*), pointer                          :: Wrapper         !< Wrapper
        logical                                    :: isAssignable    !< Check if is assignable
        integer(I4P), allocatable                  :: ValueShape(:)   !< Shape of the stored value
    !-----------------------------------------------------------------
        isAssignable = .false.
        nullify(Wrapper)
        Wrapper => this%PointToValue()
        if(associated(Wrapper)) then
            select type(Wrapper)
                class is (DimensionsWrapper1D_t)
                    ! Check same data type
                    if(Wrapper%isOfDataType(Mold=Value(1))) then
                        call Wrapper%GetShape(ValueShape)
                        ! Check right shape
                        if(all(ValueShape == shape(Value))) isAssignable = .true.
                    endif
            end select
        endif
    end function ParameterListIterator_isAssignable1D


    function ParameterListIterator_isAssignable2D(this,Value) result(isAssignable)
    !-----------------------------------------------------------------
    !< Check if a stored variable is Assignable to Value
    !-----------------------------------------------------------------
        class(ParameterListIterator_t), intent(IN) :: this            !< Parameter List Iterator
        class(*),                    intent(IN)    :: Value(1:,1:)    !< Value
        class(*), pointer                          :: Wrapper         !< Wrapper
        logical                                    :: isAssignable    !< Check if is assignable
        integer(I4P), allocatable                  :: ValueShape(:)   !< Shape of the stored value
    !-----------------------------------------------------------------
        isAssignable = .false.
        nullify(Wrapper)
        Wrapper => this%PointToValue()
        if(associated(Wrapper)) then
            select type(Wrapper)
                class is (DimensionsWrapper2D_t)
                    ! Check same data type
                    if(Wrapper%isOfDataType(Mold=Value(1,1))) then
                        call Wrapper%GetShape(ValueShape)
                        ! Check right shape
                        if(all(ValueShape == shape(Value))) isAssignable = .true.
                    endif
            end select
        endif
    end function ParameterListIterator_isAssignable2D


    function ParameterListIterator_isAssignable3D(this, Value) result(isAssignable)
    !-----------------------------------------------------------------
    !< Check if a stored variable is Assignable to Value
    !-----------------------------------------------------------------
        class(ParameterListIterator_t), intent(IN) :: this            !< Parameter List Iterator
        class(*),                    intent(IN)    :: Value(1:,1:,1:) !< Value
        class(*), pointer                          :: Wrapper         !< Wrapper
        logical                                    :: isAssignable    !< Check if is assignable
        integer(I4P), allocatable                  :: ValueShape(:)   !< Shape of the stored value
    !-----------------------------------------------------------------
        isAssignable = .false.
        nullify(Wrapper)
        Wrapper => this%PointToValue()
        if(associated(Wrapper)) then
            select type(Wrapper)
                class is (DimensionsWrapper3D_t)
                    ! Check same data type
                    if(Wrapper%isOfDataType(Mold=Value(1,1,1))) then
                        call Wrapper%GetShape(ValueShape)
                        ! Check right shape
                        if(all(ValueShape == shape(Value))) isAssignable = .true.
                    endif
            end select
        endif
    end function ParameterListIterator_isAssignable3D


    function ParameterListIterator_isAssignable4D(this, Value) result(isAssignable)
    !-----------------------------------------------------------------
    !< Check if a stored variable is Assignable to Value
    !-----------------------------------------------------------------
        class(ParameterListIterator_t), intent(IN) :: this               !< Parameter List Iterator
        class(*),                    intent(IN)    :: Value(1:,1:,1:,1:) !< Value
        class(*), pointer                          :: Wrapper            !< Wrapper
        logical                                    :: isAssignable       !< Check if is assignable
        integer(I4P), allocatable                  :: ValueShape(:)      !< Shape of the stored value
    !-----------------------------------------------------------------
        isAssignable = .false.
        nullify(Wrapper)
        Wrapper => this%PointToValue()
        if(associated(Wrapper)) then
            select type(Wrapper)
                class is (DimensionsWrapper4D_t)
                    ! Check same data type
                    if(Wrapper%isOfDataType(Mold=Value(1,1,1,1))) then
                        call Wrapper%GetShape(ValueShape)
                        ! Check right shape
                        if(all(ValueShape == shape(Value))) isAssignable = .true.
                    endif
            end select
        endif
    end function ParameterListIterator_isAssignable4D


    function ParameterListIterator_isAssignable5D(this, Value) result(isAssignable)
    !-----------------------------------------------------------------
    !< Check if a stored variable is Assignable to Value
    !-----------------------------------------------------------------
        class(ParameterListIterator_t), intent(IN) :: this                  !< Parameter List Iterator
        class(*),                    intent(IN)    :: Value(1:,1:,1:,1:,1:) !< Value
        class(*), pointer                          :: Wrapper               !< Wrapper
        logical                                    :: isAssignable          !< Check if is assignable
        integer(I4P), allocatable                  :: ValueShape(:)         !< Shape of the stored value
    !-----------------------------------------------------------------
        isAssignable = .false.
        nullify(Wrapper)
        Wrapper => this%PointToValue()
        if(associated(Wrapper)) then
            select type(Wrapper)
                class is (DimensionsWrapper5D_t)
                    ! Check same data type
                    if(Wrapper%isOfDataType(Mold=Value(1,1,1,1,1))) then
                        call Wrapper%GetShape(ValueShape)
                        ! Check right shape
                        if(all(ValueShape == shape(Value))) isAssignable = .true.
                    endif
            end select
        endif
    end function ParameterListIterator_isAssignable5D


    function ParameterListIterator_isAssignable6D(this, Value) result(isAssignable)
    !-----------------------------------------------------------------
    !< Check if a stored variable is Assignable to Value
    !-----------------------------------------------------------------
        class(ParameterListIterator_t), intent(IN) :: this                     !< Parameter List Iterator
        class(*),                    intent(IN)    :: Value(1:,1:,1:,1:,1:,1:) !< Value
        class(*), pointer                          :: Wrapper                  !< Wrapper
        logical                                    :: isAssignable             !< Check if is assignable
        integer(I4P), allocatable                  :: ValueShape(:)            !< Shape of the stored value
    !-----------------------------------------------------------------
        isAssignable = .false.
        nullify(Wrapper)
        Wrapper => this%PointToValue()
        if(associated(Wrapper)) then
            select type(Wrapper)
                class is (DimensionsWrapper6D_t)
                    ! Check same data type
                    if(Wrapper%isOfDataType(Mold=Value(1,1,1,1,1,1))) then
                        call Wrapper%GetShape(ValueShape)
                        ! Check right shape
                        if(all(ValueShape == shape(Value))) isAssignable = .true.
                    endif
            end select
        endif
    end function ParameterListIterator_isAssignable6D


    function ParameterListIterator_isAssignable7D(this, Value) result(isAssignable)
    !-----------------------------------------------------------------
    !< Check if a stored variable is Assignable to Value
    !-----------------------------------------------------------------
        class(ParameterListIterator_t), intent(IN) :: this                        !< Parameter List Iterator
        class(*),                    intent(IN)    :: Value(1:,1:,1:,1:,1:,1:,1:) !< Value
        class(*), pointer                          :: Wrapper                     !< Wrapper
        logical                                    :: isAssignable                !< Check if is assignable
        integer(I4P), allocatable                  :: ValueShape(:)               !< Shape of the stored value
    !-----------------------------------------------------------------
        isAssignable = .false.
        nullify(Wrapper)
        Wrapper => this%PointToValue()
        if(associated(Wrapper)) then
            select type(Wrapper)
                class is (DimensionsWrapper7D_t)
                    ! Check same data type
                    if(Wrapper%isOfDataType(Mold=Value(1,1,1,1,1,1,1))) then
                        call Wrapper%GetShape(ValueShape)
                        ! Check right shape
                        if(all(ValueShape == shape(Value))) isAssignable = .true.
                    endif
            end select
        endif
    end function ParameterListIterator_isAssignable7D


    function ParameterListIterator_toString(this, Separator) result(String)
    !-----------------------------------------------------------------
    !< Return a scalar Value given the Key
    !-----------------------------------------------------------------
        class(ParameterListIterator_t),       intent(IN)    :: this           !< Parameter List Iterator
        character(len=1), optional,           intent(IN)    :: Separator      !< Array values separator
        character(len=:), allocatable                       :: String         !< Returned value as string
        class(*),                    pointer                :: Wrapper        !< Wrapper
        integer(I4P)                                        :: FPLerror       !< Error flag
    !-----------------------------------------------------------------
        FPLerror = FPLSuccess
        nullify(Wrapper)
        Wrapper => this%PointToValue()
        if(associated(Wrapper)) then
            select type(Wrapper)
                class is (DimensionsWrapper_t)
                    call Wrapper%toString(String, Separator)
                class Default
                    FPLerror = FPLWrapperError
                    call msg%Error(txt='Getting [Key="'//this%GetKey()//'"]: Unknown Wrapper. Value was not modified.', &
                                   file=__FILE__, line=__LINE__ )
            end select
        else
            FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Getting [Key="'//this%GetKey()//'"]: Not present. Value was not modified.', &
                           file=__FILE__, line=__LINE__ )
        endif
    end function ParameterListIterator_toString



    recursive subroutine ParameterListIterator_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print the content of the DataBase
    !-----------------------------------------------------------------
        class(ParameterListIterator_t),       intent(IN)  :: this    !< Parameter Iterator
        integer(I4P), optional,               intent(IN)  :: unit    !< Logic unit.
        character(*), optional,               intent(IN)  :: prefix  !< Prefixing string.
        integer(I4P), optional,               intent(OUT) :: iostat  !< IO error.
        character(*), optional,               intent(OUT) :: iomsg   !< IO error message.
        character(len=:), allocatable                     :: prefd   !< Prefixing string.
        integer(I4P)                                      :: unitd   !< Logic unit.
        integer(I4P)                                      :: iostatd !< IO error.
        character(500)                                    :: iomsgd  !< Temporary variable for IO error message.
        class(*), pointer                                 :: Value   !< Unlimited polymorphic value
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        unitd = OUTPUT_UNIT; if(present(unit)) unitd = unit
        nullify(Value)
        Value => this%PointToValue()
        if(associated(Value)) then
            select type(Value)
                class is (DimensionsWrapper_t)
                    call Value%Print(unit=unitd,                                              &
                                     prefix=prefd//                                           &
                                     '['//trim(str(no_sign=.true., n=this%GetIndex()))//']'// &
                                     ' Key = '//this%GetKey()//',',                           &
                                     iostat=iostatd,                                                &
                                     iomsg=iomsgd)
            end select
        endif
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine ParameterListIterator_Print

    function ParameterListIterator_HasFinished(this) result(HasFinished)
    !-----------------------------------------------------------------
    !< Check if Iterator has reached the end of the dictionary
    !-----------------------------------------------------------------
        class(ParameterListIterator_t),     intent(INOUT) :: this        ! Dictionary iterator
        logical                                           :: HasFinished
    !-----------------------------------------------------------------
        HasFinished = .false.
        if(this%Index==this%UpperBound) HasFinished = .true.
    end function ParameterListIterator_HasFinished

end module ParameterList
