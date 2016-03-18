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

module ParameterList

USE iso_fortran_env, only: OUTPUT_UNIT
USE ErrorMessages
USE IR_Precision
USE ParameterEntryDictionary
USE ParameterEntry
USE WrapperFactoryListSingleton
USE WrapperFactory
USE DimensionsWrapper

implicit none
private
save

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
        generic,   public :: Set            => ParameterList_Set0D, &
                                               ParameterList_Set1D, &
                                               ParameterList_Set2D, &
                                               ParameterList_Set3D, &
                                               ParameterList_Set4D, &
                                               ParameterList_Set5D, &
                                               ParameterList_Set6D, &
                                               ParameterList_Set7D
        generic,   public :: Get            => ParameterList_Get0D, &
                                               ParameterList_Get1D, &
                                               ParameterList_Get2D, &
                                               ParameterList_Get3D, &
                                               ParameterList_Get4D, &
                                               ParameterList_Get5D, &
                                               ParameterList_Get6D, &
                                               ParameterList_Get7D
        generic,   public :: GetPointer     => ParameterList_GetPointer0D, &
                                               ParameterList_GetPointer1D, &
                                               ParameterList_GetPointer2D, &
                                               ParameterList_GetPointer3D, &
                                               ParameterList_GetPointer4D, &
                                               ParameterList_GetPointer5D, &
                                               ParameterList_GetPointer6D, &
                                               ParameterList_GetPointer7D
        generic,   public :: isOfDataType   => ParameterList_IsOfDataType0D, &
                                               ParameterList_IsOfDataType1D, &
                                               ParameterList_IsOfDataType2D, &
                                               ParameterList_IsOfDataType3D, &
                                               ParameterList_IsOfDataType4D, &
                                               ParameterList_IsOfDataType5D, &
                                               ParameterList_IsOfDataType6D, &
                                               ParameterList_IsOfDataType7D
        procedure, non_overridable, public :: DataSizeInBytes=> ParameterList_DataSizeInBytes
        procedure, non_overridable, public :: Del            => ParameterList_RemoveEntry
        procedure, non_overridable, public :: Init           => ParameterList_Init
        procedure, non_overridable, public :: GetShape       => ParameterList_GetShape
        procedure, non_overridable, public :: NewSubList     => ParameterList_NewSubList
        procedure, non_overridable, public :: GetSubList     => ParameterList_GetSubList
        procedure, non_overridable, public :: isPresent      => ParameterList_isPresent
        procedure, non_overridable, public :: isSubList      => ParameterList_isSubList
        procedure, non_overridable, public :: Free           => ParameterList_Free
        procedure, non_overridable, public :: Print          => ParameterList_Print
        procedure, non_overridable, public :: Length         => ParameterList_Length
        final             ::                   ParameterList_Finalize
    end type ParameterList_t

public :: ParameterList_t

contains


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
    !< Return a scalar Value given the Key
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
        class(ParameterList_t),               pointer       :: SublistPointer !< New Sublist pointer
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
        class(ParameterList_T),      pointer, intent(INOUT) :: Sublist !< Wrapper
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


    subroutine ParameterList_RemoveEntry(this, Key)
    !-----------------------------------------------------------------
    !< Remove an Entry given a Key
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(INOUT) :: this          !< Parameter List Entry Containter type
        character(len=*),                     intent(IN)    :: Key           !< String Key
    !-----------------------------------------------------------------
        call this%Dictionary%Del(Key=Key)
    end subroutine ParameterList_RemoveEntry


    function ParameterList_Length(this) result(Length)
    !-----------------------------------------------------------------
    !< Return the number of ParameterListEntries contained in the DataBase
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN) :: this       !< Parameter List Entry Containter type
        integer(I4P)                                     :: Length     !< Number of parameters in database
        integer(I4P)                                     :: DBIterator !< Database Iterator index 
    !-----------------------------------------------------------------
        Length = this%Dictionary%Length()
    end function ParameterList_Length


    subroutine ParameterList_Print(this, unit, prefix, iostat, iomsg)
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
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        unitd = OUTPUT_UNIT; if(present(unit)) unitd = unit
        write(*,fmt='(A)') prefd//' PARAMETER LIST CONTENT:'
        write(*,fmt='(A)') prefd//' -----------------------'
        call this%Dictionary%Print(unit=unitd, prefix=prefd, iostat=iostatd, iomsg=iomsgd)
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine ParameterList_Print


end module ParameterList
