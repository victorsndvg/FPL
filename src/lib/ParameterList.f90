module ParameterList

USE IR_Precision
USE ParameterEntryDictionary
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
save

    type :: ParameterList_t
    private
        type(ParameterEntryDictionary_t) :: Dictionary
    contains
    private
        procedure         ::                   ParameterList_Set0D
        procedure         ::                   ParameterList_Set1D
        procedure         ::                   ParameterList_Set2D
        procedure         ::                   ParameterList_Set3D
        procedure         ::                   ParameterList_Set4D
        procedure         ::                   ParameterList_Set5D
        procedure         ::                   ParameterList_Set6D
        procedure         ::                   ParameterList_Set7D
        procedure         ::                   ParameterList_Get0D
        procedure         ::                   ParameterList_Get1D
        procedure         ::                   ParameterList_Get2D
        procedure         ::                   ParameterList_Get3D
        procedure         ::                   ParameterList_Get4D
        procedure         ::                   ParameterList_Get5D
        procedure         ::                   ParameterList_Get6D
        procedure         ::                   ParameterList_Get7D
        procedure         ::                   ParameterList_GetPointer0D
        procedure         ::                   ParameterList_GetPointer1D
        procedure         ::                   ParameterList_GetPointer2D
        procedure         ::                   ParameterList_GetPointer3D
        procedure         ::                   ParameterList_GetPointer4D
        procedure         ::                   ParameterList_GetPointer5D
        procedure         ::                   ParameterList_GetPointer6D
        procedure         ::                   ParameterList_GetPointer7D
        procedure, public :: Init           => ParameterList_Init
        procedure, public :: NewSubList     => ParameterList_NewSubList
        procedure, public :: GetSubList     => ParameterList_GetSubList
        procedure, public :: GetShape       => ParameterList_GetShape
        procedure, public :: Free           => ParameterList_Free
        procedure, public :: Print          => ParameterList_Print
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
        procedure, public :: isPresent      => ParameterList_isPresent
!        procedure, public :: isOfDataType   => ParameterList_isOfDataType
!        procedure, public :: isSubList      => ParameterList_isSubList
        procedure, public :: Del            => ParameterList_RemoveEntry
        procedure, public :: Length         => ParameterList_Length
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


    function ParameterList_GetShape(this,Key) result(ValueShape)
    !-----------------------------------------------------------------
    !< Return a scalar Value given the Key
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN)    :: this           !< Parameter List
        character(len=*),                     intent(IN)    :: Key            !< String Key
        class(*),                    pointer                :: Wrapper        !< Wrapper
        integer(I4P), allocatable                           :: ValueShape(:)  !< Shape of the stored value
    !-----------------------------------------------------------------
        call this%Dictionary%GetPointer(Key=Key, Value=Wrapper)
        if(associated(Wrapper)) then
            select type(Wrapper)
                class is (DimensionsWrapper_t)
                    ValueShape = Wrapper%GetShape()
            end select
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


    function ParameterList_GetSublist(this,Key) result(Sublist)
    !-----------------------------------------------------------------
    !< Return a Unlimited polymorphic pointer to a Value given the Key
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN)    :: this    !< Parameter List
        character(len=*),                     intent(IN)    :: Key     !< String Key
        class(*),                              pointer      :: Value   !< Returned pointer to value
        class(ParameterList_T),                pointer      :: Sublist !< Wrapper
    !-----------------------------------------------------------------
        call this%Dictionary%GetPointer(Key=Key, Value=Value)
        if(associated(Value)) then
            select type(Value)
                class is (ParameterList_t)
                    SubList => Value
            end select
        endif
    end function ParameterList_GetSubList


    subroutine ParameterList_Set0D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Set a Key/Value pair into the Dictionary
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(INOUT) :: this           !< Parameter List
        character(len=*),                     intent(IN)    :: Key            !< String Key
        class(*),                             intent(IN)    :: Value          !< Unlimited polymorphic Value
        class(WrapperFactory_t),    pointer                 :: WrapperFactory !< WrapperFactory
        class(*),                   pointer                 :: Wrapper        !< Wrapper
    !-----------------------------------------------------------------
        WrapperFactory => TheWrapperFactoryList%GetFactory(Value=Value)
        if(associated(WrapperFactory)) then
            Wrapper => WrapperFactory%Wrap(Value=Value)
            if(associated(Wrapper)) call this%Dictionary%Set(Key=Key,Value=Wrapper)
        endif
    end subroutine ParameterList_Set0D


    subroutine ParameterList_Set1D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Set a Key/Value pair into the DataBase
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(INOUT) :: this           !< Parameter List
        character(len=*),                     intent(IN)    :: Key            !< String Key
        class(*),                             intent(IN)    :: Value(:)       !< Unlimited polymorphic 1D array Value
        class(WrapperFactory_t),    pointer                 :: WrapperFactory !< WrapperFactory
        class(*),                   pointer                 :: Wrapper        !< Wrapper
    !-----------------------------------------------------------------
        WrapperFactory => TheWrapperFactoryList%GetFactory(Value=Value)
        if(associated(WrapperFactory)) then
            Wrapper => WrapperFactory%Wrap(Value=Value)
            if(associated(Wrapper)) call this%Dictionary%Set(Key=Key,Value=Wrapper)
        endif
    end subroutine ParameterList_Set1D


    subroutine ParameterList_Set2D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Set a Key/Value pair into the DataBase
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(INOUT) :: this           !< Parameter List
        character(len=*),                     intent(IN)    :: Key            !< String Key
        class(*),                             intent(IN)    :: Value(:,:)     !< Unlimited polymorphic 2D array value
        class(WrapperFactory_t),    pointer                 :: WrapperFactory !< WrapperFactory
        class(*),                   pointer                 :: Wrapper        !< Wrapper
    !-----------------------------------------------------------------
        WrapperFactory => TheWrapperFactoryList%GetFactory(Value=Value)
        if(associated(WrapperFactory)) then
            Wrapper => WrapperFactory%Wrap(Value=Value)
            if(associated(Wrapper)) call this%Dictionary%Set(Key=Key,Value=Wrapper)
        endif
    end subroutine ParameterList_Set2D


    subroutine ParameterList_Set3D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Set a Key/Value pair into the DataBase
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(INOUT) :: this           !< Parameter List
        character(len=*),                     intent(IN)    :: Key            !< String Key
        class(*),                             intent(IN)    :: Value(:,:,:)   !< Unlimited Polimorphic 3D array Value
        class(WrapperFactory_t),    pointer                 :: WrapperFactory !< WrapperFactory
        class(*),                   pointer                 :: Wrapper        !< Wrapper
    !-----------------------------------------------------------------
        WrapperFactory => TheWrapperFactoryList%GetFactory(Value=Value)
        if(associated(WrapperFactory)) then
            Wrapper => WrapperFactory%Wrap(Value=Value)
            if(associated(Wrapper)) call this%Dictionary%Set(Key=Key,Value=Wrapper)
        endif
    end subroutine ParameterList_Set3D


    subroutine ParameterList_Set4D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Set a Key/Value pair into the DataBase
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(INOUT) :: this           !< Parameter List
        character(len=*),                     intent(IN)    :: Key            !< String Key
        class(*),                             intent(IN)    :: Value(:,:,:,:) !< Unlimited Polymorphic 4D array Value
        class(WrapperFactory_t),    pointer                 :: WrapperFactory !< WrapperFactory
        class(*),                   pointer                 :: Wrapper        !< Wrapper
    !-----------------------------------------------------------------
        WrapperFactory => TheWrapperFactoryList%GetFactory(Value=Value)
        if(associated(WrapperFactory)) then
            Wrapper => WrapperFactory%Wrap(Value=Value)
            if(associated(Wrapper)) call this%Dictionary%Set(Key=Key,Value=Wrapper)
        endif
    end subroutine ParameterList_Set4D


    subroutine ParameterList_Set5D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Set a Key/Value pair into the DataBase
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(INOUT) :: this             !< Parameter List
        character(len=*),                     intent(IN)    :: Key              !< String Key
        class(*),                             intent(IN)    :: Value(:,:,:,:,:) !< Unlimited Polymorphic 5D array Value
        class(WrapperFactory_t),    pointer                 :: WrapperFactory   !< WrapperFactory
        class(*),                   pointer                 :: Wrapper          !< Wrapper
    !-----------------------------------------------------------------
        WrapperFactory => TheWrapperFactoryList%GetFactory(Value=Value)
        if(associated(WrapperFactory)) then
            Wrapper => WrapperFactory%Wrap(Value=Value)
            if(associated(Wrapper)) call this%Dictionary%Set(Key=Key,Value=Wrapper)
        endif
    end subroutine ParameterList_Set5D


    subroutine ParameterList_Set6D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Set a Key/Value pair into the DataBase
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(INOUT) :: this               !< Parameter List
        character(len=*),                     intent(IN)    :: Key                !< String Key
        class(*),                             intent(IN)    :: Value(:,:,:,:,:,:) !< Unlimited Polymorphic 5D array Value
        class(WrapperFactory_t),    pointer                 :: WrapperFactory     !< WrapperFactory
        class(*),                   pointer                 :: Wrapper            !< Wrapper
    !-----------------------------------------------------------------
        WrapperFactory => TheWrapperFactoryList%GetFactory(Value=Value)
        if(associated(WrapperFactory)) then
            Wrapper => WrapperFactory%Wrap(Value=Value)
            if(associated(Wrapper)) call this%Dictionary%Set(Key=Key,Value=Wrapper)
        endif
    end subroutine ParameterList_Set6D


    subroutine ParameterList_Set7D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Set a Key/Value pair into the DataBase
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(INOUT) :: this                 !< Parameter List
        character(len=*),                     intent(IN)    :: Key                  !< String Key
        class(*),                             intent(IN)    :: Value(:,:,:,:,:,:,:) !< Unlimited Polymorphic 7D array Value
        class(WrapperFactory_t),    pointer                 :: WrapperFactory       !< WrapperFactory
        class(*),                   pointer                 :: Wrapper              !< Wrapper
    !-----------------------------------------------------------------
        WrapperFactory => TheWrapperFactoryList%GetFactory(Value=Value)
        if(associated(WrapperFactory)) then
            Wrapper => WrapperFactory%Wrap(Value=Value)
            if(associated(Wrapper)) call this%Dictionary%Set(Key=Key,Value=Wrapper)
        endif
    end subroutine ParameterList_Set7D


    subroutine ParameterList_Get0D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a scalar Value given the Key
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN)    :: this           !< Parameter List
        character(len=*),                     intent(IN)    :: Key            !< String Key
        class(*),                             intent(INOUT) :: Value          !< Returned value
        class(*),                    pointer                :: Wrapper        !< Wrapper
    !-----------------------------------------------------------------
        call this%Dictionary%GetPointer(Key=Key, Value=Wrapper)
        if(associated(Wrapper)) then
            select type(Wrapper)
                class is (DimensionsWrapper0D_t)
                    call Wrapper%Get(Value=Value)
            end select
        endif
    end subroutine ParameterList_Get0D


    subroutine ParameterList_Get1D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a vector Value given the Key
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN)    :: this           !< Parameter List
        character(len=*),                     intent(IN)    :: Key            !< String Key
        class(*),                             intent(INOUT) :: Value(:)       !< Returned value
        class(*),                    pointer                :: Wrapper        !< Wrapper
    !-----------------------------------------------------------------
        call this%Dictionary%GetPointer(Key=Key, Value=Wrapper)
        if(associated(Wrapper)) then
            select type(Wrapper)
                class is (DimensionsWrapper1D_t)
                call Wrapper%Get(Value=Value)
            end select
        endif
    end subroutine ParameterList_Get1D


    subroutine ParameterList_Get2D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a 2D array Value given the Key
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN)    :: this           !< Parameter List
        character(len=*),                     intent(IN)    :: Key            !< String Key
        class(*),                             intent(INOUT) :: Value(:,:)     !< Returned value
        class(*),                    pointer                :: Wrapper        !< Wrapper
    !-----------------------------------------------------------------
        call this%Dictionary%GetPointer(Key=Key, Value=Wrapper)
        if(associated(Wrapper)) then
            select type(Wrapper)
                class is (DimensionsWrapper2D_t)
                    call Wrapper%Get(Value=Value)
            end select
        endif
    end subroutine ParameterList_Get2D


    subroutine ParameterList_Get3D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a 3D array Value given the Key
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN)    :: this           !< Parameter List
        character(len=*),                     intent(IN)    :: Key            !< String Key
        class(*),                             intent(INOUT) :: Value(:,:,:)   !< Returned value
        class(*),                    pointer                :: Wrapper        !< Wrapper
    !-----------------------------------------------------------------
        call this%Dictionary%GetPointer(Key=Key, Value=Wrapper)
        if(associated(Wrapper)) then
            select type(Wrapper)
                class is (DimensionsWrapper3D_t)
                    call Wrapper%Get(Value=Value)
            end select
        endif
    end subroutine ParameterList_Get3D


    subroutine ParameterList_Get4D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a 4D array Value given the Key
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN)    :: this           !< Parameter List
        character(len=*),                     intent(IN)    :: Key            !< String Key
        class(*),                             intent(INOUT) :: Value(:,:,:,:) !< Returned value
        class(*),                    pointer                :: Wrapper        !< Wrapper
    !-----------------------------------------------------------------
        call this%Dictionary%GetPointer(Key=Key, Value=Wrapper)
        if(associated(Wrapper)) then
            select type(Wrapper)
                class is (DimensionsWrapper4D_t)
                    call Wrapper%Get(Value=Value)
            end select
        endif
    end subroutine ParameterList_Get4D


    subroutine ParameterList_Get5D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a 5D array Value given the Key
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN)    :: this             !< Parameter List
        character(len=*),                     intent(IN)    :: Key              !< String Key
        class(*),                             intent(INOUT) :: Value(:,:,:,:,:) !< Returned value
        class(*), pointer                                   :: Node             !< Pointer to a Parameter List
        class(*),                    pointer                :: Wrapper          !< Wrapper
    !-----------------------------------------------------------------
        call this%Dictionary%GetPointer(Key=Key, Value=Wrapper)
        if(associated(Wrapper)) then
            select type(Wrapper)
                class is (DimensionsWrapper5D_t)
                    call Wrapper%Get(Value=Value)
            end select
        endif
    end subroutine ParameterList_Get5D


    subroutine ParameterList_Get6D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a 6D array Value given the Key
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN)    :: this               !< Parameter List
        character(len=*),                     intent(IN)    :: Key                !< String Key
        class(*),                             intent(INOUT) :: Value(:,:,:,:,:,:) !< Returned value
        class(*),                    pointer                :: Wrapper            !< Wrapper
    !-----------------------------------------------------------------
        call this%Dictionary%GetPointer(Key=Key, Value=Wrapper)
        if(associated(Wrapper)) then
            select type(Wrapper)
                class is (DimensionsWrapper6D_t)
                    call Wrapper%Get(Value=Value)
            end select
        endif
    end subroutine ParameterList_Get6D


    subroutine ParameterList_Get7D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a 7D array Value given the Key
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN)    :: this                 !< Parameter List
        character(len=*),                     intent(IN)    :: Key                  !< String Key
        class(*),                             intent(INOUT) :: Value(:,:,:,:,:,:,:) !< Returned value
        class(*),                    pointer                :: Wrapper              !< Wrapper
    !-----------------------------------------------------------------
        call this%Dictionary%GetPointer(Key=Key, Value=Wrapper)
        if(associated(Wrapper)) then
            select type(Wrapper)
                class is (DimensionsWrapper7D_t)
                    call Wrapper%Get(Value=Value)
            end select
        endif
    end subroutine ParameterList_Get7D


    subroutine ParameterList_GetPointer0D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a Unlimited polymorphic pointer to a Value given the Key
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN)    :: this    !< Parameter List
        character(len=*),                     intent(IN)    :: Key     !< String Key
        class(*), pointer,                    intent(INOUT) :: Value   !< Returned pointer to value
        class(*),                    pointer                :: Wrapper !< Wrapper
    !-----------------------------------------------------------------
        call this%Dictionary%GetPointer(Key=Key, Value=Wrapper)
        if(associated(Wrapper)) then
            select type(Wrapper)
                class is (DimensionsWrapper0D_t)
                    Value => Wrapper%GetPointer()
            end select
        endif
    end subroutine ParameterList_GetPointer0D


    subroutine ParameterList_GetPointer1D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a Unlimited polymorphic pointer to a Value given the Key
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN)    :: this     !< Parameter List
        character(len=*),                     intent(IN)    :: Key      !< String Key
        class(*), pointer,                    intent(INOUT) :: Value(:) !< Returned pointer to value
        class(*),                    pointer                :: Wrapper  !< Wrapper
    !-----------------------------------------------------------------
        call this%Dictionary%GetPointer(Key=Key, Value=Wrapper)
        if(associated(Wrapper)) then
            select type(Wrapper)
                class is (DimensionsWrapper1D_t)
                    Value => Wrapper%GetPointer()
            end select
        endif
    end subroutine ParameterList_GetPointer1D


    subroutine ParameterList_GetPointer2D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a Unlimited polymorphic pointer to a Value given the Key
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN)    :: this       !< Parameter List
        character(len=*),                     intent(IN)    :: Key        !< String Key
        class(*), pointer,                    intent(INOUT) :: Value(:,:) !< Returned pointer to value
        class(*),                    pointer                :: Wrapper    !< Wrapper
    !-----------------------------------------------------------------
        call this%Dictionary%GetPointer(Key=Key, Value=Wrapper)
        if(associated(Wrapper)) then
            select type(Wrapper)
                class is (DimensionsWrapper2D_t)
                    Value => Wrapper%GetPointer()
            end select
        endif
    end subroutine ParameterList_GetPointer2D


    subroutine ParameterList_GetPointer3D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a Unlimited polymorphic pointer to a Value given the Key
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN)    :: this           !< Parameter List
        character(len=*),                     intent(IN)    :: Key            !< String Key
        class(*), pointer,                    intent(INOUT) :: Value(:,:,:)   !< Returned pointer to value
        class(*),                    pointer                :: Wrapper        !< Wrapper
    !-----------------------------------------------------------------
        call this%Dictionary%GetPointer(Key=Key, Value=Wrapper)
        if(associated(Wrapper)) then
            select type(Wrapper)
                class is (DimensionsWrapper3D_t)
                    Value => Wrapper%GetPointer()
            end select
        endif
    end subroutine ParameterList_GetPointer3D


    subroutine ParameterList_GetPointer4D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a Unlimited polymorphic pointer to a Value given the Key
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN)    :: this           !< Parameter List
        character(len=*),                     intent(IN)    :: Key            !< String Key
        class(*), pointer,                    intent(INOUT) :: Value(:,:,:,:) !< Returned pointer to value
        class(*),                    pointer                :: Wrapper        !< Wrapper
    !-----------------------------------------------------------------
        call this%Dictionary%GetPointer(Key=Key, Value=Wrapper)
        if(associated(Wrapper)) then
            select type(Wrapper)
                class is (DimensionsWrapper4D_t)
                    Value => Wrapper%GetPointer()
            end select
        endif
    end subroutine ParameterList_GetPointer4D


    subroutine ParameterList_GetPointer5D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a Unlimited polymorphic pointer to a Value given the Key
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN)    :: this             !< Parameter List
        character(len=*),                     intent(IN)    :: Key              !< String Key
        class(*), pointer,                    intent(INOUT) :: Value(:,:,:,:,:) !< Returned pointer to value
        class(*),                    pointer                :: Wrapper          !< Wrapper
    !-----------------------------------------------------------------
        call this%Dictionary%GetPointer(Key=Key, Value=Wrapper)
        if(associated(Wrapper)) then
            select type(Wrapper)
                class is (DimensionsWrapper5D_t)
                    Value => Wrapper%GetPointer()
            end select
        endif
    end subroutine ParameterList_GetPointer5D


    subroutine ParameterList_GetPointer6D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a Unlimited polymorphic pointer to a Value given the Key
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN)    :: this               !< Parameter List
        character(len=*),                     intent(IN)    :: Key                !< String Key
        class(*), pointer,                    intent(INOUT) :: Value(:,:,:,:,:,:) !< Returned pointer to value
        class(*),                    pointer                :: Wrapper            !< Wrapper
    !-----------------------------------------------------------------
        call this%Dictionary%GetPointer(Key=Key, Value=Wrapper)
        if(associated(Wrapper)) then
            select type(Wrapper)
                class is (DimensionsWrapper6D_t)
                    Value => Wrapper%GetPointer()
            end select
        endif
    end subroutine ParameterList_GetPointer6D


    subroutine ParameterList_GetPointer7D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a Unlimited polymorphic pointer to a Value given the Key
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN)    :: this                 !< Parameter List
        character(len=*),                     intent(IN)    :: Key                  !< String Key
        class(*), pointer,                    intent(INOUT) :: Value(:,:,:,:,:,:,:) !< Returned pointer to value
        class(*),                    pointer                :: Wrapper              !< Wrapper
    !-----------------------------------------------------------------
        call this%Dictionary%GetPointer(Key=Key, Value=Wrapper)
        if(associated(Wrapper)) then
            select type(Wrapper)
                class is (DimensionsWrapper7D_t)
                    Value => Wrapper%GetPointer()
            end select
        endif
    end subroutine ParameterList_GetPointer7D


    function ParameterList_isPresent(this,Key) result(isPresent)
    !-----------------------------------------------------------------
    !< Check if a Key is present in the DataBase
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN) :: this      !< Parameter List Entry Containter type
        character(len=*),                     intent(IN) :: Key       !< String Key
        logical                                          :: isPresent !< Boolean flag to check if a Key is present
    !-----------------------------------------------------------------
        isPresent = this%Dictionary%IsPresent(Key=Key)
    end function ParameterList_isPresent


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
        integer(I4P),                         intent(IN)  :: unit    !< Logic unit.
        character(*), optional,               intent(IN)  :: prefix  !< Prefixing string.
        integer(I4P), optional,               intent(OUT) :: iostat  !< IO error.
        character(*), optional,               intent(OUT) :: iomsg   !< IO error message.
        character(len=:), allocatable                     :: prefd   !< Prefixing string.
        integer(I4P)                                      :: iostatd !< IO error.
        character(500)                                    :: iomsgd  !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(*,fmt='(A)') prefd//' PARAMETER LIST CONTENT:'
        write(*,fmt='(A)') prefd//' -----------------------'
        call this%Dictionary%Print(unit=unit, prefix=prefd, iostat=iostatd, iomsg=iomsgd)
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine ParameterList_Print


end module ParameterList
