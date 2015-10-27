    !-----------------------------------------------------------------
    ! ParameterListEntryContainer is a datatype containing a DataBase
    ! array of ParameterListEntries made to store diferent Entries
    ! depending on the hash of his Key.
    !
    ! This work takes as a starting point the previou work of
    ! Stefano Zaghi (@szaghi, https://github.com/szaghi).
    !
    ! You can find the original source at:
    ! https://github.com/szaghi/OFF/blob/95691ca15e6d68128ba016e40df74e42123f1c54/src/Data_Type_Hash_Table.f90
    !-----------------------------------------------------------------

module ParameterListEntryContainer

USE IR_Precision
USE ParameterListEntry
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

    integer(I4P), parameter:: DefaultDataBaseSize = 999_I4P

    type :: ParameterListRoot_t
        class(ParameterListEntry_t), pointer :: Root => null()
    end type

    type, public:: ParameterListEntryContainer_t
    private
        type(ParameterListRoot_t), allocatable :: DataBase(:)
        integer(I4P)                            :: Size = 0_I4P
    contains
    private
        procedure         ::                   ParameterListEntryContainer_Set0D
        procedure         ::                   ParameterListEntryContainer_Set1D
        procedure         ::                   ParameterListEntryContainer_Set2D
        procedure         ::                   ParameterListEntryContainer_Set3D
        procedure         ::                   ParameterListEntryContainer_Set4D
        procedure         ::                   ParameterListEntryContainer_Set5D
        procedure         ::                   ParameterListEntryContainer_Set6D
        procedure         ::                   ParameterListEntryContainer_Set7D
        procedure         ::                   ParameterListEntryContainer_Get0D
        procedure         ::                   ParameterListEntryContainer_Get1D
        procedure         ::                   ParameterListEntryContainer_Get2D
        procedure         ::                   ParameterListEntryContainer_Get3D
        procedure         ::                   ParameterListEntryContainer_Get4D
        procedure         ::                   ParameterListEntryContainer_Get5D
        procedure         ::                   ParameterListEntryContainer_Get6D
        procedure         ::                   ParameterListEntryContainer_Get7D
        procedure         ::                   ParameterListEntryContainer_GetPointer0D
        procedure         ::                   ParameterListEntryContainer_GetPointer1D
        procedure         ::                   ParameterListEntryContainer_GetPointer2D
        procedure         ::                   ParameterListEntryContainer_GetPointer3D
        procedure         ::                   ParameterListEntryContainer_GetPointer4D
        procedure         ::                   ParameterListEntryContainer_GetPointer5D
        procedure         ::                   ParameterListEntryContainer_GetPointer6D
        procedure         ::                   ParameterListEntryContainer_GetPointer7D
        procedure         ::                   ParameterListEntryContainer_GetPolymorphic0D
        procedure         ::                   ParameterListEntryContainer_GetPolymorphic1D
        procedure         ::                   ParameterListEntryContainer_GetPolymorphic2D
        procedure         ::                   ParameterListEntryContainer_GetPolymorphic3D
        procedure         ::                   ParameterListEntryContainer_GetPolymorphic4D
        procedure         ::                   ParameterListEntryContainer_GetPolymorphic5D
        procedure         ::                   ParameterListEntryContainer_GetPolymorphic6D
        procedure         ::                   ParameterListEntryContainer_GetPolymorphic7D
        procedure         :: Hash           => ParameterListEntryContainer_Hash
        procedure         :: HasRoot        => ParameterListEntryContainer_HasRoot
        procedure         :: AddWrapperNode => ParameterListEntryContainer_AddWrapperNode
        procedure, public :: Init           => ParameterListEntryContainer_Init
        procedure, public :: NewSubList     => ParameterListEntryContainer_NewSubList
        procedure, public :: GetShape       => ParameterListEntryContainer_GetShape
        procedure, public :: Free           => ParameterListEntryContainer_Free
        procedure, public :: Print          => ParameterListEntryContainer_Print
        generic,   public :: Set            => ParameterListEntryContainer_Set0D, &
                                               ParameterListEntryContainer_Set1D, &
                                               ParameterListEntryContainer_Set2D, &
                                               ParameterListEntryContainer_Set3D, &
                                               ParameterListEntryContainer_Set4D, &
                                               ParameterListEntryContainer_Set5D, &
                                               ParameterListEntryContainer_Set6D, &
                                               ParameterListEntryContainer_Set7D
        generic,   public :: Get            => ParameterListEntryContainer_Get0D, &
                                               ParameterListEntryContainer_Get1D, &
                                               ParameterListEntryContainer_Get2D, &
                                               ParameterListEntryContainer_Get3D, &
                                               ParameterListEntryContainer_Get4D, &
                                               ParameterListEntryContainer_Get5D, &
                                               ParameterListEntryContainer_Get6D, &
                                               ParameterListEntryContainer_Get7D
        generic,   public :: GetPointer     => ParameterListEntryContainer_GetPointer0D, &
                                               ParameterListEntryContainer_GetPointer1D, &
                                               ParameterListEntryContainer_GetPointer2D, &
                                               ParameterListEntryContainer_GetPointer3D, &
                                               ParameterListEntryContainer_GetPointer4D, &
                                               ParameterListEntryContainer_GetPointer5D, &
                                               ParameterListEntryContainer_GetPointer6D, &
                                               ParameterListEntryContainer_GetPointer7D
        generic,   public :: GetPolymorphic => ParameterListEntryContainer_GetPolymorphic0D, &
                                               ParameterListEntryContainer_GetPolymorphic1D, &
                                               ParameterListEntryContainer_GetPolymorphic2D, &
                                               ParameterListEntryContainer_GetPolymorphic3D, &
                                               ParameterListEntryContainer_GetPolymorphic4D, &
                                               ParameterListEntryContainer_GetPolymorphic5D, &
                                               ParameterListEntryContainer_GetPolymorphic6D, &
                                               ParameterListEntryContainer_GetPolymorphic7D
        procedure, public :: isPresent      => ParameterListEntryContainer_isPresent
!        procedure, public :: isOfDataType   => ParameterListEntryContainer_isOfDataType
!        procedure, public :: isSubList      => ParameterListEntryContainer_isSubList
        procedure, public :: Del            => ParameterListEntryContainer_RemoveEntry
        procedure, public :: Length         => ParameterListEntryContainer_GetLength
        final             ::                   ParameterListEntryContainer_Finalize
    end type ParameterListEntryContainer_t


contains


    function ParameterListEntryContainer_Hash(this,Key) result(Hash)
    !-----------------------------------------------------------------
    !< String hash function
    !-----------------------------------------------------------------
        class(ParameterListEntryContainer_t), intent(IN) :: this        !< Parameter List Entry Containter type
        character(len=*),                     intent(IN) :: Key         !< String Key
        integer(I4P)                                     :: Hash        !< Hash code
        character, dimension(len(Key))                   :: CharArray   !< Character array containing the Key
        integer(I4P)                                     :: CharIterator!< Char iterator index
    !-----------------------------------------------------------------
        forall (CharIterator=1:LEN(Key))
            CharArray(CharIterator) = Key(CharIterator:CharIterator)
        end forall
        Hash = MOD(SUM(ICHAR(CharArray)), this%Size)
    end function ParameterListEntryContainer_Hash


    subroutine ParameterListEntryContainer_Init(this,Size)
    !-----------------------------------------------------------------
    !< Allocate the database with a given Szie of DefaultDataBaseSize
    !-----------------------------------------------------------------
        class(ParameterListEntryContainer_t), intent(INOUT) :: this   !< Parameter List Entry Containter type
        integer(I4P), optional,               intent(IN)    :: Size   !< DataBase Size
    !-----------------------------------------------------------------
        call this%Free()
        if (present(Size)) then
            this%Size = Size
        else
            this%Size = DefaultDataBaseSize
        endif
        allocate(this%DataBase(0:this%Size-1))
    end subroutine ParameterListEntryContainer_Init


    function ParameterListEntryContainer_GetShape(this,Key) result(ValueShape)
    !-----------------------------------------------------------------
    !< Return a scalar Value given the Key
    !-----------------------------------------------------------------
        class(ParameterListEntryContainer_t), intent(IN)    :: this           !< Parameter List Entry Containter
        character(len=*),                     intent(IN)    :: Key            !< String Key
        class(ParameterListEntry_t), pointer                :: Entry          !< Pointer to a Parameter List
        class(*),                    pointer                :: Wrapper        !< Wrapper
        integer(I4P), allocatable                           :: ValueShape(:)
    !-----------------------------------------------------------------
        Entry => this%DataBase(this%Hash(Key=Key))%Root%GetEntry(Key=Key)
        if(associated(Entry)) then
            Wrapper => Entry%PointToValue()
            select type(Wrapper)
                class is (DimensionsWrapper_t)
                    ValueShape = Wrapper%GetShape()
            end select
        end if
    end function ParameterListEntryContainer_GetShape


    function ParameterListEntryContainer_HasRoot(this,Key) result(HasRoot)
    !-----------------------------------------------------------------
    !< Check if the DataBase position for a given Key has a root node
    !-----------------------------------------------------------------
        class(ParameterListEntryContainer_t),    intent(IN) :: this    !< Parameter List Entry Containter type
        character(len=*),                        intent(IN) :: Key     !< String Key
        logical                                             :: HasRoot !< Check if has root node
    !-----------------------------------------------------------------
        HasRoot = associated(this%DataBase(this%Hash(Key=Key))%Root)
    end function


    subroutine ParameterListEntryContainer_AddWrapperNode(this,Key,Wrapper)
    !-----------------------------------------------------------------
    !< Set a Key/Wrapper pair into the DataBase
    !-----------------------------------------------------------------
        class(ParameterListEntryContainer_t),    intent(INOUT) :: this    !< Parameter List Entry Containter type
        character(len=*)                    ,    intent(IN)    :: Key     !< String Key
        class(DimensionsWrapper_t)          ,    intent(IN)    :: Wrapper !< Wrapper
    !-----------------------------------------------------------------
        if(.not. this%HasRoot(Key=Key)) then
             allocate(this%DataBase(this%Hash(Key=Key))%Root)  
        endif
        call this%DataBase(this%Hash(Key=Key))%Root%AddNode(Key=Key,Value=Wrapper)
    end subroutine ParameterListEntryContainer_AddWrapperNode


    subroutine ParameterListEntryContainer_Free(this)
    !-----------------------------------------------------------------
    !< Free ParameterListEntries and the DataBase
    !-----------------------------------------------------------------
        class(ParameterListEntryContainer_t), intent(INOUT) :: this       !< Parameter List Entry Containter type
        integer(I4P)                                        :: DBIterator !< Database Iterator index 
    !-----------------------------------------------------------------
        if (allocated(this%DataBase)) THEN
            do DBIterator=lbound(this%DataBase,dim=1),ubound(this%DataBase,dim=1)
                if(associated(this%DataBase(DBIterator)%Root)) call this%DataBase(DBIterator)%Root%Free()
                nullify(this%DataBase(DBIterator)%Root)
            enddo
            deallocate(this%DataBase)
        endif
        this%Size = 0_I4P
    end subroutine ParameterListEntryContainer_Free


    subroutine ParameterListEntryContainer_Finalize(this)
    !-----------------------------------------------------------------
    !< Destructor procedure
    !-----------------------------------------------------------------
        type(ParameterListEntryContainer_t), intent(INOUT) :: this    !< Parameter List Entry Containter type
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine ParameterListEntryContainer_Finalize


    subroutine ParameterListEntryContainer_NewSubList(this,Key, Size)
    !-----------------------------------------------------------------
    !< Set a Key/Value pair into the DataBase
    !-----------------------------------------------------------------
        class(ParameterListEntryContainer_t), intent(INOUT) :: this           !< Parameter List Entry Containter type
        character(len=*),                     intent(IN)    :: Key            !< String Key
        integer(I4P), optional,               intent(IN)    :: Size           !< Sublist Size
        class(ParameterListEntry_T), pointer                :: Entry          !< Pointer to a Parameter List Entry
        type(ParameterListEntryContainer_t)                 :: Sublist        !< New Sublist
        integer(I4P)                                        :: SublistSize    !< Sublist real Size
        class(*),                    pointer                :: SublistPointer !< Pointer to the New SubList
    !-----------------------------------------------------------------
        SublistSize = DefaultDataBaseSize
        if(present(Size)) SublistSize = Size
        call this%DataBase(this%Hash(Key=Key))%Root%AddNode(Key=Key,Value=Sublist)
        Entry => this%DataBase(this%Hash(Key=Key))%Root%GetEntry(Key=Key)
        if(associated(Entry)) then
            SublistPointer => Entry%PointToValue()
            select type(SublistPointer)
                class is (ParameterListEntryContainer_t)
                    call SublistPointer%Init(Size=SublistSize)
            end select
        end if
    end subroutine ParameterListEntryContainer_NewSubList


    subroutine ParameterListEntryContainer_Set0D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Set a Key/Value pair into the DataBase
    !-----------------------------------------------------------------
        class(ParameterListEntryContainer_t), intent(INOUT) :: this    !< Parameter List Entry Containter type
        character(len=*),                     intent(IN)    :: Key     !< String Key
        class(*),                             intent(IN)    :: Value   !< Unlimited polymorphic Value
        class(WrapperFactory_t),    pointer                 :: WrapperFactory
        class(DimensionsWrapper_t), allocatable             :: Wrapper
    !-----------------------------------------------------------------
        WrapperFactory => TheWrapperFactoryList%GetFactory(Value=Value)
        if(associated(WrapperFactory)) then
            call WrapperFactory%Wrap(Value=Value, Wrapper=Wrapper)
            call this%AddWrapperNode(Key=Key,Wrapper=Wrapper)
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
    end subroutine ParameterListEntryContainer_Set0D


    subroutine ParameterListEntryContainer_Set1D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Set a Key/Value pair into the DataBase
    !-----------------------------------------------------------------
        class(ParameterListEntryContainer_t), intent(INOUT) :: this    !< Parameter List Entry Containter type
        character(len=*),                     intent(IN)    :: Key     !< String Key
        class(*),                             intent(IN)    :: Value(:)
        class(WrapperFactory_t),    pointer                 :: WrapperFactory
        class(DimensionsWrapper_t), allocatable             :: Wrapper
    !-----------------------------------------------------------------
        WrapperFactory => TheWrapperFactoryList%GetFactory(Value=Value)
        if(associated(WrapperFactory)) then
            call WrapperFactory%Wrap(Value=Value, Wrapper=Wrapper)
            call this%AddWrapperNode(Key=Key,Wrapper=Wrapper)
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
    end subroutine ParameterListEntryContainer_Set1D


    subroutine ParameterListEntryContainer_Set2D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Set a Key/Value pair into the DataBase
    !-----------------------------------------------------------------
        class(ParameterListEntryContainer_t), intent(INOUT) :: this    !< Parameter List Entry Containter type
        character(len=*),                     intent(IN)    :: Key     !< String Key
        class(*),                             intent(IN)    :: Value(:,:)
        class(WrapperFactory_t),    pointer                 :: WrapperFactory
        class(DimensionsWrapper_t), allocatable             :: Wrapper
    !-----------------------------------------------------------------
        WrapperFactory => TheWrapperFactoryList%GetFactory(Value=Value)
        if(associated(WrapperFactory)) then
            call WrapperFactory%Wrap(Value=Value, Wrapper=Wrapper)
            call this%AddWrapperNode(Key=Key,Wrapper=Wrapper)
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
    end subroutine ParameterListEntryContainer_Set2D


    subroutine ParameterListEntryContainer_Set3D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Set a Key/Value pair into the DataBase
    !-----------------------------------------------------------------
        class(ParameterListEntryContainer_t), intent(INOUT) :: this    !< Parameter List Entry Containter type
        character(len=*),                     intent(IN)    :: Key     !< String Key
        class(*),                             intent(IN)    :: Value(:,:,:)
        class(WrapperFactory_t),    pointer                 :: WrapperFactory
        class(DimensionsWrapper_t), allocatable             :: Wrapper
    !-----------------------------------------------------------------
        WrapperFactory => TheWrapperFactoryList%GetFactory(Value=Value)
        if(associated(WrapperFactory)) then
            call WrapperFactory%Wrap(Value=Value, Wrapper=Wrapper)
            call this%AddWrapperNode(Key=Key,Wrapper=Wrapper)
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
    end subroutine ParameterListEntryContainer_Set3D


    subroutine ParameterListEntryContainer_Set4D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Set a Key/Value pair into the DataBase
    !-----------------------------------------------------------------
        class(ParameterListEntryContainer_t), intent(INOUT) :: this    !< Parameter List Entry Containter type
        character(len=*),                     intent(IN)    :: Key     !< String Key
        class(*),                             intent(IN)    :: Value(:,:,:,:)
        class(WrapperFactory_t),    pointer                 :: WrapperFactory
        class(DimensionsWrapper_t), allocatable             :: Wrapper
    !-----------------------------------------------------------------
        WrapperFactory => TheWrapperFactoryList%GetFactory(Value=Value)
        if(associated(WrapperFactory)) then
            call WrapperFactory%Wrap(Value=Value, Wrapper=Wrapper)
            call this%AddWrapperNode(Key=Key,Wrapper=Wrapper)
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
    end subroutine ParameterListEntryContainer_Set4D


    subroutine ParameterListEntryContainer_Set5D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Set a Key/Value pair into the DataBase
    !-----------------------------------------------------------------
        class(ParameterListEntryContainer_t), intent(INOUT) :: this    !< Parameter List Entry Containter type
        character(len=*),                     intent(IN)    :: Key     !< String Key
        class(*),                             intent(IN)    :: Value(:,:,:,:,:)
        class(WrapperFactory_t),    pointer                 :: WrapperFactory
        class(DimensionsWrapper_t), allocatable             :: Wrapper
    !-----------------------------------------------------------------
        WrapperFactory => TheWrapperFactoryList%GetFactory(Value=Value)
        if(associated(WrapperFactory)) then
            call WrapperFactory%Wrap(Value=Value, Wrapper=Wrapper)
            call this%AddWrapperNode(Key=Key,Wrapper=Wrapper)
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
    end subroutine ParameterListEntryContainer_Set5D


    subroutine ParameterListEntryContainer_Set6D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Set a Key/Value pair into the DataBase
    !-----------------------------------------------------------------
        class(ParameterListEntryContainer_t), intent(INOUT) :: this    !< Parameter List Entry Containter type
        character(len=*),                     intent(IN)    :: Key     !< String Key
        class(*),                             intent(IN)    :: Value(:,:,:,:,:,:)
        class(WrapperFactory_t),    pointer                 :: WrapperFactory
        class(DimensionsWrapper_t), allocatable             :: Wrapper
    !-----------------------------------------------------------------
        WrapperFactory => TheWrapperFactoryList%GetFactory(Value=Value)
        if(associated(WrapperFactory)) then
            call WrapperFactory%Wrap(Value=Value, Wrapper=Wrapper)
            call this%AddWrapperNode(Key=Key,Wrapper=Wrapper)
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
    end subroutine ParameterListEntryContainer_Set6D


    subroutine ParameterListEntryContainer_Set7D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Set a Key/Value pair into the DataBase
    !-----------------------------------------------------------------
        class(ParameterListEntryContainer_t), intent(INOUT) :: this    !< Parameter List Entry Containter type
        character(len=*),                     intent(IN)    :: Key     !< String Key
        class(*),                             intent(IN)    :: Value(:,:,:,:,:,:,:)
        class(WrapperFactory_t),    pointer                 :: WrapperFactory
        class(DimensionsWrapper_t), allocatable             :: Wrapper
    !-----------------------------------------------------------------
        WrapperFactory => TheWrapperFactoryList%GetFactory(Value=Value)
        if(associated(WrapperFactory)) then
            call WrapperFactory%Wrap(Value=Value, Wrapper=Wrapper)
            call this%AddWrapperNode(Key=Key,Wrapper=Wrapper)
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
    end subroutine ParameterListEntryContainer_Set7D


    subroutine ParameterListEntryContainer_Get0D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a scalar Value given the Key
    !-----------------------------------------------------------------
        class(ParameterListEntryContainer_t), intent(IN)    :: this           !< Parameter List Entry Containter
        character(len=*),                     intent(IN)    :: Key            !< String Key
        class(*),                             intent(INOUT) :: Value          !< Returned value
        class(ParameterListEntry_t), pointer                :: Entry          !< Pointer to a Parameter List
        class(*),                    pointer                :: Wrapper        !< Wrapper
    !-----------------------------------------------------------------
        if(this%HasRoot(Key=Key)) then
            Entry => this%DataBase(this%Hash(Key=Key))%Root%GetEntry(Key=Key)
            if(associated(Entry)) then
                Wrapper => Entry%PointToValue()
                select type(Wrapper)
                    class is (DimensionsWrapper0D_t)
                        call Wrapper%Get(Value=Value)
                end select
            end if
        endif
    end subroutine ParameterListEntryContainer_Get0D


    subroutine ParameterListEntryContainer_Get1D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a vector Value given the Key
    !-----------------------------------------------------------------
        class(ParameterListEntryContainer_t), intent(IN)    :: this           !< Parameter List Entry Containter
        character(len=*),                     intent(IN)    :: Key            !< String Key
        class(*),                             intent(INOUT) :: Value(:)       !< Returned value
        class(ParameterListEntry_t), pointer                :: Entry          !< Pointer to a Parameter List
        class(*),                    pointer                :: Wrapper        !< Wrapper
    !-----------------------------------------------------------------
        if(this%HasRoot(Key=Key)) then
            Entry => this%DataBase(this%Hash(Key=Key))%Root%GetEntry(Key=Key)
            if(associated(Entry)) then
                Wrapper => Entry%PointToValue()
                select type(Wrapper)
                    class is (DimensionsWrapper1D_t)
                        call Wrapper%Get(Value=Value)
                end select
            end if
        endif
    end subroutine ParameterListEntryContainer_Get1D


    subroutine ParameterListEntryContainer_Get2D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a 2D array Value given the Key
    !-----------------------------------------------------------------
        class(ParameterListEntryContainer_t), intent(IN)    :: this           !< Parameter List Entry Containter 
        character(len=*),                     intent(IN)    :: Key            !< String Key
        class(*),                             intent(INOUT) :: Value(:,:)     !< Returned value
        class(ParameterListEntry_t), pointer                :: Entry          !< Pointer to a Parameter List
        class(*),                    pointer                :: Wrapper        !< Wrapper
    !-----------------------------------------------------------------
        if(this%HasRoot(Key=Key)) then
            Entry => this%DataBase(this%Hash(Key=Key))%Root%GetEntry(Key=Key)
            if(associated(Entry)) then
                Wrapper => Entry%PointToValue()
                select type(Wrapper)
                    class is (DimensionsWrapper2D_t)
                        call Wrapper%Get(Value=Value)
                end select
            end if
        endif
    end subroutine ParameterListEntryContainer_Get2D


    subroutine ParameterListEntryContainer_Get3D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a 3D array Value given the Key
    !-----------------------------------------------------------------
        class(ParameterListEntryContainer_t), intent(IN)    :: this           !< Parameter List Entry Containter type
        character(len=*),                     intent(IN)    :: Key            !< String Key
        class(*),                             intent(INOUT) :: Value(:,:,:)   !< Returned value
        class(ParameterListEntry_t), pointer                :: Entry          !< Pointer to a Parameter List
        class(*),                    pointer                :: Wrapper        !< Wrapper
    !-----------------------------------------------------------------
        if(this%HasRoot(Key=Key)) then
            Entry => this%DataBase(this%Hash(Key=Key))%Root%GetEntry(Key=Key)
            if(associated(Entry)) then
                Wrapper => Entry%PointToValue()
                select type(Wrapper)
                    class is (DimensionsWrapper3D_t)
                        call Wrapper%Get(Value=Value)
                end select
            end if
        endif
    end subroutine ParameterListEntryContainer_Get3D


    subroutine ParameterListEntryContainer_Get4D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a 4D array Value given the Key
    !-----------------------------------------------------------------
        class(ParameterListEntryContainer_t), intent(IN)    :: this           !< Parameter List Entry Containter type
        character(len=*),                     intent(IN)    :: Key            !< String Key
        class(*),                             intent(INOUT) :: Value(:,:,:,:) !< Returned value
        class(ParameterListEntry_t), pointer                :: Entry          !< Pointer to a Parameter List
        class(*),                    pointer                :: Wrapper        !< Wrapper
    !-----------------------------------------------------------------
        if(this%HasRoot(Key=Key)) then
            Entry => this%DataBase(this%Hash(Key=Key))%Root%GetEntry(Key=Key)
            if(associated(Entry)) then
                Wrapper => Entry%PointToValue()
                select type(Wrapper)
                    class is (DimensionsWrapper4D_t)
                        call Wrapper%Get(Value=Value)
                end select
            end if
        endif
    end subroutine ParameterListEntryContainer_Get4D


    subroutine ParameterListEntryContainer_Get5D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a 5D array Value given the Key
    !-----------------------------------------------------------------
        class(ParameterListEntryContainer_t), intent(IN)    :: this             !< Parameter List Entry Containter type
        character(len=*),                     intent(IN)    :: Key              !< String Key
        class(*),                             intent(INOUT) :: Value(:,:,:,:,:) !< Returned value
        class(*), pointer                                   :: Node             !< Pointer to a Parameter List
        class(ParameterListEntry_t), pointer                :: Entry          !< Pointer to a Parameter List
        class(*),                    pointer                :: Wrapper        !< Wrapper
    !-----------------------------------------------------------------
        if(this%HasRoot(Key=Key)) then
            Entry => this%DataBase(this%Hash(Key=Key))%Root%GetEntry(Key=Key)
            if(associated(Entry)) then
                Wrapper => Entry%PointToValue()
                select type(Wrapper)
                    class is (DimensionsWrapper5D_t)
                        call Wrapper%Get(Value=Value)
                end select
            end if
        endif
    end subroutine ParameterListEntryContainer_Get5D


    subroutine ParameterListEntryContainer_Get6D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a 6D array Value given the Key
    !-----------------------------------------------------------------
        class(ParameterListEntryContainer_t), intent(IN)    :: this               !< Parameter List Entry Containter type
        character(len=*),                     intent(IN)    :: Key                !< String Key
        class(*),                             intent(INOUT) :: Value(:,:,:,:,:,:) !< Returned value
        class(ParameterListEntry_t), pointer                :: Entry          !< Pointer to a Parameter List
        class(*),                    pointer                :: Wrapper        !< Wrapper
    !-----------------------------------------------------------------
        if(this%HasRoot(Key=Key)) then
            Entry => this%DataBase(this%Hash(Key=Key))%Root%GetEntry(Key=Key)
            if(associated(Entry)) then
                Wrapper => Entry%PointToValue()
                select type(Wrapper)
                    class is (DimensionsWrapper6D_t)
                        call Wrapper%Get(Value=Value)
                end select
            end if
        endif
    end subroutine ParameterListEntryContainer_Get6D


    subroutine ParameterListEntryContainer_Get7D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a 7D array Value given the Key
    !-----------------------------------------------------------------
        class(ParameterListEntryContainer_t), intent(IN)    :: this                 !< Parameter List Entry Containter type
        character(len=*),                     intent(IN)    :: Key                  !< String Key
        class(*),                             intent(INOUT) :: Value(:,:,:,:,:,:,:) !< Returned value
        class(ParameterListEntry_t), pointer                :: Entry          !< Pointer to a Parameter List
        class(*),                    pointer                :: Wrapper        !< Wrapper
    !-----------------------------------------------------------------
        if(this%HasRoot(Key=Key)) then
            Entry => this%DataBase(this%Hash(Key=Key))%Root%GetEntry(Key=Key)
            if(associated(Entry)) then
                Wrapper => Entry%PointToValue()
                select type(Wrapper)
                    class is (DimensionsWrapper7D_t)
                        call Wrapper%Get(Value=Value)
                end select
            end if
        endif
    end subroutine ParameterListEntryContainer_Get7D


    subroutine ParameterListEntryContainer_GetPointer0D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a Unlimited polymorphic pointer to a Value given the Key
    !-----------------------------------------------------------------
        class(ParameterListEntryContainer_t), intent(IN)    :: this    !< Parameter List Entry Containter
        character(len=*),                     intent(IN)    :: Key     !< String Key
        class(*), pointer,                    intent(INOUT) :: Value   !< Returned pointer to value
        class(ParameterListEntry_t), pointer                :: Entry   !< Pointer to a Parameter List
        class(*),                    pointer                :: Wrapper !< Wrapper
    !-----------------------------------------------------------------
        if(this%HasRoot(Key=Key)) then
            Entry => this%DataBase(this%Hash(Key=Key))%Root%GetEntry(Key=Key)
            if(associated(Entry)) then
                Wrapper => Entry%PointToValue()
                select type(Wrapper)
                    class is (DimensionsWrapper0D_t)
                        Value => Wrapper%GetPointer()
                end select
            end if
        endif
    end subroutine ParameterListEntryContainer_GetPointer0D


    subroutine ParameterListEntryContainer_GetPointer1D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a Unlimited polymorphic pointer to a Value given the Key
    !-----------------------------------------------------------------
        class(ParameterListEntryContainer_t), intent(IN)    :: this     !< Parameter List Entry Containter
        character(len=*),                     intent(IN)    :: Key      !< String Key
        class(*), pointer,                    intent(INOUT) :: Value(:) !< Returned pointer to value
        class(ParameterListEntry_t), pointer                :: Entry    !< Pointer to a Parameter List
        class(*),                    pointer                :: Wrapper  !< Wrapper
    !-----------------------------------------------------------------
        if(this%HasRoot(Key=Key)) then
            Entry => this%DataBase(this%Hash(Key=Key))%Root%GetEntry(Key=Key)
            if(associated(Entry)) then
                Wrapper => Entry%PointToValue()
                select type(Wrapper)
                    class is (DimensionsWrapper1D_t)
                        Value => Wrapper%GetPointer()
                end select
            end if
        endif
    end subroutine ParameterListEntryContainer_GetPointer1D


    subroutine ParameterListEntryContainer_GetPointer2D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a Unlimited polymorphic pointer to a Value given the Key
    !-----------------------------------------------------------------
        class(ParameterListEntryContainer_t), intent(IN)    :: this       !< Parameter List Entry Containter
        character(len=*),                     intent(IN)    :: Key        !< String Key
        class(*), pointer,                    intent(INOUT) :: Value(:,:) !< Returned pointer to value
        class(ParameterListEntry_t), pointer                :: Entry      !< Pointer to a Parameter List
        class(*),                    pointer                :: Wrapper    !< Wrapper
    !-----------------------------------------------------------------
        if(this%HasRoot(Key=Key)) then
            Entry => this%DataBase(this%Hash(Key=Key))%Root%GetEntry(Key=Key)
            if(associated(Entry)) then
                Wrapper => Entry%PointToValue()
                select type(Wrapper)
                    class is (DimensionsWrapper2D_t)
                        Value => Wrapper%GetPointer()
                end select
            end if
        endif
    end subroutine ParameterListEntryContainer_GetPointer2D


    subroutine ParameterListEntryContainer_GetPointer3D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a Unlimited polymorphic pointer to a Value given the Key
    !-----------------------------------------------------------------
        class(ParameterListEntryContainer_t), intent(IN)    :: this           !< Parameter List Entry Containter
        character(len=*),                     intent(IN)    :: Key            !< String Key
        class(*), pointer,                    intent(INOUT) :: Value(:,:,:)   !< Returned pointer to value
        class(ParameterListEntry_t), pointer                :: Entry          !< Pointer to a Parameter List
        class(*),                    pointer                :: Wrapper        !< Wrapper
    !-----------------------------------------------------------------
        if(this%HasRoot(Key=Key)) then
            Entry => this%DataBase(this%Hash(Key=Key))%Root%GetEntry(Key=Key)
            if(associated(Entry)) then
                Wrapper => Entry%PointToValue()
                select type(Wrapper)
                    class is (DimensionsWrapper3D_t)
                        Value => Wrapper%GetPointer()
                end select
            end if
        endif
    end subroutine ParameterListEntryContainer_GetPointer3D


    subroutine ParameterListEntryContainer_GetPointer4D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a Unlimited polymorphic pointer to a Value given the Key
    !-----------------------------------------------------------------
        class(ParameterListEntryContainer_t), intent(IN)    :: this           !< Parameter List Entry Containter
        character(len=*),                     intent(IN)    :: Key            !< String Key
        class(*), pointer,                    intent(INOUT) :: Value(:,:,:,:) !< Returned pointer to value
        class(ParameterListEntry_t), pointer                :: Entry          !< Pointer to a Parameter List
        class(*),                    pointer                :: Wrapper        !< Wrapper
    !-----------------------------------------------------------------
        if(this%HasRoot(Key=Key)) then
            Entry => this%DataBase(this%Hash(Key=Key))%Root%GetEntry(Key=Key)
            if(associated(Entry)) then
                Wrapper => Entry%PointToValue()
                select type(Wrapper)
                    class is (DimensionsWrapper4D_t)
                        Value => Wrapper%GetPointer()
                end select
            end if
        endif
    end subroutine ParameterListEntryContainer_GetPointer4D


    subroutine ParameterListEntryContainer_GetPointer5D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a Unlimited polymorphic pointer to a Value given the Key
    !-----------------------------------------------------------------
        class(ParameterListEntryContainer_t), intent(IN)    :: this             !< Parameter List Entry Containter
        character(len=*),                     intent(IN)    :: Key              !< String Key
        class(*), pointer,                    intent(INOUT) :: Value(:,:,:,:,:) !< Returned pointer to value
        class(ParameterListEntry_t), pointer                :: Entry            !< Pointer to a Parameter List
        class(*),                    pointer                :: Wrapper          !< Wrapper
    !-----------------------------------------------------------------
        if(this%HasRoot(Key=Key)) then
            Entry => this%DataBase(this%Hash(Key=Key))%Root%GetEntry(Key=Key)
            if(associated(Entry)) then
                Wrapper => Entry%PointToValue()
                select type(Wrapper)
                    class is (DimensionsWrapper5D_t)
                        Value => Wrapper%GetPointer()
                end select
            end if
        endif
    end subroutine ParameterListEntryContainer_GetPointer5D


    subroutine ParameterListEntryContainer_GetPointer6D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a Unlimited polymorphic pointer to a Value given the Key
    !-----------------------------------------------------------------
        class(ParameterListEntryContainer_t), intent(IN)    :: this               !< Parameter List Entry Containter
        character(len=*),                     intent(IN)    :: Key                !< String Key
        class(*), pointer,                    intent(INOUT) :: Value(:,:,:,:,:,:) !< Returned pointer to value
        class(ParameterListEntry_t), pointer                :: Entry              !< Pointer to a Parameter List
        class(*),                    pointer                :: Wrapper            !< Wrapper
    !-----------------------------------------------------------------
        if(this%HasRoot(Key=Key)) then
            Entry => this%DataBase(this%Hash(Key=Key))%Root%GetEntry(Key=Key)
            if(associated(Entry)) then
                Wrapper => Entry%PointToValue()
                select type(Wrapper)
                    class is (DimensionsWrapper6D_t)
                        Value => Wrapper%GetPointer()
                end select
            end if
        endif
    end subroutine ParameterListEntryContainer_GetPointer6D


    subroutine ParameterListEntryContainer_GetPointer7D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a Unlimited polymorphic pointer to a Value given the Key
    !-----------------------------------------------------------------
        class(ParameterListEntryContainer_t), intent(IN)    :: this                 !< Parameter List Entry Containter
        character(len=*),                     intent(IN)    :: Key                  !< String Key
        class(*), pointer,                    intent(INOUT) :: Value(:,:,:,:,:,:,:) !< Returned pointer to value
        class(ParameterListEntry_t), pointer                :: Entry                !< Pointer to a Parameter List
        class(*),                    pointer                :: Wrapper              !< Wrapper
    !-----------------------------------------------------------------
        if(this%HasRoot(Key=Key)) then
            Entry => this%DataBase(this%Hash(Key=Key))%Root%GetEntry(Key=Key)
            if(associated(Entry)) then
                Wrapper => Entry%PointToValue()
                select type(Wrapper)
                    class is (DimensionsWrapper7D_t)
                        Value => Wrapper%GetPointer()
                end select
            end if
        endif
    end subroutine ParameterListEntryContainer_GetPointer7D


    subroutine ParameterListEntryContainer_GetPolymorphic0D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a scalar Value given the Key
    !-----------------------------------------------------------------
        class(ParameterListEntryContainer_t), intent(IN)    :: this           !< Parameter List Entry Containter
        character(len=*),                     intent(IN)    :: Key            !< String Key
        class(*), allocatable,                intent(INOUT) :: Value          !< Returned value
        class(ParameterListEntry_t), pointer                :: Entry          !< Pointer to a Parameter List
        class(*),                    pointer                :: Wrapper        !< Wrapper
    !-----------------------------------------------------------------
        if(this%HasRoot(Key=Key)) then
            Entry => this%DataBase(this%Hash(Key=Key))%Root%GetEntry(Key=Key)
            if(associated(Entry)) then
                Wrapper => Entry%PointToValue()
                select type(Wrapper)
                    class is (DimensionsWrapper0D_t)
                        call Wrapper%GetPolymorphic(Value=Value)
                end select
            end if
        endif
    end subroutine ParameterListEntryContainer_GetPolymorphic0D


    subroutine ParameterListEntryContainer_GetPolymorphic1D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a vector Value given the Key
    !-----------------------------------------------------------------
        class(ParameterListEntryContainer_t), intent(IN)    :: this           !< Parameter List Entry Containter
        character(len=*),                     intent(IN)    :: Key            !< String Key
        class(*), allocatable,                intent(OUT)   :: Value(:)       !< Returned value
        class(ParameterListEntry_t), pointer                :: Entry          !< Pointer to a Parameter List
        class(*),                    pointer                :: Wrapper        !< Wrapper
    !-----------------------------------------------------------------
        if(this%HasRoot(Key=Key)) then
            Entry => this%DataBase(this%Hash(Key=Key))%Root%GetEntry(Key=Key)
            if(associated(Entry)) then
                Wrapper => Entry%PointToValue()
                select type(Wrapper)
                    class is (DimensionsWrapper1D_t)
                        call Wrapper%GetPolymorphic(Value=Value)
                end select
            end if
        endif
    end subroutine ParameterListEntryContainer_GetPolymorphic1D


    subroutine ParameterListEntryContainer_GetPolymorphic2D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a 2D array Value given the Key
    !-----------------------------------------------------------------
        class(ParameterListEntryContainer_t), intent(IN)    :: this           !< Parameter List Entry Containter 
        character(len=*),                     intent(IN)    :: Key            !< String Key
        class(*), allocatable,                intent(OUT)   :: Value(:,:)     !< Returned value
        class(*), pointer                                   :: Node           !< Pointer to a Parameter List
        class(ParameterListEntry_t), pointer                :: Entry          !< Pointer to a Parameter List
        class(*),                    pointer                :: Wrapper        !< Wrapper
    !-----------------------------------------------------------------
        if(this%HasRoot(Key=Key)) then
            Entry => this%DataBase(this%Hash(Key=Key))%Root%GetEntry(Key=Key)
            if(associated(Entry)) then
                Wrapper => Entry%PointToValue()
                select type(Wrapper)
                    class is (DimensionsWrapper2D_t)
                        call Wrapper%GetPolymorphic(Value=Value)
                end select
            end if
        endif
    end subroutine ParameterListEntryContainer_GetPolymorphic2D


    subroutine ParameterListEntryContainer_GetPolymorphic3D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a 3D array Value given the Key
    !-----------------------------------------------------------------
        class(ParameterListEntryContainer_t), intent(IN)    :: this           !< Parameter List Entry Containter type
        character(len=*),                     intent(IN)    :: Key            !< String Key
        class(*), allocatable,                intent(OUT)   :: Value(:,:,:)   !< Returned value
        class(ParameterListEntry_t), pointer                :: Entry          !< Pointer to a Parameter List
        class(*),                    pointer                :: Wrapper        !< Wrapper
    !-----------------------------------------------------------------
        if(this%HasRoot(Key=Key)) then
            Entry => this%DataBase(this%Hash(Key=Key))%Root%GetEntry(Key=Key)
            if(associated(Entry)) then
                Wrapper => Entry%PointToValue()
                select type(Wrapper)
                    class is (DimensionsWrapper3D_t)
                        call Wrapper%GetPolymorphic(Value=Value)
                end select
            end if
        endif
    end subroutine ParameterListEntryContainer_GetPolymorphic3D


    subroutine ParameterListEntryContainer_GetPolymorphic4D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a 4D array Value given the Key
    !-----------------------------------------------------------------
        class(ParameterListEntryContainer_t), intent(IN)    :: this           !< Parameter List Entry Containter type
        character(len=*),                     intent(IN)    :: Key            !< String Key
        class(*), allocatable,                intent(OUT)   :: Value(:,:,:,:) !< Returned value
        class(ParameterListEntry_t), pointer                :: Entry          !< Pointer to a Parameter List
        class(*),                    pointer                :: Wrapper        !< Wrapper
    !-----------------------------------------------------------------
        if(this%HasRoot(Key=Key)) then
            Entry => this%DataBase(this%Hash(Key=Key))%Root%GetEntry(Key=Key)
            if(associated(Entry)) then
                Wrapper => Entry%PointToValue()
                select type(Wrapper)
                    class is (DimensionsWrapper4D_t)
                        call Wrapper%GetPolymorphic(Value=Value)
                end select
            end if
        endif
    end subroutine ParameterListEntryContainer_GetPolymorphic4D


    subroutine ParameterListEntryContainer_GetPolymorphic5D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a 5D array Value given the Key
    !-----------------------------------------------------------------
        class(ParameterListEntryContainer_t), intent(IN)    :: this             !< Parameter List Entry Containter type
        character(len=*),                     intent(IN)    :: Key              !< String Key
        class(*), allocatable,                intent(OUT)   :: Value(:,:,:,:,:) !< Returned value
        class(*), pointer                                   :: Node             !< Pointer to a Parameter List
        class(ParameterListEntry_t), pointer                :: Entry            !< Pointer to a Parameter List
        class(*),                    pointer                :: Wrapper          !< Wrapper
    !-----------------------------------------------------------------
        if(this%HasRoot(Key=Key)) then
            Entry => this%DataBase(this%Hash(Key=Key))%Root%GetEntry(Key=Key)
            if(associated(Entry)) then
                Wrapper => Entry%PointToValue()
                select type(Wrapper)
                    class is (DimensionsWrapper5D_t)
                        call Wrapper%GetPolymorphic(Value=Value)
                end select
            end if
        endif
    end subroutine ParameterListEntryContainer_GetPolymorphic5D


    subroutine ParameterListEntryContainer_GetPolymorphic6D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a 6D array Value given the Key
    !-----------------------------------------------------------------
        class(ParameterListEntryContainer_t), intent(IN)    :: this               !< Parameter List Entry Containter type
        character(len=*),                     intent(IN)    :: Key                !< String Key
        class(*), allocatable,                intent(OUT)   :: Value(:,:,:,:,:,:) !< Returned value
        class(ParameterListEntry_t), pointer                :: Entry              !< Pointer to a Parameter List
        class(*),                    pointer                :: Wrapper            !< Wrapper
    !-----------------------------------------------------------------
        if(this%HasRoot(Key=Key)) then
            Entry => this%DataBase(this%Hash(Key=Key))%Root%GetEntry(Key=Key)
            if(associated(Entry)) then
                Wrapper => Entry%PointToValue()
                select type(Wrapper)
                    class is (DimensionsWrapper6D_t)
                        call Wrapper%GetPolymorphic(Value=Value)
                end select
            end if
        endif
    end subroutine ParameterListEntryContainer_GetPolymorphic6D


    subroutine ParameterListEntryContainer_GetPolymorphic7D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a 7D array Value given the Key
    !-----------------------------------------------------------------
        class(ParameterListEntryContainer_t), intent(IN)    :: this                 !< Parameter List Entry Containter type
        character(len=*),                     intent(IN)    :: Key                  !< String Key
        class(*), allocatable,                intent(OUT)   :: Value(:,:,:,:,:,:,:) !< Returned value
        class(ParameterListEntry_t), pointer                :: Entry                !< Pointer to a Parameter List
        class(*),                    pointer                :: Wrapper              !< Wrapper
    !-----------------------------------------------------------------
        if(this%HasRoot(Key=Key)) then
            Entry => this%DataBase(this%Hash(Key=Key))%Root%GetEntry(Key=Key)
            if(associated(Entry)) then
                Wrapper => Entry%PointToValue()
                select type(Wrapper)
                    class is (DimensionsWrapper7D_t)
                        call Wrapper%GetPolymorphic(Value=Value)
                end select
            end if
        endif
    end subroutine ParameterListEntryContainer_GetPolymorphic7D



    function ParameterListEntryContainer_isPresent(this,Key) result(isPresent)
    !-----------------------------------------------------------------
    !< Check if a Key is present in the DataBase
    !-----------------------------------------------------------------
        class(ParameterListEntryContainer_t), intent(IN) :: this      !< Parameter List Entry Containter type
        character(len=*),                     intent(IN) :: Key       !< String Key
        logical                                          :: isPresent !< Boolean flag to check if a Key is present
    !-----------------------------------------------------------------
        isPresent = .false.
        if(this%HasRoot(Key=Key)) then
            isPresent = this%DataBase(this%Hash(Key=Key))%Root%isPresent(Key=Key)
        endif
    end function ParameterListEntryContainer_isPresent


    subroutine ParameterListEntryContainer_RemoveEntry(this, Key)
    !-----------------------------------------------------------------
    !< Remove an Entry given a Key
    !-----------------------------------------------------------------
        class(ParameterListEntryContainer_t), intent(INOUT) :: this          !< Parameter List Entry Containter type
        character(len=*),                     intent(IN)    :: Key           !< String Key
        class(ParameterListEntry_t), pointer                :: PreviousEntry !< The Previous Entry of a given key
        class(ParameterListEntry_t), pointer                :: CurrentEntry  !< Entry of a given key
        class(ParameterListEntry_t), pointer                :: NextEntry     !< The Next Node of a given key
    !-----------------------------------------------------------------
        if(this%HasRoot(Key=Key)) then
            call this%DataBase(this%Hash(Key=Key))%Root%RemoveNode(Key=Key,Root=this%DataBase(this%Hash(Key=Key))%Root)
        endif
    end subroutine ParameterListEntryContainer_RemoveEntry


    function ParameterListEntryContainer_GetLength(this) result(Length)
    !-----------------------------------------------------------------
    !< Return the number of ParameterListEntries contained in the DataBase
    !-----------------------------------------------------------------
        class(ParameterListEntryContainer_t), intent(IN) :: this       !< Parameter List Entry Containter type
        integer(I4P)                                     :: Length     !< Number of parameters in database
        integer(I4P)                                     :: DBIterator !< Database Iterator index 
    !-----------------------------------------------------------------
        Length = 0
        if (allocated(this%DataBase)) THEN
            do DBIterator=lbound(this%DataBase,dim=1),ubound(this%DataBase,dim=1)
                if(associated(this%DataBase(DBIterator)%Root)) &
                    Length = Length + this%DataBase(DBIterator)%Root%GetLength()
            enddo
        endif
    end function ParameterListEntryContainer_GetLength


    subroutine ParameterListEntryContainer_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print the content of the DataBase
    !-----------------------------------------------------------------
        class(ParameterListEntryContainer_t), intent(IN)  :: this    !< Linked List
        integer(I4P),                         intent(IN)  :: unit    !< Logic unit.
        character(*), optional,               intent(IN)  :: prefix  !< Prefixing string.
        integer(I4P), optional,               intent(OUT) :: iostat  !< IO error.
        character(*), optional,               intent(OUT) :: iomsg   !< IO error message.
        character(len=:), allocatable                     :: prefd   !< Prefixing string.
        integer(I4P)                                      :: iostatd !< IO error.
        character(500)                                    :: iomsgd  !< Temporary variable for IO error message.
        integer(I4P)                                      :: DBIter  !< Database iterator
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(*,fmt='(A)') prefd//' PARAMETER LIST CONTENT:'
        write(*,fmt='(A)') prefd//' -----------------------'
        if (allocated(this%DataBase)) then
            do DBIter=lbound(this%DataBase,dim=1), ubound(this%DataBase,dim=1)
                if(associated(this%DataBase(DBIter)%Root))                              &
                    call this%DataBase(DBIter)%Root%Print(unit=unit,                    &
                        prefix=prefd//'  ['//trim(str(no_sign=.true., n=DBIter))//'] ', &
                        iostat=iostatd,iomsg=iomsgd)
            enddo
        endif
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine ParameterListEntryContainer_Print


end module ParameterListEntryContainer
