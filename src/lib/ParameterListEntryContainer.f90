    !-----------------------------------------------------------------
    ! ParameterList is a datatype containing a DataBase
    ! array of ParameterListEntries made to store diferent Entries
    ! depending on the hash of his Key.
    !
    ! This work takes as a starting point the previou work of
    ! Stefano Zaghi (@szaghi, https://github.com/szaghi).
    !
    ! You can find the original source at:
    ! https://github.com/szaghi/OFF/blob/95691ca15e6d68128ba016e40df74e42123f1c54/src/Data_Type_Hash_Table.f90
    !-----------------------------------------------------------------

module ParameterList

USE IR_Precision
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
save

    type, public:: ParameterList_t
    private
        type(ParameterRootEntry_t), allocatable :: DataBase(:)
        integer(I4P)                            :: Size = 0_I4P
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
        procedure         ::                   ParameterList_GetPolymorphic0D
        procedure         ::                   ParameterList_GetPolymorphic1D
        procedure         ::                   ParameterList_GetPolymorphic2D
        procedure         ::                   ParameterList_GetPolymorphic3D
        procedure         ::                   ParameterList_GetPolymorphic4D
        procedure         ::                   ParameterList_GetPolymorphic5D
        procedure         ::                   ParameterList_GetPolymorphic6D
        procedure         ::                   ParameterList_GetPolymorphic7D
        procedure         ::                   ParameterList_HasRootFromKey
        procedure         ::                   ParameterList_HasRootFromHash
        generic           :: HasRoot        => ParameterList_HasRootFromKey, &
                                               ParameterList_HasRootFromHash
        procedure         :: Hash           => ParameterList_Hash
        procedure         :: AddWrapperNode => ParameterList_AddWrapperNode
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
        generic,   public :: GetPolymorphic => ParameterList_GetPolymorphic0D, &
                                               ParameterList_GetPolymorphic1D, &
                                               ParameterList_GetPolymorphic2D, &
                                               ParameterList_GetPolymorphic3D, &
                                               ParameterList_GetPolymorphic4D, &
                                               ParameterList_GetPolymorphic5D, &
                                               ParameterList_GetPolymorphic6D, &
                                               ParameterList_GetPolymorphic7D
        procedure, public :: isPresent      => ParameterList_isPresent
!        procedure, public :: isOfDataType   => ParameterList_isOfDataType
!        procedure, public :: isSubList      => ParameterList_isSubList
        procedure, public :: Del            => ParameterList_RemoveEntry
        procedure, public :: Length         => ParameterList_GetLength
        final             ::                   ParameterList_Finalize
    end type ParameterList_t


contains


    function ParameterList_Hash(this,Key) result(Hash)
    !-----------------------------------------------------------------
    !< String hash function
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN) :: this        !< Parameter List Entry Containter type
        character(len=*),                     intent(IN) :: Key         !< String Key
        integer(I4P)                                     :: Hash        !< Hash code
        character, dimension(len(Key))                   :: CharArray   !< Character array containing the Key
        integer(I4P)                                     :: CharIterator!< Char iterator index
    !-----------------------------------------------------------------
        forall (CharIterator=1:LEN(Key))
            CharArray(CharIterator) = Key(CharIterator:CharIterator)
        end forall
        Hash = MOD(SUM(ICHAR(CharArray)), this%Size)
    end function ParameterList_Hash


    subroutine ParameterList_Init(this,Size)
    !-----------------------------------------------------------------
    !< Allocate the database with a given Szie of DefaultDataBaseSize
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(INOUT) :: this   !< Parameter List Entry Containter type
        integer(I4P), optional,               intent(IN)    :: Size   !< DataBase Size
    !-----------------------------------------------------------------
        call this%Free()
        if (present(Size)) then
            this%Size = Size
        else
            this%Size = DefaultDataBaseSize
        endif
        allocate(this%DataBase(0:this%Size-1))
    end subroutine ParameterList_Init


    function ParameterList_GetShape(this,Key) result(ValueShape)
    !-----------------------------------------------------------------
    !< Return a scalar Value given the Key
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN)    :: this           !< Parameter List Entry Containter
        character(len=*),                     intent(IN)    :: Key            !< String Key
        class(ParameterEntry_t),     pointer                :: Entry          !< Pointer to a Parameter List
        class(*),                    pointer                :: Wrapper        !< Wrapper
        integer(I4P), allocatable                           :: ValueShape(:)
    !-----------------------------------------------------------------
        Entry => this%DataBase(this%Hash(Key=Key))%GetEntry(Key=Key)
        if(associated(Entry)) then
            Wrapper => Entry%PointToValue()
            select type(Wrapper)
                class is (DimensionsWrapper_t)
                    ValueShape = Wrapper%GetShape()
            end select
        end if
    end function ParameterList_GetShape


    function ParameterList_HasRootFromKey(this,Key) result(HasRoot)
    !-----------------------------------------------------------------
    !< Check if the DataBase position for a given Key has a root node
    !-----------------------------------------------------------------
        class(ParameterList_t),                  intent(IN) :: this    !< Parameter List Entry Containter type
        character(len=*),                        intent(IN) :: Key     !< String Key
        logical                                             :: HasRoot !< Check if has root node
    !-----------------------------------------------------------------
        HasRoot = associated(this%DataBase(this%Hash(Key=Key))%GetRoot())
    end function


    function ParameterList_HasRootFromHash(this,Hash) result(HasRoot)
    !-----------------------------------------------------------------
    !< Check if the DataBase position for a given Key has a root node
    !-----------------------------------------------------------------
        class(ParameterList_t),                  intent(IN) :: this    !< Parameter List Entry Containter type
        integer(I4P),                            intent(IN) :: Hash    !< Hash code
        logical                                             :: HasRoot !< Check if has root node
    !-----------------------------------------------------------------
        HasRoot = associated(this%DataBase(Hash)%GetRoot())
    end function

    subroutine ParameterList_AddWrapperNode(this,Key,Wrapper)
    !-----------------------------------------------------------------
    !< Set a Key/Wrapper pair into the DataBase
    !-----------------------------------------------------------------
        class(ParameterList_t),                  intent(INOUT) :: this    !< Parameter List Entry Containter type
        character(len=*),                        intent(IN)    :: Key     !< String Key
        class(DimensionsWrapper_t),              intent(IN)    :: Wrapper !< Wrapper
        integer(I4P)                                           :: Hash
    !-----------------------------------------------------------------
        Hash = this%Hash(Key=Key)
!        if(.not. this%HasRoot(Hash=Hash)) then
!             allocate(this%DataBase(Hash)%Root)  
!        endif
        call this%DataBase(Hash)%AddEntry(Key=Key,Value=Wrapper)
    end subroutine ParameterList_AddWrapperNode


    subroutine ParameterList_Free(this)
    !-----------------------------------------------------------------
    !< Free ParameterListEntries and the DataBase
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(INOUT) :: this       !< Parameter List Entry Containter type
        integer(I4P)                                        :: DBIterator !< Database Iterator index 
    !-----------------------------------------------------------------
        if (allocated(this%DataBase)) THEN
            do DBIterator=lbound(this%DataBase,dim=1),ubound(this%DataBase,dim=1)
                if(this%HasRoot(Hash=DBIterator)) then
                    call this%DataBase(DBIterator)%Free()
                endif
            enddo
            deallocate(this%DataBase)
        endif
        this%Size = 0_I4P
    end subroutine ParameterList_Free


    subroutine ParameterList_Finalize(this)
    !-----------------------------------------------------------------
    !< Destructor procedure
    !-----------------------------------------------------------------
        type(ParameterList_t),               intent(INOUT) :: this    !< Parameter List Entry Containter type
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine ParameterList_Finalize


    function ParameterList_NewSubList(this,Key, Size) result(SubListPointer)
    !-----------------------------------------------------------------
    !< Set a Key/Value pair into the DataBase
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(INOUT) :: this           !< Parameter List Entry Containter type
        character(len=*),                     intent(IN)    :: Key            !< String Key
        integer(I4P), optional,               intent(IN)    :: Size           !< Sublist Size
        class(ParameterList_t),               Pointer       :: SublistPointer !< Pointer to the New SubList
        class(*),                             pointer       :: Value          !< Pointer to the New SubList
        class(ParameterEntry_t),              pointer       :: Entry          !< Pointer to a Parameter List Entry
        type(ParameterList_t)                               :: Sublist        !< New Sublist
        integer(I4P)                                        :: SublistSize    !< Sublist real Size
        integer(I4P)                                        :: Hash           !< Hash code corresponding to Key
    !-----------------------------------------------------------------
        nullify(SubListPointer)
        SublistSize = DefaultDataBaseSize
        if(present(Size)) SublistSize = Size
        Hash = this%Hash(Key=Key)
        if(.not. this%HasRoot(Hash=Hash)) then
             allocate(this%DataBase(Hash)%Root)  
        endif
        call this%DataBase(Hash)%AddEntry(Key=Key,Value=Sublist)
        Entry => this%DataBase(Hash)%GetEntry(Key=Key)
        if(associated(Entry)) then
            Value => Entry%PointToValue()
            select type(Value)
                class is (ParameterList_t)
                    SubListPointer => Value
                    call SublistPointer%Init(Size=SublistSize)
            end select
        end if
    end function ParameterList_NewSubList


    function ParameterList_GetSublist(this,Key) result(Sublist)
    !-----------------------------------------------------------------
    !< Return a Unlimited polymorphic pointer to a Value given the Key
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN)    :: this    !< Parameter List Entry Containter
        character(len=*),                     intent(IN)    :: Key     !< String Key
        class(*),                              pointer      :: Value   !< Returned pointer to value
        class(ParameterEntry_t),               pointer      :: Entry   !< Pointer to a Parameter List
        class(ParameterList_T),                pointer      :: Sublist !< Wrapper
        integer(I4P)                                        :: Hash    !< Hash code corresponding to Key
    !-----------------------------------------------------------------
        Hash = this%Hash(Key=Key)
        if(this%HasRoot(Hash=Hash)) then
            Entry => this%DataBase(Hash)%GetEntry(Key=Key)
            if(associated(Entry)) then
                Value => Entry%PointToValue()
                select type(Value)
                    class is (ParameterList_t)
                        SubList => Value
                end select
            end if
        endif
    end function ParameterList_GetSubList


    subroutine ParameterList_Set0D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Set a Key/Value pair into the DataBase
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(INOUT) :: this    !< Parameter List Entry Containter type
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
    end subroutine ParameterList_Set0D


    subroutine ParameterList_Set1D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Set a Key/Value pair into the DataBase
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(INOUT) :: this    !< Parameter List Entry Containter type
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
    end subroutine ParameterList_Set1D


    subroutine ParameterList_Set2D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Set a Key/Value pair into the DataBase
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(INOUT) :: this    !< Parameter List Entry Containter type
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
    end subroutine ParameterList_Set2D


    subroutine ParameterList_Set3D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Set a Key/Value pair into the DataBase
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(INOUT) :: this    !< Parameter List Entry Containter type
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
    end subroutine ParameterList_Set3D


    subroutine ParameterList_Set4D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Set a Key/Value pair into the DataBase
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(INOUT) :: this    !< Parameter List Entry Containter type
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
    end subroutine ParameterList_Set4D


    subroutine ParameterList_Set5D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Set a Key/Value pair into the DataBase
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(INOUT) :: this    !< Parameter List Entry Containter type
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
    end subroutine ParameterList_Set5D


    subroutine ParameterList_Set6D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Set a Key/Value pair into the DataBase
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(INOUT) :: this    !< Parameter List Entry Containter type
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
    end subroutine ParameterList_Set6D


    subroutine ParameterList_Set7D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Set a Key/Value pair into the DataBase
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(INOUT) :: this    !< Parameter List Entry Containter type
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
    end subroutine ParameterList_Set7D


    subroutine ParameterList_Get0D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a scalar Value given the Key
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN)    :: this           !< Parameter List Entry Containter
        character(len=*),                     intent(IN)    :: Key            !< String Key
        class(*),                             intent(INOUT) :: Value          !< Returned value
        class(ParameterEntry_t),     pointer                :: Entry          !< Pointer to a Parameter List
        class(*),                    pointer                :: Wrapper        !< Wrapper
        integer(I4P)                                        :: Hash           !< Hash code corresponding to Key
    !-----------------------------------------------------------------
        Hash = this%Hash(Key=Key)
        if(this%HasRoot(Hash=Hash)) then
            Entry => this%DataBase(Hash)%GetEntry(Key=Key)
            if(associated(Entry)) then
                Wrapper => Entry%PointToValue()
                select type(Wrapper)
                    class is (DimensionsWrapper0D_t)
                        call Wrapper%Get(Value=Value)
                end select
            end if
        endif
    end subroutine ParameterList_Get0D


    subroutine ParameterList_Get1D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a vector Value given the Key
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN)    :: this           !< Parameter List Entry Containter
        character(len=*),                     intent(IN)    :: Key            !< String Key
        class(*),                             intent(INOUT) :: Value(:)       !< Returned value
        class(ParameterEntry_t),     pointer                :: Entry          !< Pointer to a Parameter List
        class(*),                    pointer                :: Wrapper        !< Wrapper
        integer(I4P)                                        :: Hash           !< Hash code corresponding to Key
    !-----------------------------------------------------------------
        Hash = this%Hash(Key=Key)
        if(this%HasRoot(Hash=Hash)) then
            Entry => this%DataBase(Hash)%GetEntry(Key=Key)
            if(associated(Entry)) then
                Wrapper => Entry%PointToValue()
                select type(Wrapper)
                    class is (DimensionsWrapper1D_t)
                        call Wrapper%Get(Value=Value)
                end select
            end if
        endif
    end subroutine ParameterList_Get1D


    subroutine ParameterList_Get2D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a 2D array Value given the Key
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN)    :: this           !< Parameter List Entry Containter 
        character(len=*),                     intent(IN)    :: Key            !< String Key
        class(*),                             intent(INOUT) :: Value(:,:)     !< Returned value
        class(ParameterEntry_t),     pointer                :: Entry          !< Pointer to a Parameter List
        class(*),                    pointer                :: Wrapper        !< Wrapper
        integer(I4P)                                        :: Hash           !< Hash code corresponding to Key
    !-----------------------------------------------------------------
        Hash = this%Hash(Key=Key)
        if(this%HasRoot(Hash=Hash)) then
            Entry => this%DataBase(Hash)%GetEntry(Key=Key)
            if(associated(Entry)) then
                Wrapper => Entry%PointToValue()
                select type(Wrapper)
                    class is (DimensionsWrapper2D_t)
                        call Wrapper%Get(Value=Value)
                end select
            end if
        endif
    end subroutine ParameterList_Get2D


    subroutine ParameterList_Get3D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a 3D array Value given the Key
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN)    :: this           !< Parameter List Entry Containter type
        character(len=*),                     intent(IN)    :: Key            !< String Key
        class(*),                             intent(INOUT) :: Value(:,:,:)   !< Returned value
        class(ParameterEntry_t),     pointer                :: Entry          !< Pointer to a Parameter List
        class(*),                    pointer                :: Wrapper        !< Wrapper
        integer(I4P)                                        :: Hash           !< Hash code corresponding to Key
    !-----------------------------------------------------------------
        Hash = this%Hash(Key=Key)
        if(this%HasRoot(Hash=Hash)) then
            Entry => this%DataBase(Hash)%GetEntry(Key=Key)
            if(associated(Entry)) then
                Wrapper => Entry%PointToValue()
                select type(Wrapper)
                    class is (DimensionsWrapper3D_t)
                        call Wrapper%Get(Value=Value)
                end select
            end if
        endif
    end subroutine ParameterList_Get3D


    subroutine ParameterList_Get4D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a 4D array Value given the Key
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN)    :: this           !< Parameter List Entry Containter type
        character(len=*),                     intent(IN)    :: Key            !< String Key
        class(*),                             intent(INOUT) :: Value(:,:,:,:) !< Returned value
        class(ParameterEntry_t),     pointer                :: Entry          !< Pointer to a Parameter List
        class(*),                    pointer                :: Wrapper        !< Wrapper
        integer(I4P)                                        :: Hash           !< Hash code corresponding to Key
    !-----------------------------------------------------------------
        Hash = this%Hash(Key=Key)
        if(this%HasRoot(Hash=Hash)) then
            Entry => this%DataBase(Hash)%GetEntry(Key=Key)
            if(associated(Entry)) then
                Wrapper => Entry%PointToValue()
                select type(Wrapper)
                    class is (DimensionsWrapper4D_t)
                        call Wrapper%Get(Value=Value)
                end select
            end if
        endif
    end subroutine ParameterList_Get4D


    subroutine ParameterList_Get5D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a 5D array Value given the Key
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN)    :: this             !< Parameter List Entry Containter type
        character(len=*),                     intent(IN)    :: Key              !< String Key
        class(*),                             intent(INOUT) :: Value(:,:,:,:,:) !< Returned value
        class(*), pointer                                   :: Node             !< Pointer to a Parameter List
        class(ParameterEntry_t),     pointer                :: Entry            !< Pointer to a Parameter List
        class(*),                    pointer                :: Wrapper          !< Wrapper
        integer(I4P)                                        :: Hash             !< Hash code corresponding to Key
    !-----------------------------------------------------------------
        Hash = this%Hash(Key=Key)
        if(this%HasRoot(Hash=Hash)) then
            Entry => this%DataBase(Hash)%GetEntry(Key=Key)
            if(associated(Entry)) then
                Wrapper => Entry%PointToValue()
                select type(Wrapper)
                    class is (DimensionsWrapper5D_t)
                        call Wrapper%Get(Value=Value)
                end select
            end if
        endif
    end subroutine ParameterList_Get5D


    subroutine ParameterList_Get6D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a 6D array Value given the Key
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN)    :: this               !< Parameter List Entry Containter type
        character(len=*),                     intent(IN)    :: Key                !< String Key
        class(*),                             intent(INOUT) :: Value(:,:,:,:,:,:) !< Returned value
        class(ParameterEntry_t),     pointer                :: Entry              !< Pointer to a Parameter List
        class(*),                    pointer                :: Wrapper            !< Wrapper
        integer(I4P)                                        :: Hash               !< Hash code corresponding to Key
    !-----------------------------------------------------------------
        Hash = this%Hash(Key=Key)
        if(this%HasRoot(Hash=Hash)) then
            Entry => this%DataBase(Hash)%GetEntry(Key=Key)
            if(associated(Entry)) then
                Wrapper => Entry%PointToValue()
                select type(Wrapper)
                    class is (DimensionsWrapper6D_t)
                        call Wrapper%Get(Value=Value)
                end select
            end if
        endif
    end subroutine ParameterList_Get6D


    subroutine ParameterList_Get7D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a 7D array Value given the Key
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN)    :: this                 !< Parameter List Entry Containter type
        character(len=*),                     intent(IN)    :: Key                  !< String Key
        class(*),                             intent(INOUT) :: Value(:,:,:,:,:,:,:) !< Returned value
        class(ParameterEntry_t),     pointer                :: Entry                !< Pointer to a Parameter List
        class(*),                    pointer                :: Wrapper              !< Wrapper
        integer(I4P)                                        :: Hash                 !< Hash code corresponding to Key
    !-----------------------------------------------------------------
        Hash = this%Hash(Key=Key)
        if(this%HasRoot(Hash=Hash)) then
            Entry => this%DataBase(Hash)%GetEntry(Key=Key)
            if(associated(Entry)) then
                Wrapper => Entry%PointToValue()
                select type(Wrapper)
                    class is (DimensionsWrapper7D_t)
                        call Wrapper%Get(Value=Value)
                end select
            end if
        endif
    end subroutine ParameterList_Get7D


    subroutine ParameterList_GetPointer0D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a Unlimited polymorphic pointer to a Value given the Key
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN)    :: this    !< Parameter List Entry Containter
        character(len=*),                     intent(IN)    :: Key     !< String Key
        class(*), pointer,                    intent(INOUT) :: Value   !< Returned pointer to value
        class(ParameterEntry_t),     pointer                :: Entry   !< Pointer to a Parameter List
        class(*),                    pointer                :: Wrapper !< Wrapper
        integer(I4P)                                        :: Hash    !< Hash code corresponding to Key
    !-----------------------------------------------------------------
        Hash = this%Hash(Key=Key)
        if(this%HasRoot(Hash=Hash)) then
            Entry => this%DataBase(Hash)%GetEntry(Key=Key)
            if(associated(Entry)) then
                Wrapper => Entry%PointToValue()
                select type(Wrapper)
                    class is (DimensionsWrapper0D_t)
                        Value => Wrapper%GetPointer()
                end select
            end if
        endif
    end subroutine ParameterList_GetPointer0D


    subroutine ParameterList_GetPointer1D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a Unlimited polymorphic pointer to a Value given the Key
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN)    :: this     !< Parameter List Entry Containter
        character(len=*),                     intent(IN)    :: Key      !< String Key
        class(*), pointer,                    intent(INOUT) :: Value(:) !< Returned pointer to value
        class(ParameterEntry_t),     pointer                :: Entry    !< Pointer to a Parameter List
        class(*),                    pointer                :: Wrapper  !< Wrapper
        integer(I4P)                                        :: Hash     !< Hash code corresponding to Key
    !-----------------------------------------------------------------
        Hash = this%Hash(Key=Key)
        if(this%HasRoot(Hash=Hash)) then
            Entry => this%DataBase(Hash)%GetEntry(Key=Key)
            if(associated(Entry)) then
                Wrapper => Entry%PointToValue()
                select type(Wrapper)
                    class is (DimensionsWrapper1D_t)
                        Value => Wrapper%GetPointer()
                end select
            end if
        endif
    end subroutine ParameterList_GetPointer1D


    subroutine ParameterList_GetPointer2D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a Unlimited polymorphic pointer to a Value given the Key
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN)    :: this       !< Parameter List Entry Containter
        character(len=*),                     intent(IN)    :: Key        !< String Key
        class(*), pointer,                    intent(INOUT) :: Value(:,:) !< Returned pointer to value
        class(ParameterEntry_t),     pointer                :: Entry      !< Pointer to a Parameter List
        class(*),                    pointer                :: Wrapper    !< Wrapper
        integer(I4P)                                        :: Hash       !< Hash code corresponding to Key
    !-----------------------------------------------------------------
        Hash = this%Hash(Key=Key)
        if(this%HasRoot(Hash=Hash)) then
            Entry => this%DataBase(Hash)%GetEntry(Key=Key)
            if(associated(Entry)) then
                Wrapper => Entry%PointToValue()
                select type(Wrapper)
                    class is (DimensionsWrapper2D_t)
                        Value => Wrapper%GetPointer()
                end select
            end if
        endif
    end subroutine ParameterList_GetPointer2D


    subroutine ParameterList_GetPointer3D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a Unlimited polymorphic pointer to a Value given the Key
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN)    :: this           !< Parameter List Entry Containter
        character(len=*),                     intent(IN)    :: Key            !< String Key
        class(*), pointer,                    intent(INOUT) :: Value(:,:,:)   !< Returned pointer to value
        class(ParameterEntry_t),     pointer                :: Entry          !< Pointer to a Parameter List
        class(*),                    pointer                :: Wrapper        !< Wrapper
        integer(I4P)                                        :: Hash           !< Hash code corresponding to Key
    !-----------------------------------------------------------------
        Hash = this%Hash(Key=Key)
        if(this%HasRoot(Hash=Hash)) then
            Entry => this%DataBase(Hash)%GetEntry(Key=Key)
            if(associated(Entry)) then
                Wrapper => Entry%PointToValue()
                select type(Wrapper)
                    class is (DimensionsWrapper3D_t)
                        Value => Wrapper%GetPointer()
                end select
            end if
        endif
    end subroutine ParameterList_GetPointer3D


    subroutine ParameterList_GetPointer4D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a Unlimited polymorphic pointer to a Value given the Key
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN)    :: this           !< Parameter List Entry Containter
        character(len=*),                     intent(IN)    :: Key            !< String Key
        class(*), pointer,                    intent(INOUT) :: Value(:,:,:,:) !< Returned pointer to value
        class(ParameterEntry_t),     pointer                :: Entry          !< Pointer to a Parameter List
        class(*),                    pointer                :: Wrapper        !< Wrapper
        integer(I4P)                                        :: Hash           !< Hash code corresponding to Key
    !-----------------------------------------------------------------
        Hash = this%Hash(Key=Key)
        if(this%HasRoot(Hash=Hash)) then
            Entry => this%DataBase(Hash)%GetEntry(Key=Key)
            if(associated(Entry)) then
                Wrapper => Entry%PointToValue()
                select type(Wrapper)
                    class is (DimensionsWrapper4D_t)
                        Value => Wrapper%GetPointer()
                end select
            end if
        endif
    end subroutine ParameterList_GetPointer4D


    subroutine ParameterList_GetPointer5D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a Unlimited polymorphic pointer to a Value given the Key
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN)    :: this             !< Parameter List Entry Containter
        character(len=*),                     intent(IN)    :: Key              !< String Key
        class(*), pointer,                    intent(INOUT) :: Value(:,:,:,:,:) !< Returned pointer to value
        class(ParameterEntry_t),     pointer                :: Entry            !< Pointer to a Parameter List
        class(*),                    pointer                :: Wrapper          !< Wrapper
        integer(I4P)                                        :: Hash             !< Hash code corresponding to Key
    !-----------------------------------------------------------------
        Hash = this%Hash(Key=Key)
        if(this%HasRoot(Hash=Hash)) then
            Entry => this%DataBase(Hash)%GetEntry(Key=Key)
            if(associated(Entry)) then
                Wrapper => Entry%PointToValue()
                select type(Wrapper)
                    class is (DimensionsWrapper5D_t)
                        Value => Wrapper%GetPointer()
                end select
            end if
        endif
    end subroutine ParameterList_GetPointer5D


    subroutine ParameterList_GetPointer6D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a Unlimited polymorphic pointer to a Value given the Key
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN)    :: this               !< Parameter List Entry Containter
        character(len=*),                     intent(IN)    :: Key                !< String Key
        class(*), pointer,                    intent(INOUT) :: Value(:,:,:,:,:,:) !< Returned pointer to value
        class(ParameterEntry_t),     pointer                :: Entry              !< Pointer to a Parameter List
        class(*),                    pointer                :: Wrapper            !< Wrapper
        integer(I4P)                                        :: Hash               !< Hash code corresponding to Key
    !-----------------------------------------------------------------
        Hash = this%Hash(Key=Key)
        if(this%HasRoot(Hash=Hash)) then
            Entry => this%DataBase(Hash)%GetEntry(Key=Key)
            if(associated(Entry)) then
                Wrapper => Entry%PointToValue()
                select type(Wrapper)
                    class is (DimensionsWrapper6D_t)
                        Value => Wrapper%GetPointer()
                end select
            end if
        endif
    end subroutine ParameterList_GetPointer6D


    subroutine ParameterList_GetPointer7D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a Unlimited polymorphic pointer to a Value given the Key
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN)    :: this                 !< Parameter List Entry Containter
        character(len=*),                     intent(IN)    :: Key                  !< String Key
        class(*), pointer,                    intent(INOUT) :: Value(:,:,:,:,:,:,:) !< Returned pointer to value
        class(ParameterEntry_t),     pointer                :: Entry                !< Pointer to a Parameter List
        class(*),                    pointer                :: Wrapper              !< Wrapper
        integer(I4P)                                        :: Hash                 !< Hash code corresponding to Key
    !-----------------------------------------------------------------
        Hash = this%Hash(Key=Key)
        if(this%HasRoot(Hash=Hash)) then
            Entry => this%DataBase(Hash)%GetEntry(Key=Key)
            if(associated(Entry)) then
                Wrapper => Entry%PointToValue()
                select type(Wrapper)
                    class is (DimensionsWrapper7D_t)
                        Value => Wrapper%GetPointer()
                end select
            end if
        endif
    end subroutine ParameterList_GetPointer7D


    subroutine ParameterList_GetPolymorphic0D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a scalar Value given the Key
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN)    :: this           !< Parameter List Entry Containter
        character(len=*),                     intent(IN)    :: Key            !< String Key
        class(*), allocatable,                intent(INOUT) :: Value          !< Returned value
        class(ParameterEntry_t),     pointer                :: Entry          !< Pointer to a Parameter List
        class(*),                    pointer                :: Wrapper        !< Wrapper
        integer(I4P)                                        :: Hash           !< Hash code corresponding to Key
    !-----------------------------------------------------------------
        Hash = this%Hash(Key=Key)
        if(this%HasRoot(Hash=Hash)) then
            Entry => this%DataBase(Hash)%GetEntry(Key=Key)
            if(associated(Entry)) then
                Wrapper => Entry%PointToValue()
                select type(Wrapper)
                    class is (DimensionsWrapper0D_t)
                        call Wrapper%GetPolymorphic(Value=Value)
                end select
            end if
        endif
    end subroutine ParameterList_GetPolymorphic0D


    subroutine ParameterList_GetPolymorphic1D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a vector Value given the Key
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN)    :: this           !< Parameter List Entry Containter
        character(len=*),                     intent(IN)    :: Key            !< String Key
        class(*), allocatable,                intent(OUT)   :: Value(:)       !< Returned value
        class(ParameterEntry_t),     pointer                :: Entry          !< Pointer to a Parameter List
        class(*),                    pointer                :: Wrapper        !< Wrapper
        integer(I4P)                                        :: Hash           !< Hash code corresponding to Key
    !-----------------------------------------------------------------
        Hash = this%Hash(Key=Key)
        if(this%HasRoot(Hash=Hash)) then
            Entry => this%DataBase(Hash)%GetEntry(Key=Key)
            if(associated(Entry)) then
                Wrapper => Entry%PointToValue()
                select type(Wrapper)
                    class is (DimensionsWrapper1D_t)
                        call Wrapper%GetPolymorphic(Value=Value)
                end select
            end if
        endif
    end subroutine ParameterList_GetPolymorphic1D


    subroutine ParameterList_GetPolymorphic2D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a 2D array Value given the Key
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN)    :: this           !< Parameter List Entry Containter 
        character(len=*),                     intent(IN)    :: Key            !< String Key
        class(*), allocatable,                intent(OUT)   :: Value(:,:)     !< Returned value
        class(ParameterEntry_t),     pointer                :: Entry          !< Pointer to a Parameter List
        class(*),                    pointer                :: Wrapper        !< Wrapper
        integer(I4P)                                        :: Hash           !< Hash code corresponding to Key
    !-----------------------------------------------------------------
        Hash = this%Hash(Key=Key)
        if(this%HasRoot(Hash=Hash)) then
            Entry => this%DataBase(Hash)%GetEntry(Key=Key)
            if(associated(Entry)) then
                Wrapper => Entry%PointToValue()
                select type(Wrapper)
                    class is (DimensionsWrapper2D_t)
                        call Wrapper%GetPolymorphic(Value=Value)
                end select
            end if
        endif
    end subroutine ParameterList_GetPolymorphic2D


    subroutine ParameterList_GetPolymorphic3D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a 3D array Value given the Key
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN)    :: this           !< Parameter List Entry Containter type
        character(len=*),                     intent(IN)    :: Key            !< String Key
        class(*), allocatable,                intent(OUT)   :: Value(:,:,:)   !< Returned value
        class(ParameterEntry_t),     pointer                :: Entry          !< Pointer to a Parameter List
        class(*),                    pointer                :: Wrapper        !< Wrapper
        integer(I4P)                                        :: Hash           !< Hash code corresponding to Key
    !-----------------------------------------------------------------
        Hash = this%Hash(Key=Key)
        if(this%HasRoot(Hash=Hash)) then
            Entry => this%DataBase(Hash)%GetEntry(Key=Key)
            if(associated(Entry)) then
                Wrapper => Entry%PointToValue()
                select type(Wrapper)
                    class is (DimensionsWrapper3D_t)
                        call Wrapper%GetPolymorphic(Value=Value)
                end select
            end if
        endif
    end subroutine ParameterList_GetPolymorphic3D


    subroutine ParameterList_GetPolymorphic4D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a 4D array Value given the Key
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN)    :: this           !< Parameter List Entry Containter type
        character(len=*),                     intent(IN)    :: Key            !< String Key
        class(*), allocatable,                intent(OUT)   :: Value(:,:,:,:) !< Returned value
        class(ParameterEntry_t),     pointer                :: Entry          !< Pointer to a Parameter List
        class(*),                    pointer                :: Wrapper        !< Wrapper
        integer(I4P)                                        :: Hash           !< Hash code corresponding to Key
    !-----------------------------------------------------------------
        Hash = this%Hash(Key=Key)
        if(this%HasRoot(Hash=Hash)) then
            Entry => this%DataBase(Hash)%GetEntry(Key=Key)
            if(associated(Entry)) then
                Wrapper => Entry%PointToValue()
                select type(Wrapper)
                    class is (DimensionsWrapper4D_t)
                        call Wrapper%GetPolymorphic(Value=Value)
                end select
            end if
        endif
    end subroutine ParameterList_GetPolymorphic4D


    subroutine ParameterList_GetPolymorphic5D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a 5D array Value given the Key
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN)    :: this             !< Parameter List Entry Containter type
        character(len=*),                     intent(IN)    :: Key              !< String Key
        class(*), allocatable,                intent(OUT)   :: Value(:,:,:,:,:) !< Returned value
        class(ParameterEntry_t),     pointer                :: Entry            !< Pointer to a Parameter List
        class(*),                    pointer                :: Wrapper          !< Wrapper
        integer(I4P)                                        :: Hash             !< Hash code corresponding to Key
    !-----------------------------------------------------------------
        Hash = this%Hash(Key=Key)
        if(this%HasRoot(Hash=Hash)) then
            Entry => this%DataBase(Hash)%GetEntry(Key=Key)
            if(associated(Entry)) then
                Wrapper => Entry%PointToValue()
                select type(Wrapper)
                    class is (DimensionsWrapper5D_t)
                        call Wrapper%GetPolymorphic(Value=Value)
                end select
            end if
        endif
    end subroutine ParameterList_GetPolymorphic5D


    subroutine ParameterList_GetPolymorphic6D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a 6D array Value given the Key
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN)    :: this               !< Parameter List Entry Containter type
        character(len=*),                     intent(IN)    :: Key                !< String Key
        class(*), allocatable,                intent(OUT)   :: Value(:,:,:,:,:,:) !< Returned value
        class(ParameterEntry_t),     pointer                :: Entry              !< Pointer to a Parameter List
        class(*),                    pointer                :: Wrapper            !< Wrapper
        integer(I4P)                                        :: Hash               !< Hash code corresponding to Key
    !-----------------------------------------------------------------
        Hash = this%Hash(Key=Key)
        if(this%HasRoot(Hash=Hash)) then
            Entry => this%DataBase(Hash)%GetEntry(Key=Key)
            if(associated(Entry)) then
                Wrapper => Entry%PointToValue()
                select type(Wrapper)
                    class is (DimensionsWrapper6D_t)
                        call Wrapper%GetPolymorphic(Value=Value)
                end select
            end if
        endif
    end subroutine ParameterList_GetPolymorphic6D


    subroutine ParameterList_GetPolymorphic7D(this,Key,Value)
    !-----------------------------------------------------------------
    !< Return a 7D array Value given the Key
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN)    :: this                 !< Parameter List Entry Containter type
        character(len=*),                     intent(IN)    :: Key                  !< String Key
        class(*), allocatable,                intent(OUT)   :: Value(:,:,:,:,:,:,:) !< Returned value
        class(ParameterEntry_t),     pointer                :: Entry                !< Pointer to a Parameter List
        class(*),                    pointer                :: Wrapper              !< Wrapper
        integer(I4P)                                        :: Hash                 !< Hash code corresponding to Key
    !-----------------------------------------------------------------
        Hash = this%Hash(Key=Key)
        if(this%HasRoot(Hash=Hash)) then
            Entry => this%DataBase(Hash)%GetEntry(Key=Key)
            if(associated(Entry)) then
                Wrapper => Entry%PointToValue()
                select type(Wrapper)
                    class is (DimensionsWrapper7D_t)
                        call Wrapper%GetPolymorphic(Value=Value)
                end select
            end if
        endif
    end subroutine ParameterList_GetPolymorphic7D



    function ParameterList_isPresent(this,Key) result(isPresent)
    !-----------------------------------------------------------------
    !< Check if a Key is present in the DataBase
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN) :: this      !< Parameter List Entry Containter type
        character(len=*),                     intent(IN) :: Key       !< String Key
        logical                                          :: isPresent !< Boolean flag to check if a Key is present
        integer(I4P)                                     :: Hash                 !< Hash code corresponding to Key
    !-----------------------------------------------------------------
        isPresent = .false.
        Hash = this%Hash(Key=Key)
        if(this%HasRoot(Hash=Hash)) then
            isPresent = this%DataBase(Hash)%isPresent(Key=Key)
        endif
    end function ParameterList_isPresent


    subroutine ParameterList_RemoveEntry(this, Key)
    !-----------------------------------------------------------------
    !< Remove an Entry given a Key
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(INOUT) :: this          !< Parameter List Entry Containter type
        character(len=*),                     intent(IN)    :: Key           !< String Key
        class(ParameterEntry_t),     pointer                :: PreviousEntry !< The Previous Entry of a given key
        class(ParameterEntry_t),     pointer                :: CurrentEntry  !< Entry of a given key
        class(ParameterEntry_t),     pointer                :: NextEntry     !< The Next Node of a given key
        integer(I4P)                                        :: Hash                 !< Hash code corresponding to Key
    !-----------------------------------------------------------------
        Hash = this%Hash(Key=Key)
        if(this%HasRoot(Hash=Hash)) then
            if(this%DataBase(Hash)%Root%HasKey()) then
                call this%DataBase(Hash)%RemoveEntry(Key=Key)
            endif
        endif
    end subroutine ParameterList_RemoveEntry


    function ParameterList_GetLength(this) result(Length)
    !-----------------------------------------------------------------
    !< Return the number of ParameterListEntries contained in the DataBase
    !-----------------------------------------------------------------
        class(ParameterList_t),               intent(IN) :: this       !< Parameter List Entry Containter type
        integer(I4P)                                     :: Length     !< Number of parameters in database
        integer(I4P)                                     :: DBIterator !< Database Iterator index 
    !-----------------------------------------------------------------
        Length = 0
        if (allocated(this%DataBase)) THEN
            do DBIterator=lbound(this%DataBase,dim=1),ubound(this%DataBase,dim=1)
                if(this%HasRoot(Hash=DBIterator)) &
                    Length = Length + this%DataBase(DBIterator)%GetLength()
            enddo
        endif
    end function ParameterList_GetLength


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
        integer(I4P)                                      :: DBIter  !< Database iterator
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(*,fmt='(A)') prefd//' PARAMETER LIST CONTENT:'
        write(*,fmt='(A)') prefd//' -----------------------'
        if (allocated(this%DataBase)) then
            do DBIter=lbound(this%DataBase,dim=1), ubound(this%DataBase,dim=1)
                if(this%HasRoot(Hash=DBIter))                                           &
                    call this%DataBase(DBIter)%Print(unit=unit,                         &
                        prefix=prefd//'  ['//trim(str(no_sign=.true., n=DBIter))//'] ', &
                        iostat=iostatd,iomsg=iomsgd)
            enddo
        endif
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine ParameterList_Print


end module ParameterList
