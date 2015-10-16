module DLCAWrapperFactory

USE WrapperFactory
USE DimensionsWrapper
USE DimensionsWrapper0D_DLCA
USE DimensionsWrapper1D_DLCA
USE DimensionsWrapper2D_DLCA
USE DimensionsWrapper3D_DLCA
USE DimensionsWrapper4D_DLCA
USE DimensionsWrapper5D_DLCA
USE DimensionsWrapper6D_DLCA
USE DimensionsWrapper7D_DLCA

implicit none
private

    type, extends(WrapperFactory_t) :: DLCAWrapperFactory_t
    private

    contains
        procedure         :: DLCAWrapperFactory_Create0D
        procedure         :: DLCAWrapperFactory_Create1D
        procedure         :: DLCAWrapperFactory_Create2D
        procedure         :: DLCAWrapperFactory_Create3D
        procedure         :: DLCAWrapperFactory_Create4D
        procedure         :: DLCAWrapperFactory_Create5D
        procedure         :: DLCAWrapperFactory_Create6D
        procedure         :: DLCAWrapperFactory_Create7D
        procedure, public :: hasSameType => DLCAWrapperFactory_hasSameType
        generic,   public :: Create      => DLCAWrapperFactory_Create0D, &
                                            DLCAWrapperFactory_Create1D, &
                                            DLCAWrapperFactory_Create2D, &
                                            DLCAWrapperFactory_Create3D, &
                                            DLCAWrapperFactory_Create4D, &
                                            DLCAWrapperFactory_Create5D, &
                                            DLCAWrapperFactory_Create6D, &
                                            DLCAWrapperFactory_Create7D
    end type

public :: DLCAWrapperFactory_t

contains

    function DLCAWrapperFactory_hasSameType(this, Value) result(hasSameType)
        class(DLCAWrapperFactory_t), intent(IN) :: this
        class(*),                   intent(IN) :: Value
        logical                                :: hasSameType
        hasSameType = .false.
        select type(Value)
            type is (character(len=*))
                hasSameType = .true.
        end select
    end function DLCAWrapperFactory_hasSameType


    subroutine DLCAWrapperFactory_Create0D(this, Value, Wrapper)
    !-----------------------------------------------------------------
    !< Create DLCA 0D Wrapper
    !-----------------------------------------------------------------
        class(DLCAWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Value)) then
            allocate(DimensionsWrapper0D_DLCA_t::Wrapper)
            select type (Wrapper)
                type is(DimensionsWrapper0D_DLCA_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end subroutine DLCAWrapperFactory_Create0D


    subroutine DLCAWrapperFactory_Create1D(this, Value, Wrapper)
    !-----------------------------------------------------------------
    !< Create DLCA 1D Wrapper
    !-----------------------------------------------------------------
        class(DLCAWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Value(1))) then
            allocate(DimensionsWrapper1D_DLCA_t::Wrapper)
            select type (Wrapper)
                type is(DimensionsWrapper1D_DLCA_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end subroutine DLCAWrapperFactory_Create1D


    subroutine DLCAWrapperFactory_Create2D(this, Value, Wrapper)
    !-----------------------------------------------------------------
    !< Create DLCA 2D Wrapper
    !-----------------------------------------------------------------
        class(DLCAWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(:,:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Value(1,1))) then
            allocate(DimensionsWrapper2D_DLCA_t::Wrapper)
            select type (Wrapper)
                type is(DimensionsWrapper2D_DLCA_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end subroutine DLCAWrapperFactory_Create2D


    subroutine DLCAWrapperFactory_Create3D(this, Value, Wrapper)
    !-----------------------------------------------------------------
    !< Create DLCA 3D Wrapper
    !-----------------------------------------------------------------
        class(DLCAWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(:,:,:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Value(1,1,1))) then
            allocate(DimensionsWrapper3D_DLCA_t::Wrapper)
            select type (Wrapper)
                type is(DimensionsWrapper3D_DLCA_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end subroutine DLCAWrapperFactory_Create3D


    subroutine DLCAWrapperFactory_Create4D(this, Value, Wrapper)
    !-----------------------------------------------------------------
    !< Create DLCA 4D Wrapper
    !-----------------------------------------------------------------
        class(DLCAWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(:,:,:,:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Value(1,1,1,1))) then
            allocate(DimensionsWrapper4D_DLCA_t::Wrapper)
            select type (Wrapper)
                type is(DimensionsWrapper4D_DLCA_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end subroutine DLCAWrapperFactory_Create4D


    subroutine DLCAWrapperFactory_Create5D(this, Value, Wrapper)
    !-----------------------------------------------------------------
    !< Create DLCA 5D Wrapper
    !-----------------------------------------------------------------
        class(DLCAWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(:,:,:,:,:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Value(1,1,1,1,1))) then
            allocate(DimensionsWrapper5D_DLCA_t::Wrapper)
            select type (Wrapper)
                type is(DimensionsWrapper5D_DLCA_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end subroutine DLCAWrapperFactory_Create5D


    subroutine DLCAWrapperFactory_Create6D(this, Value, Wrapper)
    !-----------------------------------------------------------------
    !< Create DLCA 6D Wrapper
    !-----------------------------------------------------------------
        class(DLCAWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(:,:,:,:,:,:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Value(1,1,1,1,1,1))) then
            allocate(DimensionsWrapper6D_DLCA_t::Wrapper)
            select type (Wrapper)
                type is(DimensionsWrapper6D_DLCA_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end subroutine DLCAWrapperFactory_Create6D


    subroutine DLCAWrapperFactory_Create7D(this, Value, Wrapper)
    !-----------------------------------------------------------------
    !< Create DLCA 7D Wrapper
    !-----------------------------------------------------------------
        class(DLCAWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(:,:,:,:,:,:,:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Value(1,1,1,1,1,1,1))) then
            allocate(DimensionsWrapper7D_DLCA_t::Wrapper)
            select type (Wrapper)
                type is(DimensionsWrapper7D_DLCA_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end subroutine DLCAWrapperFactory_Create7D


end module DLCAWrapperFactory
