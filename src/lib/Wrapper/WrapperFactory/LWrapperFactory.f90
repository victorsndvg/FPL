module LWrapperFactory

USE WrapperFactory
USE DimensionsWrapper
USE DimensionsWrapper0D_L
USE DimensionsWrapper1D_L
USE DimensionsWrapper2D_L
USE DimensionsWrapper3D_L
USE DimensionsWrapper4D_L
USE DimensionsWrapper5D_L
USE DimensionsWrapper6D_L
USE DimensionsWrapper7D_L

implicit none
private

    type, extends(WrapperFactory_t) :: LWrapperFactory_t
    private

    contains
        procedure         :: create0D => LWrapperFactory_Create0D
        procedure         :: create1D => LWrapperFactory_Create1D
        procedure         :: create2D => LWrapperFactory_Create2D
        procedure         :: create3D => LWrapperFactory_Create3D
        procedure         :: create4D => LWrapperFactory_Create4D
        procedure         :: create5D => LWrapperFactory_Create5D
        procedure         :: create6D => LWrapperFactory_Create6D
        procedure         :: create7D => LWrapperFactory_Create7D
        procedure, public :: hasSameType => LWrapperFactory_hasSameType
    end type

public :: LWrapperFactory_t

contains

    function LWrapperFactory_hasSameType(this, Value) result(hasSameType)
        class(LWrapperFactory_t), intent(IN) :: this
        class(*),                   intent(IN) :: Value
        logical                                :: hasSameType
        hasSameType = .false.
        select type(Value)
            type is (logical)
                hasSameType = .true.
        end select
    end function LWrapperFactory_hasSameType


    subroutine LWrapperFactory_Create0D(this, Value, Wrapper)
    !-----------------------------------------------------------------
    !< Create L 0D Wrapper
    !-----------------------------------------------------------------
        class(LWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Value)) then
            allocate(DimensionsWrapper0D_L_t::Wrapper)
            select type (Wrapper)
                type is(DimensionsWrapper0D_L_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end subroutine LWrapperFactory_Create0D


    subroutine LWrapperFactory_Create1D(this, Value, Wrapper)
    !-----------------------------------------------------------------
    !< Create L 1D Wrapper
    !-----------------------------------------------------------------
        class(LWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Value(1))) then
            allocate(DimensionsWrapper1D_L_t::Wrapper)
            select type (Wrapper)
                type is(DimensionsWrapper1D_L_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end subroutine LWrapperFactory_Create1D


    subroutine LWrapperFactory_Create2D(this, Value, Wrapper)
    !-----------------------------------------------------------------
    !< Create L 2D Wrapper
    !-----------------------------------------------------------------
        class(LWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(:,:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Value(1,1))) then
            allocate(DimensionsWrapper2D_L_t::Wrapper)
            select type (Wrapper)
                type is(DimensionsWrapper2D_L_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end subroutine LWrapperFactory_Create2D


    subroutine LWrapperFactory_Create3D(this, Value, Wrapper)
    !-----------------------------------------------------------------
    !< Create L 3D Wrapper
    !-----------------------------------------------------------------
        class(LWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(:,:,:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Value(1,1,1))) then
            allocate(DimensionsWrapper3D_L_t::Wrapper)
            select type (Wrapper)
                type is(DimensionsWrapper3D_L_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end subroutine LWrapperFactory_Create3D


    subroutine LWrapperFactory_Create4D(this, Value, Wrapper)
    !-----------------------------------------------------------------
    !< Create L 4D Wrapper
    !-----------------------------------------------------------------
        class(LWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(:,:,:,:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Value(1,1,1,1))) then
            allocate(DimensionsWrapper4D_L_t::Wrapper)
            select type (Wrapper)
                type is(DimensionsWrapper4D_L_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end subroutine LWrapperFactory_Create4D


    subroutine LWrapperFactory_Create5D(this, Value, Wrapper)
    !-----------------------------------------------------------------
    !< Create L 5D Wrapper
    !-----------------------------------------------------------------
        class(LWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(:,:,:,:,:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Value(1,1,1,1,1))) then
            allocate(DimensionsWrapper5D_L_t::Wrapper)
            select type (Wrapper)
                type is(DimensionsWrapper5D_L_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end subroutine LWrapperFactory_Create5D


    subroutine LWrapperFactory_Create6D(this, Value, Wrapper)
    !-----------------------------------------------------------------
    !< Create L 6D Wrapper
    !-----------------------------------------------------------------
        class(LWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(:,:,:,:,:,:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Value(1,1,1,1,1,1))) then
            allocate(DimensionsWrapper6D_L_t::Wrapper)
            select type (Wrapper)
                type is(DimensionsWrapper6D_L_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end subroutine LWrapperFactory_Create6D


    subroutine LWrapperFactory_Create7D(this, Value, Wrapper)
    !-----------------------------------------------------------------
    !< Create L 7D Wrapper
    !-----------------------------------------------------------------
        class(LWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(:,:,:,:,:,:,:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Value(1,1,1,1,1,1,1))) then
            allocate(DimensionsWrapper7D_L_t::Wrapper)
            select type (Wrapper)
                type is(DimensionsWrapper7D_L_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end subroutine LWrapperFactory_Create7D


end module LWrapperFactory
