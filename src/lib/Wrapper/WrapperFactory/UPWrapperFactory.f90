module UPWrapperFactory

USE IR_Precision, only: I1P
USE WrapperFactory
USE DimensionsWrapper
USE DimensionsWrapper0D_UP
USE DimensionsWrapper1D_UP
USE DimensionsWrapper2D_UP
USE DimensionsWrapper3D_UP
USE DimensionsWrapper4D_UP
USE DimensionsWrapper5D_UP
USE DimensionsWrapper6D_UP
USE DimensionsWrapper7D_UP

implicit none
private

    type, extends(WrapperFactory_t) :: UPWrapperFactory_t
    private

    contains
        procedure         :: create0D => UPWrapperFactory_Create0D
        procedure         :: create1D => UPWrapperFactory_Create1D
        procedure         :: create2D => UPWrapperFactory_Create2D
        procedure         :: create3D => UPWrapperFactory_Create3D
        procedure         :: create4D => UPWrapperFactory_Create4D
        procedure         :: create5D => UPWrapperFactory_Create5D
        procedure         :: create6D => UPWrapperFactory_Create6D
        procedure         :: create7D => UPWrapperFactory_Create7D
        procedure, public :: hasSameType => UPWrapperFactory_hasSameType
    end type

public :: UPWrapperFactory_t

contains

    function UPWrapperFactory_hasSameType(this, Value) result(hasSameType)
        class(UPWrapperFactory_t), intent(IN) :: this
        class(*),                  intent(IN) :: Value
        logical                               :: hasSameType
        hasSameType = .true.
    end function UPWrapperFactory_hasSameType


    subroutine UPWrapperFactory_Create0D(this, Value, Wrapper)
    !-----------------------------------------------------------------
    !< Create UP 0D Wrapper
    !-----------------------------------------------------------------
        class(UPWrapperFactory_t),               intent(IN)    :: this
        class(*),                                intent(IN)    :: Value
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Value)) then
            allocate(DimensionsWrapper0D_UP_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=0_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper0D_UP_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end subroutine UPWrapperFactory_Create0D


    subroutine UPWrapperFactory_Create1D(this, Value, Wrapper)
    !-----------------------------------------------------------------
    !< Create UP 1D Wrapper
    !-----------------------------------------------------------------
        class(UPWrapperFactory_t),               intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Value(1))) then
            allocate(DimensionsWrapper1D_UP_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=1_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper1D_UP_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end subroutine UPWrapperFactory_Create1D


    subroutine UPWrapperFactory_Create2D(this, Value, Wrapper)
    !-----------------------------------------------------------------
    !< Create UP 2D Wrapper
    !-----------------------------------------------------------------
        class(UPWrapperFactory_t),               intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(:,:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Value(1,1))) then
            allocate(DimensionsWrapper2D_UP_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=2_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper2D_UP_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end subroutine UPWrapperFactory_Create2D


    subroutine UPWrapperFactory_Create3D(this, Value, Wrapper)
    !-----------------------------------------------------------------
    !< Create UP 3D Wrapper
    !-----------------------------------------------------------------
        class(UPWrapperFactory_t),               intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(:,:,:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Value(1,1,1))) then
            allocate(DimensionsWrapper3D_UP_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=3_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper3D_UP_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end subroutine UPWrapperFactory_Create3D


    subroutine UPWrapperFactory_Create4D(this, Value, Wrapper)
    !-----------------------------------------------------------------
    !< Create UP 4D Wrapper
    !-----------------------------------------------------------------
        class(UPWrapperFactory_t),               intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(:,:,:,:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Value(1,1,1,1))) then
            allocate(DimensionsWrapper4D_UP_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=4_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper4D_UP_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end subroutine UPWrapperFactory_Create4D


    subroutine UPWrapperFactory_Create5D(this, Value, Wrapper)
    !-----------------------------------------------------------------
    !< Create UP 5D Wrapper
    !-----------------------------------------------------------------
        class(UPWrapperFactory_t),               intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(:,:,:,:,:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Value(1,1,1,1,1))) then
            allocate(DimensionsWrapper5D_UP_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=5_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper5D_UP_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end subroutine UPWrapperFactory_Create5D


    subroutine UPWrapperFactory_Create6D(this, Value, Wrapper)
    !-----------------------------------------------------------------
    !< Create UP 6D Wrapper
    !-----------------------------------------------------------------
        class(UPWrapperFactory_t),               intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(:,:,:,:,:,:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Value(1,1,1,1,1,1))) then
            allocate(DimensionsWrapper6D_UP_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=6_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper6D_UP_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end subroutine UPWrapperFactory_Create6D


    subroutine UPWrapperFactory_Create7D(this, Value, Wrapper)
    !-----------------------------------------------------------------
    !< Create UP 7D Wrapper
    !-----------------------------------------------------------------
        class(UPWrapperFactory_t),               intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(:,:,:,:,:,:,:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Value(1,1,1,1,1,1,1))) then
            allocate(DimensionsWrapper7D_UP_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=7_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper7D_UP_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end subroutine UPWrapperFactory_Create7D


end module UPWrapperFactory
