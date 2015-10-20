module R8PWrapperFactory

USE WrapperFactory
USE IR_Precision, only: I1P, R8P
USE DimensionsWrapper
USE DimensionsWrapper0D_R8P
USE DimensionsWrapper1D_R8P
USE DimensionsWrapper2D_R8P
USE DimensionsWrapper3D_R8P
USE DimensionsWrapper4D_R8P
USE DimensionsWrapper5D_R8P
USE DimensionsWrapper6D_R8P
USE DimensionsWrapper7D_R8P

implicit none
private

    type, extends(WrapperFactory_t) :: R8PWrapperFactory_t
    private

    contains
        procedure         :: create0D => R8PWrapperFactory_Create0D
        procedure         :: create1D => R8PWrapperFactory_Create1D
        procedure         :: create2D => R8PWrapperFactory_Create2D
        procedure         :: create3D => R8PWrapperFactory_Create3D
        procedure         :: create4D => R8PWrapperFactory_Create4D
        procedure         :: create5D => R8PWrapperFactory_Create5D
        procedure         :: create6D => R8PWrapperFactory_Create6D
        procedure         :: create7D => R8PWrapperFactory_Create7D
        procedure, public :: hasSameType => R8PWrapperFactory_hasSameType
    end type

public :: R8PWrapperFactory_t

contains

    function R8PWrapperFactory_hasSameType(this, Value) result(hasSameType)
        class(R8PWrapperFactory_t), intent(IN) :: this
        class(*),                   intent(IN) :: Value
        logical                                :: hasSameType
        hasSameType = .false.
        select type(Value)
            type is (real(R8P))
                hasSameType = .true.
        end select
    end function R8PWrapperFactory_hasSameType


    subroutine R8PWrapperFactory_Create0D(this, Value, Wrapper)
    !-----------------------------------------------------------------
    !< Create R8P 0D Wrapper
    !-----------------------------------------------------------------
        class(R8PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Value)) then
            allocate(DimensionsWrapper0D_R8P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=0_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper0D_R8P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end subroutine R8PWrapperFactory_Create0D


    subroutine R8PWrapperFactory_Create1D(this, Value, Wrapper)
    !-----------------------------------------------------------------
    !< Create R8P 1D Wrapper
    !-----------------------------------------------------------------
        class(R8PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Value(1))) then
            allocate(DimensionsWrapper1D_R8P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=1_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper1D_R8P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end subroutine R8PWrapperFactory_Create1D


    subroutine R8PWrapperFactory_Create2D(this, Value, Wrapper)
    !-----------------------------------------------------------------
    !< Create R8P 2D Wrapper
    !-----------------------------------------------------------------
        class(R8PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(:,:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Value(1,1))) then
            allocate(DimensionsWrapper2D_R8P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=2_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper2D_R8P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end subroutine R8PWrapperFactory_Create2D


    subroutine R8PWrapperFactory_Create3D(this, Value, Wrapper)
    !-----------------------------------------------------------------
    !< Create R8P 3D Wrapper
    !-----------------------------------------------------------------
        class(R8PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(:,:,:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Value(1,1,1))) then
            allocate(DimensionsWrapper3D_R8P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=3_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper3D_R8P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end subroutine R8PWrapperFactory_Create3D


    subroutine R8PWrapperFactory_Create4D(this, Value, Wrapper)
    !-----------------------------------------------------------------
    !< Create R8P 4D Wrapper
    !-----------------------------------------------------------------
        class(R8PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(:,:,:,:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Value(1,1,1,1))) then
            allocate(DimensionsWrapper4D_R8P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=4_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper4D_R8P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end subroutine R8PWrapperFactory_Create4D


    subroutine R8PWrapperFactory_Create5D(this, Value, Wrapper)
    !-----------------------------------------------------------------
    !< Create R8P 5D Wrapper
    !-----------------------------------------------------------------
        class(R8PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(:,:,:,:,:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Value(1,1,1,1,1))) then
            allocate(DimensionsWrapper5D_R8P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=5_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper5D_R8P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end subroutine R8PWrapperFactory_Create5D


    subroutine R8PWrapperFactory_Create6D(this, Value, Wrapper)
    !-----------------------------------------------------------------
    !< Create R8P 6D Wrapper
    !-----------------------------------------------------------------
        class(R8PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(:,:,:,:,:,:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Value(1,1,1,1,1,1))) then
            allocate(DimensionsWrapper6D_R8P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=6_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper6D_R8P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end subroutine R8PWrapperFactory_Create6D


    subroutine R8PWrapperFactory_Create7D(this, Value, Wrapper)
    !-----------------------------------------------------------------
    !< Create R8P 7D Wrapper
    !-----------------------------------------------------------------
        class(R8PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(:,:,:,:,:,:,:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Value(1,1,1,1,1,1,1))) then
            allocate(DimensionsWrapper7D_R8P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=7_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper7D_R8P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end subroutine R8PWrapperFactory_Create7D


end module R8PWrapperFactory
