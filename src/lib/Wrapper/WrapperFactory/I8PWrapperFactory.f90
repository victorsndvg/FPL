module I8PWrapperFactory

USE WrapperFactory
USE IR_Precision, only: I1P, I8P
USE DimensionsWrapper
USE DimensionsWrapper0D_I8P
USE DimensionsWrapper1D_I8P
USE DimensionsWrapper2D_I8P
USE DimensionsWrapper3D_I8P
USE DimensionsWrapper4D_I8P
USE DimensionsWrapper5D_I8P
USE DimensionsWrapper6D_I8P
USE DimensionsWrapper7D_I8P

implicit none
private

    type, extends(WrapperFactory_t) :: I8PWrapperFactory_t
    private

    contains
        procedure         :: create0D => I8PWrapperFactory_Create0D
        procedure         :: create1D => I8PWrapperFactory_Create1D
        procedure         :: create2D => I8PWrapperFactory_Create2D
        procedure         :: create3D => I8PWrapperFactory_Create3D
        procedure         :: create4D => I8PWrapperFactory_Create4D
        procedure         :: create5D => I8PWrapperFactory_Create5D
        procedure         :: create6D => I8PWrapperFactory_Create6D
        procedure         :: create7D => I8PWrapperFactory_Create7D
        procedure, public :: hasSameType => I8PWrapperFactory_hasSameType
    end type

    type(I8PWrapperFactory_t), public :: WrapperFactoryI8P

contains

    function I8PWrapperFactory_hasSameType(this, Value) result(hasSameType)
        class(I8PWrapperFactory_t), intent(IN) :: this
        class(*),                   intent(IN) :: Value
        logical                                :: hasSameType
        hasSameType = .false.
        select type(Value)
            type is (integer(I8P))
                hasSameType = .true.
        end select
    end function I8PWrapperFactory_hasSameType


    subroutine I8PWrapperFactory_Create0D(this, Value, Wrapper)
    !-----------------------------------------------------------------
    !< Create I8P 0D Wrapper
    !-----------------------------------------------------------------
        class(I8PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Value)) then
            allocate(DimensionsWrapper0D_I8P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=0_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper0D_I8P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end subroutine I8PWrapperFactory_Create0D


    subroutine I8PWrapperFactory_Create1D(this, Value, Wrapper)
    !-----------------------------------------------------------------
    !< Create I8P 1D Wrapper
    !-----------------------------------------------------------------
        class(I8PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Value(1))) then
            allocate(DimensionsWrapper1D_I8P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=1_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper1D_I8P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end subroutine I8PWrapperFactory_Create1D


    subroutine I8PWrapperFactory_Create2D(this, Value, Wrapper)
    !-----------------------------------------------------------------
    !< Create I8P 2D Wrapper
    !-----------------------------------------------------------------
        class(I8PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Value(1,1))) then
            allocate(DimensionsWrapper2D_I8P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=2_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper2D_I8P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end subroutine I8PWrapperFactory_Create2D


    subroutine I8PWrapperFactory_Create3D(this, Value, Wrapper)
    !-----------------------------------------------------------------
    !< Create I8P 3D Wrapper
    !-----------------------------------------------------------------
        class(I8PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Value(1,1,1))) then
            allocate(DimensionsWrapper3D_I8P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=3_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper3D_I8P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end subroutine I8PWrapperFactory_Create3D


    subroutine I8PWrapperFactory_Create4D(this, Value, Wrapper)
    !-----------------------------------------------------------------
    !< Create I8P 4D Wrapper
    !-----------------------------------------------------------------
        class(I8PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:,1:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Value(1,1,1,1))) then
            allocate(DimensionsWrapper4D_I8P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=4_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper4D_I8P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end subroutine I8PWrapperFactory_Create4D


    subroutine I8PWrapperFactory_Create5D(this, Value, Wrapper)
    !-----------------------------------------------------------------
    !< Create I8P 5D Wrapper
    !-----------------------------------------------------------------
        class(I8PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:,1:,1:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Value(1,1,1,1,1))) then
            allocate(DimensionsWrapper5D_I8P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=5_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper5D_I8P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end subroutine I8PWrapperFactory_Create5D


    subroutine I8PWrapperFactory_Create6D(this, Value, Wrapper)
    !-----------------------------------------------------------------
    !< Create I8P 6D Wrapper
    !-----------------------------------------------------------------
        class(I8PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:,1:,1:,1:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Value(1,1,1,1,1,1))) then
            allocate(DimensionsWrapper6D_I8P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=6_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper6D_I8P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end subroutine I8PWrapperFactory_Create6D


    subroutine I8PWrapperFactory_Create7D(this, Value, Wrapper)
    !-----------------------------------------------------------------
    !< Create I8P 7D Wrapper
    !-----------------------------------------------------------------
        class(I8PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:,1:,1:,1:,1:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Value(1,1,1,1,1,1,1))) then
            allocate(DimensionsWrapper7D_I8P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=7_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper7D_I8P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end subroutine I8PWrapperFactory_Create7D


end module I8PWrapperFactory
