module I2PWrapperFactory

USE WrapperFactory
USE IR_Precision, only: I2P
USE DimensionsWrapper
USE DimensionsWrapper0D_I2P
USE DimensionsWrapper1D_I2P
USE DimensionsWrapper2D_I2P
USE DimensionsWrapper3D_I2P
USE DimensionsWrapper4D_I2P
USE DimensionsWrapper5D_I2P
USE DimensionsWrapper6D_I2P
USE DimensionsWrapper7D_I2P

implicit none
private

    type, extends(WrapperFactory_t) :: I2PWrapperFactory_t
    private

    contains
        procedure         :: I2PWrapperFactory_Create0D
        procedure         :: I2PWrapperFactory_Create1D
        procedure         :: I2PWrapperFactory_Create2D
        procedure         :: I2PWrapperFactory_Create3D
        procedure         :: I2PWrapperFactory_Create4D
        procedure         :: I2PWrapperFactory_Create5D
        procedure         :: I2PWrapperFactory_Create6D
        procedure         :: I2PWrapperFactory_Create7D
        procedure, public :: hasSameType => I2PWrapperFactory_hasSameType
        generic,   public :: Create      => I2PWrapperFactory_Create0D, &
                                            I2PWrapperFactory_Create1D, &
                                            I2PWrapperFactory_Create2D, &
                                            I2PWrapperFactory_Create3D, &
                                            I2PWrapperFactory_Create4D, &
                                            I2PWrapperFactory_Create5D, &
                                            I2PWrapperFactory_Create6D, &
                                            I2PWrapperFactory_Create7D
    end type

public :: I2PWrapperFactory_t

contains

    function I2PWrapperFactory_hasSameType(this, Value) result(hasSameType)
        class(I2PWrapperFactory_t), intent(IN) :: this
        class(*),                   intent(IN) :: Value
        logical                                :: hasSameType
        hasSameType = .false.
        select type(Value)
            type is (integer(I2P))
                hasSameType = .true.
        end select
    end function I2PWrapperFactory_hasSameType


    subroutine I2PWrapperFactory_Create0D(this, Value, Wrapper)
    !-----------------------------------------------------------------
    !< Create I2P 0D Wrapper
    !-----------------------------------------------------------------
        class(I2PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Value)) then
            allocate(DimensionsWrapper0D_I2P_t::Wrapper)
            select type (Wrapper)
                type is(DimensionsWrapper0D_I2P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end subroutine I2PWrapperFactory_Create0D


    subroutine I2PWrapperFactory_Create1D(this, Value, Wrapper)
    !-----------------------------------------------------------------
    !< Create I2P 1D Wrapper
    !-----------------------------------------------------------------
        class(I2PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Value(1))) then
            allocate(DimensionsWrapper1D_I2P_t::Wrapper)
            select type (Wrapper)
                type is(DimensionsWrapper1D_I2P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end subroutine I2PWrapperFactory_Create1D


    subroutine I2PWrapperFactory_Create2D(this, Value, Wrapper)
    !-----------------------------------------------------------------
    !< Create I2P 2D Wrapper
    !-----------------------------------------------------------------
        class(I2PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(:,:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Value(1,1))) then
            allocate(DimensionsWrapper2D_I2P_t::Wrapper)
            select type (Wrapper)
                type is(DimensionsWrapper2D_I2P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end subroutine I2PWrapperFactory_Create2D


    subroutine I2PWrapperFactory_Create3D(this, Value, Wrapper)
    !-----------------------------------------------------------------
    !< Create I2P 3D Wrapper
    !-----------------------------------------------------------------
        class(I2PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(:,:,:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Value(1,1,1))) then
            allocate(DimensionsWrapper3D_I2P_t::Wrapper)
            select type (Wrapper)
                type is(DimensionsWrapper3D_I2P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end subroutine I2PWrapperFactory_Create3D


    subroutine I2PWrapperFactory_Create4D(this, Value, Wrapper)
    !-----------------------------------------------------------------
    !< Create I2P 4D Wrapper
    !-----------------------------------------------------------------
        class(I2PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(:,:,:,:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Value(1,1,1,1))) then
            allocate(DimensionsWrapper4D_I2P_t::Wrapper)
            select type (Wrapper)
                type is(DimensionsWrapper4D_I2P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end subroutine I2PWrapperFactory_Create4D


    subroutine I2PWrapperFactory_Create5D(this, Value, Wrapper)
    !-----------------------------------------------------------------
    !< Create I2P 5D Wrapper
    !-----------------------------------------------------------------
        class(I2PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(:,:,:,:,:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Value(1,1,1,1,1))) then
            allocate(DimensionsWrapper5D_I2P_t::Wrapper)
            select type (Wrapper)
                type is(DimensionsWrapper5D_I2P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end subroutine I2PWrapperFactory_Create5D


    subroutine I2PWrapperFactory_Create6D(this, Value, Wrapper)
    !-----------------------------------------------------------------
    !< Create I2P 6D Wrapper
    !-----------------------------------------------------------------
        class(I2PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(:,:,:,:,:,:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Value(1,1,1,1,1,1))) then
            allocate(DimensionsWrapper6D_I2P_t::Wrapper)
            select type (Wrapper)
                type is(DimensionsWrapper6D_I2P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end subroutine I2PWrapperFactory_Create6D


    subroutine I2PWrapperFactory_Create7D(this, Value, Wrapper)
    !-----------------------------------------------------------------
    !< Create I2P 7D Wrapper
    !-----------------------------------------------------------------
        class(I2PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(:,:,:,:,:,:,:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Value(1,1,1,1,1,1,1))) then
            allocate(DimensionsWrapper7D_I2P_t::Wrapper)
            select type (Wrapper)
                type is(DimensionsWrapper7D_I2P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end subroutine I2PWrapperFactory_Create7D


end module I2PWrapperFactory
