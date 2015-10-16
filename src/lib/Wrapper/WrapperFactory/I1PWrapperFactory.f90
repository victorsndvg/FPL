module I1PWrapperFactory

USE WrapperFactory
USE IR_Precision, only: I1P
USE DimensionsWrapper
USE DimensionsWrapper0D_I1P
USE DimensionsWrapper1D_I1P
USE DimensionsWrapper2D_I1P
USE DimensionsWrapper3D_I1P
USE DimensionsWrapper4D_I1P
USE DimensionsWrapper5D_I1P
USE DimensionsWrapper6D_I1P
USE DimensionsWrapper7D_I1P

implicit none
private

    type, extends(WrapperFactory_t) :: I1PWrapperFactory_t
    private

    contains
        procedure         :: I1PWrapperFactory_Create0D
        procedure         :: I1PWrapperFactory_Create1D
        procedure         :: I1PWrapperFactory_Create2D
        procedure         :: I1PWrapperFactory_Create3D
        procedure         :: I1PWrapperFactory_Create4D
        procedure         :: I1PWrapperFactory_Create5D
        procedure         :: I1PWrapperFactory_Create6D
        procedure         :: I1PWrapperFactory_Create7D
        procedure, public :: hasSameType => I1PWrapperFactory_hasSameType
        generic,   public :: Create      => I1PWrapperFactory_Create0D, &
                                            I1PWrapperFactory_Create1D, &
                                            I1PWrapperFactory_Create2D, &
                                            I1PWrapperFactory_Create3D, &
                                            I1PWrapperFactory_Create4D, &
                                            I1PWrapperFactory_Create5D, &
                                            I1PWrapperFactory_Create6D, &
                                            I1PWrapperFactory_Create7D
    end type

public :: I1PWrapperFactory_t

contains

    function I1PWrapperFactory_hasSameType(this, Value) result(hasSameType)
        class(I1PWrapperFactory_t), intent(IN) :: this
        class(*),                   intent(IN) :: Value
        logical                                :: hasSameType
        hasSameType = .false.
        select type(Value)
            type is (integer(I1P))
                hasSameType = .true.
        end select
    end function I1PWrapperFactory_hasSameType


    subroutine I1PWrapperFactory_Create0D(this, Value, Wrapper)
    !-----------------------------------------------------------------
    !< Create I1P 0D Wrapper
    !-----------------------------------------------------------------
        class(I1PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Value)) then
            allocate(DimensionsWrapper0D_I1P_t::Wrapper)
            select type (Wrapper)
                type is(DimensionsWrapper0D_I1P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end subroutine I1PWrapperFactory_Create0D


    subroutine I1PWrapperFactory_Create1D(this, Value, Wrapper)
    !-----------------------------------------------------------------
    !< Create I1P 1D Wrapper
    !-----------------------------------------------------------------
        class(I1PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Value(1))) then
            allocate(DimensionsWrapper1D_I1P_t::Wrapper)
            select type (Wrapper)
                type is(DimensionsWrapper1D_I1P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end subroutine I1PWrapperFactory_Create1D


    subroutine I1PWrapperFactory_Create2D(this, Value, Wrapper)
    !-----------------------------------------------------------------
    !< Create I1P 2D Wrapper
    !-----------------------------------------------------------------
        class(I1PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(:,:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Value(1,1))) then
            allocate(DimensionsWrapper2D_I1P_t::Wrapper)
            select type (Wrapper)
                type is(DimensionsWrapper2D_I1P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end subroutine I1PWrapperFactory_Create2D


    subroutine I1PWrapperFactory_Create3D(this, Value, Wrapper)
    !-----------------------------------------------------------------
    !< Create I1P 3D Wrapper
    !-----------------------------------------------------------------
        class(I1PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(:,:,:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Value(1,1,1))) then
            allocate(DimensionsWrapper3D_I1P_t::Wrapper)
            select type (Wrapper)
                type is(DimensionsWrapper3D_I1P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end subroutine I1PWrapperFactory_Create3D


    subroutine I1PWrapperFactory_Create4D(this, Value, Wrapper)
    !-----------------------------------------------------------------
    !< Create I1P 4D Wrapper
    !-----------------------------------------------------------------
        class(I1PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(:,:,:,:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Value(1,1,1,1))) then
            allocate(DimensionsWrapper4D_I1P_t::Wrapper)
            select type (Wrapper)
                type is(DimensionsWrapper4D_I1P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end subroutine I1PWrapperFactory_Create4D


    subroutine I1PWrapperFactory_Create5D(this, Value, Wrapper)
    !-----------------------------------------------------------------
    !< Create I1P 5D Wrapper
    !-----------------------------------------------------------------
        class(I1PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(:,:,:,:,:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Value(1,1,1,1,1))) then
            allocate(DimensionsWrapper5D_I1P_t::Wrapper)
            select type (Wrapper)
                type is(DimensionsWrapper5D_I1P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end subroutine I1PWrapperFactory_Create5D


    subroutine I1PWrapperFactory_Create6D(this, Value, Wrapper)
    !-----------------------------------------------------------------
    !< Create I1P 6D Wrapper
    !-----------------------------------------------------------------
        class(I1PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(:,:,:,:,:,:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Value(1,1,1,1,1,1))) then
            allocate(DimensionsWrapper6D_I1P_t::Wrapper)
            select type (Wrapper)
                type is(DimensionsWrapper6D_I1P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end subroutine I1PWrapperFactory_Create6D


    subroutine I1PWrapperFactory_Create7D(this, Value, Wrapper)
    !-----------------------------------------------------------------
    !< Create I1P 7D Wrapper
    !-----------------------------------------------------------------
        class(I1PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(:,:,:,:,:,:,:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Value(1,1,1,1,1,1,1))) then
            allocate(DimensionsWrapper7D_I1P_t::Wrapper)
            select type (Wrapper)
                type is(DimensionsWrapper7D_I1P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end subroutine I1PWrapperFactory_Create7D


end module I1PWrapperFactory
