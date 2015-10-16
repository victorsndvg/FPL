module I8PWrapperFactory

USE IR_Precision, only: I8P
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

    type :: I8PWrapperFactory_t
    private

    contains
        procedure         :: I8PWrapperFactory_Create0D
        procedure         :: I8PWrapperFactory_Create1D
        procedure         :: I8PWrapperFactory_Create2D
        procedure         :: I8PWrapperFactory_Create3D
        procedure         :: I8PWrapperFactory_Create4D
        procedure         :: I8PWrapperFactory_Create5D
        procedure         :: I8PWrapperFactory_Create6D
        procedure         :: I8PWrapperFactory_Create7D
        procedure, public :: hasSameType => I8PWrapperFactory_hasSameType
        generic,   public :: Create      => I8PWrapperFactory_Create0D, &
                                            I8PWrapperFactory_Create1D, &
                                            I8PWrapperFactory_Create2D, &
                                            I8PWrapperFactory_Create3D, &
                                            I8PWrapperFactory_Create4D, &
                                            I8PWrapperFactory_Create5D, &
                                            I8PWrapperFactory_Create6D, &
                                            I8PWrapperFactory_Create7D
    end type

public :: I8PWrapperFactory_t

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
        class(*),                                intent(IN)    :: Value(:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Value(1))) then
            allocate(DimensionsWrapper1D_I8P_t::Wrapper)
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
        class(*),                                intent(IN)    :: Value(:,:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Value(1,1))) then
            allocate(DimensionsWrapper2D_I8P_t::Wrapper)
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
        class(*),                                intent(IN)    :: Value(:,:,:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Value(1,1,1))) then
            allocate(DimensionsWrapper3D_I8P_t::Wrapper)
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
        class(*),                                intent(IN)    :: Value(:,:,:,:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Value(1,1,1,1))) then
            allocate(DimensionsWrapper4D_I8P_t::Wrapper)
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
        class(*),                                intent(IN)    :: Value(:,:,:,:,:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Value(1,1,1,1,1))) then
            allocate(DimensionsWrapper5D_I8P_t::Wrapper)
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
        class(*),                                intent(IN)    :: Value(:,:,:,:,:,:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Value(1,1,1,1,1,1))) then
            allocate(DimensionsWrapper6D_I8P_t::Wrapper)
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
        class(*),                                intent(IN)    :: Value(:,:,:,:,:,:,:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Value(1,1,1,1,1,1,1))) then
            allocate(DimensionsWrapper7D_I8P_t::Wrapper)
            select type (Wrapper)
                type is(DimensionsWrapper7D_I8P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end subroutine I8PWrapperFactory_Create7D


end module I8PWrapperFactory
