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
        procedure         :: Wrap0D      => UPWrapperFactory_Wrap0D
        procedure         :: Wrap1D      => UPWrapperFactory_Wrap1D
        procedure         :: Wrap2D      => UPWrapperFactory_Wrap2D
        procedure         :: Wrap3D      => UPWrapperFactory_Wrap3D
        procedure         :: Wrap4D      => UPWrapperFactory_Wrap4D
        procedure         :: Wrap5D      => UPWrapperFactory_Wrap5D
        procedure         :: Wrap6D      => UPWrapperFactory_Wrap6D
        procedure         :: Wrap7D      => UPWrapperFactory_Wrap7D
        procedure         :: UnWrap0D    => UPWrapperFactory_UnWrap0D
        procedure         :: UnWrap1D    => UPWrapperFactory_UnWrap1D
        procedure         :: UnWrap2D    => UPWrapperFactory_UnWrap2D
        procedure         :: UnWrap3D    => UPWrapperFactory_UnWrap3D
        procedure         :: UnWrap4D    => UPWrapperFactory_UnWrap4D
        procedure         :: UnWrap5D    => UPWrapperFactory_UnWrap5D
        procedure         :: UnWrap6D    => UPWrapperFactory_UnWrap6D
        procedure         :: UnWrap7D    => UPWrapperFactory_UnWrap7D
        procedure, public :: hasSameType => UPWrapperFactory_hasSameType
    end type

    type(UPWrapperFactory_t), public, target :: WrapperFactoryUP

contains

    function UPWrapperFactory_hasSameType(this, Value) result(hasSameType)
        class(UPWrapperFactory_t), intent(IN) :: this
        class(*),                  intent(IN) :: Value
        logical                               :: hasSameType
        hasSameType = .true.
    end function UPWrapperFactory_hasSameType


    subroutine UPWrapperFactory_Wrap0D(this, Value, Wrapper)
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
    end subroutine UPWrapperFactory_Wrap0D


    subroutine UPWrapperFactory_Wrap1D(this, Value, Wrapper)
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
    end subroutine UPWrapperFactory_Wrap1D


    subroutine UPWrapperFactory_Wrap2D(this, Value, Wrapper)
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
    end subroutine UPWrapperFactory_Wrap2D


    subroutine UPWrapperFactory_Wrap3D(this, Value, Wrapper)
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
    end subroutine UPWrapperFactory_Wrap3D


    subroutine UPWrapperFactory_Wrap4D(this, Value, Wrapper)
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
    end subroutine UPWrapperFactory_Wrap4D


    subroutine UPWrapperFactory_Wrap5D(this, Value, Wrapper)
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
    end subroutine UPWrapperFactory_Wrap5D


    subroutine UPWrapperFactory_Wrap6D(this, Value, Wrapper)
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
    end subroutine UPWrapperFactory_Wrap6D


    subroutine UPWrapperFactory_Wrap7D(this, Value, Wrapper)
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
    end subroutine UPWrapperFactory_Wrap7D


    subroutine UPWrapperFactory_UnWrap0D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the UP 0D Wrapped Value
    !-----------------------------------------------------------------
        class(UPWrapperFactory_t),               intent(IN)    :: this
        class(DimensionsWrapper_t), allocatable, intent(IN)    :: Wrapper
        class(*),                   allocatable, intent(INOUT) :: Value
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper0D_UP_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine UPWrapperFactory_UnWrap1D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the UP 1D Wrapped Value
    !-----------------------------------------------------------------
        class(UPWrapperFactory_t),               intent(IN)    :: this
        class(DimensionsWrapper_t), allocatable, intent(IN)    :: Wrapper
        class(*),                   allocatable, intent(INOUT) :: Value(:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper1D_UP_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine UPWrapperFactory_UnWrap2D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the UP 2D Wrapped Value
    !-----------------------------------------------------------------
        class(UPWrapperFactory_t),               intent(IN)    :: this
        class(DimensionsWrapper_t), allocatable, intent(IN)    :: Wrapper
        class(*),                   allocatable, intent(INOUT) :: Value(:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper2D_UP_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine UPWrapperFactory_UnWrap3D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the UP 3D Wrapped Value
    !-----------------------------------------------------------------
        class(UPWrapperFactory_t),               intent(IN)    :: this
        class(DimensionsWrapper_t), allocatable, intent(IN)    :: Wrapper
        class(*),                   allocatable, intent(INOUT) :: Value(:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper3D_UP_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine UPWrapperFactory_UnWrap4D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the UP 4D Wrapped Value
    !-----------------------------------------------------------------
        class(UPWrapperFactory_t),               intent(IN)    :: this
        class(DimensionsWrapper_t), allocatable, intent(IN)    :: Wrapper
        class(*),                   allocatable, intent(INOUT) :: Value(:,:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper4D_UP_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine UPWrapperFactory_UnWrap5D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the UP 5D Wrapped Value
    !-----------------------------------------------------------------
        class(UPWrapperFactory_t),               intent(IN)    :: this
        class(DimensionsWrapper_t), allocatable, intent(IN)    :: Wrapper
        class(*),                   allocatable, intent(INOUT) :: Value(:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper5D_UP_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine UPWrapperFactory_UnWrap6D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the UP 6D Wrapped Value
    !-----------------------------------------------------------------
        class(UPWrapperFactory_t),               intent(IN)    :: this
        class(DimensionsWrapper_t), allocatable, intent(IN)    :: Wrapper
        class(*),                   allocatable, intent(INOUT) :: Value(:,:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper6D_UP_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine UPWrapperFactory_UnWrap7D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the UP 7D Wrapped Value
    !-----------------------------------------------------------------
        class(UPWrapperFactory_t),               intent(IN)    :: this
        class(DimensionsWrapper_t), allocatable, intent(IN)    :: Wrapper
        class(*),                   allocatable, intent(INOUT) :: Value(:,:,:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper7D_UP_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


end module UPWrapperFactory
