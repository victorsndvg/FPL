module DLCAWrapperFactory

USE IR_Precision, only: I1P
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
        procedure         :: Create0D    => DLCAWrapperFactory_Create0D
        procedure         :: Create1D    => DLCAWrapperFactory_Create1D
        procedure         :: Create2D    => DLCAWrapperFactory_Create2D
        procedure         :: Create3D    => DLCAWrapperFactory_Create3D
        procedure         :: Create4D    => DLCAWrapperFactory_Create4D
        procedure         :: Create5D    => DLCAWrapperFactory_Create5D
        procedure         :: Create6D    => DLCAWrapperFactory_Create6D
        procedure         :: Create7D    => DLCAWrapperFactory_Create7D
        procedure         :: Wrap0D      => DLCAWrapperFactory_Wrap0D
        procedure         :: Wrap1D      => DLCAWrapperFactory_Wrap1D
        procedure         :: Wrap2D      => DLCAWrapperFactory_Wrap2D
        procedure         :: Wrap3D      => DLCAWrapperFactory_Wrap3D
        procedure         :: Wrap4D      => DLCAWrapperFactory_Wrap4D
        procedure         :: Wrap5D      => DLCAWrapperFactory_Wrap5D
        procedure         :: Wrap6D      => DLCAWrapperFactory_Wrap6D
        procedure         :: Wrap7D      => DLCAWrapperFactory_Wrap7D
        procedure         :: UnWrap0D    => DLCAWrapperFactory_UnWrap0D
        procedure         :: UnWrap1D    => DLCAWrapperFactory_UnWrap1D
        procedure         :: UnWrap2D    => DLCAWrapperFactory_UnWrap2D
        procedure         :: UnWrap3D    => DLCAWrapperFactory_UnWrap3D
        procedure         :: UnWrap4D    => DLCAWrapperFactory_UnWrap4D
        procedure         :: UnWrap5D    => DLCAWrapperFactory_UnWrap5D
        procedure         :: UnWrap6D    => DLCAWrapperFactory_UnWrap6D
        procedure         :: UnWrap7D    => DLCAWrapperFactory_UnWrap7D
        procedure, public :: hasSameType => DLCAWrapperFactory_hasSameType
    end type

    type(DLCAWrapperFactory_t), public, target :: WrapperFactoryDLCA

contains

    function DLCAWrapperFactory_hasSameType(this, Value) result(hasSameType)
        class(DLCAWrapperFactory_t), intent(IN) :: this
        class(*),                    intent(IN) :: Value
        logical                                 :: hasSameType
        hasSameType = .false.
        select type(Value)
            type is (character(len=*))
                hasSameType = .true.
        end select
    end function DLCAWrapperFactory_hasSameType


    subroutine DLCAWrapperFactory_Create0D(this, Mold, Wrapper)
    !-----------------------------------------------------------------
    !< Create an empty DLCA 0D Wrapper
    !-----------------------------------------------------------------
        class(DLCAWrapperFactory_t),             intent(IN)    :: this
        class(*),                                intent(IN)    :: Mold
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Mold)) then
            allocate(DimensionsWrapper0D_DLCA_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=0_I1P)
        endif
    end subroutine DLCAWrapperFactory_Create0D


    subroutine DLCAWrapperFactory_Create1D(this, Mold, Wrapper)
    !-----------------------------------------------------------------
    !< Create an empty DLCA 1D Wrapper
    !-----------------------------------------------------------------
        class(DLCAWrapperFactory_t),             intent(IN)    :: this
        class(*),                                intent(IN)    :: Mold(1:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Mold(1))) then
            allocate(DimensionsWrapper1D_DLCA_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=1_I1P)
        endif
    end subroutine DLCAWrapperFactory_Create1D


    subroutine DLCAWrapperFactory_Create2D(this, Mold, Wrapper)
    !-----------------------------------------------------------------
    !< Create an empty DLCA 2D Wrapper
    !-----------------------------------------------------------------
        class(DLCAWrapperFactory_t),             intent(IN)    :: this
        class(*),                                intent(IN)    :: Mold(1:,1:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Mold(1,1))) then
            allocate(DimensionsWrapper2D_DLCA_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=2_I1P)
        endif
    end subroutine DLCAWrapperFactory_Create2D


    subroutine DLCAWrapperFactory_Create3D(this, Mold, Wrapper)
    !-----------------------------------------------------------------
    !< Create an empty DLCA 3D Wrapper
    !-----------------------------------------------------------------
        class(DLCAWrapperFactory_t),             intent(IN)    :: this
        class(*),                                intent(IN)    :: Mold(1:,1:,1:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Mold(1,1,1))) then
            allocate(DimensionsWrapper3D_DLCA_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=3_I1P)
        endif
    end subroutine DLCAWrapperFactory_Create3D


    subroutine DLCAWrapperFactory_Create4D(this, Mold, Wrapper)
    !-----------------------------------------------------------------
    !< Create an empty DLCA 4D Wrapper
    !-----------------------------------------------------------------
        class(DLCAWrapperFactory_t),             intent(IN)    :: this
        class(*),                                intent(IN)    :: Mold(1:,1:,1:,1:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Mold(1,1,1,1))) then
            allocate(DimensionsWrapper4D_DLCA_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=4_I1P)
        endif
    end subroutine DLCAWrapperFactory_Create4D


    subroutine DLCAWrapperFactory_Create5D(this, Mold, Wrapper)
    !-----------------------------------------------------------------
    !< Create an empty DLCA 5D Wrapper
    !-----------------------------------------------------------------
        class(DLCAWrapperFactory_t),             intent(IN)    :: this
        class(*),                                intent(IN)    :: Mold(1:,1:,1:,1:,1:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Mold(1,1,1,1,1))) then
            allocate(DimensionsWrapper5D_DLCA_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=5_I1P)
        endif
    end subroutine DLCAWrapperFactory_Create5D


    subroutine DLCAWrapperFactory_Create6D(this, Mold, Wrapper)
    !-----------------------------------------------------------------
    !< Create an empty DLCA 6D Wrapper
    !-----------------------------------------------------------------
        class(DLCAWrapperFactory_t),             intent(IN)    :: this
        class(*),                                intent(IN)    :: Mold(1:,1:,1:,1:,1:,1:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Mold(1,1,1,1,1,1))) then
            allocate(DimensionsWrapper6D_DLCA_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=6_I1P)
        endif
    end subroutine DLCAWrapperFactory_Create6D


    subroutine DLCAWrapperFactory_Create7D(this, Mold, Wrapper)
    !-----------------------------------------------------------------
    !< Create an empty DLCA 7D Wrapper
    !-----------------------------------------------------------------
        class(DLCAWrapperFactory_t),             intent(IN)    :: this
        class(*),                                intent(IN)    :: Mold(1:,1:,1:,1:,1:,1:,1:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        if(allocated(Wrapper)) then
            call Wrapper%Free()
            deallocate(Wrapper)
        endif
        if(this%hasSameType(Mold(1,1,1,1,1,1,1))) then
            allocate(DimensionsWrapper7D_DLCA_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=7_I1P)
        endif
    end subroutine DLCAWrapperFactory_Create7D


    subroutine DLCAWrapperFactory_Wrap0D(this, Value, Wrapper)
    !-----------------------------------------------------------------
    !< Create a filled DLCA 0D Wrapper
    !-----------------------------------------------------------------
        class(DLCAWrapperFactory_t),             intent(IN)    :: this
        class(*),                                intent(IN)    :: Value
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        call this%Create(Mold=Value, Wrapper=Wrapper)
        if(allocated(Wrapper)) then
            select type (Wrapper)
                type is(DimensionsWrapper0D_DLCA_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end subroutine DLCAWrapperFactory_Wrap0D


    subroutine DLCAWrapperFactory_Wrap1D(this, Value, Wrapper)
    !-----------------------------------------------------------------
    !< Create a filled DLCA 1D Wrapper
    !-----------------------------------------------------------------
        class(DLCAWrapperFactory_t),             intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        call this%Create(Mold=Value, Wrapper=Wrapper)
        if(allocated(Wrapper)) then
            select type (Wrapper)
                type is(DimensionsWrapper1D_DLCA_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end subroutine DLCAWrapperFactory_Wrap1D


    subroutine DLCAWrapperFactory_Wrap2D(this, Value, Wrapper)
    !-----------------------------------------------------------------
    !< Create a filled DLCA 2D Wrapper
    !-----------------------------------------------------------------
        class(DLCAWrapperFactory_t),             intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        call this%Create(Mold=Value, Wrapper=Wrapper)
        if(allocated(Wrapper)) then
            select type (Wrapper)
                type is(DimensionsWrapper2D_DLCA_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end subroutine DLCAWrapperFactory_Wrap2D


    subroutine DLCAWrapperFactory_Wrap3D(this, Value, Wrapper)
    !-----------------------------------------------------------------
    !< Create a filled DLCA 3D Wrapper
    !-----------------------------------------------------------------
        class(DLCAWrapperFactory_t),             intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        call this%Create(Mold=Value, Wrapper=Wrapper)
        if(allocated(Wrapper)) then
            select type (Wrapper)
                type is(DimensionsWrapper3D_DLCA_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end subroutine DLCAWrapperFactory_Wrap3D


    subroutine DLCAWrapperFactory_Wrap4D(this, Value, Wrapper)
    !-----------------------------------------------------------------
    !< Create a filled DLCA 4D Wrapper
    !-----------------------------------------------------------------
        class(DLCAWrapperFactory_t),             intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:,1:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        call this%Create(Mold=Value, Wrapper=Wrapper)
        if(allocated(Wrapper)) then
            select type (Wrapper)
                type is(DimensionsWrapper4D_DLCA_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end subroutine DLCAWrapperFactory_Wrap4D


    subroutine DLCAWrapperFactory_Wrap5D(this, Value, Wrapper)
    !-----------------------------------------------------------------
    !< Create a filled DLCA 5D Wrapper
    !-----------------------------------------------------------------
        class(DLCAWrapperFactory_t),             intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:,1:,1:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        call this%Create(Mold=Value, Wrapper=Wrapper)
        if(allocated(Wrapper)) then
            select type (Wrapper)
                type is(DimensionsWrapper5D_DLCA_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end subroutine DLCAWrapperFactory_Wrap5D


    subroutine DLCAWrapperFactory_Wrap6D(this, Value, Wrapper)
    !-----------------------------------------------------------------
    !< Create a filled DLCA 6D Wrapper
    !-----------------------------------------------------------------
        class(DLCAWrapperFactory_t),             intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:,1:,1:,1:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        call this%Create(Mold=Value, Wrapper=Wrapper)
        if(allocated(Wrapper)) then
            select type (Wrapper)
                type is(DimensionsWrapper6D_DLCA_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end subroutine DLCAWrapperFactory_Wrap6D


    subroutine DLCAWrapperFactory_Wrap7D(this, Value, Wrapper)
    !-----------------------------------------------------------------
    !< Create a filled DLCA 7D Wrapper
    !-----------------------------------------------------------------
        class(DLCAWrapperFactory_t),             intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:,1:,1:,1:,1:)
        class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
    !-----------------------------------------------------------------
        call this%Create(Mold=Value, Wrapper=Wrapper)
        if(allocated(Wrapper)) then
            select type (Wrapper)
                type is(DimensionsWrapper7D_DLCA_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end subroutine DLCAWrapperFactory_Wrap7D


    subroutine DLCAWrapperFactory_UnWrap0D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the DLCA 0D Wrapped Value
    !-----------------------------------------------------------------
        class(DLCAWrapperFactory_t),             intent(IN)    :: this
        class(DimensionsWrapper_t), allocatable, intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper0D_DLCA_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine DLCAWrapperFactory_UnWrap1D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the DLCA 1D Wrapped Value
    !-----------------------------------------------------------------
        class(DLCAWrapperFactory_t),             intent(IN)    :: this
        class(DimensionsWrapper_t), allocatable, intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper1D_DLCA_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine DLCAWrapperFactory_UnWrap2D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the DLCA 2D Wrapped Value
    !-----------------------------------------------------------------
        class(DLCAWrapperFactory_t),             intent(IN)    :: this
        class(DimensionsWrapper_t), allocatable, intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper2D_DLCA_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine DLCAWrapperFactory_UnWrap3D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the DLCA 3D Wrapped Value
    !-----------------------------------------------------------------
        class(DLCAWrapperFactory_t),             intent(IN)    :: this
        class(DimensionsWrapper_t), allocatable, intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper3D_DLCA_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine DLCAWrapperFactory_UnWrap4D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the DLCA 4D Wrapped Value
    !-----------------------------------------------------------------
        class(DLCAWrapperFactory_t),             intent(IN)    :: this
        class(DimensionsWrapper_t), allocatable, intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper4D_DLCA_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine DLCAWrapperFactory_UnWrap5D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the DLCA 5D Wrapped Value
    !-----------------------------------------------------------------
        class(DLCAWrapperFactory_t),             intent(IN)    :: this
        class(DimensionsWrapper_t), allocatable, intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper5D_DLCA_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine DLCAWrapperFactory_UnWrap6D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the DLCA 6D Wrapped Value
    !-----------------------------------------------------------------
        class(DLCAWrapperFactory_t),             intent(IN)    :: this
        class(DimensionsWrapper_t), allocatable, intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper6D_DLCA_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine DLCAWrapperFactory_UnWrap7D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the DLCA 7D Wrapped Value
    !-----------------------------------------------------------------
        class(DLCAWrapperFactory_t),             intent(IN)    :: this
        class(DimensionsWrapper_t), allocatable, intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:,:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper7D_DLCA_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


end module DLCAWrapperFactory
