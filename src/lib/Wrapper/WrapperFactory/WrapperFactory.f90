module WrapperFactory

USE IR_Precision, only: I1P, I2P, I4P, I8P, R4P, R8P
USE DimensionsWrapper

implicit none
private

    type, abstract :: WrapperFactory_t
    private
    contains
        private
        procedure(WrapperFactory_Wrap0D),              deferred :: Wrap0D
        procedure(WrapperFactory_Wrap1D),              deferred :: Wrap1D
        procedure(WrapperFactory_Wrap2D),              deferred :: Wrap2D
        procedure(WrapperFactory_Wrap3D),              deferred :: Wrap3D
        procedure(WrapperFactory_Wrap4D),              deferred :: Wrap4D
        procedure(WrapperFactory_Wrap5D),              deferred :: Wrap5D
        procedure(WrapperFactory_Wrap6D),              deferred :: Wrap6D
        procedure(WrapperFactory_Wrap7D),              deferred :: Wrap7D
        procedure(WrapperFactory_UnWrap0D),            deferred :: UnWrap0D
        procedure(WrapperFactory_UnWrap1D),            deferred :: UnWrap1D
        procedure(WrapperFactory_UnWrap2D),            deferred :: UnWrap2D
        procedure(WrapperFactory_UnWrap3D),            deferred :: UnWrap3D
        procedure(WrapperFactory_UnWrap4D),            deferred :: UnWrap4D
        procedure(WrapperFactory_UnWrap5D),            deferred :: UnWrap5D
        procedure(WrapperFactory_UnWrap6D),            deferred :: UnWrap6D
        procedure(WrapperFactory_UnWrap7D),            deferred :: UnWrap7D
        procedure(WrapperFactory_hasSameType), public, deferred :: hasSameType 
        generic, public :: Wrap =>   Wrap0D, &
                                     Wrap1D, &
                                     Wrap2D, &
                                     Wrap3D, &
                                     Wrap4D, &
                                     Wrap5D, &
                                     Wrap6D, &
                                     Wrap7D
        generic, public :: UnWrap => UnWrap0D, &
                                     UnWrap1D, &
                                     UnWrap2D, &
                                     UnWrap3D, &
                                     UnWrap4D, &
                                     UnWrap5D, &
                                     UnWrap6D, &
                                     UnWrap7D
    end type

    type, extends(WrapperFactory_t) :: DLCAWrapperFactory_t
    private
    contains
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

    type, extends(WrapperFactory_t) :: I1PWrapperFactory_t
    private
    contains
        procedure         :: Wrap0D      => I1PWrapperFactory_Wrap0D
        procedure         :: Wrap1D      => I1PWrapperFactory_Wrap1D
        procedure         :: Wrap2D      => I1PWrapperFactory_Wrap2D
        procedure         :: Wrap3D      => I1PWrapperFactory_Wrap3D
        procedure         :: Wrap4D      => I1PWrapperFactory_Wrap4D
        procedure         :: Wrap5D      => I1PWrapperFactory_Wrap5D
        procedure         :: Wrap6D      => I1PWrapperFactory_Wrap6D
        procedure         :: Wrap7D      => I1PWrapperFactory_Wrap7D
        procedure         :: UnWrap0D    => I1PWrapperFactory_UnWrap0D
        procedure         :: UnWrap1D    => I1PWrapperFactory_UnWrap1D
        procedure         :: UnWrap2D    => I1PWrapperFactory_UnWrap2D
        procedure         :: UnWrap3D    => I1PWrapperFactory_UnWrap3D
        procedure         :: UnWrap4D    => I1PWrapperFactory_UnWrap4D
        procedure         :: UnWrap5D    => I1PWrapperFactory_UnWrap5D
        procedure         :: UnWrap6D    => I1PWrapperFactory_UnWrap6D
        procedure         :: UnWrap7D    => I1PWrapperFactory_UnWrap7D
        procedure, public :: hasSameType => I1PWrapperFactory_hasSameType
    end type

    type, extends(WrapperFactory_t) :: I2PWrapperFactory_t
    private
    contains
        procedure         :: Wrap0D      => I2PWrapperFactory_Wrap0D
        procedure         :: Wrap1D      => I2PWrapperFactory_Wrap1D
        procedure         :: Wrap2D      => I2PWrapperFactory_Wrap2D
        procedure         :: Wrap3D      => I2PWrapperFactory_Wrap3D
        procedure         :: Wrap4D      => I2PWrapperFactory_Wrap4D
        procedure         :: Wrap5D      => I2PWrapperFactory_Wrap5D
        procedure         :: Wrap6D      => I2PWrapperFactory_Wrap6D
        procedure         :: Wrap7D      => I2PWrapperFactory_Wrap7D
        procedure         :: UnWrap0D    => I2PWrapperFactory_UnWrap0D
        procedure         :: UnWrap1D    => I2PWrapperFactory_UnWrap1D
        procedure         :: UnWrap2D    => I2PWrapperFactory_UnWrap2D
        procedure         :: UnWrap3D    => I2PWrapperFactory_UnWrap3D
        procedure         :: UnWrap4D    => I2PWrapperFactory_UnWrap4D
        procedure         :: UnWrap5D    => I2PWrapperFactory_UnWrap5D
        procedure         :: UnWrap6D    => I2PWrapperFactory_UnWrap6D
        procedure         :: UnWrap7D    => I2PWrapperFactory_UnWrap7D
        procedure, public :: hasSameType => I2PWrapperFactory_hasSameType
    end type

    type, extends(WrapperFactory_t) :: I4PWrapperFactory_t
    private
    contains
        procedure         :: Wrap0D      => I4PWrapperFactory_Wrap0D
        procedure         :: Wrap1D      => I4PWrapperFactory_Wrap1D
        procedure         :: Wrap2D      => I4PWrapperFactory_Wrap2D
        procedure         :: Wrap3D      => I4PWrapperFactory_Wrap3D
        procedure         :: Wrap4D      => I4PWrapperFactory_Wrap4D
        procedure         :: Wrap5D      => I4PWrapperFactory_Wrap5D
        procedure         :: Wrap6D      => I4PWrapperFactory_Wrap6D
        procedure         :: Wrap7D      => I4PWrapperFactory_Wrap7D
        procedure         :: UnWrap0D    => I4PWrapperFactory_UnWrap0D
        procedure         :: UnWrap1D    => I4PWrapperFactory_UnWrap1D
        procedure         :: UnWrap2D    => I4PWrapperFactory_UnWrap2D
        procedure         :: UnWrap3D    => I4PWrapperFactory_UnWrap3D
        procedure         :: UnWrap4D    => I4PWrapperFactory_UnWrap4D
        procedure         :: UnWrap5D    => I4PWrapperFactory_UnWrap5D
        procedure         :: UnWrap6D    => I4PWrapperFactory_UnWrap6D
        procedure         :: UnWrap7D    => I4PWrapperFactory_UnWrap7D
        procedure, public :: hasSameType => I4PWrapperFactory_hasSameType
    end type

    type, extends(WrapperFactory_t) :: I8PWrapperFactory_t
    private
    contains
        procedure         :: Wrap0D      => I8PWrapperFactory_Wrap0D
        procedure         :: Wrap1D      => I8PWrapperFactory_Wrap1D
        procedure         :: Wrap2D      => I8PWrapperFactory_Wrap2D
        procedure         :: Wrap3D      => I8PWrapperFactory_Wrap3D
        procedure         :: Wrap4D      => I8PWrapperFactory_Wrap4D
        procedure         :: Wrap5D      => I8PWrapperFactory_Wrap5D
        procedure         :: Wrap6D      => I8PWrapperFactory_Wrap6D
        procedure         :: Wrap7D      => I8PWrapperFactory_Wrap7D
        procedure         :: UnWrap0D    => I8PWrapperFactory_UnWrap0D
        procedure         :: UnWrap1D    => I8PWrapperFactory_UnWrap1D
        procedure         :: UnWrap2D    => I8PWrapperFactory_UnWrap2D
        procedure         :: UnWrap3D    => I8PWrapperFactory_UnWrap3D
        procedure         :: UnWrap4D    => I8PWrapperFactory_UnWrap4D
        procedure         :: UnWrap5D    => I8PWrapperFactory_UnWrap5D
        procedure         :: UnWrap6D    => I8PWrapperFactory_UnWrap6D
        procedure         :: UnWrap7D    => I8PWrapperFactory_UnWrap7D
        procedure, public :: hasSameType => I8PWrapperFactory_hasSameType
    end type

    type, extends(WrapperFactory_t) :: LWrapperFactory_t
    private
    contains
        procedure         :: Wrap0D      => LWrapperFactory_Wrap0D
        procedure         :: Wrap1D      => LWrapperFactory_Wrap1D
        procedure         :: Wrap2D      => LWrapperFactory_Wrap2D
        procedure         :: Wrap3D      => LWrapperFactory_Wrap3D
        procedure         :: Wrap4D      => LWrapperFactory_Wrap4D
        procedure         :: Wrap5D      => LWrapperFactory_Wrap5D
        procedure         :: Wrap6D      => LWrapperFactory_Wrap6D
        procedure         :: Wrap7D      => LWrapperFactory_Wrap7D
        procedure         :: UnWrap0D    => LWrapperFactory_UnWrap0D
        procedure         :: UnWrap1D    => LWrapperFactory_UnWrap1D
        procedure         :: UnWrap2D    => LWrapperFactory_UnWrap2D
        procedure         :: UnWrap3D    => LWrapperFactory_UnWrap3D
        procedure         :: UnWrap4D    => LWrapperFactory_UnWrap4D
        procedure         :: UnWrap5D    => LWrapperFactory_UnWrap5D
        procedure         :: UnWrap6D    => LWrapperFactory_UnWrap6D
        procedure         :: UnWrap7D    => LWrapperFactory_UnWrap7D
        procedure, public :: hasSameType => LWrapperFactory_hasSameType
    end type

    type, extends(WrapperFactory_t) :: R4PWrapperFactory_t
    private
    contains
        procedure         :: Wrap0D      => R4PWrapperFactory_Wrap0D
        procedure         :: Wrap1D      => R4PWrapperFactory_Wrap1D
        procedure         :: Wrap2D      => R4PWrapperFactory_Wrap2D
        procedure         :: Wrap3D      => R4PWrapperFactory_Wrap3D
        procedure         :: Wrap4D      => R4PWrapperFactory_Wrap4D
        procedure         :: Wrap5D      => R4PWrapperFactory_Wrap5D
        procedure         :: Wrap6D      => R4PWrapperFactory_Wrap6D
        procedure         :: Wrap7D      => R4PWrapperFactory_Wrap7D
        procedure         :: UnWrap0D    => R4PWrapperFactory_UnWrap0D
        procedure         :: UnWrap1D    => R4PWrapperFactory_UnWrap1D
        procedure         :: UnWrap2D    => R4PWrapperFactory_UnWrap2D
        procedure         :: UnWrap3D    => R4PWrapperFactory_UnWrap3D
        procedure         :: UnWrap4D    => R4PWrapperFactory_UnWrap4D
        procedure         :: UnWrap5D    => R4PWrapperFactory_UnWrap5D
        procedure         :: UnWrap6D    => R4PWrapperFactory_UnWrap6D
        procedure         :: UnWrap7D    => R4PWrapperFactory_UnWrap7D
        procedure, public :: hasSameType => R4PWrapperFactory_hasSameType
    end type

    type, extends(WrapperFactory_t) :: R8PWrapperFactory_t
    private
    contains
        procedure         :: Wrap0D      => R8PWrapperFactory_Wrap0D
        procedure         :: Wrap1D      => R8PWrapperFactory_Wrap1D
        procedure         :: Wrap2D      => R8PWrapperFactory_Wrap2D
        procedure         :: Wrap3D      => R8PWrapperFactory_Wrap3D
        procedure         :: Wrap4D      => R8PWrapperFactory_Wrap4D
        procedure         :: Wrap5D      => R8PWrapperFactory_Wrap5D
        procedure         :: Wrap6D      => R8PWrapperFactory_Wrap6D
        procedure         :: Wrap7D      => R8PWrapperFactory_Wrap7D
        procedure         :: UnWrap0D    => R8PWrapperFactory_UnWrap0D
        procedure         :: UnWrap1D    => R8PWrapperFactory_UnWrap1D
        procedure         :: UnWrap2D    => R8PWrapperFactory_UnWrap2D
        procedure         :: UnWrap3D    => R8PWrapperFactory_UnWrap3D
        procedure         :: UnWrap4D    => R8PWrapperFactory_UnWrap4D
        procedure         :: UnWrap5D    => R8PWrapperFactory_UnWrap5D
        procedure         :: UnWrap6D    => R8PWrapperFactory_UnWrap6D
        procedure         :: UnWrap7D    => R8PWrapperFactory_UnWrap7D
        procedure, public :: hasSameType => R8PWrapperFactory_hasSameType
    end type

    abstract interface
        function WrapperFactory_hasSameType(this, Value) result(hasSameType)
            import WrapperFactory_t
            class(WrapperFactory_t), intent(IN) :: this
            class(*),                intent(IN) :: Value
            logical                             :: hasSameType
        end function

        function WrapperFactory_Wrap0D(this, Value) result(Wrapper)
            import WrapperFactory_t
            import DimensionsWrapper_t
            class(WrapperFactory_t),                 intent(IN)    :: this
            class(*),                                intent(IN)    :: Value
            class(DimensionsWrapper_t), pointer                    :: Wrapper
        end function

        function WrapperFactory_Wrap1D(this, Value) result(Wrapper)
            import WrapperFactory_t
            import DimensionsWrapper_t
            class(WrapperFactory_t),                 intent(IN)    :: this
            class(*),                                intent(IN)    :: Value(:)
            class(DimensionsWrapper_t), pointer                    :: Wrapper
        end function

        function WrapperFactory_Wrap2D(this, Value) result(Wrapper)
            import WrapperFactory_t
            import DimensionsWrapper_t
            class(WrapperFactory_t),                 intent(IN)    :: this
            class(*),                                intent(IN)    :: Value(:,:)
            class(DimensionsWrapper_t), pointer                    :: Wrapper
        end function

        function WrapperFactory_Wrap3D(this, Value) result(Wrapper)
            import WrapperFactory_t
            import DimensionsWrapper_t
            class(WrapperFactory_t),                 intent(IN)    :: this
            class(*),                                intent(IN)    :: Value(:,:,:)
            class(DimensionsWrapper_t), pointer                    :: Wrapper
        end function

        function WrapperFactory_Wrap4D(this, Value) result(Wrapper)
            import WrapperFactory_t
            import DimensionsWrapper_t
            class(WrapperFactory_t),                 intent(IN)    :: this
            class(*),                                intent(IN)    :: Value(:,:,:,:)
            class(DimensionsWrapper_t), pointer                    :: Wrapper
        end function

        function WrapperFactory_Wrap5D(this, Value) result(Wrapper)
            import WrapperFactory_t
            import DimensionsWrapper_t
            class(WrapperFactory_t),                 intent(IN)    :: this
            class(*),                                intent(IN)    :: Value(:,:,:,:,:)
            class(DimensionsWrapper_t), pointer                    :: Wrapper
        end function

        function WrapperFactory_Wrap6D(this, Value) result(Wrapper)
            import WrapperFactory_t
            import DimensionsWrapper_t
            class(WrapperFactory_t),                 intent(IN)    :: this
            class(*),                                intent(IN)    :: Value(:,:,:,:,:,:)
            class(DimensionsWrapper_t), pointer                    :: Wrapper
        end function

        function WrapperFactory_Wrap7D(this, Value) result(Wrapper)
            import WrapperFactory_t
            import DimensionsWrapper_t
            class(WrapperFactory_t),                 intent(IN)    :: this
            class(*),                                intent(IN)    :: Value(:,:,:,:,:,:,:)
            class(DimensionsWrapper_t), pointer                    :: Wrapper
        end function

        subroutine WrapperFactory_UnWrap0D(this, Wrapper, Value)
            import WrapperFactory_t
            import DimensionsWrapper_t
            class(WrapperFactory_t),                 intent(IN)    :: this
            class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
            class(*),                                intent(INOUT) :: Value
        end subroutine

        subroutine WrapperFactory_UnWrap1D(this, Wrapper, Value)
            import WrapperFactory_t
            import DimensionsWrapper_t
            class(WrapperFactory_t),                 intent(IN)    :: this
            class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
            class(*),                                intent(INOUT) :: Value(:)
        end subroutine

        subroutine WrapperFactory_UnWrap2D(this, Wrapper, Value)
            import WrapperFactory_t
            import DimensionsWrapper_t
            class(WrapperFactory_t),                 intent(IN)    :: this
            class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
            class(*),                                intent(INOUT) :: Value(:,:)
        end subroutine

        subroutine WrapperFactory_UnWrap3D(this, Wrapper, Value)
            import WrapperFactory_t
            import DimensionsWrapper_t
            class(WrapperFactory_t),                 intent(IN)    :: this
            class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
            class(*),                                intent(INOUT) :: Value(:,:,:)
        end subroutine

        subroutine WrapperFactory_UnWrap4D(this, Wrapper, Value)
            import WrapperFactory_t
            import DimensionsWrapper_t
            class(WrapperFactory_t),                 intent(IN)    :: this
            class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
            class(*),                                intent(INOUT) :: Value(:,:,:,:)
        end subroutine

        subroutine WrapperFactory_UnWrap5D(this, Wrapper, Value)
            import WrapperFactory_t
            import DimensionsWrapper_t
            class(WrapperFactory_t),                 intent(IN)    :: this
            class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
            class(*),                                intent(INOUT) :: Value(:,:,:,:,:)
        end subroutine

        subroutine WrapperFactory_UnWrap6D(this, Wrapper, Value)
            import WrapperFactory_t
            import DimensionsWrapper_t
            class(WrapperFactory_t),                 intent(IN)    :: this
            class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
            class(*),                                intent(INOUT) :: Value(:,:,:,:,:,:)
        end subroutine

        subroutine WrapperFactory_UnWrap7D(this, Wrapper, Value)
            import WrapperFactory_t
            import DimensionsWrapper_t
            class(WrapperFactory_t),                 intent(IN)    :: this
            class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
            class(*),                                intent(INOUT) :: Value(:,:,:,:,:,:,:)
        end subroutine

    end interface

    type(DLCAWrapperFactory_t), public :: WrapperFactoryDLCA
    type(LWrapperFactory_t),    public :: WrapperFactoryL
    type(I1PWrapperFactory_t),  public :: WrapperFactoryI1P
    type(I2PWrapperFactory_t),  public :: WrapperFactoryI2P
    type(I4PWrapperFactory_t),  public :: WrapperFactoryI4P
    type(I8PWrapperFactory_t),  public :: WrapperFactoryI8P
    type(R4PWrapperFactory_t),  public :: WrapperFactoryR4P
    type(R8PWrapperFactory_t),  public :: WrapperFactoryR8P

public :: WrapperFactory_t

contains

    function DLCAWrapperFactory_hasSameType(this, Value) result(hasSameType)
    !-----------------------------------------------------------------
    !< Check if Value type agrees with wrapper type
    !-----------------------------------------------------------------
        class(DLCAWrapperFactory_t), intent(IN) :: this
        class(*),                   intent(IN) :: Value
        logical                                :: hasSameType
    !-----------------------------------------------------------------
        hasSameType = .false.
        select type(Value)
            type is (character(len=*))
                hasSameType = .true.
        end select
    end function DLCAWrapperFactory_hasSameType


    function DLCAWrapperFactory_Wrap0D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create DLCA 0D Wrapper
    !-----------------------------------------------------------------
        class(DLCAWrapperFactory_t),             intent(IN)    :: this
        class(*),                                intent(IN)    :: Value
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value)) then
            allocate(DimensionsWrapper0D_DLCA_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=0_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper0D_DLCA_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function DLCAWrapperFactory_Wrap0D


    function DLCAWrapperFactory_Wrap1D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create DLCA 1D Wrapper
    !-----------------------------------------------------------------
        class(DLCAWrapperFactory_t),             intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1))) then
            allocate(DimensionsWrapper1D_DLCA_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=1_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper1D_DLCA_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function DLCAWrapperFactory_Wrap1D


    function DLCAWrapperFactory_Wrap2D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create DLCA 2D Wrapper
    !-----------------------------------------------------------------
        class(DLCAWrapperFactory_t),             intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1))) then
            allocate(DimensionsWrapper2D_DLCA_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=2_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper2D_DLCA_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function DLCAWrapperFactory_Wrap2D


    function DLCAWrapperFactory_Wrap3D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create DLCA 3D Wrapper
    !-----------------------------------------------------------------
        class(DLCAWrapperFactory_t),             intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1,1))) then
            allocate(DimensionsWrapper3D_DLCA_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=3_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper3D_DLCA_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function DLCAWrapperFactory_Wrap3D


    function DLCAWrapperFactory_Wrap4D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create DLCA 4D Wrapper
    !-----------------------------------------------------------------
        class(DLCAWrapperFactory_t),             intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1,1,1))) then
            allocate(DimensionsWrapper4D_DLCA_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=4_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper4D_DLCA_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function DLCAWrapperFactory_Wrap4D


    function DLCAWrapperFactory_Wrap5D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create DLCA 5D Wrapper
    !-----------------------------------------------------------------
        class(DLCAWrapperFactory_t),             intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:,1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1,1,1,1))) then
            allocate(DimensionsWrapper5D_DLCA_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=5_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper5D_DLCA_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function DLCAWrapperFactory_Wrap5D


    function DLCAWrapperFactory_Wrap6D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create DLCA 6D Wrapper
    !-----------------------------------------------------------------
        class(DLCAWrapperFactory_t),             intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:,1:,1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1,1,1,1,1))) then
            allocate(DimensionsWrapper6D_DLCA_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=6_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper6D_DLCA_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function DLCAWrapperFactory_Wrap6D


    function DLCAWrapperFactory_Wrap7D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create DLCA 7D Wrapper
    !-----------------------------------------------------------------
        class(DLCAWrapperFactory_t),             intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:,1:,1:,1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1,1,1,1,1,1))) then
            allocate(DimensionsWrapper7D_DLCA_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=7_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper7D_DLCA_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function DLCAWrapperFactory_Wrap7D


    subroutine DLCAWrapperFactory_UnWrap0D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the DLCA 0D Wrapped Value
    !-----------------------------------------------------------------
        class(DLCAWrapperFactory_t),             intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
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
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
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
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
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
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
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
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
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
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
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
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
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
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:,:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper7D_DLCA_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    function I1PWrapperFactory_hasSameType(this, Value) result(hasSameType)
    !-----------------------------------------------------------------
    !< Check if Value type agrees with wrapper type
    !-----------------------------------------------------------------
        class(I1PWrapperFactory_t), intent(IN) :: this
        class(*),                   intent(IN) :: Value
        logical                                :: hasSameType
    !-----------------------------------------------------------------
        hasSameType = .false.
        select type(Value)
            type is (integer(I1P))
                hasSameType = .true.
        end select
    end function I1PWrapperFactory_hasSameType


    function I1PWrapperFactory_Wrap0D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create I1P 0D Wrapper
    !-----------------------------------------------------------------
        class(I1PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value)) then
            allocate(DimensionsWrapper0D_I1P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=0_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper0D_I1P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function I1PWrapperFactory_Wrap0D


    function I1PWrapperFactory_Wrap1D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create I1P 1D Wrapper
    !-----------------------------------------------------------------
        class(I1PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1))) then
            allocate(DimensionsWrapper1D_I1P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=1_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper1D_I1P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function I1PWrapperFactory_Wrap1D


    function I1PWrapperFactory_Wrap2D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create I1P 2D Wrapper
    !-----------------------------------------------------------------
        class(I1PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1))) then
            allocate(DimensionsWrapper2D_I1P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=2_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper2D_I1P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function I1PWrapperFactory_Wrap2D


    function I1PWrapperFactory_Wrap3D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create I1P 3D Wrapper
    !-----------------------------------------------------------------
        class(I1PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1,1))) then
            allocate(DimensionsWrapper3D_I1P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=3_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper3D_I1P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function I1PWrapperFactory_Wrap3D


    function I1PWrapperFactory_Wrap4D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create I1P 4D Wrapper
    !-----------------------------------------------------------------
        class(I1PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1,1,1))) then
            allocate(DimensionsWrapper4D_I1P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=4_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper4D_I1P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function I1PWrapperFactory_Wrap4D


    function I1PWrapperFactory_Wrap5D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create I1P 5D Wrapper
    !-----------------------------------------------------------------
        class(I1PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:,1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1,1,1,1))) then
            allocate(DimensionsWrapper5D_I1P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=5_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper5D_I1P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function I1PWrapperFactory_Wrap5D


    function I1PWrapperFactory_Wrap6D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create I1P 6D Wrapper
    !-----------------------------------------------------------------
        class(I1PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:,1:,1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1,1,1,1,1))) then
            allocate(DimensionsWrapper6D_I1P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=6_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper6D_I1P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function I1PWrapperFactory_Wrap6D


    function I1PWrapperFactory_Wrap7D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create I1P 7D Wrapper
    !-----------------------------------------------------------------
        class(I1PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:,1:,1:,1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1,1,1,1,1,1))) then
            allocate(DimensionsWrapper7D_I1P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=7_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper7D_I1P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function I1PWrapperFactory_Wrap7D


    subroutine I1PWrapperFactory_UnWrap0D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the I1P 0D Wrapped Value
    !-----------------------------------------------------------------
        class(I1PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper0D_I1P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine I1PWrapperFactory_UnWrap1D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the I1P 1D Wrapped Value
    !-----------------------------------------------------------------
        class(I1PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper1D_I1P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine I1PWrapperFactory_UnWrap2D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the I1P 2D Wrapped Value
    !-----------------------------------------------------------------
        class(I1PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper2D_I1P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine I1PWrapperFactory_UnWrap3D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the I1P 3D Wrapped Value
    !-----------------------------------------------------------------
        class(I1PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper3D_I1P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine I1PWrapperFactory_UnWrap4D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the I1P 4D Wrapped Value
    !-----------------------------------------------------------------
        class(I1PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper4D_I1P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine I1PWrapperFactory_UnWrap5D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the I1P 5D Wrapped Value
    !-----------------------------------------------------------------
        class(I1PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper5D_I1P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine I1PWrapperFactory_UnWrap6D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the I1P 6D Wrapped Value
    !-----------------------------------------------------------------
        class(I1PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper6D_I1P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine I1PWrapperFactory_UnWrap7D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the I1P 7D Wrapped Value
    !-----------------------------------------------------------------
        class(I1PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:,:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper7D_I1P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    function I2PWrapperFactory_hasSameType(this, Value) result(hasSameType)
    !-----------------------------------------------------------------
    !< Check if Value type agrees with wrapper type
    !-----------------------------------------------------------------
        class(I2PWrapperFactory_t), intent(IN) :: this
        class(*),                   intent(IN) :: Value
        logical                                :: hasSameType
    !-----------------------------------------------------------------
        hasSameType = .false.
        select type(Value)
            type is (integer(I2P))
                hasSameType = .true.
        end select
    end function I2PWrapperFactory_hasSameType


    function I2PWrapperFactory_Wrap0D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create I2P 0D Wrapper
    !-----------------------------------------------------------------
        class(I2PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value)) then
            allocate(DimensionsWrapper0D_I2P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=0_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper0D_I2P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function I2PWrapperFactory_Wrap0D


    function I2PWrapperFactory_Wrap1D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create I2P 1D Wrapper
    !-----------------------------------------------------------------
        class(I2PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1))) then
            allocate(DimensionsWrapper1D_I2P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=1_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper1D_I2P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function I2PWrapperFactory_Wrap1D


    function I2PWrapperFactory_Wrap2D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create I2P 2D Wrapper
    !-----------------------------------------------------------------
        class(I2PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1))) then
            allocate(DimensionsWrapper2D_I2P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=2_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper2D_I2P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function I2PWrapperFactory_Wrap2D


    function I2PWrapperFactory_Wrap3D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create I2P 3D Wrapper
    !-----------------------------------------------------------------
        class(I2PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1,1))) then
            allocate(DimensionsWrapper3D_I2P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=3_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper3D_I2P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function I2PWrapperFactory_Wrap3D


    function I2PWrapperFactory_Wrap4D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create I2P 4D Wrapper
    !-----------------------------------------------------------------
        class(I2PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1,1,1))) then
            allocate(DimensionsWrapper4D_I2P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=4_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper4D_I2P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function I2PWrapperFactory_Wrap4D


    function I2PWrapperFactory_Wrap5D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create I2P 5D Wrapper
    !-----------------------------------------------------------------
        class(I2PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:,1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1,1,1,1))) then
            allocate(DimensionsWrapper5D_I2P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=5_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper5D_I2P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function I2PWrapperFactory_Wrap5D


    function I2PWrapperFactory_Wrap6D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create I2P 6D Wrapper
    !-----------------------------------------------------------------
        class(I2PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:,1:,1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1,1,1,1,1))) then
            allocate(DimensionsWrapper6D_I2P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=6_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper6D_I2P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function I2PWrapperFactory_Wrap6D


    function I2PWrapperFactory_Wrap7D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create I2P 7D Wrapper
    !-----------------------------------------------------------------
        class(I2PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:,1:,1:,1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1,1,1,1,1,1))) then
            allocate(DimensionsWrapper7D_I2P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=7_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper7D_I2P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function I2PWrapperFactory_Wrap7D


    subroutine I2PWrapperFactory_UnWrap0D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the I2P 0D Wrapped Value
    !-----------------------------------------------------------------
        class(I2PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper0D_I2P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine I2PWrapperFactory_UnWrap1D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the I2P 1D Wrapped Value
    !-----------------------------------------------------------------
        class(I2PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper1D_I2P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine I2PWrapperFactory_UnWrap2D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the I2P 2D Wrapped Value
    !-----------------------------------------------------------------
        class(I2PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper2D_I2P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine I2PWrapperFactory_UnWrap3D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the I2P 3D Wrapped Value
    !-----------------------------------------------------------------
        class(I2PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper3D_I2P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine I2PWrapperFactory_UnWrap4D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the I2P 4D Wrapped Value
    !-----------------------------------------------------------------
        class(I2PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper4D_I2P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine I2PWrapperFactory_UnWrap5D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the I2P 5D Wrapped Value
    !-----------------------------------------------------------------
        class(I2PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper5D_I2P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine I2PWrapperFactory_UnWrap6D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the I2P 6D Wrapped Value
    !-----------------------------------------------------------------
        class(I2PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper6D_I2P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine I2PWrapperFactory_UnWrap7D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the I2P 7D Wrapped Value
    !-----------------------------------------------------------------
        class(I2PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:,:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper7D_I2P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    function I4PWrapperFactory_hasSameType(this, Value) result(hasSameType)
    !-----------------------------------------------------------------
    !< Check if Value type agrees with wrapper type
    !-----------------------------------------------------------------
        class(I4PWrapperFactory_t), intent(IN) :: this
        class(*),                   intent(IN) :: Value
        logical                                :: hasSameType
    !-----------------------------------------------------------------
        hasSameType = .false.
        select type(Value)
            type is (integer(I4P))
                hasSameType = .true.
        end select
    end function I4PWrapperFactory_hasSameType


    function I4PWrapperFactory_Wrap0D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create I4P 0D Wrapper
    !-----------------------------------------------------------------
        class(I4PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value)) then
            allocate(DimensionsWrapper0D_I4P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=0_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper0D_I4P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function I4PWrapperFactory_Wrap0D


    function I4PWrapperFactory_Wrap1D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create I4P 1D Wrapper
    !-----------------------------------------------------------------
        class(I4PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1))) then
            allocate(DimensionsWrapper1D_I4P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=1_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper1D_I4P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function I4PWrapperFactory_Wrap1D


    function I4PWrapperFactory_Wrap2D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create I4P 2D Wrapper
    !-----------------------------------------------------------------
        class(I4PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1))) then
            allocate(DimensionsWrapper2D_I4P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=2_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper2D_I4P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function I4PWrapperFactory_Wrap2D


    function I4PWrapperFactory_Wrap3D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create I4P 3D Wrapper
    !-----------------------------------------------------------------
        class(I4PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1,1))) then
            allocate(DimensionsWrapper3D_I4P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=3_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper3D_I4P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function I4PWrapperFactory_Wrap3D


    function I4PWrapperFactory_Wrap4D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create I4P 4D Wrapper
    !-----------------------------------------------------------------
        class(I4PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1,1,1))) then
            allocate(DimensionsWrapper4D_I4P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=4_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper4D_I4P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function I4PWrapperFactory_Wrap4D


    function I4PWrapperFactory_Wrap5D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create I4P 5D Wrapper
    !-----------------------------------------------------------------
        class(I4PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:,1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1,1,1,1))) then
            allocate(DimensionsWrapper5D_I4P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=5_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper5D_I4P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function I4PWrapperFactory_Wrap5D


    function I4PWrapperFactory_Wrap6D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create I4P 6D Wrapper
    !-----------------------------------------------------------------
        class(I4PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:,1:,1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1,1,1,1,1))) then
            allocate(DimensionsWrapper6D_I4P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=6_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper6D_I4P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function I4PWrapperFactory_Wrap6D


    function I4PWrapperFactory_Wrap7D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create I4P 7D Wrapper
    !-----------------------------------------------------------------
        class(I4PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:,1:,1:,1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1,1,1,1,1,1))) then
            allocate(DimensionsWrapper7D_I4P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=7_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper7D_I4P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function I4PWrapperFactory_Wrap7D


    subroutine I4PWrapperFactory_UnWrap0D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the I4P 0D Wrapped Value
    !-----------------------------------------------------------------
        class(I4PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper0D_I4P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine I4PWrapperFactory_UnWrap1D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the I4P 1D Wrapped Value
    !-----------------------------------------------------------------
        class(I4PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper1D_I4P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine I4PWrapperFactory_UnWrap2D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the I4P 2D Wrapped Value
    !-----------------------------------------------------------------
        class(I4PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper2D_I4P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine I4PWrapperFactory_UnWrap3D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the I4P 3D Wrapped Value
    !-----------------------------------------------------------------
        class(I4PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper3D_I4P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine I4PWrapperFactory_UnWrap4D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the I4P 4D Wrapped Value
    !-----------------------------------------------------------------
        class(I4PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper4D_I4P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine I4PWrapperFactory_UnWrap5D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the I4P 5D Wrapped Value
    !-----------------------------------------------------------------
        class(I4PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper5D_I4P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine I4PWrapperFactory_UnWrap6D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the I4P 6D Wrapped Value
    !-----------------------------------------------------------------
        class(I4PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper6D_I4P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine I4PWrapperFactory_UnWrap7D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the I4P 7D Wrapped Value
    !-----------------------------------------------------------------
        class(I4PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:,:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper7D_I4P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    function I8PWrapperFactory_hasSameType(this, Value) result(hasSameType)
    !-----------------------------------------------------------------
    !< Check if Value type agrees with wrapper type
    !-----------------------------------------------------------------
        class(I8PWrapperFactory_t), intent(IN) :: this
        class(*),                   intent(IN) :: Value
        logical                                :: hasSameType
    !-----------------------------------------------------------------
        hasSameType = .false.
        select type(Value)
            type is (integer(I8P))
                hasSameType = .true.
        end select
    end function I8PWrapperFactory_hasSameType


    function I8PWrapperFactory_Wrap0D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create I8P 0D Wrapper
    !-----------------------------------------------------------------
        class(I8PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value)) then
            allocate(DimensionsWrapper0D_I8P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=0_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper0D_I8P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function I8PWrapperFactory_Wrap0D


    function I8PWrapperFactory_Wrap1D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create I8P 1D Wrapper
    !-----------------------------------------------------------------
        class(I8PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1))) then
            allocate(DimensionsWrapper1D_I8P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=1_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper1D_I8P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function I8PWrapperFactory_Wrap1D


    function I8PWrapperFactory_Wrap2D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create I8P 2D Wrapper
    !-----------------------------------------------------------------
        class(I8PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1))) then
            allocate(DimensionsWrapper2D_I8P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=2_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper2D_I8P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function I8PWrapperFactory_Wrap2D


    function I8PWrapperFactory_Wrap3D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create I8P 3D Wrapper
    !-----------------------------------------------------------------
        class(I8PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1,1))) then
            allocate(DimensionsWrapper3D_I8P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=3_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper3D_I8P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function I8PWrapperFactory_Wrap3D


    function I8PWrapperFactory_Wrap4D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create I8P 4D Wrapper
    !-----------------------------------------------------------------
        class(I8PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1,1,1))) then
            allocate(DimensionsWrapper4D_I8P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=4_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper4D_I8P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function I8PWrapperFactory_Wrap4D


    function I8PWrapperFactory_Wrap5D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create I8P 5D Wrapper
    !-----------------------------------------------------------------
        class(I8PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:,1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1,1,1,1))) then
            allocate(DimensionsWrapper5D_I8P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=5_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper5D_I8P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function I8PWrapperFactory_Wrap5D


    function I8PWrapperFactory_Wrap6D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create I8P 6D Wrapper
    !-----------------------------------------------------------------
        class(I8PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:,1:,1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1,1,1,1,1))) then
            allocate(DimensionsWrapper6D_I8P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=6_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper6D_I8P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function I8PWrapperFactory_Wrap6D


    function I8PWrapperFactory_Wrap7D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create I8P 7D Wrapper
    !-----------------------------------------------------------------
        class(I8PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:,1:,1:,1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1,1,1,1,1,1))) then
            allocate(DimensionsWrapper7D_I8P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=7_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper7D_I8P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function I8PWrapperFactory_Wrap7D


    subroutine I8PWrapperFactory_UnWrap0D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the I8P 0D Wrapped Value
    !-----------------------------------------------------------------
        class(I8PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper0D_I8P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine I8PWrapperFactory_UnWrap1D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the I8P 1D Wrapped Value
    !-----------------------------------------------------------------
        class(I8PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper1D_I8P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine I8PWrapperFactory_UnWrap2D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the I8P 2D Wrapped Value
    !-----------------------------------------------------------------
        class(I8PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper2D_I8P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine I8PWrapperFactory_UnWrap3D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the I8P 3D Wrapped Value
    !-----------------------------------------------------------------
        class(I8PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper3D_I8P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine I8PWrapperFactory_UnWrap4D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the I8P 4D Wrapped Value
    !-----------------------------------------------------------------
        class(I8PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper4D_I8P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine I8PWrapperFactory_UnWrap5D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the I8P 5D Wrapped Value
    !-----------------------------------------------------------------
        class(I8PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper5D_I8P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine I8PWrapperFactory_UnWrap6D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the I8P 6D Wrapped Value
    !-----------------------------------------------------------------
        class(I8PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper6D_I8P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine I8PWrapperFactory_UnWrap7D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the I8P 7D Wrapped Value
    !-----------------------------------------------------------------
        class(I8PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:,:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper7D_I8P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    function LWrapperFactory_hasSameType(this, Value) result(hasSameType)
    !-----------------------------------------------------------------
    !< Check if Value type agrees with wrapper type
    !-----------------------------------------------------------------
        class(LWrapperFactory_t),   intent(IN) :: this
        class(*),                   intent(IN) :: Value
        logical                                :: hasSameType
    !-----------------------------------------------------------------
        hasSameType = .false.
        select type(Value)
            type is (logical)
                hasSameType = .true.
        end select
    end function LWrapperFactory_hasSameType


    function LWrapperFactory_Wrap0D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create L 0D Wrapper
    !-----------------------------------------------------------------
        class(LWrapperFactory_t),                intent(IN)    :: this
        class(*),                                intent(IN)    :: Value
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value)) then
            allocate(DimensionsWrapper0D_L_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=0_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper0D_L_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function LWrapperFactory_Wrap0D


    function LWrapperFactory_Wrap1D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create L 1D Wrapper
    !-----------------------------------------------------------------
        class(LWrapperFactory_t),                intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1))) then
            allocate(DimensionsWrapper1D_L_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=1_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper1D_L_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function LWrapperFactory_Wrap1D


    function LWrapperFactory_Wrap2D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create L 2D Wrapper
    !-----------------------------------------------------------------
        class(LWrapperFactory_t),                intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1))) then
            allocate(DimensionsWrapper2D_L_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=2_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper2D_L_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function LWrapperFactory_Wrap2D


    function LWrapperFactory_Wrap3D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create L 3D Wrapper
    !-----------------------------------------------------------------
        class(LWrapperFactory_t),                intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1,1))) then
            allocate(DimensionsWrapper3D_L_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=3_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper3D_L_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function LWrapperFactory_Wrap3D


    function LWrapperFactory_Wrap4D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create L 4D Wrapper
    !-----------------------------------------------------------------
        class(LWrapperFactory_t),                intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1,1,1))) then
            allocate(DimensionsWrapper4D_L_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=4_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper4D_L_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function LWrapperFactory_Wrap4D


    function LWrapperFactory_Wrap5D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create L 5D Wrapper
    !-----------------------------------------------------------------
        class(LWrapperFactory_t),                intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:,1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1,1,1,1))) then
            allocate(DimensionsWrapper5D_L_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=5_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper5D_L_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function LWrapperFactory_Wrap5D


    function LWrapperFactory_Wrap6D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create L 6D Wrapper
    !-----------------------------------------------------------------
        class(LWrapperFactory_t),                intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:,1:,1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1,1,1,1,1))) then
            allocate(DimensionsWrapper6D_L_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=6_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper6D_L_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function LWrapperFactory_Wrap6D


    function LWrapperFactory_Wrap7D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create L 7D Wrapper
    !-----------------------------------------------------------------
        class(LWrapperFactory_t),                intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:,1:,1:,1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1,1,1,1,1,1))) then
            allocate(DimensionsWrapper7D_L_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=7_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper7D_L_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function LWrapperFactory_Wrap7D


    subroutine LWrapperFactory_UnWrap0D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the L 0D Wrapped Value
    !-----------------------------------------------------------------
        class(LWrapperFactory_t),                intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper0D_L_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine LWrapperFactory_UnWrap1D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the L 1D Wrapped Value
    !-----------------------------------------------------------------
        class(LWrapperFactory_t),                intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper1D_L_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine LWrapperFactory_UnWrap2D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the L 2D Wrapped Value
    !-----------------------------------------------------------------
        class(LWrapperFactory_t),                intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper2D_L_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine LWrapperFactory_UnWrap3D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the L 3D Wrapped Value
    !-----------------------------------------------------------------
        class(LWrapperFactory_t),                intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper3D_L_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine LWrapperFactory_UnWrap4D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the L 4D Wrapped Value
    !-----------------------------------------------------------------
        class(LWrapperFactory_t),                intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper4D_L_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine LWrapperFactory_UnWrap5D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the L 5D Wrapped Value
    !-----------------------------------------------------------------
        class(LWrapperFactory_t),                intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper5D_L_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine LWrapperFactory_UnWrap6D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the L 6D Wrapped Value
    !-----------------------------------------------------------------
        class(LWrapperFactory_t),                intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper6D_L_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine LWrapperFactory_UnWrap7D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the L 7D Wrapped Value
    !-----------------------------------------------------------------
        class(LWrapperFactory_t),                intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:,:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper7D_L_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    function R4PWrapperFactory_hasSameType(this, Value) result(hasSameType)
    !-----------------------------------------------------------------
    !< Check if Value type agrees with wrapper type
    !-----------------------------------------------------------------
        class(R4PWrapperFactory_t), intent(IN) :: this
        class(*),                   intent(IN) :: Value
        logical                                :: hasSameType
    !-----------------------------------------------------------------
        hasSameType = .false.
        select type(Value)
            type is (real(R4P))
                hasSameType = .true.
        end select
    end function R4PWrapperFactory_hasSameType


    function R4PWrapperFactory_Wrap0D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create R4P 0D Wrapper
    !-----------------------------------------------------------------
        class(R4PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value)) then
            allocate(DimensionsWrapper0D_R4P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=0_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper0D_R4P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function R4PWrapperFactory_Wrap0D


    function R4PWrapperFactory_Wrap1D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create R4P 1D Wrapper
    !-----------------------------------------------------------------
        class(R4PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1))) then
            allocate(DimensionsWrapper1D_R4P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=1_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper1D_R4P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function R4PWrapperFactory_Wrap1D


    function R4PWrapperFactory_Wrap2D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create R4P 2D Wrapper
    !-----------------------------------------------------------------
        class(R4PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1))) then
            allocate(DimensionsWrapper2D_R4P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=2_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper2D_R4P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function R4PWrapperFactory_Wrap2D


    function R4PWrapperFactory_Wrap3D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create R4P 3D Wrapper
    !-----------------------------------------------------------------
        class(R4PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1,1))) then
            allocate(DimensionsWrapper3D_R4P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=3_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper3D_R4P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function R4PWrapperFactory_Wrap3D


    function R4PWrapperFactory_Wrap4D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create R4P 4D Wrapper
    !-----------------------------------------------------------------
        class(R4PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1,1,1))) then
            allocate(DimensionsWrapper4D_R4P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=4_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper4D_R4P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function R4PWrapperFactory_Wrap4D


    function R4PWrapperFactory_Wrap5D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create R4P 5D Wrapper
    !-----------------------------------------------------------------
        class(R4PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:,1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1,1,1,1))) then
            allocate(DimensionsWrapper5D_R4P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=5_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper5D_R4P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function R4PWrapperFactory_Wrap5D


    function R4PWrapperFactory_Wrap6D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create R4P 6D Wrapper
    !-----------------------------------------------------------------
        class(R4PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:,1:,1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1,1,1,1,1))) then
            allocate(DimensionsWrapper6D_R4P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=6_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper6D_R4P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function R4PWrapperFactory_Wrap6D


    function R4PWrapperFactory_Wrap7D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create R4P 7D Wrapper
    !-----------------------------------------------------------------
        class(R4PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:,1:,1:,1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1,1,1,1,1,1))) then
            allocate(DimensionsWrapper7D_R4P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=7_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper7D_R4P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function R4PWrapperFactory_Wrap7D


    subroutine R4PWrapperFactory_UnWrap0D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the R4P 0D Wrapped Value
    !-----------------------------------------------------------------
        class(R4PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper0D_R4P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine R4PWrapperFactory_UnWrap1D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the R4P 1D Wrapped Value
    !-----------------------------------------------------------------
        class(R4PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper1D_R4P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine R4PWrapperFactory_UnWrap2D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the R4P 2D Wrapped Value
    !-----------------------------------------------------------------
        class(R4PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper2D_R4P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine R4PWrapperFactory_UnWrap3D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the R4P 3D Wrapped Value
    !-----------------------------------------------------------------
        class(R4PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper3D_R4P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine R4PWrapperFactory_UnWrap4D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the R4P 4D Wrapped Value
    !-----------------------------------------------------------------
        class(R4PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper4D_R4P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine R4PWrapperFactory_UnWrap5D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the R4P 5D Wrapped Value
    !-----------------------------------------------------------------
        class(R4PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper5D_R4P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine R4PWrapperFactory_UnWrap6D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the R4P 6D Wrapped Value
    !-----------------------------------------------------------------
        class(R4PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper6D_R4P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine R4PWrapperFactory_UnWrap7D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the R4P 7D Wrapped Value
    !-----------------------------------------------------------------
        class(R4PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:,:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper7D_R4P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    function R8PWrapperFactory_hasSameType(this, Value) result(hasSameType)
    !-----------------------------------------------------------------
    !< Check if Value type agrees with wrapper type
    !-----------------------------------------------------------------
        class(R8PWrapperFactory_t), intent(IN) :: this
        class(*),                   intent(IN) :: Value
        logical                                :: hasSameType
    !-----------------------------------------------------------------
        hasSameType = .false.
        select type(Value)
            type is (real(R8P))
                hasSameType = .true.
        end select
    end function R8PWrapperFactory_hasSameType


    function R8PWrapperFactory_Wrap0D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create R8P 0D Wrapper
    !-----------------------------------------------------------------
        class(R8PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value)) then
            allocate(DimensionsWrapper0D_R8P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=0_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper0D_R8P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function R8PWrapperFactory_Wrap0D


    function R8PWrapperFactory_Wrap1D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create R8P 1D Wrapper
    !-----------------------------------------------------------------
        class(R8PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1))) then
            allocate(DimensionsWrapper1D_R8P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=1_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper1D_R8P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function R8PWrapperFactory_Wrap1D


    function R8PWrapperFactory_Wrap2D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create R8P 2D Wrapper
    !-----------------------------------------------------------------
        class(R8PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1))) then
            allocate(DimensionsWrapper2D_R8P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=2_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper2D_R8P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function R8PWrapperFactory_Wrap2D


    function R8PWrapperFactory_Wrap3D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create R8P 3D Wrapper
    !-----------------------------------------------------------------
        class(R8PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1,1))) then
            allocate(DimensionsWrapper3D_R8P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=3_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper3D_R8P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function R8PWrapperFactory_Wrap3D


    function R8PWrapperFactory_Wrap4D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create R8P 4D Wrapper
    !-----------------------------------------------------------------
        class(R8PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1,1,1))) then
            allocate(DimensionsWrapper4D_R8P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=4_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper4D_R8P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function R8PWrapperFactory_Wrap4D


    function R8PWrapperFactory_Wrap5D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create R8P 5D Wrapper
    !-----------------------------------------------------------------
        class(R8PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:,1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1,1,1,1))) then
            allocate(DimensionsWrapper5D_R8P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=5_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper5D_R8P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function R8PWrapperFactory_Wrap5D


    function R8PWrapperFactory_Wrap6D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create R8P 6D Wrapper
    !-----------------------------------------------------------------
        class(R8PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:,1:,1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1,1,1,1,1))) then
            allocate(DimensionsWrapper6D_R8P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=6_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper6D_R8P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function R8PWrapperFactory_Wrap6D


    function R8PWrapperFactory_Wrap7D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create R8P 7D Wrapper
    !-----------------------------------------------------------------
        class(R8PWrapperFactory_t),              intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:,1:,1:,1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value(1,1,1,1,1,1,1))) then
            allocate(DimensionsWrapper7D_R8P_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=7_I1P)
            select type (Wrapper)
                type is(DimensionsWrapper7D_R8P_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function R8PWrapperFactory_Wrap7D


    subroutine R8PWrapperFactory_UnWrap0D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the R8P 0D Wrapped Value
    !-----------------------------------------------------------------
        class(R8PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper0D_R8P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine R8PWrapperFactory_UnWrap1D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the R8P 1D Wrapped Value
    !-----------------------------------------------------------------
        class(R8PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper1D_R8P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine R8PWrapperFactory_UnWrap2D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the R8P 2D Wrapped Value
    !-----------------------------------------------------------------
        class(R8PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper2D_R8P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine R8PWrapperFactory_UnWrap3D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the R8P 3D Wrapped Value
    !-----------------------------------------------------------------
        class(R8PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper3D_R8P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine R8PWrapperFactory_UnWrap4D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the R8P 4D Wrapped Value
    !-----------------------------------------------------------------
        class(R8PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper4D_R8P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine R8PWrapperFactory_UnWrap5D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the R8P 5D Wrapped Value
    !-----------------------------------------------------------------
        class(R8PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper5D_R8P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine R8PWrapperFactory_UnWrap6D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the R8P 6D Wrapped Value
    !-----------------------------------------------------------------
        class(R8PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper6D_R8P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


    subroutine R8PWrapperFactory_UnWrap7D(this, Wrapper, Value)
    !-----------------------------------------------------------------
    !< Return the R8P 7D Wrapped Value
    !-----------------------------------------------------------------
        class(R8PWrapperFactory_t),              intent(IN)    :: this
        class(DimensionsWrapper_t), pointer,     intent(IN)    :: Wrapper
        class(*),                                intent(INOUT) :: Value(:,:,:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Wrapper)
            type is(DimensionsWrapper7D_R8P_t)
                call Wrapper%Get(Value = Value)
        end select
    end subroutine


end module WrapperFactory
