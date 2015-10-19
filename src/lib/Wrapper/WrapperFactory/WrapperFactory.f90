module WrapperFactory

USE DimensionsWrapper

implicit none
private

    type, abstract :: WrapperFactory_t
    private

    contains
        private
        procedure(WrapperFactory_Create0D),            deferred :: Create0D
        procedure(WrapperFactory_Create1D),            deferred :: Create1D
        procedure(WrapperFactory_Create2D),            deferred :: Create2D
        procedure(WrapperFactory_Create3D),            deferred :: Create3D
        procedure(WrapperFactory_Create4D),            deferred :: Create4D
        procedure(WrapperFactory_Create5D),            deferred :: Create5D
        procedure(WrapperFactory_Create6D),            deferred :: Create6D
        procedure(WrapperFactory_Create7D),            deferred :: Create7D
        procedure(WrapperFactory_hasSameType), public, deferred :: hasSameType 
        generic,                               public           :: Create => Create0D, &
                                                                             Create1D, &
                                                                             Create2D, &
                                                                             Create3D, &
                                                                             Create4D, &
                                                                             Create5D, &
                                                                             Create6D, &
                                                                             Create7D
    end type

    abstract interface
        function WrapperFactory_hasSameType(this, Value) result(hasSameType)
            import WrapperFactory_t
            class(WrapperFactory_t), intent(IN) :: this
            class(*),                intent(IN) :: Value
            logical                             :: hasSameType
        end function

        subroutine WrapperFactory_Create0D(this, Value, Wrapper)
            import WrapperFactory_t
            import DimensionsWrapper_t
            class(WrapperFactory_t),                 intent(IN)    :: this
            class(*),                                intent(IN)    :: Value
            class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
        end subroutine

        subroutine WrapperFactory_Create1D(this, Value, Wrapper)
            import WrapperFactory_t
            import DimensionsWrapper_t
            class(WrapperFactory_t),                 intent(IN)    :: this
            class(*),                                intent(IN)    :: Value(:)
            class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
        end subroutine

        subroutine WrapperFactory_Create2D(this, Value, Wrapper)
            import WrapperFactory_t
            import DimensionsWrapper_t
            class(WrapperFactory_t),                 intent(IN)    :: this
            class(*),                                intent(IN)    :: Value(:,:)
            class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
        end subroutine

        subroutine WrapperFactory_Create3D(this, Value, Wrapper)
            import WrapperFactory_t
            import DimensionsWrapper_t
            class(WrapperFactory_t),                 intent(IN)    :: this
            class(*),                                intent(IN)    :: Value(:,:,:)
            class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
        end subroutine

        subroutine WrapperFactory_Create4D(this, Value, Wrapper)
            import WrapperFactory_t
            import DimensionsWrapper_t
            class(WrapperFactory_t),                 intent(IN)    :: this
            class(*),                                intent(IN)    :: Value(:,:,:,:)
            class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
        end subroutine

        subroutine WrapperFactory_Create5D(this, Value, Wrapper)
            import WrapperFactory_t
            import DimensionsWrapper_t
            class(WrapperFactory_t),                 intent(IN)    :: this
            class(*),                                intent(IN)    :: Value(:,:,:,:,:)
            class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
        end subroutine

        subroutine WrapperFactory_Create6D(this, Value, Wrapper)
            import WrapperFactory_t
            import DimensionsWrapper_t
            class(WrapperFactory_t),                 intent(IN)    :: this
            class(*),                                intent(IN)    :: Value(:,:,:,:,:,:)
            class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
        end subroutine

        subroutine WrapperFactory_Create7D(this, Value, Wrapper)
            import WrapperFactory_t
            import DimensionsWrapper_t
            class(WrapperFactory_t),                 intent(IN)    :: this
            class(*),                                intent(IN)    :: Value(:,:,:,:,:,:,:)
            class(DimensionsWrapper_t), allocatable, intent(INOUT) :: Wrapper
        end subroutine
    end interface

public :: WrapperFactory_t

end module WrapperFactory
