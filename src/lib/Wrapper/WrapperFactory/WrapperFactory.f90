module WrapperFactory

USE DimensionsWrapper

implicit none
private

    type, abstract :: WrapperFactory_t
    private

    contains
        private
        procedure(WrapperFactory_Wrap0D),      public, deferred :: Wrap0D
        procedure(WrapperFactory_Wrap1D),      public, deferred :: Wrap1D
        procedure(WrapperFactory_Wrap2D),      public, deferred :: Wrap2D
        procedure(WrapperFactory_Wrap3D),      public, deferred :: Wrap3D
        procedure(WrapperFactory_Wrap4D),      public, deferred :: Wrap4D
        procedure(WrapperFactory_Wrap5D),      public, deferred :: Wrap5D
        procedure(WrapperFactory_Wrap6D),      public, deferred :: Wrap6D
        procedure(WrapperFactory_Wrap7D),      public, deferred :: Wrap7D
        procedure(WrapperFactory_hasSameType), public, deferred :: hasSameType 
        generic, public :: Wrap =>   Wrap0D, &
                                     Wrap1D, &
                                     Wrap2D, &
                                     Wrap3D, &
                                     Wrap4D, &
                                     Wrap5D, &
                                     Wrap6D, &
                                     Wrap7D
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

public :: WrapperFactory_t

end module WrapperFactory
