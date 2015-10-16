module WrapperFactory

USE DimensionsWrapper
USE DimensionsWrapper0D_L
USE DimensionsWrapper1D_L
USE DimensionsWrapper2D_L
USE DimensionsWrapper3D_L
USE DimensionsWrapper4D_L
USE DimensionsWrapper5D_L
USE DimensionsWrapper6D_L
USE DimensionsWrapper7D_L

implicit none
private

    type, abstract :: WrapperFactory_t
    private

    contains
        procedure(WrapperFactory_hasSameType), deferred :: hasSameType 
    end type

    abstract interface
        function WrapperFactory_hasSameType(this, Value) result(hasSameType)
            import WrapperFactory_t
            class(WrapperFactory_t), intent(IN) :: this
            class(*),                intent(IN) :: Value
            logical                             :: hasSameType
        end function
    end interface

public :: WrapperFactory_t

end module WrapperFactory
