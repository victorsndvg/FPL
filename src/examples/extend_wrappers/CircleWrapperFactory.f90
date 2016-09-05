module CircleWrapperFactory

USE Circle                      !< USE the data type to store
USE CircleWrapper               !< USE the corresponding Wrapper
USE DimensionsWrapper           !< USE the DimensionsWrapper abstract class
USE WrapperFactory              !< USE the WrapperFactory abstract class
USE ErrorMessages               !< USE the ErrorMessages for printing error messages
USE PENF, only: I1P             !< USE I1P data type

implicit none
private

    type, extends(WrapperFactory_t) :: CircleWrapperFactory_t
    private

    contains
        procedure         :: Wrap0D      => CircleWrapperFactory_Wrap0D         !< Wraps scalar Circles
        procedure         :: Wrap1D      => CircleWrapperFactory_Wrap1D         !< Wraps 1D arrays of Circles
        procedure         :: Wrap2D      => CircleWrapperFactory_Wrap2D         !< Wraps 2D arrays of Circles
        procedure         :: Wrap3D      => CircleWrapperFactory_Wrap3D         !< Wraps 3D arrays of Circles
        procedure         :: Wrap4D      => CircleWrapperFactory_Wrap4D         !< Wraps 4D arrays of Circles
        procedure         :: Wrap5D      => CircleWrapperFactory_Wrap5D         !< Wraps 5D arrays of Circles
        procedure         :: Wrap6D      => CircleWrapperFactory_Wrap6D         !< Wraps 6D arrays of Circles
        procedure         :: Wrap7D      => CircleWrapperFactory_Wrap7D         !< Wraps 7D arrays of Circles
        procedure, public :: hasSameType => CircleWrapperFactory_hasSameType    !< Check if the data type of a input Mold is Circle_t
    end type

    type(CircleWrapperFactory_t), public :: WrapperFactoryCircle                !< Public Wrapper Factory (singleton)

contains

    function CircleWrapperFactory_hasSameType(this, Value) result(hasSameType)
    !-----------------------------------------------------------------
    !< Check if Value type agrees with wrapper type
    !-----------------------------------------------------------------
        class(CircleWrapperFactory_t), intent(IN) :: this
        class(*),                   intent(IN) :: Value
        logical                                :: hasSameType
    !-----------------------------------------------------------------
        hasSameType = .false.
        select type(Value)
            type is (Circle_t)
                hasSameType = .true.
        end select
    end function CircleWrapperFactory_hasSameType


    function CircleWrapperFactory_Wrap0D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create Circle 0D Wrapper
    !-----------------------------------------------------------------
        class(CircleWrapperFactory_t),           intent(IN)    :: this
        class(*),                                intent(IN)    :: Value
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        if(this%hasSameType(Value)) then
            allocate(CircleWrapper_t::Wrapper)
            call Wrapper%SetDimensions(Dimensions=0_I1P)
            select type (Wrapper)
                type is(CircleWrapper_t)
                    call Wrapper%Set(Value=Value)
            end select
        endif
    end function CircleWrapperFactory_Wrap0D


    function CircleWrapperFactory_Wrap1D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create Circle 1D Wrapper
    !-----------------------------------------------------------------
        class(CircleWrapperFactory_t),           intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        call msg%Error(txt='Setting Value: Only scalar circle data type allowed', &
                            file=__FILE__, line=__LINE__ )
    end function CircleWrapperFactory_Wrap1D


    function CircleWrapperFactory_Wrap2D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create Circle 2D Wrapper
    !-----------------------------------------------------------------
        class(CircleWrapperFactory_t),           intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        call msg%Error(txt='Setting Value: Only scalar circle data type allowed', &
                            file=__FILE__, line=__LINE__ )
    end function CircleWrapperFactory_Wrap2D


    function CircleWrapperFactory_Wrap3D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create Circle 3D Wrapper
    !-----------------------------------------------------------------
        class(CircleWrapperFactory_t),           intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        call msg%Error(txt='Setting Value: Only scalar circle data type allowed', &
                            file=__FILE__, line=__LINE__ )
    end function CircleWrapperFactory_Wrap3D


    function CircleWrapperFactory_Wrap4D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create Circle 4D Wrapper
    !-----------------------------------------------------------------
        class(CircleWrapperFactory_t),           intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        call msg%Error(txt='Setting Value: Only scalar circle data type allowed', &
                            file=__FILE__, line=__LINE__ )
    end function CircleWrapperFactory_Wrap4D


    function CircleWrapperFactory_Wrap5D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create Circle 5D Wrapper
    !-----------------------------------------------------------------
        class(CircleWrapperFactory_t),           intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:,1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        call msg%Error(txt='Setting Value: Only scalar circle data type allowed', &
                            file=__FILE__, line=__LINE__ )
    end function CircleWrapperFactory_Wrap5D


    function CircleWrapperFactory_Wrap6D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create Circle 6D Wrapper
    !-----------------------------------------------------------------
        class(CircleWrapperFactory_t),           intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:,1:,1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        call msg%Error(txt='Setting Value: Only scalar circle data type allowed', &
                            file=__FILE__, line=__LINE__ )
    end function CircleWrapperFactory_Wrap6D


    function CircleWrapperFactory_Wrap7D(this, Value) result(Wrapper)
    !-----------------------------------------------------------------
    !< Create Circle 7D Wrapper
    !-----------------------------------------------------------------
        class(CircleWrapperFactory_t),           intent(IN)    :: this
        class(*),                                intent(IN)    :: Value(1:,1:,1:,1:,1:,1:,1:)
        class(DimensionsWrapper_t), pointer                    :: Wrapper
    !-----------------------------------------------------------------
        call msg%Error(txt='Setting Value: Only scalar circle data type allowed', &
                            file=__FILE__, line=__LINE__ )
    end function CircleWrapperFactory_Wrap7D

end module CircleWrapperFactory
