module DimensionsWrapper

USE IR_Precision, only: I1P, I4P

implicit none
private

    type, abstract :: DimensionsWrapper_t
    private
        integer(I1P) :: Dimensions = -1
    contains
    private
        procedure         ::                  DimensionsWrapper_Set
        procedure         ::                  DimensionsWrapper_Get
        procedure, public :: SetDimensions => DimensionsWrapper_SetDimensions
        procedure, public :: GetDimensions => DimensionsWrapper_GetDimensions
        generic,   public :: Set           => DimensionsWrapper_Set
        generic,   public :: Get           => DimensionsWrapper_Get
        procedure(DimensionsWrapper_isOfDataType), public, deferred :: isOfDataType
        procedure(DimensionsWrapper_Print),        public, deferred :: Print
        procedure(DimensionsWrapper_Free),         public, deferred :: Free
    end type

    abstract interface
        subroutine DimensionsWrapper_Free(this)
            import DimensionsWrapper_t
            class(DimensionsWrapper_t), intent(INOUT) :: this
        end subroutine
        subroutine DimensionsWrapper_Print(this, unit, prefix, iostat, iomsg)
            import DimensionsWrapper_t
            import I4P
            class(DimensionsWrapper_t), intent(IN)  :: this
            integer(I4P),               intent(IN)  :: unit
            character(*), optional,     intent(IN)  :: prefix
            integer(I4P), optional,     intent(OUT) :: iostat
            character(*), optional,     intent(OUT) :: iomsg
        end subroutine
        function DimensionsWrapper_isOfDataType(this, Mold) result(isOfDataType)
            import DimensionsWrapper_t
            class(DimensionsWrapper_t), intent(IN) :: this
            class(*),                   intent(IN) :: Mold
            logical                                :: isOfDataType
        end function
    end interface

public :: DimensionsWrapper_t


contains

    subroutine DimensionsWrapper_SetDimensions(this, Dimensions)
    !-----------------------------------------------------------------
    !< Set the dimensions of the Value contained in the wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper_t), intent(INOUT) :: this
        integer(I1P),               intent(IN)    :: Dimensions
    !-----------------------------------------------------------------
        this%Dimensions = Dimensions
    end subroutine


    function DimensionsWrapper_GetDimensions(this) result(Dimensions)
    !-----------------------------------------------------------------
    !< Get the dimensions of the Value contained in the wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper_t), intent(IN) :: this
    !-----------------------------------------------------------------
        integer(I1P)                              :: Dimensions
        Dimensions = this%Dimensions
    end function

    subroutine DimensionsWrapper_Set(this)
    !-----------------------------------------------------------------
    !< Empty Set implementation
    !-----------------------------------------------------------------
        class(DimensionsWrapper_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
    end subroutine

    subroutine DimensionsWrapper_Get(this)
    !-----------------------------------------------------------------
    !< Empty Get implementation
    !-----------------------------------------------------------------
        class(DimensionsWrapper_t), intent(IN) :: this
    !-----------------------------------------------------------------
    end subroutine

end module DimensionsWrapper
