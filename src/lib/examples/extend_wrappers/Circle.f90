module Circle

implicit none
private

    type :: Circle_t
    private
        real :: Radius
    contains
    private
        procedure         ::                    Circle_Assign
        procedure, public :: SetRadius       => Circle_SetRadius
        procedure, public :: GetRadius       => Circle_GetRadius
        procedure, public :: DataSizeInBytes => Circle_DataSizeInBytes
        generic,   public :: assignment(=)   => Circle_Assign
    end type Circle_t

public :: Circle_t

contains

    subroutine Circle_Assign(A,B)
    !-----------------------------------------------------------------
    !< Assignment overloading
    !-----------------------------------------------------------------

        class(Circle_t), intent(OUT) :: A
        class(Circle_t), intent(IN)  :: B
        real                         :: Radius
    !-----------------------------------------------------------------
        call B%GetRadius(Radius=Radius)
        call A%SetRadius(Radius=Radius)
    end subroutine

    subroutine Circle_SetRadius(this, Radius)
    !-----------------------------------------------------------------
    !< Set the radius of the Circle
    !-----------------------------------------------------------------

        class(Circle_t), intent(INOUT) :: this
        real,            intent(IN)    :: Radius
    !-----------------------------------------------------------------
        this%Radius = Radius
    end subroutine

    subroutine Circle_GetRadius(this, Radius)
    !-----------------------------------------------------------------
    !< Return the radius of the circle
    !-----------------------------------------------------------------

        class(Circle_t), intent(IN)  :: this
        real,            intent(OUT) :: Radius
    !-----------------------------------------------------------------
        Radius = this%Radius
    end subroutine


    function Circle_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(Circle_t),           intent(IN) :: this             !< Circle wrapper 0D
        integer                               :: DataSizeInBytes  !< Data size of the stored data in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = 4
    end function Circle_DataSizeInBytes

end module
