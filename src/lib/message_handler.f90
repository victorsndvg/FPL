module message_handler

USE iso_fortran_env, only: OUTPUT_UNIT, ERROR_UNIT

implicit none
private

    type :: message_handler_t
    private
        character(len=5) :: prefix = '[FPL]'
    contains
        procedure :: Print => message_handler_Print
        procedure :: Warn  => message_handler_Warn
        procedure :: Error => message_handler_Error
    end type

    type(message_handler_t) :: msg

public :: msg

contains

    subroutine message_handler_Print(this, msg, newline)
        class(message_handler_t), intent(IN) :: this
        character(len=*),         intent(IN) :: msg
        logical, optional,        intent(IN) :: newline
        logical                              :: nl

        nl = .true.
        if(present(newline)) nl = newline
        if(nl) then
            write(unit=OUTPUT_UNIT, fmt='(A)') this%prefix//' '//trim(adjustl(msg))
        else
            write(unit=OUTPUT_UNIT, fmt='(A,$)') this%prefix//' '//trim(adjustl(msg))
        endif
    end subroutine message_handler_Print

    subroutine message_handler_Warn(this, msg)
        class(message_handler_t), intent(IN) :: this
        character(len=*),         intent(IN) :: msg

        call this%Print('WARNING!'//' '//trim(adjustl(msg)))
    end subroutine message_handler_Warn


    subroutine message_handler_Error(this, msg)
        class(message_handler_t), intent(IN) :: this
        character(len=*),         intent(IN) :: msg

        write(unit=ERROR_UNIT, fmt='(A)') this%prefix//' '//'ERROR!'//' '//trim(adjustl(msg))
    end subroutine message_handler_Error

end module message_handler
