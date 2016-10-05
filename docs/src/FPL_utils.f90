module FPL_Utils

USE PENF, only: I1P, I4P

contains

    elemental function byte_size_logical(l) result(bytes)
    !-----------------------------------------------------------------
    !< Procedure for computing the number of bytes of a logical variable.
    !-----------------------------------------------------------------
        logical, intent(IN):: l        !< Character variable whose number of bits must be computed.
        integer(I4P)       :: bytes    !< Number of bits of l.
        integer(I1P)       :: mold(1)  !< "Molding" dummy variable for bits counting.
    !-----------------------------------------------------------------
        bytes = size(transfer(l,mold),dim=1,kind=I1P)
        return
    end function byte_size_logical
end module FPL_Utils
