Program ParameterListEntryContainer_Test

USE iso_fortran_env, only: OUTPUT_UNIT
USE PENF, only: I4P, R4P, str
USE FPL

type(ParameterList_t) :: Parameters
integer(I4P),allocatable :: array(:)
integer :: iter, numiters, loop

numiters = 7

call FPL_Init()

call Parameters%Init()

do loop = 1, numiters
do iter = 1, numiters
    if(allocated(array)) deallocate(array); allocate(array(iter)); array = iter
    write(unit=OUTPUT_UNIT, fmt='(A)', advance="no") 'Setting: "'//'I4P_1D'//trim(str(no_sign=.true., n=iter))//'" ... '
    if(Parameters%Set(Key='I4P_1D'//trim(str(no_sign=.true., n=iter)), Value=array) /= 0) stop -1
    if(Parameters%isPresent(Key='I4P_1D'//trim(str(no_sign=.true., n=iter)))) then
        write(unit=OUTPUT_UNIT, fmt='(A)') ' Ok!'
    else
        write(unit=OUTPUT_UNIT, fmt= '(A)') ' FAIL!!!!'
        stop -1
    endif
enddo
enddo

write(unit=OUTPUT_UNIT, fmt='(A)') ''
call Parameters%Print(unit=OUTPUT_UNIT)
write(unit=OUTPUT_UNIT, fmt='(A,I4)') ' Parameter List Length: ',Parameters%Length()
write(unit=OUTPUT_UNIT, fmt='(A)') ''

do iter = 1, numiters
    if(Parameters%GetDimensions(Key='I4P_1D'//trim(str(no_sign=.true., n=iter))) /= 1) stop -1
    if(allocated(array)) deallocate(array); allocate(array(iter))
    write(unit=OUTPUT_UNIT, fmt='(A)', advance="no") 'Getting: "'//'I4P_1D'//trim(str(no_sign=.true., n=iter))//'" ... '
    if(.not. Parameters%isAssignable(Key='I4P_1D'//trim(str(no_sign=.true., n=iter)), Value=array)) stop -1
    if(Parameters%Get(Key='I4P_1D'//trim(str(no_sign=.true., n=iter)), Value=array) /= 0) stop -1
    if(all(array == iter)) then
        write(unit=OUTPUT_UNIT, fmt='(A)') ' Ok!'
    else
        write(unit=OUTPUT_UNIT, fmt= '(A)') ' FAIL!!!!'
        stop -1
    endif
enddo

write(unit=OUTPUT_UNIT, fmt='(A)') ''

do iter = numiters, 1, -1
    if(allocated(array)) deallocate(array); allocate(array(iter)); array = iter
    if(Parameters%isPresent(Key='I4P_1D'//trim(str(no_sign=.true., n=iter)))) then
        write(unit=OUTPUT_UNIT, fmt='(A)', advance="no") 'Removing: "'//'I4P_1D'//trim(str(no_sign=.true., n=iter))//'" ... '
        call Parameters%Del(Key='I4P_1D'//trim(str(no_sign=.true., n=iter)))
        if(Parameters%isPresent(Key='I4P_1D'//trim(str(no_sign=.true., n=iter)))) then
            write(unit=OUTPUT_UNIT, fmt= '(A)') ' FAIL!!!!'
            stop -1
        else
            write(unit=OUTPUT_UNIT, fmt='(A)') ' Ok!'
        endif
    endif
enddo

call Parameters%Free()

call FPL_Finalize()

if(allocated(array)) deallocate(array)

end Program
