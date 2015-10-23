Program ParameterListEntryContainer_Test

USE iso_fortran_env, only: OUTPUT_UNIT
USE IR_Precision, only: I4P, R4P, str
USE FPL

type(ParameterListEntryContainer_t) :: Parameters
integer(I4P),allocatable :: array(:)
class(*), allocatable :: UParray(:)
integer :: iter, numiters

numiters = 7

call FPL_Init()

call Parameters%Init(Size=3)

do iter = 1, numiters
    if(allocated(array)) deallocate(array); allocate(array(iter)); array = iter
    write(unit=OUTPUT_UNIT, fmt='(A)') 'Setting: "'//'I4P_1D'//trim(str(no_sign=.true., n=iter))//'" ... Ok!'
    call Parameters%Set(Key='I4P_1D'//trim(str(no_sign=.true., n=iter)), Value=array)
enddo

write(unit=OUTPUT_UNIT, fmt='(A)') ''
call Parameters%Print(unit=OUTPUT_UNIT)
write(unit=OUTPUT_UNIT, fmt='(A)') ''

do iter = 1, numiters
    if(allocated(array)) deallocate(array); allocate(array(iter))
    write(unit=OUTPUT_UNIT, fmt='(A,$)') 'Getting: "'//'I4P_1D'//trim(str(no_sign=.true., n=iter))//'" ... '
    call Parameters%Get(Key='I4P_1D'//trim(str(no_sign=.true., n=iter)), Value=array)
    if(all(array == iter)) then
        write(unit=OUTPUT_UNIT, fmt='(A,$)') ' Ok!'
    else
        write(unit=OUTPUT_UNIT, fmt= '(A,$)') ' FAIL!!!!'
        stop -1
    endif

    write(unit=OUTPUT_UNIT, fmt='(A,$)') ' - Alloc Unlimited Polymorphic: '
    call Parameters%GetPolymorphic(Key='I4P_1D'//trim(str(no_sign=.true., n=iter)), Value=UParray)
    if(size(UParray, dim=1) == iter) then
        Select type (UParray)
            type is (Integer(I4P))
                write(unit=OUTPUT_UNIT, fmt='(A)') ' Ok!'
            class Default
                write(unit=OUTPUT_UNIT, fmt= '(A)') ' FAIL!!!!'
                stop -1
        end select
    else
        write(unit=OUTPUT_UNIT, fmt= '(A)') ' FAIL!!!!'
        stop -1
    endif
enddo

write(unit=OUTPUT_UNIT, fmt='(A)') ''

do iter = numiters, 1, -1
    if(allocated(array)) deallocate(array); allocate(array(iter)); array = iter
    if(Parameters%isPresent(Key='I4P_1D'//trim(str(no_sign=.true., n=iter)))) then
        write(unit=OUTPUT_UNIT, fmt='(A,$)') 'Removing: "'//'I4P_1D'//trim(str(no_sign=.true., n=iter))//'" ... '
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
