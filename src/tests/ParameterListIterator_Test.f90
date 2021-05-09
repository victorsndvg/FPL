Program ParameterListEntryContainer_Test

USE iso_fortran_env, only: OUTPUT_UNIT
USE PENF, only: I4P, R4P, str
USE FPL

type(ParameterList_t)         :: Parameters
type(ParameterListIterator_t) :: Iterator
integer(I4P),     allocatable :: array(:)
integer(I4P),     allocatable :: shape(:)
character(len=:), allocatable :: string
integer :: iter, numiters, loop

numiters = 7

call FPL_Init()

call Parameters%Init(Size=3)

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

Iterator = Parameters%GetIterator()
do while (.not. Iterator%HasFinished())
    if(Iterator%GetDimensions() /= 1) stop -1
    if(Iterator%GetShape(Shape=shape) /= 0) stop -1
    if(.not. Iterator%IsOfDataType(Mold=array)) stop -1
    write(unit=OUTPUT_UNIT, fmt='(A)') 'Iterating over: "'//'I4P_1D'//trim(str(no_sign=.true., n=shape(1)))//'" ... '
    if(allocated(array)) deallocate(array)
    allocate(array(shape(1)))
    if(Iterator%isAssignable(Value=array)) then
        FPLError = Iterator%Get(Value=array)
        if(FPLError == 0) then
            write(unit=OUTPUT_UNIT, fmt='(A)') ' Ok!'
        else
            write(unit=OUTPUT_UNIT, fmt= '(A)') ' FAIL!!!!'
            stop -1
        endif
    else
        write(unit=OUTPUT_UNIT, fmt= '(A)') ' FAIL!!!!'
        stop -1
    endif
    FPLError = Iterator%GetAsString(String, Separator=' ')
    if(FPLError /= 0) stop -1
    print*, '  Key = '//Iterator%GetKey()
    print*, '  Bytes = '//trim(str(n=Iterator%DataSizeInBytes()))
    print*, '  Dimensions = '//trim(str(n=Iterator%GetDimensions()))
    print*, '  Value = '//String
    print*, '  Shape = '//trim(str(n=shape))
    if(all(array == shape(1))) then
        write(unit=OUTPUT_UNIT, fmt='(A)') 'Ok!'
    else
        write(unit=OUTPUT_UNIT, fmt= '(A)') 'FAIL!!!!'
        stop -1
    endif
    call Iterator%Next()
enddo

write(unit=OUTPUT_UNIT, fmt='(A)') ''

do iter = numiters, 1, -1
    if(allocated(array)) deallocate(array)
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
call Iterator%Free()

call FPL_Finalize()

if(allocated(array)) deallocate(array)

end Program
