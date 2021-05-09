Program FortranParameterList_Example

!-----------------------------------------------------------------
!< Example based on Teuchos::ParameterList documentation
!-----------------------------------------------------------------

USE FPL
USE iso_fortran_env, only: REAL64, OUTPUT_UNIT

type(ParameterList_t)                  :: My_List
type(ParameterListIterator_t)          :: My_List_Iterator
type(ParameterList_t), pointer         :: Prec_List
type(ParameterListIterator_t)          :: Prec_List_Iterator
integer                                :: FPLError
logical                                :: solver_defined
logical                                :: prec_defined
logical                                :: has_same_type
real(REAL64)                           :: Tolerance
character(len=:), allocatable          :: String

call FPL_Init()

call My_List%Init()

!< Setting parameters in My_List
FPLError = My_List%Set(Key='Max Iters', Value=1500)
FPLError = My_List%Set(Key='Tolerance', Value=1.e-10_REAL64)
FPLError = My_List%Set(Key='Solver', Value='GMRES')

!< Create a 'Preconditioner' SubList of parameters
Prec_List => My_List%NewSubList(Key='Preconditioner')

!< Setting parameters in Prec_List
FPLError = Prec_List%Set(Key='Type', Value='ILU')
FPLError = Prec_List%Set(Key='Drop Tolerance', Value=1.e-3_REAL64)

!< Has a solver been chosen
solver_defined = My_List%isPresent(Key='Solver')

!< Has a preconditioner been chosen
prec_defined = My_List%isSubList(Key='Preconditioner')

!< Has tolerance been chosen and is of the expected data type
has_same_type = My_List%isOfDataType(Key='Tolerance', Mold=Tolerance)

!< Get method thtat retrieves a parameter of a particular type
FPLError = My_List%Get(Key='Tolerance', Value=Tolerance)

write(unit=OUTPUT_UNIT, fmt='(A)') ' -----------'
write(unit=OUTPUT_UNIT, fmt='(A)') ' | My_List |'
write(unit=OUTPUT_UNIT, fmt='(A)') ' -----------'
call My_List%Print(unit=OUTPUT_UNIT)
write(unit=OUTPUT_UNIT, fmt='(A)') ' '
write(unit=OUTPUT_UNIT, fmt='(A)') ' -------------'
write(unit=OUTPUT_UNIT, fmt='(A)') ' | Prec_List |'
write(unit=OUTPUT_UNIT, fmt='(A)') ' -------------'
call Prec_List%Print(unit=OUTPUT_UNIT)


write(unit=OUTPUT_UNIT, fmt='(A)') ' '
write(unit=OUTPUT_UNIT, fmt='(A)') ' -------------'
write(unit=OUTPUT_UNIT, fmt='(A)') ' | Iterators |'
write(unit=OUTPUT_UNIT, fmt='(A)') ' -------------'
nullify(Prec_List)
String = ''
My_List_Iterator = My_List%GetIterator()
do while (.not. My_List_Iterator%HasFinished())
    write(unit=OUTPUT_UNIT, fmt='(A)') 'Iterating over: "'//My_List_Iterator%GetKey()//'" ... '
    if(My_List_Iterator%isSubList()) then
        FPLError = My_List_Iterator%GetSubList(Prec_List)
        Prec_List_Iterator = Prec_List%GetIterator()
        do while (.not. Prec_List_Iterator%HasFinished())
            write(unit=OUTPUT_UNIT, fmt='(A)') '   Iterating over: "'//Prec_List_Iterator%GetKey()//'" ... '
            if(.not. Prec_List_Iterator%isSubList()) then
                call Prec_List_Iterator%Print(prefix='     ')
                 String = String // Prec_List_Iterator%GetKey() // '=' // Prec_List_Iterator%toString(Separator=' ') // '; '
            endif
            call Prec_List_Iterator%Next()
        enddo
    else
        String = String // My_List_Iterator%GetKey() // '=' // My_List_Iterator%toString(Separator=' ') // '; '
        call My_List_Iterator%Print(prefix='  ')
    endif
    call My_List_Iterator%Next()
enddo

write(unit=OUTPUT_UNIT, fmt='(A)') ' '
write(unit=OUTPUT_UNIT, fmt='(A)') ' -----------------'
write(unit=OUTPUT_UNIT, fmt='(A)') ' | All in a line |'
write(unit=OUTPUT_UNIT, fmt='(A)') ' -----------------'
print*, String

call My_List%Free()
call My_List_Iterator%Free()
call Prec_List_Iterator%Free()

call FPL_Finalize()

end program
