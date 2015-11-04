Program FortranParameterList_Example

!-----------------------------------------------------------------
!< Example based on Teuchos::ParameterList documentation
!-----------------------------------------------------------------

USE FPL
USE iso_fortran_env, only: REAL64, OUTPUT_UNIT

type(ParameterList_t)          :: My_List
type(ParameterList_t), pointer :: Prec_List
integer                        :: FPLError
logical                        :: solver_defined
logical                        :: prec_defined
logical                        :: has_same_type
real(REAL64)                   :: Tolerance

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

call My_List%Free()

call FPL_Finalize()

end program
