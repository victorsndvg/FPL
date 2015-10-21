Program ParameterListEntryContainer_Test

USE ParameterListEntryContainer

type(ParameterListEntryContainer_t) :: Parameters
integer :: i = -1
real, allocatable :: r(:)

allocate(r(2))

call Parameters%Init()
call Parameters%Set(Key='Integer_scalar', Value=1)
call Parameters%Set(Key='Real_1D_array', Value=(/1.0,-2.0/))
call Parameters%Set(Key='Logical', Value=.true.)
call Parameters%Set(Key='Character', Value='Parameter')
print*, '----------------------------------------------'
!call Parameters%Get(Key='Integer_scalar', Value=i)
!print*, i
call Parameters%Get(Key='Real_1D_array', Value=r)
print*, r

end Program
