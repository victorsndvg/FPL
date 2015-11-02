Program ParameterListEntryContainer_Test

USE iso_fortran_env, only: OUTPUT_UNIT
USE IR_Precision, only: I1P, I2P, I4P, I8P, R4P, R8P, str
USE FPL

type(ParameterList_t) :: Parameters
integer(I1P),     allocatable :: I1PArray(:,:,:,:)
integer(I2P),     allocatable :: I2PArray(:,:,:,:)
integer(I4P),     allocatable :: I4PArray(:,:,:,:)
integer(I8P),     allocatable :: I8PArray(:,:,:,:)
real(R4P),        allocatable :: R4PArray(:,:,:,:)
real(R8P),        allocatable :: R8PArray(:,:,:,:)
logical,          allocatable :: LArray(:,:,:,:)
character(len=:), allocatable :: DLCAarray(:,:,:,:)


if(allocated(I1Parray))  deallocate(I1Parray);  allocate(I1Parray(2,1,1,1));  I1Parray  = 1
if(allocated(I2Parray))  deallocate(I2Parray);  allocate(I2Parray(1,2,1,1));  I2Parray  = 2
if(allocated(I4Parray))  deallocate(I4Parray);  allocate(I4Parray(1,1,2,1));  I4Parray  = 4
if(allocated(I8Parray))  deallocate(I8Parray);  allocate(I8Parray(1,1,1,2));  I8Parray  = 8
if(allocated(R4Parray))  deallocate(R4Parray);  allocate(R4Parray(1,1,2,1));  R4Parray  = 0.4
if(allocated(R8Parray))  deallocate(R8Parray);  allocate(R8Parray(1,2,1,1));  R8Parray  = 0.8
if(allocated(Larray))    deallocate(Larray);    allocate(Larray(2,1,1,1));    Larray    = .true.
if(allocated(DLCAarray)) deallocate(DLCAarray); allocate(character(len=6):: DLCAarray(1,2,1,1)); DLCAarray = 'String'

call FPL_Init()

call Parameters%Init(Size=3)


write(unit=OUTPUT_UNIT, fmt='(A)') 'Setting Values ...'

call Parameters%Set(Key='I1P',  Value=I1PArray)
call Parameters%Set(Key='I2P',  Value=I2PArray)
call Parameters%Set(Key='I4P',  Value=I4PArray)
call Parameters%Set(Key='I8P',  Value=I8PArray)
call Parameters%Set(Key='R4P',  Value=R4PArray)
call Parameters%Set(Key='R8P',  Value=R8PArray)
call Parameters%Set(Key='L',    Value=LArray)
call Parameters%Set(Key='DLCA', Value=DLCAArray)

if(.not. Parameters%isPresent(Key='I1P'))  Stop -1
if(.not. Parameters%isPresent(Key='I2P'))  Stop -1
if(.not. Parameters%isPresent(Key='I4P'))  Stop -1
if(.not. Parameters%isPresent(Key='I8P'))  Stop -1
if(.not. Parameters%isPresent(Key='R4P'))  Stop -1
if(.not. Parameters%isPresent(Key='R8P'))  Stop -1
if(.not. Parameters%isPresent(Key='L'))    Stop -1
if(.not. Parameters%isPresent(Key='DLCA')) Stop -1

write(unit=OUTPUT_UNIT, fmt='(A)') ''
write(unit=OUTPUT_UNIT, fmt='(A,I4)') 'Parameter List Length: ',Parameters%Length()
call Parameters%Print(unit=OUTPUT_UNIT)

write(unit=OUTPUT_UNIT, fmt='(A)') ''
write(unit=OUTPUT_UNIT, fmt='(A)') 'Checking Data Types ...'

write(unit=OUTPUT_UNIT, fmt=*) 'I1P isOfDataType:',  Parameters%isOfDataType(Key='I1P',   Mold=I1PArray)
write(unit=OUTPUT_UNIT, fmt=*) 'I2P isOfDataType:',  Parameters%isOfDataType(Key='I2P',   Mold=I2PArray)
write(unit=OUTPUT_UNIT, fmt=*) 'I4P isOfDataType:',  Parameters%isOfDataType(Key='I4P',   Mold=I4PArray)
write(unit=OUTPUT_UNIT, fmt=*) 'I8P isOfDataType:',  Parameters%isOfDataType(Key='I8P',   Mold=I8PArray)
write(unit=OUTPUT_UNIT, fmt=*) 'R4P isOfDataType:',  Parameters%isOfDataType(Key='R4P',   Mold=R4PArray)
write(unit=OUTPUT_UNIT, fmt=*) 'R8P isOfDataType:',  Parameters%isOfDataType(Key='R8P',   Mold=R8PArray)
write(unit=OUTPUT_UNIT, fmt=*) 'L isOfDataType:',    Parameters%isOfDataType(Key='L',     Mold=LArray)
write(unit=OUTPUT_UNIT, fmt=*) 'DLCA isOfDataType:', Parameters%isOfDataType(Key='DLCA',  Mold=DLCAArray)

write(unit=OUTPUT_UNIT, fmt='(A)') ''
write(unit=OUTPUT_UNIT, fmt='(A)') 'Checking shapes ...'

write(unit=OUTPUT_UNIT, fmt='(A,4I4)') 'I1P Shape:',  Parameters%GetShape(Key='I1P')
write(unit=OUTPUT_UNIT, fmt='(A,4I4)') 'I2P Shape:',  Parameters%GetShape(Key='I2P')
write(unit=OUTPUT_UNIT, fmt='(A,4I4)') 'I4P Shape:',  Parameters%GetShape(Key='I4P')
write(unit=OUTPUT_UNIT, fmt='(A,4I4)') 'I8P Shape:',  Parameters%GetShape(Key='I8P')
write(unit=OUTPUT_UNIT, fmt='(A,4I4)') 'R4P Shape:',  Parameters%GetShape(Key='R4P')
write(unit=OUTPUT_UNIT, fmt='(A,4I4)') 'R8P Shape:',  Parameters%GetShape(Key='R8P')
write(unit=OUTPUT_UNIT, fmt='(A,4I4)') 'L Shape:',    Parameters%GetShape(Key='L')
write(unit=OUTPUT_UNIT, fmt='(A,4I4)') 'DLCA Shape:', Parameters%GetShape(Key='DLCA')

write(unit=OUTPUT_UNIT, fmt='(A)') ''
write(unit=OUTPUT_UNIT, fmt='(A)') 'Getting Values ...'

call Parameters%Get(Key='I1P',  Value=I1PArray)
call Parameters%Get(Key='I2P',  Value=I2PArray)
call Parameters%Get(Key='I4P',  Value=I4PArray)
call Parameters%Get(Key='I8P',  Value=I8PArray)
call Parameters%Get(Key='R4P',  Value=R4PArray)
call Parameters%Get(Key='R8P',  Value=R8PArray)
call Parameters%Get(Key='L',    Value=LArray)
call Parameters%Get(Key='DLCA', Value=DLCAArray)

write(unit=OUTPUT_UNIT, fmt='(A)') ''
write(unit=OUTPUT_UNIT, fmt='(A)') 'Deleting entries ...'

call Parameters%Del(Key='I1P')
call Parameters%Del(Key='I2P')
call Parameters%Del(Key='I4P')
call Parameters%Del(Key='I8P')
call Parameters%Del(Key='R4P')
call Parameters%Del(Key='R8P')
call Parameters%Del(Key='L')
call Parameters%Del(Key='DLCA')

write(unit=OUTPUT_UNIT, fmt='(A)') ''

call Parameters%Free()

call FPL_Finalize()

if(allocated(I1Parray))  deallocate(I1Parray)
if(allocated(I2Parray))  deallocate(I2Parray)
if(allocated(I4Parray))  deallocate(I4Parray)
if(allocated(I8Parray))  deallocate(I8Parray)
if(allocated(R4Parray))  deallocate(R4Parray)
if(allocated(R8Parray))  deallocate(R8Parray)
if(allocated(Larray))    deallocate(Larray)
if(allocated(DLCAarray)) deallocate(DLCAarray)


end Program
