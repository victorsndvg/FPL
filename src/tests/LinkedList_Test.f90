program LinkedList_Test

USE iso_fortran_env, only: OUTPUT_UNIT
USE LinkedList

implicit none

type(LinkedList_t) :: list

write(*,*) 'Adding Key1, Key2 and Key3...'
call list%AddNode(Key="Key1")
call list%AddNode(Key="Key2")
call list%AddNode(Key="Key3")
call list%Print(unit=OUTPUT_UNIT)
write(*,*) 'List length:', list%GetLength()

write(*,*) ''; write(*,*) 'Removing Key2...'
call list%RemoveNode(Key="Key2")
call list%Print(unit=OUTPUT_UNIT)
write(*,*) 'List length:', list%GetLength()

write(*,*) ''; write(*,*) 'Adding duplicated Key1...'
call list%AddNode(Key="Key1")
call list%Print(unit=OUTPUT_UNIT)
write(*,*) 'List length:', list%GetLength()

write(*,*) ''; write(*,*) 'Adding Key1, Key2, Key3 and Key4...'
call list%AddNode(Key="Key1")
call list%AddNode(Key="Key2")
call list%AddNode(Key="Key3")
call list%AddNode(Key="Key4")
call list%Print(unit=OUTPUT_UNIT)
write(*,*) 'List length:', list%GetLength()

write(*,*) ''; write(*,*) 'is Present Key2:', list%isPresent(Key='Key2')
write(*,*) ''; write(*,*) 'is Present Key5:', list%isPresent(Key='Key5')

call list%Free()


end program LinkedList_Test
