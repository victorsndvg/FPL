program LinkedList_Test

USE iso_fortran_env, only: OUTPUT_UNIT
USE LinkedList

implicit none

type(LinkedList_t) :: linkedlist

write(*,*) 'Adding Key1, Key2 and Key3...'
call linkedlist%AddNode(Key="Key1")
call linkedlist%AddNode(Key="Key2")
call linkedlist%AddNode(Key="Key3")
call linkedlist%Print(unit=OUTPUT_UNIT)
write(*,*) 'List length:', linkedlist%GetLength()

write(*,*) ''; write(*,*) 'Removing Key2...'
call linkedlist%RemoveNode(Key="Key2")
call linkedlist%Print(unit=OUTPUT_UNIT)
write(*,*) 'List length:', linkedlist%GetLength()

write(*,*) ''; write(*,*) 'Adding duplicated Key1...'
call linkedlist%AddNode(Key="Key1")
call linkedlist%Print(unit=OUTPUT_UNIT)
write(*,*) 'List length:', linkedlist%GetLength()

write(*,*) ''; write(*,*) 'Adding Key1, Key2, Key3 and Key4...'
call linkedlist%AddNode(Key="Key1")
call linkedlist%AddNode(Key="Key2")
call linkedlist%AddNode(Key="Key3")
call linkedlist%AddNode(Key="Key4")
call linkedlist%Print(unit=OUTPUT_UNIT)
write(*,*) 'List length:', linkedlist%GetLength()

write(*,*) ''; write(*,*) 'is Present Key2:', linkedlist%isPresent(Key='Key2')
write(*,*) ''; write(*,*) 'is Present Key5:', linkedlist%isPresent(Key='Key5')

call linkedlist%Free()


end program LinkedList_Test
