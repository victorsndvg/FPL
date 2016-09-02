program main

USE FPL
USE Circle
USE CircleWrapperFactory
USE iso_fortran_env, only: OUTPUT_UNIT

type(Circle_t)                 :: MyCircle
type(ParameterList_t)          :: MyList
type(ParameterList_t), pointer :: CircleList
integer                        :: FPLError
character(len=:), allocatable  :: String

!< Initialize FPL with the default WrapperFactories
call FPL_Init()

!< Add the new WrapperFactory to the list of factories
call TheWrapperFactoryList%AddWrapperFactory(Key='CircleFactory', WrapperFactory=WrapperFactoryCircle) 

!< Sets the default size of the Dictionary
call myList%Init()

!< Add parameters to the list
FPLError   =  MyList%Set(Key='NumberOfCircles',Value=5)
FPLError   =  MyList%GetAsString(Key='NumberOfCircles',String=String)
print*, 'NumberOfCircles = '//String

!< Add a SubList to the list
CircleList => MyList%NewSubList(Key='Circles')

!< Add parameters to the Cicles SubList
call myCircle%SetRadius(Radius=1.0); FPLError = CircleList%Set(Key='Circle_1',Value=myCircle)
call myCircle%SetRadius(Radius=2.0); FPLError = CircleList%Set(Key='Circle_2',Value=myCircle)
call myCircle%SetRadius(Radius=3.0); FPLError = CircleList%Set(Key='Circle_3',Value=myCircle)
call myCircle%SetRadius(Radius=4.0); FPLError = CircleList%Set(Key='Circle_4',Value=myCircle)
call myCircle%SetRadius(Radius=5.0); FPLError = CircleList%Set(Key='Circle_5',Value=myCircle)

!< Print the content of MyList (Recursive)
call MyList%Print()

!< Free MyList
call MyList%Free()

!< Finalize FPL and free TheWrapperFactoryList
call FPL_Finalize()

end program
