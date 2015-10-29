program WrapperTheWrapperFactoryList_Test

USE iso_fortran_env, only: OUTPUT_UNIT
USE IR_Precision, only: I4P
USE WrapperFactoryListSingleton
USE WrapperFactory
USE DimensionsWrapper

implicit none

class(WrapperFactory_t),    pointer :: factory
class(DimensionsWrapper_t), pointer :: wrapper
character(len=1)                    :: val0D = 'A'
character(len=1)                    :: val1D(1) = 'A'
character(len=1)                    :: val2D(1,1) = 'A'
character(len=1)                    :: val3D(1,1,1) = 'A'
character(len=1)                    :: val4D(1,1,1,1) = 'A'
character(len=1)                    :: val5D(1,1,1,1,1) = 'A'
character(len=1)                    :: val6D(1,1,1,1,1,1) = 'A'
character(len=1)                    :: val7D(1,1,1,1,1,1,1) = 'A'


call TheWrapperFactoryList_Init()
call TheWrapperFactoryList%Print(unit=OUTPUT_UNIT)

factory => TheWrapperFactoryList%GetFactory(Value=val0D)
if(associated(factory)) wrapper => factory%Wrap(Value=val0D)
if(associated(wrapper)) call Wrapper%Print(unit=OUTPUT_UNIT)
nullify(factory); call wrapper%Free(); deallocate(wrapper)

factory => TheWrapperFactoryList%GetFactory(Value=val1D)
if(associated(factory)) wrapper => factory%Wrap(Value=val0D)
if(associated(wrapper)) call Wrapper%Print(unit=OUTPUT_UNIT)
nullify(factory); call wrapper%Free(); deallocate(wrapper)

factory => TheWrapperFactoryList%GetFactory(Value=val2D)
if(associated(factory)) wrapper => factory%Wrap(Value=val0D)
if(associated(wrapper)) call Wrapper%Print(unit=OUTPUT_UNIT)
nullify(factory); call wrapper%Free(); deallocate(wrapper)

factory => TheWrapperFactoryList%GetFactory(Value=val3D)
if(associated(factory)) wrapper => factory%Wrap(Value=val0D)
if(associated(wrapper)) call Wrapper%Print(unit=OUTPUT_UNIT)
nullify(factory); call wrapper%Free(); deallocate(wrapper)

factory => TheWrapperFactoryList%GetFactory(Value=val4D)
if(associated(factory)) wrapper => factory%Wrap(Value=val0D)
if(associated(wrapper)) call Wrapper%Print(unit=OUTPUT_UNIT)
nullify(factory); call wrapper%Free(); deallocate(wrapper)

factory => TheWrapperFactoryList%GetFactory(Value=val5D)
if(associated(factory)) wrapper => factory%Wrap(Value=val0D)
if(associated(wrapper)) call Wrapper%Print(unit=OUTPUT_UNIT)
nullify(factory); call wrapper%Free(); deallocate(wrapper)

factory => TheWrapperFactoryList%GetFactory(Value=val6D)
if(associated(factory)) wrapper => factory%Wrap(Value=val0D)
if(associated(wrapper)) call Wrapper%Print(unit=OUTPUT_UNIT)
nullify(factory); call wrapper%Free(); deallocate(wrapper)

factory => TheWrapperFactoryList%GetFactory(Value=val7D)
if(associated(factory)) wrapper => factory%Wrap(Value=val0D)
if(associated(wrapper)) call Wrapper%Print(unit=OUTPUT_UNIT)
nullify(factory); call wrapper%Free(); deallocate(wrapper)

call TheWrapperFactoryList%Free()

end program WrapperTheWrapperFactoryList_Test


