program WrapperFactoryList_I2P_Test

USE iso_fortran_env, only: OUTPUT_UNIT
USE IR_Precision, only: I2P
USE WrapperFactoryListSingleton
USE WrapperFactory
USE DimensionsWrapper

implicit none

class(WrapperFactory_t),    pointer     :: factory
class(DimensionsWrapper_t), pointer     :: wrapper
integer(I2P)                            :: val0D = 9
integer(I2P)                            :: val1D(1) = 9
integer(I2P)                            :: val2D(1,1) = 9
integer(I2P)                            :: val3D(1,1,1) = 9
integer(I2P)                            :: val4D(1,1,1,1) = 9
integer(I2P)                            :: val5D(1,1,1,1,1) = 9
integer(I2P)                            :: val6D(1,1,1,1,1,1) = 9
integer(I2P)                            :: val7D(1,1,1,1,1,1,1) = 9


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

end program WrapperFactoryList_I2P_Test


