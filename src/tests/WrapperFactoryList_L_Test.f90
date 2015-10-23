program WrapperFactoryList_Test

USE iso_fortran_env, only: OUTPUT_UNIT
USE WrapperFactoryListSingleton
USE WrapperFactory
USE DimensionsWrapper

implicit none

class(WrapperFactory_t),    pointer     :: factory
class(DimensionsWrapper_t), allocatable :: wrapper
logical                                 :: val0D = .true.
logical                                 :: val1D(1) = .true.
logical                                 :: val2D(1,1) = .true.
logical                                 :: val3D(1,1,1) = .true.
logical                                 :: val4D(1,1,1,1) = .true.
logical                                 :: val5D(1,1,1,1,1) = .true.
logical                                 :: val6D(1,1,1,1,1,1) = .true.
logical                                 :: val7D(1,1,1,1,1,1,1) = .true.


call TheWrapperFactoryList_Init()
call TheWrapperFactoryList%Print(unit=OUTPUT_UNIT)

factory => TheWrapperFactoryList%GetFactory(Value=val0D)
if(associated(factory)) call factory%Wrap(Value=val0D, Wrapper=wrapper)
if(allocated(wrapper)) call Wrapper%Print(unit=OUTPUT_UNIT)
nullify(factory)

factory => TheWrapperFactoryList%GetFactory(Value=val1D)
if(associated(factory)) call factory%Wrap(Value=val1D, Wrapper=wrapper)
if(allocated(wrapper)) call Wrapper%Print(unit=OUTPUT_UNIT)
nullify(factory)

factory => TheWrapperFactoryList%GetFactory(Value=val2D)
if(associated(factory)) call factory%Wrap(Value=val2D, Wrapper=wrapper)
if(allocated(wrapper)) call Wrapper%Print(unit=OUTPUT_UNIT)
nullify(factory)

factory => TheWrapperFactoryList%GetFactory(Value=val3D)
if(associated(factory)) call factory%Wrap(Value=val3D, Wrapper=wrapper)
if(allocated(wrapper)) call Wrapper%Print(unit=OUTPUT_UNIT)
nullify(factory)

factory => TheWrapperFactoryList%GetFactory(Value=val4D)
if(associated(factory)) call factory%Wrap(Value=val4D, Wrapper=wrapper)
if(allocated(wrapper)) call Wrapper%Print(unit=OUTPUT_UNIT)
nullify(factory)

factory => TheWrapperFactoryList%GetFactory(Value=val5D)
if(associated(factory)) call factory%Wrap(Value=val5D, Wrapper=wrapper)
if(allocated(wrapper)) call Wrapper%Print(unit=OUTPUT_UNIT)
nullify(factory)

factory => TheWrapperFactoryList%GetFactory(Value=val6D)
if(associated(factory)) call factory%Wrap(Value=val6D, Wrapper=wrapper)
if(allocated(wrapper)) call Wrapper%Print(unit=OUTPUT_UNIT)
nullify(factory)

factory => TheWrapperFactoryList%GetFactory(Value=val7D)
if(associated(factory)) call factory%Wrap(Value=val7D, Wrapper=wrapper)
if(allocated(wrapper)) call Wrapper%Print(unit=OUTPUT_UNIT)
nullify(factory)

call wrapper%Free()
call TheWrapperFactoryList%Free()
nullify(factory)
if(allocated(wrapper)) deallocate(wrapper)


end program WrapperFactoryList_Test


