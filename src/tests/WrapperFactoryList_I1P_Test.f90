program WrapperFactoryList_I1P_Test

USE iso_fortran_env, only: OUTPUT_UNIT
USE IR_Precision, only: I1P
USE WrapperFactoryList
USE WrapperFactory
USE DimensionsWrapper

implicit none

type(WrapperFactoryList_t)              :: factorylist
class(WrapperFactory_t),    pointer     :: factory
class(DimensionsWrapper_t), allocatable :: wrapper
integer(I1P)                            :: val0D = 9
integer(I1P)                            :: val1D(1) = 9
integer(I1P)                            :: val2D(1,1) = 9
integer(I1P)                            :: val3D(1,1,1) = 9
integer(I1P)                            :: val4D(1,1,1,1) = 9
integer(I1P)                            :: val5D(1,1,1,1,1) = 9
integer(I1P)                            :: val6D(1,1,1,1,1,1) = 9
integer(I1P)                            :: val7D(1,1,1,1,1,1,1) = 9


call factorylist%Init()
call factorylist%Print(unit=OUTPUT_UNIT)

factory => factorylist%GetFactory(Value=val0D)
if(associated(factory)) call factory%Create(Value=val0D, Wrapper=wrapper)
if(allocated(wrapper)) call Wrapper%Print(unit=OUTPUT_UNIT)
nullify(factory)

factory => factorylist%GetFactory(Value=val1D)
if(associated(factory)) call factory%Create(Value=val1D, Wrapper=wrapper)
if(allocated(wrapper)) call Wrapper%Print(unit=OUTPUT_UNIT)
nullify(factory)

factory => factorylist%GetFactory(Value=val2D)
if(associated(factory)) call factory%Create(Value=val2D, Wrapper=wrapper)
if(allocated(wrapper)) call Wrapper%Print(unit=OUTPUT_UNIT)
nullify(factory)

factory => factorylist%GetFactory(Value=val3D)
if(associated(factory)) call factory%Create(Value=val3D, Wrapper=wrapper)
if(allocated(wrapper)) call Wrapper%Print(unit=OUTPUT_UNIT)
nullify(factory)

factory => factorylist%GetFactory(Value=val4D)
if(associated(factory)) call factory%Create(Value=val4D, Wrapper=wrapper)
if(allocated(wrapper)) call Wrapper%Print(unit=OUTPUT_UNIT)
nullify(factory)

factory => factorylist%GetFactory(Value=val5D)
if(associated(factory)) call factory%Create(Value=val5D, Wrapper=wrapper)
if(allocated(wrapper)) call Wrapper%Print(unit=OUTPUT_UNIT)
nullify(factory)

factory => factorylist%GetFactory(Value=val6D)
if(associated(factory)) call factory%Create(Value=val6D, Wrapper=wrapper)
if(allocated(wrapper)) call Wrapper%Print(unit=OUTPUT_UNIT)
nullify(factory)

factory => factorylist%GetFactory(Value=val7D)
if(associated(factory)) call factory%Create(Value=val7D, Wrapper=wrapper)
if(allocated(wrapper)) call Wrapper%Print(unit=OUTPUT_UNIT)
nullify(factory)

call wrapper%Free()
call factorylist%Free()
nullify(factory)
if(allocated(wrapper)) deallocate(wrapper)


end program WrapperFactoryList_I1P_Test


