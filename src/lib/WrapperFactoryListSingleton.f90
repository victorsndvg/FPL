module WrapperFactoryListSingleton

USE WrapperFactoryList
USE DLCAWrapperFactory
USE I1PWrapperFactory
USE I2PWrapperFactory
USE I4PWrapperFactory
USE I8PWrapperFactory
USE LWrapperFactory
USE R4PWrapperFactory
USE R8PWrapperFactory
!USE UPWrapperFactory

implicit none
private

    type(WrapperFactoryList_t) :: TheWrapperFactoryList

public :: TheWrapperFactoryList
public :: TheWrapperFactoryList_Init

contains

    subroutine TheWrapperFactoryList_Init()
    !-----------------------------------------------------------------
    !< Set the dimensions of the Value contained in the wrapper
    !-----------------------------------------------------------------
        ! Add some Wrapper Factories to the list
        call TheWrapperFactoryList%AddNode(key='I1P',  WrapperFactory=WrapperFactoryI1P)
        call TheWrapperFactoryList%AddNode(key='I2P',  WrapperFactory=WrapperFactoryI2P)
        call TheWrapperFactoryList%AddNode(key='I4P',  WrapperFactory=WrapperFactoryI4P)
        call TheWrapperFactoryList%AddNode(key='I8P',  WrapperFactory=WrapperFactoryI8P)
        call TheWrapperFactoryList%AddNode(key='R4P',  WrapperFactory=WrapperFactoryR4P)
        call TheWrapperFactoryList%AddNode(key='R8P',  WrapperFactory=WrapperFactoryR8P)
        call TheWrapperFactoryList%AddNode(key='L',    WrapperFactory=WrapperFactoryL)
        call TheWrapperFactoryList%AddNode(key='DLCA', WrapperFactory=WrapperFactoryDLCA)
        ! Add Unlimited Polymorphic WrapperFactory as default
!        call TheWrapperFactoryList%SetDefaultFactory(DefaultFactory=WrapperFactoryUP)
    end subroutine TheWrapperFactoryList_Init

end module WrapperFactoryListSingleton
