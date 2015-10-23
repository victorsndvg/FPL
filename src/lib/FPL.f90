module FPL

USE IR_Precision, only: I4P
USE ParameterListEntryContainer
USE WrapperFactoryListSingleton

public :: ParameterListEntryContainer_t

contains

    subroutine FPL_Init()
    !-----------------------------------------------------------------
    !< Initialize FPL
    !-----------------------------------------------------------------
        call TheWrapperFactoryList_Init()
    end subroutine FPL_Init


    subroutine FPL_Finalize()
    !-----------------------------------------------------------------
    !< Finalize FPL
    !-----------------------------------------------------------------
        call TheWrapperFactoryList%Free()
    end subroutine FPL_Finalize

end module FPL
