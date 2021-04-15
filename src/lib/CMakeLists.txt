#################################################################
# Search F90 files recursively in all subdirs
#################################################################

SET(SRC_PATH "${CMAKE_CURRENT_LIST_DIR}")
FILE(GLOB_RECURSE WRAPPER_LIB_SRC  ${SRC_PATH}/Wrapper/*.f90)
FILE(GLOB_RECURSE PENF_LIB_SRC  ${SRC_PATH}/PENF/src/lib/*.F90)
LIST(APPEND LIB_SRC ${WRAPPER_LIB_SRC} ${PENF_LIB_SRC})

TARGET_SOURCES(
    ${PROJECT_NAME} PRIVATE
    ${LIB_SRC}
    ${SRC_PATH}/ErrorMessages.f90
    ${SRC_PATH}/FPL_utils.f90
    ${SRC_PATH}/FPL.f90
    ${SRC_PATH}/ParameterEntry.f90
    ${SRC_PATH}/ParameterEntryDictionary.f90
    ${SRC_PATH}/ParameterList.f90
    ${SRC_PATH}/ParameterRootEntry.f90
    )