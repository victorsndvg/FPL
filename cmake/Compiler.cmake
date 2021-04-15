MESSAGE(STATUS
"Is the Fortran compiler loaded? ${CMAKE_Fortran_COMPILER_LOADED}")

IF(CMAKE_Fortran_COMPILER_LOADED)
  MESSAGE(STATUS "Fortran compiler ID = ${CMAKE_Fortran_COMPILER_ID}")
  MESSAGE(STATUS
    "The Fortran compiler version is: ${CMAKE_Fortran_COMPILER_VERSION}")
ENDIF()

IF(NOT CMAKE_BUILD_TYPE)
  SET(CMAKE_BUILD_TYPE Release CACHE STRING "Build type" FORCE)
ENDIF()

IF (${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU" OR Fortran_COMPILER_NAME MATCHES "gfortran*")
  LIST(APPEND FORTRAN_FLAGS "-ffree-form" "-ffree-line-length-none" "-cpp" "-std=f2008" "-fimplicit-none" "-Werror" )
  LIST(APPEND FORTRAN_FLAGS_RELEASE "-O3" )
  LIST(APPEND FORTRAN_FLAGS_DEBUG "-fbounds-check" "-g" "-fbacktrace" "-Wextra" "-Wall" "-fprofile-arcs" "-ftest-coverage" "-Wimplicit-interface" )

ELSEIF(${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel" OR Fortran_COMPILER_NAME MATCHES "ifort*")
  LIST(APPEND FORTRAN_FLAGS "-r8" "-fpp" "-W1")
  LIST(APPEND FORTRAN_FLAGS_RELEASE "-O3")
  LIST(APPEND FORTRAN_FLAGS_DEBUG "-O0" "-traceback" "-g" "-debug all" "-check all" "-ftrapuv" "-warn" "nointerfaces")

ELSEIF (${CMAKE_Fortran_COMPILER_ID} STREQUAL "XL" OR Fortran_COMPILER_NAME MATCHES "xlf*")
    LIST(APPEND FORTRAN_FLAGS "-q64" "-qrealsize=8" "-qsuffix=f=f90:cpp=f90")
    LIST(APPEND FORTRAN_FLAGS_RELEASE "-O3" "-qstrict")
    LIST(APPEND FORTRAN_FLAGS_DEBUG "-O0" "-g" "-qfullpath" "-qkeepparm")
ELSE ()
    MESSAGE(ERROR "No optimized Fortran compiler flags are known")
ENDIF ()

CMAKE_PRINT_VARIABLES(FORTRAN_FLAGS)
CMAKE_PRINT_VARIABLES(FORTRAN_FLAGS_RELEASE)
CMAKE_PRINT_VARIABLES(FORTRAN_FLAGS_DEBUG)