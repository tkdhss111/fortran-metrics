set ( EXE unit_test1 )
set ( SRCS ./unit_test1.f90
           ../src/metrics_mo.f90
    )

set ( CMAKE_Fortran_FLAGS
  "-ffree-line-length-none -fdec-structure -fcoarray=single")
set ( CMAKE_Fortran_FLAGS_DEBUG
  "-Ddebug -O0 -fbounds-check -fbacktrace -Wall -Wno-uninitialized -Wno-maybe-uninitialized" )
set ( CMAKE_Fortran_FLAGS_RELEASE "-Drelease -O3" )
set ( CMAKE_C_FLAGS "-O -Wno-stringop-overflow")

add_executable ( ${EXE} ${SRCS} )
add_test ( NAME ${EXE} COMMAND ${EXE} ${ARGS} WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR} )
