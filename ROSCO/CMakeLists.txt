cmake_minimum_required(VERSION 3.6)
project(ROSCO VERSION 2.3.0 LANGUAGES Fortran)

set(CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/ftnmods")

if (NOT CMAKE_BUILD_TYPE)
  # Sets the optimization level to -O2 and includes -g for debugging
  set(CMAKE_BUILD_TYPE "RelWithDebInfo" CACHE STRING "Choose the build type: Debug RelWithDebInfo Release" FORCE)
endif()

message(STATUS "CMAKE_Fortran_COMPILER_ID = ${CMAKE_Fortran_COMPILER_ID}")
if(APPLE OR UNIX)
  # Enable .dll export
  if (CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -DIMPLICIT_DLLEXPORT -r8 -double-size 64 -cpp -lzmq")
  else()
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -DIMPLICIT_DLLEXPORT -ffree-line-length-0 -fdefault-real-8 -fdefault-double-8 -cpp -lzmq")
  endif()
elseif(WIN32 AND MINGW)
  # Ensure static linking to avoid requiring Fortran runtime dependencies
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -ffree-line-length-0 -static-libgcc -static-libgfortran -static -fdefault-real-8 -fdefault-double-8 -cpp -lzmq")
endif()

set(SOURCES
    src/Constants.f90
    src/ControllerBlocks.f90
    src/Controllers.f90
    src/DISCON.F90
    src/ROSCO_Types.f90
    src/Filters.f90
    src/Functions.f90
    src/ReadSetParameters.f90
    src/ZeroMQInterface.f90
    src/zmq_client.c
    src/zmq_client.o
)

# Git version
if( DEFINED GIT_DESCRIBE )
  message( WARNING
    "Version information has been set as a CMake flag. This should only used when the git-version cannot be set automatically."
  )
else()
  include(${CMAKE_SOURCE_DIR}/cmake/GetGitRevisionDescription.cmake)
  git_describe(GIT_DESCRIBE)
endif()
add_definitions(-DGIT_VERSION_INFO="${GIT_DESCRIBE}")

# Library
add_library(discon SHARED ${SOURCES})

# Add ZeroMQ library
find_package(PkgConfig)
pkg_check_modules(PC_ZeroMQ libzmq)
find_path(ZeroMQ_INCLUDE_DIR
        NAMES zmq.h
        PATHS ${PC_ZeroMQ_INCLUDE_DIRS}
        )
find_library(ZeroMQ_LIBRARY
        NAMES zmq
        PATHS ${PC_ZeroMQ_LIBRARY_DIRS}
        )

# add the ZeroMQ include directory to our compile directives
target_include_directories(discon PUBLIC ${ZeroMQ_INCLUDE_DIR})

# at the ZeroMQ library to our link directive
target_link_libraries(discon PUBLIC ${ZeroMQ_LIBRARY})

install(TARGETS discon
  EXPORT "${CMAKE_PROJECT_NAME}Libraries"
  RUNTIME DESTINATION lib
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib
)
if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  set(CMAKE_INSTALL_PREFIX "${CMAKE_SOURCE_DIR}/install")
endif()
