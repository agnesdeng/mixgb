UNAME := $(shell uname)

PKG_CXXFLAGS = $(SHLIB_OPENMP_CXXFLAGS)
PKG_LIBS = $(SHLIB_OPENMP_CXXFLAGS) $(LAPACK_LIBS) $(BLAS_LIBS) $(FLIBS)

ifeq ($(UNAME),Darwin)
PKG_LIBS = $(SHLIB_OPENMP_CXXFLAGS) -framework Accelerate
endif



#This is a platform-specific string containing the correct compiler flags to enable OpenMP for C++ code.
#“Use the OpenMP-safe compiler flags provided by R.”
PKG_CXXFLAGS = $(SHLIB_OPENMP_CXXFLAGS)
#This tells the linker which libraries and flags should be used when building the final .so or .dll.
PKG_LIBS = $(SHLIB_OPENMP_CXXFLAGS)
