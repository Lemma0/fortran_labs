################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
F90_SRCS += \
../src/environment.f90 \
../src/main.f90 

OBJS += \
./src/environment.o \
./src/main.o 


# Each subdirectory must supply rules for building sources it contributes
src/%.o: ../src/%.f90
	@echo 'Building file: $<'
	@echo 'Invoking: GNU Fortran Compiler'
	gfortran -funderscoring -O3 -ftree-vectorize -fopt-info-vec -g -Wall -c -fmessage-length=0 -std=f2008ts -fimplicit-none -Wno-maybe-uninitialized -static-libgfortran -flto -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '

src/environment.o: ../src/environment.f90

src/main.o: ../src/main.f90 src/environment.o


