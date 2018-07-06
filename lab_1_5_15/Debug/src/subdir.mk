################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
F90_SRCS += \
../src/environment.f90 \
../src/list_io.f90 \
../src/list_process.f90 \
../src/main.f90 

OBJS += \
./src/environment.o \
./src/list_io.o \
./src/list_process.o \
./src/main.o 


# Each subdirectory must supply rules for building sources it contributes
src/%.o: ../src/%.f90
	@echo 'Building file: $<'
	@echo 'Invoking: GNU Fortran Compiler'
	gfortran -funderscoring -O0 -g -Wall -c -fmessage-length=0 -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '

src/environment.o: ../src/environment.f90

src/list_io.o: ../src/list_io.f90 src/environment.o

src/list_process.o: ../src/list_process.f90 src/environment.o src/list_io.o

src/main.o: ../src/main.f90 src/environment.o src/list_io.o src/list_process.o


