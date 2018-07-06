################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
F90_SRCS += \
../src/environment.f90 \
../src/main.f90 \
../src/text_io.f90 \
../src/text_process.f90 

OBJS += \
./src/environment.o \
./src/main.o \
./src/text_io.o \
./src/text_process.o 


# Each subdirectory must supply rules for building sources it contributes
src/%.o: ../src/%.f90
	@echo 'Building file: $<'
	@echo 'Invoking: GNU Fortran Compiler'
	gfortran -funderscoring -O0 -g -Wall -c -fmessage-length=0 -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '

src/environment.o: ../src/environment.f90

src/main.o: ../src/main.f90 src/environment.o src/text_io.o src/text_process.o

src/text_io.o: ../src/text_io.f90 src/environment.o

src/text_process.o: ../src/text_process.f90 src/environment.o src/text_io.o


