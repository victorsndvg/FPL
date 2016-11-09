# FPL
**F**ortran **P**arameter **L**ist

[![Build Status](https://travis-ci.org/victorsndvg/FPL.svg?branch=master)](https://travis-ci.org/victorsndvg/FPL)
[![codecov.io](https://codecov.io/github/victorsndvg/FPL/coverage.svg?branch=master)](https://codecov.io/github/victorsndvg/FPL?branch=master)

## License

[![License](https://img.shields.io/badge/license-GNU%20LESSER%20GENERAL%20PUBLIC%20LICENSE%20v3%2C%20LGPLv3-red.svg)](http://www.gnu.org/licenses/lgpl-3.0.txt)

## What is FPL?

**FPL** is pure fortran 2003 library that can manage the parameters of your program from a single point.

**FPL** is an extendible container (dictionary) of ```<Key, Value>``` pairs, where the *Key* is a character string and the *Value* can be, by the default, of the following data types:

- Integer (kinds 1, 2, 4, 8)
- Real (kinds 4, 8)
- Logical
- String

Value can be a scalar or an array of any dimension.

**FPL** stores copies of the passed data by assignment.

**FPL** is based in [Teuchos::ParameterList](https://trilinos.org/docs/dev/packages/teuchos/doc/html/classTeuchos_1_1ParameterList.html)  of the [Trilinos](https://trilinos.org/) project.

## How to get FPL

```git clone --recursive https://github.com/victorsndvg/FPL.git ```

## Compilation

**FPL** compile with GNU Fortran compiler 5.1 (and newer versions) and Intel Fortran compiler 15.0.1 (and newer versions).

**Note:** *Due to an issue with IBM XLF 14.0.1 Fortran compiler, if you want to use this compiler use the branch XLF_workaround*

**FPL** uses [CMake](https://cmake.org/) as a portable compilation system. 

The easiest way to compile **FPL** under Linux is:

```
$ cd FPL
$ mkdir build
$ cd build
$ cmake ../
$ make
```

*To compile FPL under Windows use de equivalent commands*


## Getting started with FPL

Notes:
- [Source code documentation](http://victorsndvg.github.io/FPL/)
- **FPL** cannot handle non-allocated variables while calling `Set(Key, Value)` or `Get(Key, Value)` procedures.
- To succesfull get a stored value into your target variable, **data type** and **shape** of both variables must agree.


### Using FPL in your program

```fortran
USE FPL

type(ParameterList_t) :: My_List

call FPL_Init()
call My_List%Init()

... [Program body]

call My_List%Free()
call FPL_Finalize()
```

### Setting parameters

```fortran
FPLError = My_List%Set(Key='Max Iters', Value=1500)
FPLError = My_List%Set(Key='Tolerance', Value=1.e-10)
FPLError = My_List%Set(Key='Solver', Value='GMRES')
```

### Getting parameters

```fortran
integer :: MaxIters

FPLError = My_List%Get(Key='Max Iters', Value=MaxIters)
```

### Getting parameters as strings

```fortran
character(len=:), allocatable :: MaxItersString

FPLError = My_List%GetAsString(Key='Max Iters', String=MaxItersString)
```

### Check if you can assign a parameter to your variable

Check if the target variable has the same type and shape as the stored variable :bangbang:

```fortran
integer :: MaxIters

if(My_List%isAssignable(Key='Max Iters', Value=MaxIters)) then
    FPLError = My_List%Get(Key='Max Iters', Value=MaxIters)
endif
```

### Deleting parameters

```fortran
call My_List%Del(Key='Max Iters')
```

### Checking if a parameter is present

```fortran
logical :: solver_defined

solver_defined = My_List%isPresent(Key='Solver')
```

### Checking if a parameter is of the expected data type

```fortran
logical :: has_same_type
real    :: Tolerance

has_same_type = My_List%isOfDataType(Key='Tolerance', Mold=Tolerance)
```

### Checking the shape of a parameter

```fortran
logical :: has_same_type
integer, allocatable :: Shape(:)

FPLError = My_List%GetShape(Key='Tolerance', Shape=Shape)
```

### Working with parameter sublists

Every parameter list can recursively store parameter sublists.

```fortran
type(ParameterList_t), pointer :: Prec_List

Prec_List => My_List%NewSubList(Key='Preconditioner')

call Prec_List%Set(Key='Type', Value='ILU')
call Prec_List%Set(Key='Drop Tolerance', Value=1.e-3)
```

### Checking if it is a parameter sublist

```fortran
logical :: prec_defined

prec_defined = My_List%isSubList(Key='Preconditioner')
```

### Print (recursive) the content of a parameter list

```fortran
call My_List%Print()
```

### Iterate on a ParameterList

ParameterList also includes a derived type that works like an iterator to go through all stored parameters without asking for the key.

ParameterList_Iterator interface is almost the same than ParameterList interface plus some procedures like `HasFinished()` and `Next()` to manage the advance of the iterator.

The example below iterates on a ParameterList containing integer vectors and getting the stored values.

```fortran
type(ParameterListIterator_t) :: Iterator
integer,allocatable :: array(:)
integer,allocatable :: shape(:)

Iterator = Parameters%GetIterator()
do while (.not. Iterator%HasFinished())
    if(Iterator%GetDimensions() /= 1) stop -1
    if(Iterator%GetShape(Shape=shape) /= 0) stop -1
    if(.not. Iterator%IsOfDataType(Mold=array)) stop -1
    if(allocated(array)) deallocate(array)
    allocate(array(shape(1)))
    if(Iterator%Get(array) /= 0) stop -1
    print*, '  Key = '//Iterator%GetKey()
    print*, '  Bytes = ', Iterator%DataSizeInBytes()
    print*, '  Dimensions = ', Iterator%GetDimensions()
    print*, '  Value = ', array)
    call Iterator%Next()
enddo
```

