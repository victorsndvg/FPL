#FPL
**F**ortran **P**arameter **L**ist

[![Build Status](https://travis-ci.org/victorsndvg/FPL.svg?branch=master)](https://travis-ci.org/victorsndvg/FPL)

##License

[LGPL v3](http://www.gnu.org/licenses/lgpl-3.0.txt)

##What is FPL?

**FPL** is pure fortran 2003 library that can manage the parameters of your program from a single point.

**FPL** is an extendible container (dictionary) of ```<Key, Value>``` pairs, where the *Key* is a character string and the *Value* can be, by the default, of the following data types:

- Integer (kinds 1, 2, 4, 8)
- Real (kinds 4, 8)
- Logical
- String

Value can be a scalar or an array of any dimension.

**FPL** stores copies of the passed data by assignment.

**FPL** is based in [Teuchos::ParameterList](https://trilinos.org/docs/dev/packages/teuchos/doc/html/classTeuchos_1_1ParameterList.html)  of the [Trilinos](https://trilinos.org/) project.

##How to get FPL

```git clone --recursive https://github.com/victorsndvg/FPL.git ```

##Compilation

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


##Getting started with FPL


###Using FPL in your program

```fortran
USE FPL

type(ParameterList_t) :: My_List

call FPL_Init()
call My_List%Init()

... [Program boddy]

call My_List%Free()
call FPL_Finalize()
```

###Setting parameters

```fortran
FPLError = My_List%Set(Key='Max Iters', Value=1500)
FPLError = My_List%Set(Key='Tolerance', Value=1.e-10)
FPLError = My_List%Set(Key='Solver', Value='GMRES')
```

###Getting parameters

```fortran
integer :: MaxIters

FPLError = My_List%Get(Key='Max Iters', Value=MaxIters)
```

###Deleting parameters

```fortran
call My_List%Del(Key='Max Iters')
```

###Checking if a parameter is present

```fortran
logical :: solver_defined

solver_defined = My_List%isPresent(Key='Solver')
```

###Checking if a parameter is of the expected data type

```fortran
logical :: has_same_type
real    :: Tolerance

has_same_type = My_List%isOfDataType(Key='Tolerance', Mold=Tolerance)
```

###Checking the shape of a parameter

```fortran
logical :: has_same_type
integer, allocatable :: Shape(:)

Shape = My_List%Shape(Key='Tolerance')
```

###Working with parameter sublists

Every parameter list can recursively store other parameter sublists that have the same functionality of the original.

```fortran
type(ParameterList_t), pointer :: Prec_List

Prec_List => My_List%NewSubList(Key='Preconditioner')

call Prec_List%Set(Key='Type', Value='ILU')
call Prec_List%Set(Key='Drop Tolerance', Value=1.e-3)
```

###Checking if is a parameter sublist

```fortran
logical :: solver_defined

prec_defined = My_List%isSubList(Key='Preconditioner')
```

###Print the content of a parameter list

```fortran
call My_List%Print()
```

