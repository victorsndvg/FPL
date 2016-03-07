!-----------------------------------------------------------------
! FPL (Fortran Parameter List)
! Copyright (c) 2015 Santiago Badia, Alberto F. Martín, 
! Javier Principe and Víctor Sande.
! All rights reserved.
!
! This library is free software; you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public
! License as published by the Free Software Foundation; either
! version 3.0 of the License, or (at your option) any later version.
!
! This library is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public
! License along with this library.
!-----------------------------------------------------------------

module DimensionsWrapper

USE IR_Precision, only: I1P, I2P, I4P, I8P, R4P, R8P, str, byte_size
USE FPL_utils
USE ErrorMessages

implicit none
private

    type, abstract :: DimensionsWrapper_t
    private
        integer(I1P) :: Dimensions = -1
    contains
    private
        procedure, public :: SetDimensions => DimensionsWrapper_SetDimensions
        procedure, public :: GetDimensions => DimensionsWrapper_GetDimensions
        procedure, public :: Print         => DimensionsWrapper_Print
        procedure(DimensionsWrapper_DataSizeInBytes), public, deferred :: DataSizeInBytes
        procedure(DimensionsWrapper_isOfDataType),    public, deferred :: isOfDataType
        procedure(DimensionsWrapper_Free),            public, deferred :: Free
        procedure(DimensionsWrapper_GetShape),        public, deferred :: GetShape
    end type

    type, extends(DimensionsWrapper_t), abstract :: DimensionsWrapper0D_t
    private
    contains
        procedure(DimensionsWrapper0D_Set),            deferred :: Set
        procedure(DimensionsWrapper0D_Get),            deferred :: Get
        procedure(DimensionsWrapper0D_GetPointer),     deferred :: GetPointer
    end type

    type, extends(DimensionsWrapper_t), abstract :: DimensionsWrapper1D_t
    private
    contains
        procedure(DimensionsWrapper1D_Set),            deferred :: Set
        procedure(DimensionsWrapper1D_Get),            deferred :: Get
        procedure(DimensionsWrapper1D_GetPointer),     deferred :: GetPointer
    end type

    type, extends(DimensionsWrapper_t), abstract :: DimensionsWrapper2D_t
    private
    contains
        procedure(DimensionsWrapper2D_Set),            deferred :: Set
        procedure(DimensionsWrapper2D_Get),            deferred :: Get
        procedure(DimensionsWrapper2D_GetPointer),     deferred :: GetPointer
    end type

    type, extends(DimensionsWrapper_t), abstract :: DimensionsWrapper3D_t
    private
    contains
        procedure(DimensionsWrapper3D_Set),            deferred :: Set
        procedure(DimensionsWrapper3D_Get),            deferred :: Get
        procedure(DimensionsWrapper3D_GetPointer),     deferred :: GetPointer
    end type

    type, extends(DimensionsWrapper_t), abstract :: DimensionsWrapper4D_t
    private
    contains
        procedure(DimensionsWrapper4D_Set),            deferred :: Set
        procedure(DimensionsWrapper4D_Get),            deferred :: Get
        procedure(DimensionsWrapper4D_GetPointer),     deferred :: GetPointer
    end type

    type, extends(DimensionsWrapper_t), abstract :: DimensionsWrapper5D_t
    private
    contains
        procedure(DimensionsWrapper5D_Set),            deferred :: Set
        procedure(DimensionsWrapper5D_Get),            deferred :: Get
        procedure(DimensionsWrapper5D_GetPointer),     deferred :: GetPointer
    end type

    type, extends(DimensionsWrapper_t), abstract :: DimensionsWrapper6D_t
    private
    contains
        procedure(DimensionsWrapper6D_Set),            deferred :: Set
        procedure(DimensionsWrapper6D_Get),            deferred :: Get
        procedure(DimensionsWrapper6D_GetPointer),     deferred :: GetPointer
    end type

    type, extends(DimensionsWrapper_t), abstract :: DimensionsWrapper7D_t
    private
    contains
        procedure(DimensionsWrapper7D_Set),            deferred :: Set
        procedure(DimensionsWrapper7D_Get),            deferred :: Get
        procedure(DimensionsWrapper7D_GetPointer),     deferred :: GetPointer
    end type

    type, extends(DimensionsWrapper0D_t) :: DimensionsWrapper0D_DLCA_t
        character(len=:), allocatable :: Value
    contains
    private
        procedure, public :: Set            => DimensionsWrapper0D_DLCA_Set
        procedure, public :: Get            => DimensionsWrapper0D_DLCA_Get
        procedure, public :: GetShape       => DimensionsWrapper0D_DLCA_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper0D_DLCA_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper0D_DLCA_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper0D_DLCA_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper0D_DLCA_isOfDataType
        procedure, public :: Free           => DimensionsWrapper0D_DLCA_Free
        procedure, public :: Print          => DimensionsWrapper0D_DLCA_Print
        final             ::                   DimensionsWrapper0D_DLCA_Final
    end type

    type, extends(DimensionsWrapper0D_t) :: DimensionsWrapper0D_I1P_t
        integer(I1P), allocatable :: Value
    contains
    private
        procedure, public :: Set            => DimensionsWrapper0D_I1P_Set
        procedure, public :: Get            => DimensionsWrapper0D_I1P_Get
        procedure, public :: GetShape       => DimensionsWrapper0D_I1P_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper0D_I1P_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper0D_I1P_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper0D_I1P_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper0D_I1P_isOfDataType
        procedure, public :: Free           => DimensionsWrapper0D_I1P_Free
        procedure, public :: Print          => DimensionsWrapper0D_I1P_Print
        final             ::                   DimensionsWrapper0D_I1P_Final
    end type

    type, extends(DimensionsWrapper0D_t) :: DimensionsWrapper0D_I2P_t
        integer(I2P), allocatable :: Value
    contains
    private
        procedure, public :: Set            => DimensionsWrapper0D_I2P_Set
        procedure, public :: Get            => DimensionsWrapper0D_I2P_Get
        procedure, public :: GetShape       => DimensionsWrapper0D_I2P_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper0D_I2P_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper0D_I2P_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper0D_I2P_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper0D_I2P_isOfDataType
        procedure, public :: Free           => DimensionsWrapper0D_I2P_Free
        procedure, public :: Print          => DimensionsWrapper0D_I2P_Print
        final             ::                   DimensionsWrapper0D_I2P_Final
    end type

    type, extends(DimensionsWrapper0D_t) :: DimensionsWrapper0D_I4P_t
        integer(I4P), allocatable :: Value
    contains
    private
        procedure, public :: Set            => DimensionsWrapper0D_I4P_Set
        procedure, public :: Get            => DimensionsWrapper0D_I4P_Get
        procedure, public :: GetShape       => DimensionsWrapper0D_I4P_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper0D_I4P_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper0D_I4P_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper0D_I4P_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper0D_I4P_isOfDataType
        procedure, public :: Free           => DimensionsWrapper0D_I4P_Free
        procedure, public :: Print          => DimensionsWrapper0D_I4P_Print
        final             ::                   DimensionsWrapper0D_I4P_Final
    end type

    type, extends(DimensionsWrapper0D_t) :: DimensionsWrapper0D_I8P_t
        integer(I8P), allocatable :: Value
    contains
    private
        procedure, public :: Set            => DimensionsWrapper0D_I8P_Set
        procedure, public :: Get            => DimensionsWrapper0D_I8P_Get
        procedure, public :: GetShape       => DimensionsWrapper0D_I8P_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper0D_I8P_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper0D_I8P_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper0D_I8P_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper0D_I8P_isOfDataType
        procedure, public :: Free           => DimensionsWrapper0D_I8P_Free
        procedure, public :: Print          => DimensionsWrapper0D_I8P_Print
        final             ::                   DimensionsWrapper0D_I8P_Final
    end type

    type, extends(DimensionsWrapper0D_t) :: DimensionsWrapper0D_L_t
        logical, allocatable :: Value
    contains
    private
        procedure, public :: Set            => DimensionsWrapper0D_L_Set
        procedure, public :: Get            => DimensionsWrapper0D_L_Get
        procedure, public :: GetShape       => DimensionsWrapper0D_L_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper0D_L_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper0D_L_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper0D_L_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper0D_L_isOfDataType
        procedure, public :: Free           => DimensionsWrapper0D_L_Free
        procedure, public :: Print          => DimensionsWrapper0D_L_Print
        final             ::                   DimensionsWrapper0D_L_Final
    end type

    type, extends(DimensionsWrapper0D_t) :: DimensionsWrapper0D_R4P_t
        real(R4P), allocatable :: Value
    contains
    private
        procedure, public :: Set            => DimensionsWrapper0D_R4P_Set
        procedure, public :: Get            => DimensionsWrapper0D_R4P_Get
        procedure, public :: GetShape       => DimensionsWrapper0D_R4P_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper0D_R4P_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper0D_R4P_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper0D_R4P_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper0D_R4P_isOfDataType
        procedure, public :: Print          => DimensionsWrapper0D_R4P_Print
        procedure, public :: Free           => DimensionsWrapper0D_R4P_Free
        final             ::                   DimensionsWrapper0D_R4P_Final
    end type

    type, extends(DimensionsWrapper0D_t) :: DimensionsWrapper0D_R8P_t
        real(R8P), allocatable :: Value
    contains
    private
        procedure, public :: Set            => DimensionsWrapper0D_R8P_Set
        procedure, public :: Get            => DimensionsWrapper0D_R8P_Get
        procedure, public :: GetShape       => DimensionsWrapper0D_R8P_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper0D_R8P_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper0D_R8P_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper0D_R8P_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper0D_R8P_isOfDataType
        procedure, public :: Free           => DimensionsWrapper0D_R8P_Free
        procedure, public :: Print          => DimensionsWrapper0D_R8P_Print
        final             ::                   DimensionsWrapper0D_R8P_Final
    end type

    type, extends(DimensionsWrapper1D_t) :: DimensionsWrapper1D_DLCA_t
        character(len=:), allocatable :: Value(:)
    contains
    private
        procedure, public :: Set            => DimensionsWrapper1D_DLCA_Set
        procedure, public :: Get            => DimensionsWrapper1D_DLCA_Get
        procedure, public :: GetShape       => DimensionsWrapper1D_DLCA_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper1D_DLCA_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper1D_DLCA_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper1D_DLCA_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper1D_DLCA_isOfDataType
        procedure, public :: Free           => DimensionsWrapper1D_DLCA_Free
        procedure, public :: Print          => DimensionsWrapper1D_DLCA_Print
        final             ::                   DimensionsWrapper1D_DLCA_Final
    end type

    type, extends(DimensionsWrapper1D_t) :: DimensionsWrapper1D_I1P_t
        integer(I1P), allocatable :: Value(:)
    contains
    private
        procedure, public :: Set            => DimensionsWrapper1D_I1P_Set
        procedure, public :: Get            => DimensionsWrapper1D_I1P_Get
        procedure, public :: GetShape       => DimensionsWrapper1D_I1P_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper1D_I1P_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper1D_I1P_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper1D_I1P_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper1D_I1P_isOfDataType
        procedure, public :: Free           => DimensionsWrapper1D_I1P_Free
        procedure, public :: Print          => DimensionsWrapper1D_I1P_Print
        final             ::                   DimensionsWrapper1D_I1P_Final
    end type

    type, extends(DimensionsWrapper1D_t) :: DimensionsWrapper1D_I2P_t
        integer(I2P), allocatable :: Value(:)
    contains
    private
        procedure, public :: Set            => DimensionsWrapper1D_I2P_Set
        procedure, public :: Get            => DimensionsWrapper1D_I2P_Get
        procedure, public :: GetShape       => DimensionsWrapper1D_I2P_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper1D_I2P_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper1D_I2P_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper1D_I2P_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper1D_I2P_isOfDataType
        procedure, public :: Free           => DimensionsWrapper1D_I2P_Free
        procedure, public :: Print          => DimensionsWrapper1D_I2P_Print
        final             ::                   DimensionsWrapper1D_I2P_Final
    end type

    type, extends(DimensionsWrapper1D_t) :: DimensionsWrapper1D_I4P_t
        integer(I4P), allocatable :: Value(:)
    contains
    private
        procedure, public :: Set            => DimensionsWrapper1D_I4P_Set
        procedure, public :: Get            => DimensionsWrapper1D_I4P_Get
        procedure, public :: GetShape       => DimensionsWrapper1D_I4P_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper1D_I4P_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper1D_I4P_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper1D_I4P_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper1D_I4P_isOfDataType
        procedure, public :: Free           => DimensionsWrapper1D_I4P_Free
        procedure, public :: Print          => DimensionsWrapper1D_I4P_Print
        final             ::                   DimensionsWrapper1D_I4P_Final
    end type

    type, extends(DimensionsWrapper1D_t) :: DimensionsWrapper1D_I8P_t
        integer(I8P), allocatable :: Value(:)
    contains
    private
        procedure, public :: Set            => DimensionsWrapper1D_I8P_Set
        procedure, public :: Get            => DimensionsWrapper1D_I8P_Get
        procedure, public :: GetShape       => DimensionsWrapper1D_I8P_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper1D_I8P_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper1D_I8P_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper1D_I8P_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper1D_I8P_isOfDataType
        procedure, public :: Print          => DimensionsWrapper1D_I8P_Print
        procedure, public :: Free           => DimensionsWrapper1D_I8P_Free
        final             ::                   DimensionsWrapper1D_I8P_Final
    end type

    type, extends(DimensionsWrapper1D_t) :: DimensionsWrapper1D_L_t
        logical, allocatable :: Value(:)
    contains
    private
        procedure, public :: Set            => DimensionsWrapper1D_L_Set
        procedure, public :: Get            => DimensionsWrapper1D_L_Get
        procedure, public :: GetShape       => DimensionsWrapper1D_L_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper1D_L_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper1D_L_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper1D_L_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper1D_L_isOfDataType
        procedure, public :: Free           => DimensionsWrapper1D_L_Free
        procedure, public :: Print          => DimensionsWrapper1D_L_Print
        final             ::                   DimensionsWrapper1D_L_Final
    end type

    type, extends(DimensionsWrapper1D_t) :: DimensionsWrapper1D_R4P_t
        real(R4P), allocatable :: Value(:)
    contains
    private
        procedure, public :: Set            => DimensionsWrapper1D_R4P_Set
        procedure, public :: Get            => DimensionsWrapper1D_R4P_Get
        procedure, public :: GetShape       => DimensionsWrapper1D_R4P_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper1D_R4P_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper1D_R4P_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper1D_R4P_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper1D_R4P_isOfDataType
        procedure, public :: Free           => DimensionsWrapper1D_R4P_Free
        procedure, public :: Print          => DimensionsWrapper1D_R4P_Print
        final             ::                   DimensionsWrapper1D_R4P_Final
    end type

    type, extends(DimensionsWrapper1D_t) :: DimensionsWrapper1D_R8P_t
        real(R8P), allocatable :: Value(:)
    contains
    private
        procedure, public :: Set            => DimensionsWrapper1D_R8P_Set
        procedure, public :: Get            => DimensionsWrapper1D_R8P_Get
        procedure, public :: GetShape       => DimensionsWrapper1D_R8P_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper1D_R8P_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper1D_R8P_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper1D_R8P_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper1D_R8P_isOfDataType
        procedure, public :: Free           => DimensionsWrapper1D_R8P_Free
        procedure, public :: Print          => DimensionsWrapper1D_R8P_Print
        final             ::                   DimensionsWrapper1D_R8P_Final
    end type

    type, extends(DimensionsWrapper2D_t) :: DimensionsWrapper2D_DLCA_t
        character(len=:), allocatable :: Value(:,:)
    contains
    private
        procedure, public :: Set            => DimensionsWrapper2D_DLCA_Set
        procedure, public :: Get            => DimensionsWrapper2D_DLCA_Get
        procedure, public :: GetShape       => DimensionsWrapper2D_DLCA_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper2D_DLCA_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper2D_DLCA_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper2D_DLCA_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper2D_DLCA_isOfDataType
        procedure, public :: Free           => DimensionsWrapper2D_DLCA_Free
        procedure, public :: Print          => DimensionsWrapper2D_DLCA_Print
        final             ::                   DimensionsWrapper2D_DLCA_Final
    end type

    type, extends(DimensionsWrapper2D_t) :: DimensionsWrapper2D_I1P_t
        integer(I1P), allocatable :: Value(:,:)
    contains
    private
        procedure, public :: Set            => DimensionsWrapper2D_I1P_Set
        procedure, public :: Get            => DimensionsWrapper2D_I1P_Get
        procedure, public :: GetShape       => DimensionsWrapper2D_I1P_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper2D_I1P_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper2D_I1P_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper2D_I1P_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper2D_I1P_isOfDataType
        procedure, public :: Free           => DimensionsWrapper2D_I1P_Free
        procedure, public :: Print          => DimensionsWrapper2D_I1P_Print
        final             ::                   DimensionsWrapper2D_I1P_Final
    end type

    type, extends(DimensionsWrapper2D_t) :: DimensionsWrapper2D_I2P_t
        integer(I2P), allocatable :: Value(:,:)
    contains
    private
        procedure, public :: Set            => DimensionsWrapper2D_I2P_Set
        procedure, public :: Get            => DimensionsWrapper2D_I2P_Get
        procedure, public :: GetShape       => DimensionsWrapper2D_I2P_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper2D_I2P_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper2D_I2P_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper2D_I2P_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper2D_I2P_isOfDataType
        procedure, public :: Free           => DimensionsWrapper2D_I2P_Free
        procedure, public :: Print          => DimensionsWrapper2D_I2P_Print
        final             ::                   DimensionsWrapper2D_I2P_Final
    end type

    type, extends(DimensionsWrapper2D_t) :: DimensionsWrapper2D_I4P_t
        integer(I4P), allocatable :: Value(:,:)
    contains
    private
        procedure, public :: Set            => DimensionsWrapper2D_I4P_Set
        procedure, public :: Get            => DimensionsWrapper2D_I4P_Get
        procedure, public :: GetShape       => DimensionsWrapper2D_I4P_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper2D_I4P_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper2D_I4P_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper2D_I4P_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper2D_I4P_isOfDataType
        procedure, public :: Free           => DimensionsWrapper2D_I4P_Free
        procedure, public :: Print          => DimensionsWrapper2D_I4P_Print
        final             ::                   DimensionsWrapper2D_I4P_Final
    end type

    type, extends(DimensionsWrapper2D_t) :: DimensionsWrapper2D_I8P_t
        integer(I8P), allocatable :: Value(:,:)
    contains
    private
        procedure, public :: Set            => DimensionsWrapper2D_I8P_Set
        procedure, public :: Get            => DimensionsWrapper2D_I8P_Get
        procedure, public :: GetShape       => DimensionsWrapper2D_I8P_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper2D_I8P_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper2D_I8P_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper2D_I8P_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper2D_I8P_isOfDataType
        procedure, public :: Free           => DimensionsWrapper2D_I8P_Free
        procedure, public :: Print          => DimensionsWrapper2D_I8P_Print
        final             ::                   DimensionsWrapper2D_I8P_Final
    end type

    type, extends(DimensionsWrapper2D_t) :: DimensionsWrapper2D_L_t
        logical, allocatable :: Value(:,:)
    contains
    private
        procedure, public :: Set            => DimensionsWrapper2D_L_Set
        procedure, public :: Get            => DimensionsWrapper2D_L_Get
        procedure, public :: GetShape       => DimensionsWrapper2D_L_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper2D_L_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper2D_L_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper2D_L_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper2D_L_isOfDataType
        procedure, public :: Free           => DimensionsWrapper2D_L_Free
        procedure, public :: Print          => DimensionsWrapper2D_L_Print
        final             ::                   DimensionsWrapper2D_L_Final
    end type

    type, extends(DimensionsWrapper2D_t) :: DimensionsWrapper2D_R4P_t
        real(R4P), allocatable :: Value(:,:)
    contains
    private
        procedure, public :: Set            => DimensionsWrapper2D_R4P_Set
        procedure, public :: Get            => DimensionsWrapper2D_R4P_Get
        procedure, public :: GetShape       => DimensionsWrapper2D_R4P_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper2D_R4P_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper2D_R4P_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper2D_R4P_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper2D_R4P_isOfDataType
        procedure, public :: Free           => DimensionsWrapper2D_R4P_Free
        procedure, public :: Print          => DimensionsWrapper2D_R4P_Print
        final             ::                   DimensionsWrapper2D_R4P_Final
    end type

    type, extends(DimensionsWrapper2D_t) :: DimensionsWrapper2D_R8P_t
        real(R8P), allocatable :: Value(:,:)
    contains
    private
        procedure, public :: Set            => DimensionsWrapper2D_R8P_Set
        procedure, public :: Get            => DimensionsWrapper2D_R8P_Get
        procedure, public :: GetShape       => DimensionsWrapper2D_R8P_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper2D_R8P_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper2D_R8P_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper2D_R8P_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper2D_R8P_isOfDataType
        procedure, public :: Free           => DimensionsWrapper2D_R8P_Free
        procedure, public :: Print          => DimensionsWrapper2D_R8P_Print
        final             ::                   DimensionsWrapper2D_R8P_Final
    end type

    type, extends(DimensionsWrapper3D_t) :: DimensionsWrapper3D_DLCA_t
        character(len=:), allocatable :: Value(:,:,:)
    contains
    private
        procedure, public :: Set            => DimensionsWrapper3D_DLCA_Set
        procedure, public :: Get            => DimensionsWrapper3D_DLCA_Get
        procedure, public :: GetShape       => DimensionsWrapper3D_DLCA_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper3D_DLCA_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper3D_DLCA_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper3D_DLCA_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper3D_DLCA_isOfDataType
        procedure, public :: Free           => DimensionsWrapper3D_DLCA_Free
        procedure, public :: Print          => DimensionsWrapper3D_DLCA_Print
        final             ::                   DimensionsWrapper3D_DLCA_Final
    end type

    type, extends(DimensionsWrapper3D_t) :: DimensionsWrapper3D_I1P_t
        integer(I1P), allocatable :: Value(:,:,:)
    contains
    private
        procedure, public :: Set            => DimensionsWrapper3D_I1P_Set
        procedure, public :: Get            => DimensionsWrapper3D_I1P_Get
        procedure, public :: GetShape       => DimensionsWrapper3D_I1P_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper3D_I1P_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper3D_I1P_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper3D_I1P_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper3D_I1P_isOfDataType
        procedure, public :: Free           => DimensionsWrapper3D_I1P_Free
        procedure, public :: Print          => DimensionsWrapper3D_I1P_Print
        final             ::                   DimensionsWrapper3D_I1P_Final
    end type

    type, extends(DimensionsWrapper3D_t) :: DimensionsWrapper3D_I2P_t
        integer(I2P), allocatable :: Value(:,:,:)
    contains
    private
        procedure, public :: Set            => DimensionsWrapper3D_I2P_Set
        procedure, public :: Get            => DimensionsWrapper3D_I2P_Get
        procedure, public :: GetShape       => DimensionsWrapper3D_I2P_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper3D_I2P_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper3D_I2P_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper3D_I2P_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper3D_I2P_isOfDataType
        procedure, public :: Free           => DimensionsWrapper3D_I2P_Free
        procedure, public :: Print          => DimensionsWrapper3D_I2P_Print
        final             ::                   DimensionsWrapper3D_I2P_Final
    end type

    type, extends(DimensionsWrapper3D_t) :: DimensionsWrapper3D_I4P_t
        integer(I4P), allocatable :: Value(:,:,:)
    contains
    private
        procedure, public :: Set            => DimensionsWrapper3D_I4P_Set
        procedure, public :: Get            => DimensionsWrapper3D_I4P_Get
        procedure, public :: GetShape       => DimensionsWrapper3D_I4P_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper3D_I4P_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper3D_I4P_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper3D_I4P_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper3D_I4P_isOfDataType
        procedure, public :: Free           => DimensionsWrapper3D_I4P_Free
        procedure, public :: Print          => DimensionsWrapper3D_I4P_Print
        final             ::                   DimensionsWrapper3D_I4P_Final
    end type

    type, extends(DimensionsWrapper3D_t) :: DimensionsWrapper3D_I8P_t
        integer(I8P), allocatable :: Value(:,:,:)
    contains
    private
        procedure, public :: Set            => DimensionsWrapper3D_I8P_Set
        procedure, public :: Get            => DimensionsWrapper3D_I8P_Get
        procedure, public :: GetShape       => DimensionsWrapper3D_I8P_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper3D_I8P_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper3D_I8P_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper3D_I8P_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper3D_I8P_isOfDataType
        procedure, public :: Free           => DimensionsWrapper3D_I8P_Free
        procedure, public :: Print          => DimensionsWrapper3D_I8P_Print
        final             ::                   DimensionsWrapper3D_I8P_Final
    end type

    type, extends(DimensionsWrapper3D_t) :: DimensionsWrapper3D_L_t
        logical, allocatable :: Value(:,:,:)
    contains
    private
        procedure, public :: Set            => DimensionsWrapper3D_L_Set
        procedure, public :: Get            => DimensionsWrapper3D_L_Get
        procedure, public :: GetShape       => DimensionsWrapper3D_L_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper3D_L_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper3D_L_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper3D_L_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper3D_L_isOfDataType
        procedure, public :: Free           => DimensionsWrapper3D_L_Free
        procedure, public :: Print          => DimensionsWrapper3D_L_Print
        final             ::                   DimensionsWrapper3D_L_Final
    end type

    type, extends(DimensionsWrapper3D_t) :: DimensionsWrapper3D_R4P_t
        real(R4P), allocatable :: Value(:,:,:)
    contains
    private
        procedure, public :: Set            => DimensionsWrapper3D_R4P_Set
        procedure, public :: Get            => DimensionsWrapper3D_R4P_Get
        procedure, public :: GetShape       => DimensionsWrapper3D_R4P_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper3D_R4P_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper3D_R4P_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper3D_R4P_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper3D_R4P_isOfDataType
        procedure, public :: Free           => DimensionsWrapper3D_R4P_Free
        procedure, public :: Print          => DimensionsWrapper3D_R4P_Print
        final             ::                   DimensionsWrapper3D_R4P_Final
    end type

    type, extends(DimensionsWrapper3D_t) :: DimensionsWrapper3D_R8P_t
        real(R8P), allocatable :: Value(:,:,:)
    contains
    private
        procedure, public :: Set            => DimensionsWrapper3D_R8P_Set
        procedure, public :: Get            => DimensionsWrapper3D_R8P_Get
        procedure, public :: GetShape       => DimensionsWrapper3D_R8P_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper3D_R8P_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper3D_R8P_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper3D_R8P_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper3D_R8P_isOfDataType
        procedure, public :: Free           => DimensionsWrapper3D_R8P_Free
        procedure, public :: Print          => DimensionsWrapper3D_R8P_Print
        final             ::                   DimensionsWrapper3D_R8P_Final
    end type

    type, extends(DimensionsWrapper4D_t) :: DimensionsWrapper4D_DLCA_t
        character(len=:), allocatable :: Value(:,:,:,:)
    contains
    private
        procedure, public :: Set            => DimensionsWrapper4D_DLCA_Set
        procedure, public :: Get            => DimensionsWrapper4D_DLCA_Get
        procedure, public :: GetShape       => DimensionsWrapper4D_DLCA_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper4D_DLCA_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper4D_DLCA_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper4D_DLCA_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper4D_DLCA_isOfDataType
        procedure, public :: Print          => DimensionsWrapper4D_DLCA_Print
        procedure, public :: Free           => DimensionsWrapper4D_DLCA_Free
        final             ::                   DimensionsWrapper4D_DLCA_Final
    end type

    type, extends(DimensionsWrapper4D_t) :: DimensionsWrapper4D_I1P_t
        integer(I1P), allocatable :: Value(:,:,:,:)
    contains
    private
        procedure, public :: Set            => DimensionsWrapper4D_I1P_Set
        procedure, public :: Get            => DimensionsWrapper4D_I1P_Get
        procedure, public :: GetShape       => DimensionsWrapper4D_I1P_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper4D_I1P_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper4D_I1P_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper4D_I1P_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper4D_I1P_isOfDataType
        procedure, public :: Print          => DimensionsWrapper4D_I1P_Print
        procedure, public :: Free           => DimensionsWrapper4D_I1P_Free
        final             ::                   DimensionsWrapper4D_I1P_Final
    end type

    type, extends(DimensionsWrapper4D_t) :: DimensionsWrapper4D_I2P_t
        integer(I2P), allocatable :: Value(:,:,:,:)
    contains
    private
        procedure, public :: Set            => DimensionsWrapper4D_I2P_Set
        procedure, public :: Get            => DimensionsWrapper4D_I2P_Get
        procedure, public :: GetShape       => DimensionsWrapper4D_I2P_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper4D_I2P_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper4D_I2P_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper4D_I2P_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper4D_I2P_isOfDataType
        procedure, public :: Print          => DimensionsWrapper4D_I2P_Print
        procedure, public :: Free           => DimensionsWrapper4D_I2P_Free
        final             ::                   DimensionsWrapper4D_I2P_Final
    end type

    type, extends(DimensionsWrapper4D_t) :: DimensionsWrapper4D_I4P_t
        integer(I4P), allocatable :: Value(:,:,:,:)
    contains
    private
        procedure, public :: Set            => DimensionsWrapper4D_I4P_Set
        procedure, public :: Get            => DimensionsWrapper4D_I4P_Get
        procedure, public :: GetShape       => DimensionsWrapper4D_I4P_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper4D_I4P_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper4D_I4P_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper4D_I4P_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper4D_I4P_isOfDataType
        procedure, public :: Print          => DimensionsWrapper4D_I4P_Print
        procedure, public :: Free           => DimensionsWrapper4D_I4P_Free
        final             ::                   DimensionsWrapper4D_I4P_Final
    end type

    type, extends(DimensionsWrapper4D_t) :: DimensionsWrapper4D_I8P_t
        integer(I8P), allocatable :: Value(:,:,:,:)
    contains
    private
        procedure, public :: Set            => DimensionsWrapper4D_I8P_Set
        procedure, public :: Get            => DimensionsWrapper4D_I8P_Get
        procedure, public :: GetShape       => DimensionsWrapper4D_I8P_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper4D_I8P_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper4D_I8P_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper4D_I8P_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper4D_I8P_isOfDataType
        procedure, public :: Print          => DimensionsWrapper4D_I8P_Print
        procedure, public :: Free           => DimensionsWrapper4D_I8P_Free
        final             ::                   DimensionsWrapper4D_I8P_Final
    end type

    type, extends(DimensionsWrapper4D_t) :: DimensionsWrapper4D_L_t
        logical, allocatable :: Value(:,:,:,:)
    contains
    private
        procedure, public :: Set            => DimensionsWrapper4D_L_Set
        procedure, public :: Get            => DimensionsWrapper4D_L_Get
        procedure, public :: GetShape       => DimensionsWrapper4D_L_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper4D_L_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper4D_L_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper4D_L_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper4D_L_isOfDataType
        procedure, public :: Print          => DimensionsWrapper4D_L_Print
        procedure, public :: Free           => DimensionsWrapper4D_L_Free
        final             ::                   DimensionsWrapper4D_L_Final
    end type

    type, extends(DimensionsWrapper4D_t) :: DimensionsWrapper4D_R4P_t
        real(R4P), allocatable :: Value(:,:,:,:)
    contains
    private
        procedure, public :: Set            => DimensionsWrapper4D_R4P_Set
        procedure, public :: Get            => DimensionsWrapper4D_R4P_Get
        procedure, public :: GetShape       => DimensionsWrapper4D_R4P_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper4D_R4P_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper4D_R4P_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper4D_R4P_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper4D_R4P_isOfDataType
        procedure, public :: Free           => DimensionsWrapper4D_R4P_Free
        procedure, public :: Print          => DimensionsWrapper4D_R4P_Print
        final             ::                   DimensionsWrapper4D_R4P_Final
    end type

    type, extends(DimensionsWrapper4D_t) :: DimensionsWrapper4D_R8P_t
        real(R8P), allocatable :: Value(:,:,:,:)
    contains
    private
        procedure, public :: Set            => DimensionsWrapper4D_R8P_Set
        procedure, public :: Get            => DimensionsWrapper4D_R8P_Get
        procedure, public :: GetShape       => DimensionsWrapper4D_R8P_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper4D_R8P_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper4D_R8P_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper4D_R8P_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper4D_R8P_isOfDataType
        procedure, public :: Free           => DimensionsWrapper4D_R8P_Free
        procedure, public :: Print          => DimensionsWrapper4D_R8P_Print
        final             ::                   DimensionsWrapper4D_R8P_Final
    end type

    type, extends(DimensionsWrapper5D_t) :: DimensionsWrapper5D_DLCA_t
        character(len=:), allocatable :: Value(:,:,:,:,:)
    contains
    private
        procedure, public :: Set            => DimensionsWrapper5D_DLCA_Set
        procedure, public :: Get            => DimensionsWrapper5D_DLCA_Get
        procedure, public :: GetShape       => DimensionsWrapper5D_DLCA_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper5D_DLCA_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper5D_DLCA_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper5D_DLCA_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper5D_DLCA_isOfDataType
        procedure, public :: Print          => DimensionsWrapper5D_DLCA_Print
        procedure, public :: Free           => DimensionsWrapper5D_DLCA_Free
        final             ::                   DimensionsWrapper5D_DLCA_Final
    end type

    type, extends(DimensionsWrapper5D_t) :: DimensionsWrapper5D_I1P_t
        integer(I1P), allocatable :: Value(:,:,:,:,:)
    contains
    private
        procedure, public :: Set            => DimensionsWrapper5D_I1P_Set
        procedure, public :: Get            => DimensionsWrapper5D_I1P_Get
        procedure, public :: GetShape       => DimensionsWrapper5D_I1P_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper5D_I1P_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper5D_I1P_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper5D_I1P_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper5D_I1P_isOfDataType
        procedure, public :: Print          => DimensionsWrapper5D_I1P_Print
        procedure, public :: Free           => DimensionsWrapper5D_I1P_Free
        final             ::                   DimensionsWrapper5D_I1P_Final
    end type

    type, extends(DimensionsWrapper5D_t) :: DimensionsWrapper5D_I2P_t
        integer(I2P), allocatable :: Value(:,:,:,:,:)
    contains
    private
        procedure, public :: Set            => DimensionsWrapper5D_I2P_Set
        procedure, public :: Get            => DimensionsWrapper5D_I2P_Get
        procedure, public :: GetShape       => DimensionsWrapper5D_I2P_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper5D_I2P_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper5D_I2P_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper5D_I2P_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper5D_I2P_isOfDataType
        procedure, public :: Print          => DimensionsWrapper5D_I2P_Print
        procedure, public :: Free           => DimensionsWrapper5D_I2P_Free
        final             ::                   DimensionsWrapper5D_I2P_Final
    end type

    type, extends(DimensionsWrapper5D_t) :: DimensionsWrapper5D_I4P_t
        integer(I4P), allocatable :: Value(:,:,:,:,:)
    contains
    private
        procedure, public :: Set            => DimensionsWrapper5D_I4P_Set
        procedure, public :: Get            => DimensionsWrapper5D_I4P_Get
        procedure, public :: GetShape       => DimensionsWrapper5D_I4P_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper5D_I4P_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper5D_I4P_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper5D_I4P_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper5D_I4P_isOfDataType
        procedure, public :: Print          => DimensionsWrapper5D_I4P_Print
        procedure, public :: Free           => DimensionsWrapper5D_I4P_Free
        final             ::                   DimensionsWrapper5D_I4P_Final
    end type

    type, extends(DimensionsWrapper5D_t) :: DimensionsWrapper5D_I8P_t
        integer(I8P), allocatable :: Value(:,:,:,:,:)
    contains
    private
        procedure, public :: Set            => DimensionsWrapper5D_I8P_Set
        procedure, public :: Get            => DimensionsWrapper5D_I8P_Get
        procedure, public :: GetShape       => DimensionsWrapper5D_I8P_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper5D_I8P_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper5D_I8P_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper5D_I8P_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper5D_I8P_isOfDataType
        procedure, public :: Print          => DimensionsWrapper5D_I8P_Print
        procedure, public :: Free           => DimensionsWrapper5D_I8P_Free
        final             ::                   DimensionsWrapper5D_I8P_Final
    end type

    type, extends(DimensionsWrapper5D_t) :: DimensionsWrapper5D_L_t
        logical, allocatable :: Value(:,:,:,:,:)
    contains
    private
        procedure, public :: Set            => DimensionsWrapper5D_L_Set
        procedure, public :: Get            => DimensionsWrapper5D_L_Get
        procedure, public :: GetShape       => DimensionsWrapper5D_L_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper5D_L_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper5D_L_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper5D_L_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper5D_L_isOfDataType
        procedure, public :: Print          => DimensionsWrapper5D_L_Print
        procedure, public :: Free           => DimensionsWrapper5D_L_Free
        final             ::                   DimensionsWrapper5D_L_Final
    end type

    type, extends(DimensionsWrapper5D_t) :: DimensionsWrapper5D_R4P_t
        real(R4P), allocatable :: Value(:,:,:,:,:)
    contains
    private
        procedure, public :: Set            => DimensionsWrapper5D_R4P_Set
        procedure, public :: Get            => DimensionsWrapper5D_R4P_Get
        procedure, public :: GetShape       => DimensionsWrapper5D_R4P_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper5D_R4P_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper5D_R4P_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper5D_R4P_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper5D_R4P_isOfDataType
        procedure, public :: Print          => DimensionsWrapper5D_R4P_Print
        procedure, public :: Free           => DimensionsWrapper5D_R4P_Free
        final             ::                   DimensionsWrapper5D_R4P_Final
    end type

    type, extends(DimensionsWrapper5D_t) :: DimensionsWrapper5D_R8P_t
        real(R8P), allocatable :: Value(:,:,:,:,:)
    contains
    private
        procedure, public :: Set            => DimensionsWrapper5D_R8P_Set
        procedure, public :: Get            => DimensionsWrapper5D_R8P_Get
        procedure, public :: GetShape       => DimensionsWrapper5D_R8P_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper5D_R8P_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper5D_R8P_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper5D_R8P_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper5D_R8P_isOfDataType
        procedure, public :: Print          => DimensionsWrapper5D_R8P_Print
        procedure, public :: Free           => DimensionsWrapper5D_R8P_Free
        final             ::                   DimensionsWrapper5D_R8P_Final
    end type

    type, extends(DimensionsWrapper6D_t) :: DimensionsWrapper6D_DLCA_t
        character(len=:), allocatable :: Value(:,:,:,:,:,:)
    contains
    private
        procedure, public :: Set            => DimensionsWrapper6D_DLCA_Set
        procedure, public :: Get            => DimensionsWrapper6D_DLCA_Get
        procedure, public :: GetShape       => DimensionsWrapper6D_DLCA_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper6D_DLCA_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper6D_DLCA_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper6D_DLCA_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper6D_DLCA_isOfDataType
        procedure, public :: Print          => DimensionsWrapper6D_DLCA_Print
        procedure, public :: Free           => DimensionsWrapper6D_DLCA_Free
        final             ::                   DimensionsWrapper6D_DLCA_Final
    end type

    type, extends(DimensionsWrapper6D_t) :: DimensionsWrapper6D_I1P_t
        integer(I1P), allocatable :: Value(:,:,:,:,:,:)
    contains
    private
        procedure, public :: Set            => DimensionsWrapper6D_I1P_Set
        procedure, public :: Get            => DimensionsWrapper6D_I1P_Get
        procedure, public :: GetShape       => DimensionsWrapper6D_I1P_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper6D_I1P_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper6D_I1P_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper6D_I1P_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper6D_I1P_isOfDataType
        procedure, public :: Print          => DimensionsWrapper6D_I1P_Print
        procedure, public :: Free           => DimensionsWrapper6D_I1P_Free
        final             ::                   DimensionsWrapper6D_I1P_Final
    end type

    type, extends(DimensionsWrapper6D_t) :: DimensionsWrapper6D_I2P_t
        integer(I2P), allocatable :: Value(:,:,:,:,:,:)
    contains
    private
        procedure, public :: Set            => DimensionsWrapper6D_I2P_Set
        procedure, public :: Get            => DimensionsWrapper6D_I2P_Get
        procedure, public :: GetShape       => DimensionsWrapper6D_I2P_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper6D_I2P_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper6D_I2P_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper6D_I2P_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper6D_I2P_isOfDataType
        procedure, public :: Print          => DimensionsWrapper6D_I2P_Print
        procedure, public :: Free           => DimensionsWrapper6D_I2P_Free
        final             ::                   DimensionsWrapper6D_I2P_Final
    end type

    type, extends(DimensionsWrapper6D_t) :: DimensionsWrapper6D_I4P_t
        integer(I4P), allocatable :: Value(:,:,:,:,:,:)
    contains
    private
        procedure, public :: Set            => DimensionsWrapper6D_I4P_Set
        procedure, public :: Get            => DimensionsWrapper6D_I4P_Get
        procedure, public :: GetShape       => DimensionsWrapper6D_I4P_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper6D_I4P_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper6D_I4P_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper6D_I4P_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper6D_I4P_isOfDataType
        procedure, public :: Print          => DimensionsWrapper6D_I4P_Print
        procedure, public :: Free           => DimensionsWrapper6D_I4P_Free
        final             ::                   DimensionsWrapper6D_I4P_Final
    end type

    type, extends(DimensionsWrapper6D_t) :: DimensionsWrapper6D_I8P_t
        integer(I8P), allocatable :: Value(:,:,:,:,:,:)
    contains
    private
        procedure, public :: Set            => DimensionsWrapper6D_I8P_Set
        procedure, public :: Get            => DimensionsWrapper6D_I8P_Get
        procedure, public :: GetShape       => DimensionsWrapper6D_I8P_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper6D_I8P_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper6D_I8P_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper6D_I8P_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper6D_I8P_isOfDataType
        procedure, public :: Print          => DimensionsWrapper6D_I8P_Print
        procedure, public :: Free           => DimensionsWrapper6D_I8P_Free
        final             ::                   DimensionsWrapper6D_I8P_Final
    end type

    type, extends(DimensionsWrapper6D_t) :: DimensionsWrapper6D_L_t
        logical, allocatable :: Value(:,:,:,:,:,:)
    contains
    private
        procedure, public :: Set            => DimensionsWrapper6D_L_Set
        procedure, public :: Get            => DimensionsWrapper6D_L_Get
        procedure, public :: GetShape       => DimensionsWrapper6D_L_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper6D_L_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper6D_L_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper6D_L_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper6D_L_isOfDataType
        procedure, public :: Print          => DimensionsWrapper6D_L_Print
        procedure, public :: Free           => DimensionsWrapper6D_L_Free
        final             ::                   DimensionsWrapper6D_L_Final
    end type

    type, extends(DimensionsWrapper6D_t) :: DimensionsWrapper6D_R4P_t
        real(R4P), allocatable :: Value(:,:,:,:,:,:)
    contains
    private
        procedure, public :: Set            => DimensionsWrapper6D_R4P_Set
        procedure, public :: Get            => DimensionsWrapper6D_R4P_Get
        procedure, public :: GetShape       => DimensionsWrapper6D_R4P_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper6D_R4P_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper6D_R4P_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper6D_R4P_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper6D_R4P_isOfDataType
        procedure, public :: Print          => DimensionsWrapper6D_R4P_Print
        procedure, public :: Free           => DimensionsWrapper6D_R4P_Free
        final             ::                   DimensionsWrapper6D_R4P_Final
    end type

    type, extends(DimensionsWrapper6D_t) :: DimensionsWrapper6D_R8P_t
        real(R8P), allocatable :: Value(:,:,:,:,:,:)
    contains
    private
        procedure, public :: Set            => DimensionsWrapper6D_R8P_Set
        procedure, public :: Get            => DimensionsWrapper6D_R8P_Get
        procedure, public :: GetShape       => DimensionsWrapper6D_R8P_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper6D_R8P_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper6D_R8P_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper6D_R8P_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper6D_R8P_isOfDataType
        procedure, public :: Print          => DimensionsWrapper6D_R8P_Print
        procedure, public :: Free           => DimensionsWrapper6D_R8P_Free
        final             ::                   DimensionsWrapper6D_R8P_Final
    end type

    type, extends(DimensionsWrapper7D_t) :: DimensionsWrapper7D_DLCA_t
        character(len=:), allocatable :: Value(:,:,:,:,:,:,:)
    contains
    private
        procedure, public :: Set            => DimensionsWrapper7D_DLCA_Set
        procedure, public :: Get            => DimensionsWrapper7D_DLCA_Get
        procedure, public :: GetShape       => DimensionsWrapper7D_DLCA_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper7D_DLCA_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper7D_DLCA_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper7D_DLCA_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper7D_DLCA_isOfDataType
        procedure, public :: Print          => DimensionsWrapper7D_DLCA_Print
        procedure, public :: Free           => DimensionsWrapper7D_DLCA_Free
        final             ::                   DimensionsWrapper7D_DLCA_Final
    end type

    type, extends(DimensionsWrapper7D_t) :: DimensionsWrapper7D_I1P_t
        integer(I1P), allocatable :: Value(:,:,:,:,:,:,:)
    contains
    private
        procedure, public :: Set            => DimensionsWrapper7D_I1P_Set
        procedure, public :: Get            => DimensionsWrapper7D_I1P_Get
        procedure, public :: GetShape       => DimensionsWrapper7D_I1P_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper7D_I1P_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper7D_I1P_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper7D_I1P_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper7D_I1P_isOfDataType
        procedure, public :: Print          => DimensionsWrapper7D_I1P_Print
        procedure, public :: Free           => DimensionsWrapper7D_I1P_Free
        final             ::                   DimensionsWrapper7D_I1P_Final
    end type

    type, extends(DimensionsWrapper7D_t) :: DimensionsWrapper7D_I2P_t
        integer(I2P), allocatable :: Value(:,:,:,:,:,:,:)
    contains
    private
        procedure, public :: Set            => DimensionsWrapper7D_I2P_Set
        procedure, public :: Get            => DimensionsWrapper7D_I2P_Get
        procedure, public :: GetShape       => DimensionsWrapper7D_I2P_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper7D_I2P_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper7D_I2P_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper7D_I2P_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper7D_I2P_isOfDataType
        procedure, public :: Print          => DimensionsWrapper7D_I2P_Print
        procedure, public :: Free           => DimensionsWrapper7D_I2P_Free
        final             ::                   DimensionsWrapper7D_I2P_Final
    end type

    type, extends(DimensionsWrapper7D_t) :: DimensionsWrapper7D_I4P_t
        integer(I4P), allocatable :: Value(:,:,:,:,:,:,:)
    contains
    private
        procedure, public :: Set            => DimensionsWrapper7D_I4P_Set
        procedure, public :: Get            => DimensionsWrapper7D_I4P_Get
        procedure, public :: GetShape       => DimensionsWrapper7D_I4P_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper7D_I4P_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper7D_I4P_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper7D_I4P_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper7D_I4P_isOfDataType
        procedure, public :: Print          => DimensionsWrapper7D_I4P_Print
        procedure, public :: Free           => DimensionsWrapper7D_I4P_Free
        final             ::                   DimensionsWrapper7D_I4P_Final
    end type

    type, extends(DimensionsWrapper7D_t) :: DimensionsWrapper7D_I8P_t
        integer(I8P), allocatable :: Value(:,:,:,:,:,:,:)
    contains
    private
        procedure, public :: Set            => DimensionsWrapper7D_I8P_Set
        procedure, public :: Get            => DimensionsWrapper7D_I8P_Get
        procedure, public :: GetShape       => DimensionsWrapper7D_I8P_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper7D_I8P_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper7D_I8P_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper7D_I8P_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper7D_I8P_isOfDataType
        procedure, public :: Print          => DimensionsWrapper7D_I8P_Print
        procedure, public :: Free           => DimensionsWrapper7D_I8P_Free
        final             ::                   DimensionsWrapper7D_I8P_Final
    end type

    type, extends(DimensionsWrapper7D_t) :: DimensionsWrapper7D_L_t
        logical, allocatable :: Value(:,:,:,:,:,:,:)
    contains
    private
        procedure, public :: Set            => DimensionsWrapper7D_L_Set
        procedure, public :: Get            => DimensionsWrapper7D_L_Get
        procedure, public :: GetShape       => DimensionsWrapper7D_L_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper7D_L_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper7D_L_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper7D_L_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper7D_L_isOfDataType
        procedure, public :: Print          => DimensionsWrapper7D_L_Print
        procedure, public :: Free           => DimensionsWrapper7D_L_Free
        final             ::                   DimensionsWrapper7D_L_Final
    end type

    type, extends(DimensionsWrapper7D_t) :: DimensionsWrapper7D_R4P_t
        real(R4P), allocatable :: Value(:,:,:,:,:,:,:)
    contains
    private
        procedure, public :: Set            => DimensionsWrapper7D_R4P_Set
        procedure, public :: Get            => DimensionsWrapper7D_R4P_Get
        procedure, public :: GetShape       => DimensionsWrapper7D_R4P_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper7D_R4P_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper7D_R4P_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper7D_R4P_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper7D_R4P_isOfDataType
        procedure, public :: Print          => DimensionsWrapper7D_R4P_Print
        procedure, public :: Free           => DimensionsWrapper7D_R4P_Free
        final             ::                   DimensionsWrapper7D_R4P_Final
    end type

    type, extends(DimensionsWrapper7D_t) :: DimensionsWrapper7D_R8P_t
        real(R8P), allocatable :: Value(:,:,:,:,:,:,:)
    contains
    private
        procedure, public :: Set            => DimensionsWrapper7D_R8P_Set
        procedure, public :: Get            => DimensionsWrapper7D_R8P_Get
        procedure, public :: GetShape       => DimensionsWrapper7D_R8P_GetShape
        procedure, public :: GetPointer     => DimensionsWrapper7D_R8P_GetPointer
        procedure, public :: GetPolymorphic => DimensionsWrapper7D_R8P_GetPolymorphic
        procedure, public :: DataSizeInBytes=> DimensionsWrapper7D_R8P_DataSizeInBytes
        procedure, public :: isOfDataType   => DimensionsWrapper7D_R8P_isOfDataType
        procedure, public :: Print          => DimensionsWrapper7D_R8P_Print
        procedure, public :: Free           => DimensionsWrapper7D_R8P_Free
        final             ::                   DimensionsWrapper7D_R8P_Final
    end type

    abstract interface
        subroutine DimensionsWrapper_Free(this)
            import DimensionsWrapper_t
            class(DimensionsWrapper_t), intent(INOUT) :: this
        end subroutine

        function DimensionsWrapper_DataSizeInBytes(this) result(DataSizeInBytes)
            import DimensionsWrapper_t
            import I4P
            class(DimensionsWrapper_t), intent(IN) :: this
            integer(I4P)                           :: DataSizeInBytes
        end function

        function DimensionsWrapper_isOfDataType(this, Mold) result(isOfDataType)
            import DimensionsWrapper_t
            class(DimensionsWrapper_t), intent(IN) :: this
            class(*),                   intent(IN) :: Mold
            logical                                :: isOfDataType
        end function

        function DimensionsWrapper_GetShape(this) result(ValueShape)
            import DimensionsWrapper_t
            import I4P
            class(DimensionsWrapper_t), intent(IN) :: this
            integer(I4P), allocatable              :: ValueShape(:)
        end function

        subroutine DimensionsWrapper0D_Set(this, Value)
            import DimensionsWrapper0D_t
            class(DimensionsWrapper0D_t), intent(INOUT)  :: this
            class(*),                     intent(IN)     :: Value
        end subroutine

        subroutine DimensionsWrapper0D_Get(this, Value)
            import DimensionsWrapper0D_t
            class(DimensionsWrapper0D_t), intent(IN)  :: this
            class(*),                     intent(OUT) :: Value
        end subroutine

        function DimensionsWrapper0D_GetPointer(this) result(Value)
            import DimensionsWrapper0D_t
            class(DimensionsWrapper0D_t), target, intent(IN)  :: this
            class(*), pointer                                 :: Value
        end function

        subroutine DimensionsWrapper0D_GetPolymorphic(this, Value)
            import DimensionsWrapper0D_t
            class(DimensionsWrapper0D_t), intent(IN)  :: this
            class(*), allocatable,        intent(OUT) :: Value
        end subroutine

        subroutine DimensionsWrapper1D_Set(this, Value)
            import DimensionsWrapper1D_t
            class(DimensionsWrapper1D_t), intent(INOUT) :: this
            class(*),                     intent(IN)    :: Value(:)
        end subroutine

        subroutine DimensionsWrapper1D_Get(this, Value)
            import DimensionsWrapper1D_t
            class(DimensionsWrapper1D_t), intent(IN)  :: this
            class(*),                     intent(OUT) :: Value(:)
        end subroutine

        function DimensionsWrapper1D_GetPointer(this) result(Value)
            import DimensionsWrapper1D_t
            class(DimensionsWrapper1D_t), target, intent(IN)  :: this
            class(*), pointer                                 :: Value(:)
        end function

        subroutine DimensionsWrapper1D_GetPolymorphic(this, Value)
            import DimensionsWrapper1D_t
            class(DimensionsWrapper1D_t), intent(IN)  :: this
            class(*), allocatable,        intent(OUT) :: Value(:)
        end subroutine

        subroutine DimensionsWrapper2D_Set(this, Value)
            import DimensionsWrapper2D_t
            class(DimensionsWrapper2D_t), intent(INOUT) :: this
            class(*),                     intent(IN)    :: Value(:,:)
        end subroutine

        subroutine DimensionsWrapper2D_Get(this, Value)
            import DimensionsWrapper2D_t
            class(DimensionsWrapper2D_t), intent(IN)  :: this
            class(*),                     intent(OUT) :: Value(:,:)
        end subroutine

        function DimensionsWrapper2D_GetPointer(this) result(Value)
            import DimensionsWrapper2D_t
            class(DimensionsWrapper2D_t), target, intent(IN)  :: this
            class(*), pointer                                 :: Value(:,:)
        end function

        subroutine DimensionsWrapper2D_GetPolymorphic(this, Value)
            import DimensionsWrapper2D_t
            class(DimensionsWrapper2D_t), intent(IN)  :: this
            class(*), allocatable,        intent(OUT) :: Value(:,:)
        end subroutine

        subroutine DimensionsWrapper3D_Set(this, Value)
            import DimensionsWrapper3D_t
            class(DimensionsWrapper3D_t), intent(INOUT) :: this
            class(*),                     intent(IN)    :: Value(:,:,:)
        end subroutine

        subroutine DimensionsWrapper3D_Get(this, Value)
            import DimensionsWrapper3D_t
            class(DimensionsWrapper3D_t), intent(IN)  :: this
            class(*),                     intent(OUT) :: Value(:,:,:)
        end subroutine

        function DimensionsWrapper3D_GetPointer(this) result(Value)
            import DimensionsWrapper3D_t
            class(DimensionsWrapper3D_t), target, intent(IN)  :: this
            class(*), pointer                                 :: Value(:,:,:)
        end function

        subroutine DimensionsWrapper3D_GetPolymorphic(this, Value)
            import DimensionsWrapper3D_t
            class(DimensionsWrapper3D_t), intent(IN)  :: this
            class(*), allocatable,        intent(OUT) :: Value(:,:,:)
        end subroutine

        subroutine DimensionsWrapper4D_Set(this, Value)
            import DimensionsWrapper4D_t
            class(DimensionsWrapper4D_t), intent(INOUT) :: this
            class(*),                     intent(IN)    :: Value(:,:,:,:)
        end subroutine

        subroutine DimensionsWrapper4D_Get(this, Value)
            import DimensionsWrapper4D_t
            class(DimensionsWrapper4D_t), intent(IN)  :: this
            class(*),                     intent(OUT) :: Value(:,:,:,:)
        end subroutine

        function DimensionsWrapper4D_GetPointer(this) result(Value)
            import DimensionsWrapper4D_t
            class(DimensionsWrapper4D_t), target, intent(IN)  :: this
            class(*), pointer                                 :: Value(:,:,:,:)
        end function

        subroutine DimensionsWrapper4D_GetPolymorphic(this, Value)
            import DimensionsWrapper4D_t
            class(DimensionsWrapper4D_t), intent(IN)  :: this
            class(*), allocatable,        intent(OUT) :: Value(:,:,:,:)
        end subroutine

        subroutine DimensionsWrapper5D_Set(this, Value)
            import DimensionsWrapper5D_t
            class(DimensionsWrapper5D_t), intent(INOUT) :: this
            class(*),                     intent(IN)    :: Value(:,:,:,:,:)
        end subroutine

        subroutine DimensionsWrapper5D_Get(this, Value)
            import DimensionsWrapper5D_t
            class(DimensionsWrapper5D_t), intent(IN)  :: this
            class(*),                     intent(OUT) :: Value(:,:,:,:,:)
        end subroutine

        function DimensionsWrapper5D_GetPointer(this) result(Value)
            import DimensionsWrapper5D_t
            class(DimensionsWrapper5D_t), target, intent(IN)  :: this
            class(*), pointer                                 :: Value(:,:,:,:,:)
        end function

        subroutine DimensionsWrapper5D_GetPolymorphic(this, Value)
            import DimensionsWrapper5D_t
            class(DimensionsWrapper5D_t), intent(IN)  :: this
            class(*), allocatable,        intent(OUT) :: Value(:,:,:,:,:)
        end subroutine

        subroutine DimensionsWrapper6D_Set(this, Value)
            import DimensionsWrapper6D_t
            class(DimensionsWrapper6D_t), intent(INOUT) :: this
            class(*),                     intent(IN)    :: Value(:,:,:,:,:,:)
        end subroutine

        subroutine DimensionsWrapper6D_Get(this, Value)
            import DimensionsWrapper6D_t
            class(DimensionsWrapper6D_t), intent(IN)  :: this
            class(*),                     intent(OUT) :: Value(:,:,:,:,:,:)
        end subroutine

        function DimensionsWrapper6D_GetPointer(this) result(Value)
            import DimensionsWrapper6D_t
            class(DimensionsWrapper6D_t), target, intent(IN)  :: this
            class(*), pointer                                 :: Value(:,:,:,:,:,:)
        end function

        subroutine DimensionsWrapper6D_GetPolymorphic(this, Value)
            import DimensionsWrapper6D_t
            class(DimensionsWrapper6D_t), intent(IN)  :: this
            class(*), allocatable,        intent(OUT) :: Value(:,:,:,:,:,:)
        end subroutine

        subroutine DimensionsWrapper7D_Set(this, Value)
            import DimensionsWrapper7D_t
            class(DimensionsWrapper7D_t), intent(INOUT) :: this
            class(*),                     intent(IN)    :: Value(:,:,:,:,:,:,:)
        end subroutine

        subroutine DimensionsWrapper7D_Get(this, Value)
            import DimensionsWrapper7D_t
            class(DimensionsWrapper7D_t), intent(IN)  :: this
            class(*),                     intent(OUT) :: Value(:,:,:,:,:,:,:)
        end subroutine

        function DimensionsWrapper7D_GetPointer(this) result(Value)
            import DimensionsWrapper7D_t
            class(DimensionsWrapper7D_t), target, intent(IN)  :: this
            class(*), pointer                                 :: Value(:,:,:,:,:,:,:)
        end function

        subroutine DimensionsWrapper7D_GetPolymorphic(this, Value)
            import DimensionsWrapper7D_t
            class(DimensionsWrapper7D_t), intent(IN)  :: this
            class(*), allocatable,        intent(OUT) :: Value(:,:,:,:,:,:,:)
        end subroutine
    end interface

public :: DimensionsWrapper_t

public :: DimensionsWrapper0D_t
public :: DimensionsWrapper1D_t
public :: DimensionsWrapper2D_t
public :: DimensionsWrapper3D_t
public :: DimensionsWrapper4D_t
public :: DimensionsWrapper5D_t
public :: DimensionsWrapper6D_t
public :: DimensionsWrapper7D_t

public :: DimensionsWrapper0D_DLCA_t
public :: DimensionsWrapper0D_L_t
public :: DimensionsWrapper0D_I1P_t
public :: DimensionsWrapper0D_I2P_t
public :: DimensionsWrapper0D_I4P_t
public :: DimensionsWrapper0D_I8P_t
public :: DimensionsWrapper0D_R4P_t
public :: DimensionsWrapper0D_R8P_t

public :: DimensionsWrapper1D_DLCA_t
public :: DimensionsWrapper1D_L_t
public :: DimensionsWrapper1D_I1P_t
public :: DimensionsWrapper1D_I2P_t
public :: DimensionsWrapper1D_I4P_t
public :: DimensionsWrapper1D_I8P_t
public :: DimensionsWrapper1D_R4P_t
public :: DimensionsWrapper1D_R8P_t

public :: DimensionsWrapper2D_DLCA_t
public :: DimensionsWrapper2D_L_t
public :: DimensionsWrapper2D_I1P_t
public :: DimensionsWrapper2D_I2P_t
public :: DimensionsWrapper2D_I4P_t
public :: DimensionsWrapper2D_I8P_t
public :: DimensionsWrapper2D_R4P_t
public :: DimensionsWrapper2D_R8P_t

public :: DimensionsWrapper3D_DLCA_t
public :: DimensionsWrapper3D_L_t
public :: DimensionsWrapper3D_I1P_t
public :: DimensionsWrapper3D_I2P_t
public :: DimensionsWrapper3D_I4P_t
public :: DimensionsWrapper3D_I8P_t
public :: DimensionsWrapper3D_R4P_t
public :: DimensionsWrapper3D_R8P_t

public :: DimensionsWrapper4D_DLCA_t
public :: DimensionsWrapper4D_L_t
public :: DimensionsWrapper4D_I1P_t
public :: DimensionsWrapper4D_I2P_t
public :: DimensionsWrapper4D_I4P_t
public :: DimensionsWrapper4D_I8P_t
public :: DimensionsWrapper4D_R4P_t
public :: DimensionsWrapper4D_R8P_t

public :: DimensionsWrapper5D_DLCA_t
public :: DimensionsWrapper5D_L_t
public :: DimensionsWrapper5D_I1P_t
public :: DimensionsWrapper5D_I2P_t
public :: DimensionsWrapper5D_I4P_t
public :: DimensionsWrapper5D_I8P_t
public :: DimensionsWrapper5D_R4P_t
public :: DimensionsWrapper5D_R8P_t

public :: DimensionsWrapper6D_DLCA_t
public :: DimensionsWrapper6D_L_t
public :: DimensionsWrapper6D_I1P_t
public :: DimensionsWrapper6D_I2P_t
public :: DimensionsWrapper6D_I4P_t
public :: DimensionsWrapper6D_I8P_t
public :: DimensionsWrapper6D_R4P_t
public :: DimensionsWrapper6D_R8P_t

public :: DimensionsWrapper7D_DLCA_t
public :: DimensionsWrapper7D_L_t
public :: DimensionsWrapper7D_I1P_t
public :: DimensionsWrapper7D_I2P_t
public :: DimensionsWrapper7D_I4P_t
public :: DimensionsWrapper7D_I8P_t
public :: DimensionsWrapper7D_R4P_t
public :: DimensionsWrapper7D_R8P_t



contains

    subroutine DimensionsWrapper_SetDimensions(this, Dimensions)
    !-----------------------------------------------------------------
    !< Set the dimensions of the Value contained in the wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper_t), intent(INOUT) :: this
        integer(I1P),               intent(IN)    :: Dimensions
    !-----------------------------------------------------------------
        this%Dimensions = Dimensions
    end subroutine


    function DimensionsWrapper_GetDimensions(this) result(Dimensions)
    !-----------------------------------------------------------------
    !< Get the dimensions of the Value contained in the wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper_t), intent(IN) :: this
    !-----------------------------------------------------------------
        integer(I1P)                              :: Dimensions
        Dimensions = this%Dimensions
    end function


    subroutine DimensionsWrapper_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Generic Wrapper Print
    !-----------------------------------------------------------------
        class(DimensionsWrapper_t),       intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = -, '//&
                            ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper_Print

    subroutine DimensionsWrapper0D_DLCA_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper0D
    !-----------------------------------------------------------------
        type(DimensionsWrapper0D_DLCA_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper0D_DLCA_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set DLCA Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_DLCA_t), intent(INOUT) :: this
        class(*),                          intent(IN)    :: Value
        integer                                          :: err
    !-----------------------------------------------------------------
        select type (Value)
            type is (character(len=*))
                this%Value = Value
            class Default
                call msg%Warn(txt='Setting value: Expected data type (character(*))',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    subroutine DimensionsWrapper0D_DLCA_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get deferred length character array Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_DLCA_t), intent(IN)  :: this
        class(*),                          intent(OUT) :: Value
    !-----------------------------------------------------------------
        select type (Value)
            type is (character(len=*))
                Value = this%Value
            class Default
                call msg%Warn(txt='Getting value: Expected data type (character(*))',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    function DimensionsWrapper0D_DLCA_GetShape(this)  result(ValueShape)
    !-----------------------------------------------------------------
    !< Return the shape of the Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_DLCA_t), intent(IN)  :: this
        integer(I4P), allocatable                      :: ValueShape(:)
    !-----------------------------------------------------------------
		allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper0D_DLCA_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic pointer to Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_DLCA_t), target, intent(IN)  :: this
        class(*), pointer                                      :: Value
    !-----------------------------------------------------------------
        Value => this%Value
    end function


    subroutine DimensionsWrapper0D_DLCA_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_DLCA_t), intent(IN)  :: this
        class(*), allocatable,             intent(OUT) :: Value
    !-----------------------------------------------------------------
        allocate(Value, source = this%Value)
    end subroutine


    subroutine DimensionsWrapper0D_DLCA_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper0D
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_DLCA_t), intent(INOUT) :: this
        integer                                          :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    subroutine DimensionsWrapper0D_DLCA_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_DLCA_t), intent(IN)  :: this        !< DimensionsWrapper
        integer(I4P),                      intent(IN)  :: unit        !< Logic unit.
        character(*), optional,            intent(IN)  :: prefix      !< Prefixing string.
        integer(I4P), optional,            intent(OUT) :: iostat      !< IO error.
        character(*), optional,            intent(OUT) :: iomsg       !< IO error message.
        character(len=:), allocatable                  :: prefd       !< Prefixing string.
        integer(I4P)                                   :: iostatd     !< IO error.
        character(500)                                 :: iomsgd      !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = DLCA'//&
                            ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                            ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                            ', Value = '//trim(this%Value)
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper0D_DLCA_Print


    function DimensionsWrapper0D_DLCA_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the size of the data stored in bytes
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_DLCA_t), intent(IN) :: this            !< Dimensions wrapper 0D
        integer(I4P)                                  :: DataSizeInBytes !< Data size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = 0
        if(allocated(this%value)) DataSizeInBytes = byte_size(this%value)
    end function DimensionsWrapper0D_DLCA_DataSizeInBytes


    function DimensionsWrapper0D_DLCA_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_DLCA_t), intent(IN) :: this         !< Dimensions wrapper 0D
        class(*),                          intent(IN) :: Mold         !< Mold for data type comparison
        logical                                  :: isOfDataType      !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (character(len=*))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper0D_DLCA_isOfDataType


    subroutine DimensionsWrapper0D_I1P_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper0D
    !-----------------------------------------------------------------
        type(DimensionsWrapper0D_I1P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper0D_I1P_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set I1P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_I1P_t), intent(INOUT) :: this
        class(*),                         intent(IN)    :: Value
        integer                                         :: err
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I1P))
                allocate(this%Value, stat=err)
                this%Value = Value
                if(err/=0) &
                    call msg%Error(txt='Setting Value: Allocation error ('//&
                                   str(no_sign=.true.,n=err)//')', &
                                   file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn(txt='Setting value: Expected data type (I1P)',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    subroutine DimensionsWrapper0D_I1P_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get I1P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_I1P_t), intent(IN)  :: this
        class(*),                         intent(OUT) :: Value
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I1P))
                Value = this%Value
            class Default
                call msg%Warn(txt='Getting value: Expected data type (I1P)',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine

    function DimensionsWrapper0D_I1P_GetShape(this)  result(ValueShape)
    !-----------------------------------------------------------------
    !< Return the shape of the Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_I1P_t), intent(IN)  :: this
        integer(I4P), allocatable                     :: ValueShape(:)
    !-----------------------------------------------------------------
		allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper0D_I1P_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic pointer to Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_I1P_t), target, intent(IN)  :: this
        class(*), pointer                                     :: Value
    !-----------------------------------------------------------------
        Value => this%Value
    end function


    subroutine DimensionsWrapper0D_I1P_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_I1P_t), intent(IN)  :: this
        class(*), allocatable,            intent(OUT) :: Value
    !-----------------------------------------------------------------
        allocate(Value, source = this%Value)
    end subroutine


    subroutine DimensionsWrapper0D_I1P_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper0D
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_I1P_t), intent(INOUT) :: this
        integer                                         :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    function DimensionsWrapper0D_I1P_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the size of the data stored in bytes
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_I1P_t), intent(IN) :: this            !< Dimensions wrapper 0D
        integer(I4P)                                  :: DataSizeInBytes !< Data size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = byte_size(this%value)
    end function DimensionsWrapper0D_I1P_DataSizeInBytes


    function DimensionsWrapper0D_I1P_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_I1P_t), intent(IN) :: this          !< Dimensions wrapper 0D
        class(*),                         intent(IN) :: Mold          !< Mold for data type comparison
        logical                                      :: isOfDataType  !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (integer(I1P))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper0D_I1P_isOfDataType


    subroutine DimensionsWrapper0D_I1P_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_I1P_t), intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = I1P'//&
                            ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                            ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                            ', Value = '//str(no_sign=.true., n=this%Value)
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper0D_I1P_Print


    subroutine DimensionsWrapper0D_I2P_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper0D
    !-----------------------------------------------------------------
        type(DimensionsWrapper0D_I2P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper0D_I2P_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set I2P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_I2P_t), intent(INOUT) :: this
        class(*),                         intent(IN)    :: Value
        integer                                         :: err
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I2P))
                allocate(this%Value, stat=err)
                this%Value = Value
                if(err/=0) call msg%Error(txt='Setting Value: Allocation error ('// &
                                          str(no_sign=.true.,n=err)//')',           &
                                          file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn(txt='Setting value: Expected data type (I2P)', &
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    subroutine DimensionsWrapper0D_I2P_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get I2P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_I2P_t), intent(IN)  :: this
        class(*),                         intent(OUT) :: Value
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I2P))
                Value = this%Value
            class Default
                call msg%Warn(txt='Getting value: Expected data type (I2P)', &
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    function DimensionsWrapper0D_I2P_GetShape(this)  result(ValueShape)
    !-----------------------------------------------------------------
    !< Return the shape of the Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_I2P_t), intent(IN)  :: this
        integer(I4P), allocatable                     :: ValueShape(:)
    !-----------------------------------------------------------------
		allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper0D_I2P_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic pointer to Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_I2P_t), target, intent(IN)  :: this
        class(*), pointer                                     :: Value
    !-----------------------------------------------------------------
        Value => this%Value
    end function


    subroutine DimensionsWrapper0D_I2P_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_I2P_t), intent(IN)  :: this
        class(*), allocatable,            intent(OUT) :: Value
    !-----------------------------------------------------------------
        allocate(Value, source = this%Value)
    end subroutine


    subroutine DimensionsWrapper0D_I2P_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper0D
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_I2P_t), intent(INOUT) :: this
        integer                                         :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    function DimensionsWrapper0D_I2P_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the size of the data stored in bytes
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_I2P_t), intent(IN) :: this            !< Dimensions wrapper 0D
        integer(I4P)                                 :: DataSizeInBytes !< Data size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = byte_size(this%value)
    end function DimensionsWrapper0D_I2P_DataSizeInBytes


    function DimensionsWrapper0D_I2P_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_I2P_t), intent(IN) :: this          !< Dimensions wrapper 0D
        class(*),                         intent(IN) :: Mold          !< Mold for data type comparison
        logical                                      :: isOfDataType  !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (integer(I2P))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper0D_I2P_isOfDataType


    subroutine DimensionsWrapper0D_I2P_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_I2P_t), intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = I2P'//&
                            ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                            ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                            ', Value = '//str(no_sign=.true., n=this%Value)
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper0D_I2P_Print


    subroutine DimensionsWrapper0D_I4P_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper0D
    !-----------------------------------------------------------------
        type(DimensionsWrapper0D_I4P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper0D_I4P_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set I4P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_I4P_t), intent(INOUT) :: this
        class(*),                         intent(IN)    :: Value
        integer                                         :: err
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I4P))
                allocate(this%Value, stat=err)
                this%Value = Value
                if(err/=0) call msg%Error(txt='Setting Value: Allocation error ('// &
                                          str(no_sign=.true.,n=err)//')',           &
                                          file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn(txt='Setting value: Expected data type (I4P)', &
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    subroutine DimensionsWrapper0D_I4P_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get I4P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_I4P_t), intent(IN)  :: this
        class(*),                         intent(OUT) :: Value
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I4P))
                Value = this%Value
            class Default
                call msg%Warn(txt='Getting value: Expected data type (I4P)', &
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    function DimensionsWrapper0D_I4P_GetShape(this)  result(ValueShape)
    !-----------------------------------------------------------------
    !< Return the shape of the Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_I4P_t), intent(IN)  :: this
        integer(I4P), allocatable                     :: ValueShape(:)
    !-----------------------------------------------------------------
		allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper0D_I4P_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic pointer to Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_I4P_t), target, intent(IN)  :: this
        class(*), pointer                                     :: Value
    !-----------------------------------------------------------------
        Value => this%Value
    end function


    subroutine DimensionsWrapper0D_I4P_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_I4P_t), intent(IN)  :: this
        class(*), allocatable,            intent(OUT) :: Value
    !-----------------------------------------------------------------
        allocate(Value, source = this%Value)
    end subroutine


    subroutine DimensionsWrapper0D_I4P_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper0D
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_I4P_t), intent(INOUT) :: this
        integer                                         :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    function DimensionsWrapper0D_I4P_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the size of the data stored in bytes
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_I4P_t), intent(IN) :: this            !< Dimensions wrapper 0D
        integer(I4P)                                 :: DataSizeInBytes !< Data size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = byte_size(this%value)
    end function DimensionsWrapper0D_I4P_DataSizeInBytes


    function DimensionsWrapper0D_I4P_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_I4P_t), intent(IN) :: this           !< Dimensions wrapper 0D
        class(*),                         intent(IN) :: Mold           !< Mold for data type comparison
        logical                                      :: isOfDataType   !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (integer(I4P))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper0D_I4P_isOfDataType

    subroutine DimensionsWrapper0D_I4P_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_I4P_t), intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = I4P'//&
                            ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                            ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                            ', Value = '//str(no_sign=.true., n=this%Value)
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper0D_I4P_Print

    subroutine DimensionsWrapper0D_I8P_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper0D
    !-----------------------------------------------------------------
        type(DimensionsWrapper0D_I8P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper0D_I8P_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set I8P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_I8P_t), intent(INOUT) :: this
        class(*),                         intent(IN)    :: Value
        integer                                         :: err
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I8P))
                allocate(this%Value, stat=err)
                this%Value = Value
                if(err/=0) call msg%Error(txt='Setting Value: Allocation error ('// &
                                          str(no_sign=.true.,n=err)//')',           &
                                          file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn(txt='Setting value: Expected data type (I8P)', &
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    subroutine DimensionsWrapper0D_I8P_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get I8P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_I8P_t), intent(IN)  :: this
        class(*),                         intent(OUT) :: Value
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I8P))
                Value = this%Value
            class Default
                call msg%Warn(txt='Getting value: Expected data type (I8P)', &
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    function DimensionsWrapper0D_I8P_GetShape(this)  result(ValueShape)
    !-----------------------------------------------------------------
    !< Return the shape of the Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_I8P_t), intent(IN)  :: this
        integer(I4P), allocatable                     :: ValueShape(:)
    !-----------------------------------------------------------------
		allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper0D_I8P_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic pointer to Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_I8P_t), target, intent(IN)  :: this
        class(*), pointer                                     :: Value
    !-----------------------------------------------------------------
        Value => this%Value
    end function


    subroutine DimensionsWrapper0D_I8P_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_I8P_t), intent(IN)  :: this
        class(*), allocatable,            intent(OUT) :: Value
    !-----------------------------------------------------------------
        allocate(Value, source = this%Value)
    end subroutine


    subroutine DimensionsWrapper0D_I8P_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper0D
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_I8P_t), intent(INOUT) :: this
        integer                                         :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    function DimensionsWrapper0D_I8P_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the size of the data stored in bytes
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_I8P_t), intent(IN) :: this            !< Dimensions wrapper 0D
        integer(I4P)                                 :: DataSizeInBytes !< Data size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = byte_size(this%value)
    end function DimensionsWrapper0D_I8P_DataSizeInBytes


    function DimensionsWrapper0D_I8P_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_I8P_t), intent(IN) :: this          !< Dimensions wrapper 0D
        class(*),                         intent(IN) :: Mold          !< Mold for data type comparison
        logical                                      :: isOfDataType  !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (integer(I8P))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper0D_I8P_isOfDataType


    subroutine DimensionsWrapper0D_I8P_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_I8P_t), intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = I8P'//&
                            ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                            ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                            ', Value = '//str(no_sign=.true., n=this%Value)
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper0D_I8P_Print


    subroutine DimensionsWrapper0D_L_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper0D
    !-----------------------------------------------------------------
        type(DimensionsWrapper0D_L_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper0D_L_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set logical Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_L_t), intent(INOUT) :: this
        class(*),                       intent(IN)    :: Value
        integer                                       :: err
    !-----------------------------------------------------------------
        select type (Value)
            type is (logical)
                allocate(this%Value, stat=err)
                this%Value = Value
                if(err/=0) call msg%Error(txt='Setting Value: Allocation error ('// &
                                          str(no_sign=.true.,n=err)//')', &
                                          file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn(txt='Setting value: Expected data type (logical)', &
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    subroutine DimensionsWrapper0D_L_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get logical Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_L_t), intent(IN)  :: this
        class(*),                       intent(OUT) :: Value
    !-----------------------------------------------------------------
        select type (Value)
            type is (logical)
                Value = this%Value
            class Default
                call msg%Warn(txt='Getting value: Expected data type (logical)', &
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    function DimensionsWrapper0D_L_GetShape(this)  result(ValueShape)
    !-----------------------------------------------------------------
    !< Return the shape of the Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_L_t), intent(IN)  :: this
        integer(I4P), allocatable                   :: ValueShape(:)
    !-----------------------------------------------------------------
1		allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper0D_L_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic pointer to Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_L_t), target, intent(IN)  :: this
        class(*), pointer                                   :: Value
    !-----------------------------------------------------------------
        Value => this%Value
    end function


    subroutine DimensionsWrapper0D_L_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_L_t), intent(IN)  :: this
        class(*), allocatable,          intent(OUT) :: Value
    !-----------------------------------------------------------------
        allocate(Value, source = this%Value)
    end subroutine


    subroutine DimensionsWrapper0D_L_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper0D
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_L_t), intent(INOUT) :: this
        integer                                       :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    function DimensionsWrapper0D_L_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the size of the data stored in bytes
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_L_t), intent(IN) :: this            !< Dimensions wrapper 0D
        integer(I4P)                               :: DataSizeInBytes !< Data size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = byte_size_logical(this%value)
    end function DimensionsWrapper0D_L_DataSizeInBytes


    function DimensionsWrapper0D_L_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_L_t), intent(IN) :: this            !< Dimensions wrapper 0D
        class(*),                       intent(IN) :: Mold            !< Mold for data type comparison
        logical                                    :: isOfDataType    !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (logical)
                isOfDataType = .true.
        end select
    end function DimensionsWrapper0D_L_isOfDataType


    subroutine DimensionsWrapper0D_L_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_L_t),   intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = L'//&
                            ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                            ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                            ', Value = '//str(n=this%Value)
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper0D_L_Print


    subroutine DimensionsWrapper0D_R4P_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper0D
    !-----------------------------------------------------------------
        type(DimensionsWrapper0D_R4P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper0D_R4P_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set R4P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_R4P_t), intent(INOUT) :: this
        class(*),                         intent(IN)    :: Value
        integer                                         :: err
    !-----------------------------------------------------------------
        select type (Value)
            type is (real(R4P))
                allocate(this%Value, stat=err)
                this%Value = Value
                if(err/=0) call msg%Error(txt='Setting Value: Allocation error ('// &
                                          str(no_sign=.true.,n=err)//')',           &
                                          file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn(txt='Setting value: Expected data type (R4P)', &
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    subroutine DimensionsWrapper0D_R4P_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get R4P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_R4P_t), intent(IN)  :: this
        class(*),                         intent(OUT) :: Value
    !-----------------------------------------------------------------
        select type (Value)
            type is (real(R4P))
                Value = this%Value
            class Default
                call msg%Warn(txt='Getting value: Expected data type (R4P)', &
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    function DimensionsWrapper0D_R4P_GetShape(this)  result(ValueShape)
    !-----------------------------------------------------------------
    !< Return the shape of the Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_R4P_t), intent(IN)  :: this
        integer(I4P), allocatable                     :: ValueShape(:)
    !-----------------------------------------------------------------
		allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper0D_R4P_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic pointer to Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_R4P_t), target, intent(IN)  :: this
        class(*), pointer                                     :: Value
    !-----------------------------------------------------------------
        Value => this%Value
    end function


    subroutine DimensionsWrapper0D_R4P_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_R4P_t), intent(IN)  :: this
        class(*), allocatable,            intent(OUT) :: Value
    !-----------------------------------------------------------------
        allocate(Value, source = this%Value)
    end subroutine


    subroutine DimensionsWrapper0D_R4P_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper0D
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_R4P_t), intent(INOUT) :: this
        integer                                         :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    function DimensionsWrapper0D_R4P_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the size of the data stored in bytes
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_R4P_t), intent(IN) :: this            !< Dimensions wrapper 0D
        integer(I4P)                                 :: DataSizeInBytes !< Data size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = byte_size(this%value)
    end function DimensionsWrapper0D_R4P_DataSizeInBytes


    function DimensionsWrapper0D_R4P_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_R4P_t), intent(IN) :: this          !< Dimensions wrapper 0D
        class(*),                         intent(IN) :: Mold          !< Mold for data type comparison
        logical                                      :: isOfDataType  !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (real(R4P))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper0D_R4P_isOfDataType


    subroutine DimensionsWrapper0D_R4P_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_R4P_t), intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = R4P'//&
                            ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                            ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                            ', Value = '//str(no_sign=.true., n=this%Value)
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper0D_R4P_Print


    subroutine DimensionsWrapper0D_R8P_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper0D
    !-----------------------------------------------------------------
        type(DimensionsWrapper0D_R8P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper0D_R8P_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set R8P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_R8P_t), intent(INOUT) :: this
        class(*),                         intent(IN)    :: Value
        integer                                         :: err
    !-----------------------------------------------------------------
        select type (Value)
            type is (real(R8P))
                allocate(this%Value, stat=err)
                this%Value = Value
                if(err/=0) call msg%Error(txt='Setting Value: Allocation error ('// &
                                          str(no_sign=.true.,n=err)//')',           &
                                          file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn(txt='Setting value: Expected data type (R8P)', &
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    subroutine DimensionsWrapper0D_R8P_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get R8P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_R8P_t), intent(IN)  :: this
        class(*),                         intent(OUT) :: Value
    !-----------------------------------------------------------------
        select type (Value)
            type is (real(R8P))
                Value = this%Value
            class Default
                call msg%Warn(txt='Getting value: Expected data type (R8P)', &
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    function DimensionsWrapper0D_R8P_GetShape(this)  result(ValueShape)
    !-----------------------------------------------------------------
    !< Return the shape of the Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_R8P_t), intent(IN)  :: this
        integer(I4P), allocatable                     :: ValueShape(:)
    !-----------------------------------------------------------------
		allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper0D_R8P_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic pointer to Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_R8P_t), target, intent(IN)  :: this
        class(*), pointer                                     :: Value
    !-----------------------------------------------------------------
        Value => this%Value
    end function


    subroutine DimensionsWrapper0D_R8P_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_R8P_t), intent(IN)  :: this
        class(*), allocatable,            intent(OUT) :: Value
    !-----------------------------------------------------------------
        allocate(Value, source = this%Value)
    end subroutine


    subroutine DimensionsWrapper0D_R8P_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper0D
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_R8P_t), intent(INOUT) :: this
        integer                                         :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    function DimensionsWrapper0D_R8P_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the size of the data stored in bytes
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_R8P_t), intent(IN) :: this            !< Dimensions wrapper 0D
        integer(I4P)                                 :: DataSizeInBytes !< Data size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = byte_size(this%value)
    end function DimensionsWrapper0D_R8P_DataSizeInBytes


    function DimensionsWrapper0D_R8P_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_R8P_t), intent(IN) :: this          !< Dimensions wrapper 0D
        class(*),                         intent(IN) :: Mold          !< Mold for data type comparison
        logical                                      :: isOfDataType  !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (real(R8P))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper0D_R8P_isOfDataType


    subroutine DimensionsWrapper0D_R8P_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper0D_R8P_t), intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = R8P'//&
                            ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                            ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                            ', Value = '//str(no_sign=.true., n=this%Value)
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper0D_R8P_Print


    subroutine DimensionsWrapper1D_DLCA_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper1D
    !-----------------------------------------------------------------
        type(DimensionsWrapper1D_DLCA_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper1D_DLCA_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set DLCA Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_DLCA_t), intent(INOUT) :: this
        class(*),                          intent(IN)    :: Value(:)
        integer                                          :: err
    !-----------------------------------------------------------------
#ifdef __GFORTRAN__ 
        call msg%Warn(txt='Setting value: Array of deferred length allocatable arrays not supported in Gfortran)',&
                      file=__FILE__, line=__LINE__ )
#else   
        select type (Value)
            type is (character(len=*))


                allocate(character(len=len(Value))::this%Value(size(Value,dim=1)), stat=err)
                this%Value = Value
                if(err/=0) call msg%Error(txt='Setting Value: Allocation error ('// &
                                          str(no_sign=.true.,n=err)//')',           &
                                          file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn(txt='Setting value: Expected data type (character(*))',&
                              file=__FILE__, line=__LINE__ )
        end select
#endif
    end subroutine


    subroutine DimensionsWrapper1D_DLCA_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get deferred length character array Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_DLCA_t), intent(IN)    :: this
        class(*),                          intent(OUT) :: Value(:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (character(len=*))
                if(all(this%GetShape() == shape(Value))) then
                    Value = this%Value
                else
                    call msg%Warn(txt='Getting value: Expected shape ('//&
                                  str(no_sign=.true.,n=this%GetShape())//')',&
                                  file=__FILE__, line=__LINE__ )
                endif
            class Default
                call msg%Warn(txt='Getting value: Expected data type (character(*))',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    function DimensionsWrapper1D_DLCA_GetShape(this) result(ValueShape)
    !-----------------------------------------------------------------
    !< Get Wrapper Value Shape
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_DLCA_t), intent(IN) :: this
        integer(I4P), allocatable                     :: ValueShape(:)
    !-----------------------------------------------------------------
		allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value, kind=I4P)
    end function


    function DimensionsWrapper1D_DLCA_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_DLCA_t), target, intent(IN)  :: this
        class(*), pointer                                      :: Value(:)
    !-----------------------------------------------------------------
        Value => this%value
    end function


    subroutine DimensionsWrapper1D_DLCA_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_DLCA_t), intent(IN)  :: this
        class(*), allocatable,             intent(OUT) :: Value(:)
    !-----------------------------------------------------------------
!        allocate(Value(size(this%Value,dim=1)),source=this%Value)
    end subroutine


    subroutine DimensionsWrapper1D_DLCA_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper1D
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_DLCA_t), intent(INOUT) :: this
        integer                                         :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    function DimensionsWrapper1D_DLCA_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the size of the data stored in bytes
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_DLCA_t), intent(IN) :: this            !< Dimensions wrapper 0D
        integer(I4P)                                  :: DataSizeInBytes !< Data size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = 0
        if(allocated(this%value)) DataSizeInBytes = byte_size(this%value(1))*size(this%value)
    end function DimensionsWrapper1D_DLCA_DataSizeInBytes


    function DimensionsWrapper1D_DLCA_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_DLCA_t), intent(IN) :: this         !< Dimensions wrapper 1D
        class(*),                          intent(IN) :: Mold         !< Mold for data type comparison
        logical                                  :: isOfDataType      !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (character(len=*))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper1D_DLCA_isOfDataType


    subroutine DimensionsWrapper1D_DLCA_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_DLCA_t),intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A,$)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = DLCA'//&
                        ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                        ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                        ', Value = '
        write(unit=unit,fmt=*,iostat=iostatd,iomsg=iomsgd) this%Value

        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper1D_DLCA_Print


    subroutine DimensionsWrapper1D_I1P_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper1D
    !-----------------------------------------------------------------
        type(DimensionsWrapper1D_I1P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper1D_I1P_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set I1P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_I1P_t), intent(INOUT) :: this
        class(*),                         intent(IN)    :: Value(:)
        integer                                         :: err
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I1P))
                allocate(this%Value(size(Value,dim=1)), &
                                    stat=err)
                this%Value = Value
                if(err/=0) &
                    call msg%Error( txt='Setting Value: Allocation error ('//&
                                    str(no_sign=.true.,n=err)//')', &
                                    file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn( txt='Setting value: Expected data type (I1P)', &
                               file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    subroutine DimensionsWrapper1D_I1P_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get I1P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_I1P_t), intent(IN)  :: this
        class(*),                         intent(OUT) :: Value(:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I1P))
                if(all(this%GetShape() == shape(Value))) then
                    Value = this%Value
                else
                    call msg%Warn(txt='Getting value: Expected shape ('//    &
                                  str(no_sign=.true.,n=this%GetShape())//')',&
                                  file=__FILE__, line=__LINE__ )
                endif
            class Default
                call msg%Warn(txt='Getting value: Expected data type (I1P)',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    function DimensionsWrapper1D_I1P_GetShape(this) result(ValueShape)
    !-----------------------------------------------------------------
    !< Get Wrapper Value Shape
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_I1P_t), intent(IN) :: this
        integer(I4P), allocatable                    :: ValueShape(:)
    !-----------------------------------------------------------------
		allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper1D_I1P_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_I1P_t), target, intent(IN)  :: this
        class(*), pointer                                     :: Value(:)
    !-----------------------------------------------------------------
        Value => this%value
    end function


    subroutine DimensionsWrapper1D_I1P_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_I1P_t), intent(IN)  :: this
        class(*), allocatable,            intent(OUT) :: Value(:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1)),source=this%Value)
    end subroutine


    subroutine DimensionsWrapper1D_I1P_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper1D
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_I1P_t), intent(INOUT) :: this
        integer                                         :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    function DimensionsWrapper1D_I1P_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the size of the data stored in bytes
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_I1P_t), intent(IN) :: this            !< Dimensions wrapper 0D
        integer(I4P)                                 :: DataSizeInBytes !< Data size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = byte_size(this%value(1))*size(this%value)
    end function DimensionsWrapper1D_I1P_DataSizeInBytes


    function DimensionsWrapper1D_I1P_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_I1P_t), intent(IN) :: this          !< Dimensions wrapper 1D
        class(*),                         intent(IN) :: Mold          !< Mold for data type comparison
        logical                                      :: isOfDataType  !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (integer(I1P))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper1D_I1P_isOfDataType


    subroutine DimensionsWrapper1D_I1P_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_I1P_t), intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = I1P'//&
                            ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                            ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                            ', Value = '//trim(str(no_sign=.true., n=this%Value))
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper1D_I1P_Print


    subroutine DimensionsWrapper1D_I2P_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper1D
    !-----------------------------------------------------------------
        type(DimensionsWrapper1D_I2P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper1D_I2P_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set I2P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_I2P_t), intent(INOUT) :: this
        class(*),                         intent(IN)    :: Value(:)
        integer                                         :: err
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I2P))
                allocate(this%Value(size(Value,dim=1)), stat=err)
                this%Value = Value
                if(err/=0) call msg%Error(txt='Setting Value: Allocation error ('//&
                                   str(no_sign=.true.,n=err)//')', &
                                   file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn(txt='Setting value: Expected data type (I2P)',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    subroutine DimensionsWrapper1D_I2P_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get I2P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_I2P_t), intent(IN)  :: this
        class(*),                         intent(OUT) :: Value(:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I2P))
                if(all(this%GetShape() == shape(Value))) then
                    Value = this%Value
                else
                    call msg%Warn(txt='Getting value: Expected shape ('//&
                                  str(no_sign=.true.,n=this%GetShape())//')',&
                                  file=__FILE__, line=__LINE__ )
                endif
            class Default
                call msg%Warn(txt='Getting value: Expected data type (I2P)',&
                              file=__FILE__, line=__LINE__ )

        end select
    end subroutine


    function DimensionsWrapper1D_I2P_GetShape(this) result(ValueShape)
    !-----------------------------------------------------------------
    !< Get Wrapper Value Shape
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_I2P_t), intent(IN) :: this
        integer(I4P), allocatable                    :: ValueShape(:)
    !-----------------------------------------------------------------
		allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper1D_I2P_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic W2apper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_I2P_t), target, intent(IN)  :: this
        class(*), pointer                                     :: Value(:)
    !-----------------------------------------------------------------
        Value => this%value
    end function


    subroutine DimensionsWrapper1D_I2P_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_I2P_t), intent(IN)  :: this
        class(*), allocatable,            intent(OUT) :: Value(:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1)),source=this%Value)
    end subroutine


    subroutine DimensionsWrapper1D_I2P_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper1D
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_I2P_t), intent(INOUT) :: this
        integer                                         :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    function DimensionsWrapper1D_I2P_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the size of the data stored in bytes
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_I2P_t), intent(IN) :: this            !< Dimensions wrapper 0D
        integer(I4P)                                 :: DataSizeInBytes !< Data size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = byte_size(this%value(1))*size(this%value)
    end function DimensionsWrapper1D_I2P_DataSizeInBytes


    function DimensionsWrapper1D_I2P_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_I2P_t), intent(IN) :: this          !< Dimensions wrapper 1D
        class(*),                         intent(IN) :: Mold          !< Mold for data type comparison
        logical                                      :: isOfDataType  !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (integer(I2P))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper1D_I2P_isOfDataType


    subroutine DimensionsWrapper1D_I2P_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_I2P_t), intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = I2P'//&
                            ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                            ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                            ', Value = '//trim(str(no_sign=.true., n=this%Value))
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper1D_I2P_Print


    subroutine DimensionsWrapper1D_I4P_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper1D
    !-----------------------------------------------------------------
        type(DimensionsWrapper1D_I4P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper1D_I4P_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set I4P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_I4P_t), intent(INOUT) :: this
        class(*),                         intent(IN)    :: Value(:)
        integer                                         :: err
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I4P))
                allocate(this%Value(size(Value,dim=1)), stat=err)
				this%Value = Value
                if(err/=0) &
                    call msg%Error( txt='Setting Value: Allocation error ('//&
                                    str(no_sign=.true.,n=err)//')', &
                                    file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn( txt='Setting value: Expected data type (I4P)', &
                               file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    subroutine DimensionsWrapper1D_I4P_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get I4P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_I4P_t), intent(IN)  :: this
        class(*),                         intent(OUT) :: Value(:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I4P))
                if(all(this%GetShape() == shape(Value))) then
                    Value = this%Value
                else
                    call msg%Warn(txt='Getting value: Expected shape ('//    &
                                  str(no_sign=.true.,n=this%GetShape())//')',&
                                  file=__FILE__, line=__LINE__ )
                endif
            class Default
                call msg%Warn(txt='Getting value: Expected data type (I4P)',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    function DimensionsWrapper1D_I4P_GetShape(this) result(ValueShape)
    !-----------------------------------------------------------------
    !< Get Wrapper Value Shape
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_I4P_t), intent(IN) :: this
        integer(I4P), allocatable                    :: ValueShape(:)
    !-----------------------------------------------------------------
        allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper1D_I4P_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic W2apper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_I4P_t), target, intent(IN)  :: this
        class(*), pointer                                     :: Value(:)
    !-----------------------------------------------------------------
        Value => this%value
    end function


    subroutine DimensionsWrapper1D_I4P_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_I4P_t), intent(IN)  :: this
        class(*), allocatable,            intent(OUT) :: Value(:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1)),source=this%Value)
    end subroutine


    subroutine DimensionsWrapper1D_I4P_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper1D
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_I4P_t), intent(INOUT) :: this
        integer                                         :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine



    function DimensionsWrapper1D_I4P_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the size of the data stored in bytes
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_I4P_t), intent(IN) :: this            !< Dimensions wrapper 0D
        integer(I4P)                                 :: DataSizeInBytes !< Data size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = byte_size(this%value(1))*size(this%value)
    end function DimensionsWrapper1D_I4P_DataSizeInBytes


    function DimensionsWrapper1D_I4P_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_I4P_t), intent(IN) :: this           !< Dimensions wrapper 1D
        class(*),                         intent(IN) :: Mold           !< Mold for data type comparison
        logical                                      :: isOfDataType   !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (integer(I4P))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper1D_I4P_isOfDataType


    subroutine DimensionsWrapper1D_I4P_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_I4P_t), intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = I4P'//&
                            ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                            ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                            ', Value = '//trim(str(no_sign=.true., n=this%Value))
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper1D_I4P_Print


    subroutine DimensionsWrapper1D_I8P_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper1D
    !-----------------------------------------------------------------
        type(DimensionsWrapper1D_I8P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper1D_I8P_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set I8P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_I8P_t), intent(INOUT) :: this
        class(*),                         intent(IN)    :: Value(:)
        integer                                         :: err
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I8P))
                allocate(this%Value(size(Value,dim=1)), stat=err)
                this%Value = Value
                if(err/=0) call msg%Error(txt='Setting Value: Allocation error ('//&
                                   str(no_sign=.true.,n=err)//')', &
                                   file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn(txt='Setting value: Expected data type (I8P)',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    subroutine DimensionsWrapper1D_I8P_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get I8P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_I8P_t), intent(IN)  :: this
        class(*),                         intent(OUT) :: Value(:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I8P))
                if(all(this%GetShape() == shape(Value))) then
                    Value = this%Value
                else
                    call msg%Warn(txt='Getting value: Expected shape ('//&
                                  str(no_sign=.true.,n=this%GetShape())//')',&
                                  file=__FILE__, line=__LINE__ )
                endif
            class Default
                call msg%Warn(txt='Getting value: Expected data type (I8P)',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    function DimensionsWrapper1D_I8P_GetShape(this) result(ValueShape)
    !-----------------------------------------------------------------
    !< Get Wrapper Value Shape
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_I8P_t), intent(IN) :: this
        integer(I4P), allocatable                    :: ValueShape(:)
    !-----------------------------------------------------------------
		allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper1D_I8P_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic W2apper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_I8P_t), target, intent(IN)  :: this
        class(*), pointer                                     :: Value(:)
    !-----------------------------------------------------------------
        Value => this%value
    end function


    subroutine DimensionsWrapper1D_I8P_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_I8P_t), intent(IN)  :: this
        class(*), allocatable,            intent(OUT) :: Value(:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1)),source=this%Value)
    end subroutine


    subroutine DimensionsWrapper1D_I8P_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper1D
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_I8P_t), intent(INOUT) :: this
        integer                                         :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    function DimensionsWrapper1D_I8P_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the size of the data stored in bytes
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_I8P_t), intent(IN) :: this            !< Dimensions wrapper 0D
        integer(I4P)                                 :: DataSizeInBytes !< Data size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = byte_size(this%value(1))*size(this%value)
    end function DimensionsWrapper1D_I8P_DataSizeInBytes


    function DimensionsWrapper1D_I8P_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_I8P_t), intent(IN) :: this          !< Dimensions wrapper 1D
        class(*),                         intent(IN) :: Mold          !< Mold for data type comparison
        logical                                      :: isOfDataType  !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (integer(I8P))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper1D_I8P_isOfDataType


    subroutine DimensionsWrapper1D_I8P_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_I8P_t), intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = I8P'//&
                            ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                            ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                            ', Value = '//trim(str(no_sign=.true., n=this%Value))
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper1D_I8P_Print


    subroutine DimensionsWrapper1D_L_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper1D
    !-----------------------------------------------------------------
        type(DimensionsWrapper1D_L_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper1D_L_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set logical Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_L_t), intent(INOUT) :: this
        class(*),                       intent(IN)    :: Value(:)
        integer                                       :: err
    !-----------------------------------------------------------------
        select type (Value)
            type is (logical)
                allocate(this%Value(size(Value,dim=1)), stat=err)
                this%Value = Value
                if(err/=0) call msg%Error(txt='Setting Value: Allocation error ('//&
                                   str(no_sign=.true.,n=err)//')', &
                                   file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn(txt='Setting value: Expected data type (logical)',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    subroutine DimensionsWrapper1D_L_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get logical Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_L_t), intent(IN)  :: this
        class(*),                       intent(OUT) :: Value(:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (logical)
                if(all(this%GetShape() == shape(Value))) then
                    Value = this%Value
                else
                    call msg%Warn(txt='Getting value: Expected shape ('//&
                                  str(no_sign=.true.,n=this%GetShape())//')',&
                                  file=__FILE__, line=__LINE__ )
                endif
            class Default
                call msg%Warn(txt='Getting value: Expected data type (logical)',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    function DimensionsWrapper1D_L_GetShape(this) result(ValueShape)
    !-----------------------------------------------------------------
    !< Get Wrapper Value Shape
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_L_t), intent(IN) :: this
        integer(I4P), allocatable                  :: ValueShape(:)
    !-----------------------------------------------------------------
		allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper1D_L_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic W2apper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_L_t), target, intent(IN)  :: this
        class(*), pointer                                   :: Value(:)
    !-----------------------------------------------------------------
        Value => this%value
    end function


    subroutine DimensionsWrapper1D_L_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_L_t), intent(IN)  :: this
        class(*), allocatable,          intent(OUT) :: Value(:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1)),source=this%Value)
    end subroutine


    subroutine DimensionsWrapper1D_L_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper1D
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_L_t), intent(INOUT) :: this
        integer                                         :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    function DimensionsWrapper1D_L_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the size of the data stored in bytes
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_L_t), intent(IN) :: this            !< Dimensions wrapper 0D
        integer(I4P)                                 :: DataSizeInBytes !< Data size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = byte_size_logical(this%value(1))*size(this%value)
    end function DimensionsWrapper1D_L_DataSizeInBytes


    function DimensionsWrapper1D_L_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_L_t), intent(IN) :: this            !< Dimensions wrapper 1D
        class(*),                       intent(IN) :: Mold            !< Mold for data type comparison
        logical                                    :: isOfDataType    !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (logical)
                isOfDataType = .true.
        end select
    end function DimensionsWrapper1D_L_isOfDataType


    subroutine DimensionsWrapper1D_L_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_L_t),   intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A,$)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = L'//&
                        ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                        ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                        ', Value = '
        write(unit=unit,fmt=*,iostat=iostatd,iomsg=iomsgd) str(n=this%Value)
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper1D_L_Print


    subroutine DimensionsWrapper1D_R4P_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper1D
    !-----------------------------------------------------------------
        type(DimensionsWrapper1D_R4P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper1D_R4P_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set R4P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_R4P_t), intent(INOUT) :: this
        class(*),                         intent(IN)    :: Value(:)
        integer                                         :: err
    !-----------------------------------------------------------------
        select type (Value)
            type is (real(R4P))
                allocate(this%Value(size(Value,dim=1)), stat=err)
                this%Value = Value
                if(err/=0) call msg%Error(txt='Setting Value: Allocation error ('//&
                                   str(no_sign=.true.,n=err)//')', &
                                   file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn(txt='Setting value: Expected data type (R4P)',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    subroutine DimensionsWrapper1D_R4P_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get R4P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_R4P_t), intent(IN)  :: this
        class(*),                         intent(OUT) :: Value(:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (real(R4P))
                if(all(this%GetShape() == shape(Value))) then
                    Value = this%Value
                else
                    call msg%Warn(txt='Getting value: Expected shape ('//&
                                  str(no_sign=.true.,n=this%GetShape())//')',&
                                  file=__FILE__, line=__LINE__ )
                endif
            class Default
                call msg%Warn(txt='Getting value: Expected data type (R4P)',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    function DimensionsWrapper1D_R4P_GetShape(this) result(ValueShape)
    !-----------------------------------------------------------------
    !< Get Wrapper Value Shape
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_R4P_t), intent(IN) :: this
        integer(I4P), allocatable                    :: ValueShape(:)
    !-----------------------------------------------------------------
		allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper1D_R4P_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic W2apper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_R4P_t), target, intent(IN)  :: this
        class(*), pointer                                     :: Value(:)
    !-----------------------------------------------------------------
        Value => this%value
    end function


    subroutine DimensionsWrapper1D_R4P_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_R4P_t), intent(IN)  :: this
        class(*), allocatable,            intent(OUT) :: Value(:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1)),source=this%Value)
    end subroutine


    subroutine DimensionsWrapper1D_R4P_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper1D
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_R4P_t), intent(INOUT) :: this
        integer                                         :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    function DimensionsWrapper1D_R4P_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the size of the data stored in bytes
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_R4P_t), intent(IN) :: this            !< Dimensions wrapper 0D
        integer(I4P)                                 :: DataSizeInBytes !< Data size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = byte_size(this%value(1))*size(this%value)
    end function DimensionsWrapper1D_R4P_DataSizeInBytes


    function DimensionsWrapper1D_R4P_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_R4P_t), intent(IN) :: this          !< Dimensions wrapper 1D
        class(*),                         intent(IN) :: Mold          !< Mold for data type comparison
        logical                                      :: isOfDataType  !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (real(R4P))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper1D_R4P_isOfDataType


    subroutine DimensionsWrapper1D_R4P_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_R4P_t), intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = R4P'//&
                            ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                            ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                            ', Value = '//trim(str(no_sign=.true., n=this%Value))
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper1D_R4P_Print


    subroutine DimensionsWrapper1D_R8P_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper1D
    !-----------------------------------------------------------------
        type(DimensionsWrapper1D_R8P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper1D_R8P_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set R8P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_R8P_t), intent(INOUT) :: this
        class(*),                         intent(IN)    :: Value(:)
        integer                                         :: err
    !-----------------------------------------------------------------
        select type (Value)
            type is (real(R8P))
                allocate(this%Value(size(Value,dim=1)), stat=err)
                this%Value = Value
                if(err/=0) call msg%Error(txt='Setting Value: Allocation error ('//&
                                   str(no_sign=.true.,n=err)//')', &
                                   file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn(txt='Setting value: Expected data type (R8P)',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    subroutine DimensionsWrapper1D_R8P_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get R8P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_R8P_t), intent(IN)  :: this
        class(*),                         intent(OUT) :: Value(:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (real(R8P))
                if(all(this%GetShape() == shape(Value))) then
                    Value = this%Value
                else
                    call msg%Warn(txt='Getting value: Expected shape ('//&
                                  str(no_sign=.true.,n=this%GetShape())//')',&
                                  file=__FILE__, line=__LINE__ )
                endif
            class Default
                call msg%Warn(txt='Getting value: Expected data type (R8P)',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    function DimensionsWrapper1D_R8P_GetShape(this) result(ValueShape)
    !-----------------------------------------------------------------
    !< Get Wrapper Value Shape
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_R8P_t), intent(IN) :: this
        integer(I4P), allocatable                    :: ValueShape(:)
    !-----------------------------------------------------------------
		allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper1D_R8P_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic W2apper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_R8P_t), target, intent(IN)  :: this
        class(*), pointer                                     :: Value(:)
    !-----------------------------------------------------------------
        Value => this%value
    end function


    subroutine DimensionsWrapper1D_R8P_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_R8P_t), intent(IN)  :: this
        class(*), allocatable,            intent(OUT) :: Value(:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1)),source=this%Value)
    end subroutine


    subroutine DimensionsWrapper1D_R8P_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper1D
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_R8P_t), intent(INOUT) :: this
        integer                                         :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    function DimensionsWrapper1D_R8P_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the size of the data stored in bytes
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_R8P_t), intent(IN) :: this            !< Dimensions wrapper 0D
        integer(I4P)                                 :: DataSizeInBytes !< Data size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = byte_size(this%value(1))*size(this%value)
    end function DimensionsWrapper1D_R8P_DataSizeInBytes


    function DimensionsWrapper1D_R8P_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_R8P_t), intent(IN) :: this          !< Dimensions wrapper 1D
        class(*),                         intent(IN) :: Mold          !< Mold for data type comparison
        logical                                      :: isOfDataType  !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (real(R8P))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper1D_R8P_isOfDataType


    subroutine DimensionsWrapper1D_R8P_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper1D_R8P_t), intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = R8P'//&
                            ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                            ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                            ', Value = '//trim(str(no_sign=.true., n=this%Value))
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper1D_R8P_Print


    subroutine DimensionsWrapper2D_DLCA_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper2D
    !-----------------------------------------------------------------
        type(DimensionsWrapper2D_DLCA_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper2D_DLCA_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set DLCA Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_DLCA_t), intent(INOUT) :: this
        class(*),                          intent(IN)    :: Value(:,:)
        integer                                          :: err
    !-----------------------------------------------------------------
#ifdef __GFORTRAN__ 
        call msg%Warn(txt='Setting value: Array of deferred length allocatable arrays not supported in Gfortran)',&
                      file=__FILE__, line=__LINE__ )
#else   
        select type (Value)
            type is (character(len=*))
                allocate(character(len=len(Value))::               &
                                    this%Value(size(Value,dim=1),  &
                                    size(Value,dim=2)),            &
                                    stat=err)
                this%Value = Value
                if(err/=0) &
                    call msg%Error( txt='Setting Value: Allocation error ('//&
                                    str(no_sign=.true.,n=err)//')', &
                                    file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn( txt='Setting value: Expected data type (character(*))', &
                               file=__FILE__, line=__LINE__ )
        end select
#endif
    end subroutine


    subroutine DimensionsWrapper2D_DLCA_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get deferred length character array Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_DLCA_t), intent(IN)  :: this
        class(*),                          intent(OUT) :: Value(:,:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (character(len=*))
                if(all(this%GetShape() == shape(Value))) then
                    Value = this%Value
                else
                    call msg%Warn(txt='Getting value: Expected shape ('// &
                                  str(no_sign=.true.,n=this%GetShape())//')',&
                                  file=__FILE__, line=__LINE__ )
                endif
            class Default
                call msg%Warn(txt='Getting value: Expected data type (character(*))',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    function DimensionsWrapper2D_DLCA_GetShape(this) result(ValueShape) 
    !-----------------------------------------------------------------
    !< Get Wrapper Value Shape
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_DLCA_t), intent(IN)  :: this
        integer(I4P), allocatable                      :: ValueShape(:)
    !-----------------------------------------------------------------
        allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper2D_DLCA_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_DLCA_t), target, intent(IN) :: this
        class(*), pointer                                     :: Value(:,:)
    !-----------------------------------------------------------------
        Value => this%value
    end function


    subroutine DimensionsWrapper2D_DLCA_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_DLCA_t), intent(IN)  :: this
        class(*), allocatable,             intent(OUT) :: Value(:,:)
    !-----------------------------------------------------------------
!        allocate(Value(size(this%Value,dim=1),  &
!                       size(this%Value,dim=2)), &
!                       source=this%Value)
    end subroutine


    subroutine DimensionsWrapper2D_DLCA_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper2D
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_DLCA_t), intent(INOUT) :: this
        integer                                          :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    function DimensionsWrapper2D_DLCA_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the size of the data stored in bytes
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_DLCA_t), intent(IN) :: this            !< Dimensions wrapper 0D
        integer(I4P)                                 :: DataSizeInBytes !< Data size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = 0
        if(allocated(this%value)) DataSizeInBytes = byte_size(this%value(1,1))*size(this%value)
    end function DimensionsWrapper2D_DLCA_DataSizeInBytes


    function DimensionsWrapper2D_DLCA_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_DLCA_t), intent(IN) :: this         !< Dimensions wrapper 2D
        class(*),                          intent(IN) :: Mold         !< Mold for data type comparison
        logical                                  :: isOfDataType      !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (character(len=*))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper2D_DLCA_isOfDataType


    subroutine DimensionsWrapper2D_DLCA_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_DLCA_t),intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A,$)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = DLCA'//&
                        ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                        ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                        ', Value = '
        write(unit=unit,fmt=*,iostat=iostatd,iomsg=iomsgd) this%Value

        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper2D_DLCA_Print


    subroutine DimensionsWrapper2D_I1P_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper2D
    !-----------------------------------------------------------------
        type(DimensionsWrapper2D_I1P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper2D_I1P_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set I1P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_I1P_t), intent(INOUT) :: this
        class(*),                         intent(IN)    :: Value(:,:)
        integer                                         :: err
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I1P))
                allocate(this%Value(size(Value,dim=1),  &
                                    size(Value,dim=2)), &
                                    stat=err)
                this%Value = Value
                if(err/=0) &
                    call msg%Error( txt='Setting Value: Allocation error ('//&
                                    str(no_sign=.true.,n=err)//')', &
                                    file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn( txt='Setting value: Expected data type (I1P)', &
                               file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    subroutine DimensionsWrapper2D_I1P_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get I1P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_I1P_t), intent(IN)  :: this
        class(*),                         intent(OUT) :: Value(:,:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I1P))
                if(all(this%GetShape() == shape(Value))) then
                    Value = this%Value
                else
                    call msg%Warn(txt='Getting value: Expected shape ('//    &
                                  str(no_sign=.true.,n=this%GetShape())//')',&
                                  file=__FILE__, line=__LINE__ )
                endif
            class Default
                call msg%Warn(txt='Getting value: Expected data type (I1P)',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    function DimensionsWrapper2D_I1P_GetShape(this) result(ValueShape) 
    !-----------------------------------------------------------------
    !< Get Wrapper Value Shape
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_I1P_t), intent(IN)  :: this
        integer(I4P), allocatable                     :: ValueShape(:)
    !-----------------------------------------------------------------
        allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper2D_I1P_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic pointer to Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_I1P_t), target, intent(IN) :: this
        class(*), pointer                                    :: Value(:,:)
    !-----------------------------------------------------------------
        Value => this%value
    end function


    subroutine DimensionsWrapper2D_I1P_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_I1P_t), intent(IN)  :: this
        class(*), allocatable,            intent(OUT) :: Value(:,:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1),  &
                       size(this%Value,dim=2)), &
                       source=this%Value)
    end subroutine


    subroutine DimensionsWrapper2D_I1P_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper2D
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_I1P_t), intent(INOUT) :: this
        integer                                         :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    function DimensionsWrapper2D_I1P_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the size of the data stored in bytes
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_I1P_t), intent(IN) :: this            !< Dimensions wrapper 0D
        integer(I4P)                                 :: DataSizeInBytes !< Data size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = byte_size(this%value(1,1))*size(this%value)
    end function DimensionsWrapper2D_I1P_DataSizeInBytes


    function DimensionsWrapper2D_I1P_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_I1P_t), intent(IN) :: this          !< Dimensions wrapper 2D
        class(*),                         intent(IN) :: Mold          !< Mold for data type comparison
        logical                                      :: isOfDataType  !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (integer(I1P))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper2D_I1P_isOfDataType


    subroutine DimensionsWrapper2D_I1P_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_I1P_t), intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A,$)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = I1P'//&
                        ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                        ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                        ', Value = '
        write(unit=unit,fmt=*,iostat=iostatd,iomsg=iomsgd) str(no_sign=.true., n=this%Value)

        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper2D_I1P_Print


    subroutine DimensionsWrapper2D_I2P_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper2D
    !-----------------------------------------------------------------
        type(DimensionsWrapper2D_I2P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper2D_I2P_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set I2P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_I2P_t), intent(INOUT) :: this
        class(*),                         intent(IN)    :: Value(:,:)
        integer                                         :: err
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I2P))
                allocate(this%Value(size(Value,dim=1),  &
                                    size(Value,dim=2)), &
                                    stat=err)
                this%Value = Value
                if(err/=0) &
                    call msg%Error( txt='Setting Value: Allocation error ('//&
                                    str(no_sign=.true.,n=err)//')', &
                                    file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn( txt='Setting value: Expected data type (I2P)', &
                               file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    subroutine DimensionsWrapper2D_I2P_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get I2P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_I2P_t), intent(IN)  :: this
        class(*),                         intent(OUT) :: Value(:,:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I2P))
                if(all(this%GetShape() == shape(Value))) then
                    Value = this%Value
                else
                    call msg%Warn(txt='Getting value: Expected shape ('//    &
                                  str(no_sign=.true.,n=this%GetShape())//')',&
                                  file=__FILE__, line=__LINE__ )
                endif
            class Default
                call msg%Warn(txt='Getting value: Expected data type (I2P)',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    function DimensionsWrapper2D_I2P_GetShape(this) result(ValueShape) 
    !-----------------------------------------------------------------
    !< Get Wrapper Value Shape
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_I2P_t), intent(IN)  :: this
        integer(I4P), allocatable                     :: ValueShape(:)
    !-----------------------------------------------------------------
        allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper2D_I2P_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic pointer to Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_I2P_t), target, intent(IN) :: this
        class(*), pointer                                    :: Value(:,:)
    !-----------------------------------------------------------------
        Value => this%value
    end function


    subroutine DimensionsWrapper2D_I2P_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_I2P_t), intent(IN)  :: this
        class(*), allocatable,            intent(OUT) :: Value(:,:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1),  &
                       size(this%Value,dim=2)), &
                       source=this%Value)
    end subroutine


    function DimensionsWrapper2D_I2P_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the size of the data stored in bytes
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_I2P_t), intent(IN) :: this            !< Dimensions wrapper 0D
        integer(I4P)                                 :: DataSizeInBytes !< Data size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = byte_size(this%value(1,1))*size(this%value)
    end function DimensionsWrapper2D_I2P_DataSizeInBytes


    function DimensionsWrapper2D_I2P_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_I2P_t), intent(IN) :: this          !< Dimensions wrapper 2D
        class(*),                         intent(IN) :: Mold          !< Mold for data type comparison
        logical                                      :: isOfDataType  !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (integer(I2P))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper2D_I2P_isOfDataType


    subroutine DimensionsWrapper2D_I2P_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper2D
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_I2P_t), intent(INOUT) :: this
        integer                                         :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    subroutine DimensionsWrapper2D_I2P_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_I2P_t), intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A,$)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = I2P'//&
                        ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                        ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                        ', Value = '
        write(unit=unit,fmt=*,iostat=iostatd,iomsg=iomsgd) str(no_sign=.true., n=this%Value)

        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper2D_I2P_Print


    subroutine DimensionsWrapper2D_I4P_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper2D
    !-----------------------------------------------------------------
        type(DimensionsWrapper2D_I4P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper2D_I4P_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set I4P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_I4P_t), intent(INOUT) :: this
        class(*),                         intent(IN)    :: Value(:,:)
        integer                                         :: err
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I4P))
                allocate(this%Value(size(Value,dim=1),  &
                                    size(Value,dim=2)), &
                                    stat=err)
                this%Value = Value
                if(err/=0) &
                    call msg%Error( txt='Setting Value: Allocation error ('//&
                                    str(no_sign=.true.,n=err)//')', &
                                    file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn( txt='Setting value: Expected data type (I4P)', &
                               file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    subroutine DimensionsWrapper2D_I4P_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get I4P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_I4P_t), intent(IN)  :: this
        class(*),                         intent(OUT) :: Value(:,:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I4P))
                if(all(this%GetShape() == shape(Value))) then
                    Value = this%Value
                else
                    call msg%Warn(txt='Getting value: Expected shape ('//    &
                                  str(no_sign=.true.,n=this%GetShape())//')',&
                                  file=__FILE__, line=__LINE__ )
                endif
            class Default
                call msg%Warn(txt='Getting value: Expected data type (I4P)',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    function DimensionsWrapper2D_I4P_GetShape(this) result(ValueShape) 
    !-----------------------------------------------------------------
    !< Get Wrapper Value Shape
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_I4P_t), intent(IN)  :: this
        integer(I4P), allocatable                     :: ValueShape(:)
    !-----------------------------------------------------------------
        allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper2D_I4P_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic pointer to Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_I4P_t), target, intent(IN) :: this
        class(*), pointer                                    :: Value(:,:)
    !-----------------------------------------------------------------
        Value => this%value
    end function


    subroutine DimensionsWrapper2D_I4P_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_I4P_t), intent(IN)  :: this
        class(*), allocatable,            intent(OUT) :: Value(:,:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1),  &
                       size(this%Value,dim=2)), &
                       source=this%Value)
    end subroutine


    subroutine DimensionsWrapper2D_I4P_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper2D
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_I4P_t), intent(INOUT) :: this
        integer                                         :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    function DimensionsWrapper2D_I4P_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the size of the data stored in bytes
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_I4P_t), intent(IN) :: this            !< Dimensions wrapper 0D
        integer(I4P)                                 :: DataSizeInBytes !< Data size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = byte_size(this%value(1,1))*size(this%value)
    end function DimensionsWrapper2D_I4P_DataSizeInBytes


    function DimensionsWrapper2D_I4P_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_I4P_t), intent(IN) :: this           !< Dimensions wrapper 2D
        class(*),                         intent(IN) :: Mold           !< Mold for data type comparison
        logical                                      :: isOfDataType   !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (integer(I4P))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper2D_I4P_isOfDataType


    subroutine DimensionsWrapper2D_I4P_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_I4P_t), intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A,$)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = I4P'//&
                        ', Dimensions = '//trim(str(no_sign=.true.,n=this%GetDimensions()))//&
                        ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                        ', Value = '
        write(unit=unit,fmt=*,iostat=iostatd,iomsg=iomsgd) str(no_sign=.true., n=this%Value)
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper2D_I4P_Print


    subroutine DimensionsWrapper2D_I8P_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper2D
    !-----------------------------------------------------------------
        type(DimensionsWrapper2D_I8P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper2D_I8P_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set I8P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_I8P_t), intent(INOUT) :: this
        class(*),                         intent(IN)    :: Value(:,:)
        integer                                         :: err
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I8P))
                allocate(this%Value(size(Value,dim=1),  &
                                    size(Value,dim=2)), &
                                    stat=err)
                this%Value = Value
                if(err/=0) &
                    call msg%Error( txt='Setting Value: Allocation error ('//&
                                    str(no_sign=.true.,n=err)//')', &
                                    file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn( txt='Setting value: Expected data type (I8P)', &
                               file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    subroutine DimensionsWrapper2D_I8P_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get I8P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_I8P_t), intent(IN)  :: this
        class(*),                         intent(OUT) :: Value(:,:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I8P))
                if(all(this%GetShape() == shape(Value))) then
                    Value = this%Value
                else
                    call msg%Warn(txt='Getting value: Expected shape ('//    &
                                  str(no_sign=.true.,n=this%GetShape())//')',&
                                  file=__FILE__, line=__LINE__ )
                endif
            class Default
                call msg%Warn(txt='Getting value: Expected data type (I8P)',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    function DimensionsWrapper2D_I8P_GetShape(this) result(ValueShape) 
    !-----------------------------------------------------------------
    !< Get Wrapper Value Shape
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_I8P_t), intent(IN)  :: this
        integer(I4P), allocatable                     :: ValueShape(:)
    !-----------------------------------------------------------------
        allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper2D_I8P_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic pointer to Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_I8P_t), target, intent(IN) :: this
        class(*), pointer                                    :: Value(:,:)
    !-----------------------------------------------------------------
        Value => this%value
    end function


    subroutine DimensionsWrapper2D_I8P_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_I8P_t), intent(IN)  :: this
        class(*), allocatable,            intent(OUT) :: Value(:,:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1),  &
                       size(this%Value,dim=2)), &
                       source=this%Value)
    end subroutine


    subroutine DimensionsWrapper2D_I8P_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper2D
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_I8P_t), intent(INOUT) :: this
        integer                                         :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    function DimensionsWrapper2D_I8P_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the size of the data stored in bytes
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_I8P_t), intent(IN) :: this            !< Dimensions wrapper 0D
        integer(I4P)                                 :: DataSizeInBytes !< Data size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = byte_size(this%value(1,1))*size(this%value)
    end function DimensionsWrapper2D_I8P_DataSizeInBytes


    function DimensionsWrapper2D_I8P_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_I8P_t), intent(IN) :: this          !< Dimensions wrapper 2D
        class(*),                         intent(IN) :: Mold          !< Mold for data type comparison
        logical                                      :: isOfDataType  !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (integer(I8P))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper2D_I8P_isOfDataType


    subroutine DimensionsWrapper2D_I8P_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_I8P_t), intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A,$)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = I8P'//&
                        ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                        ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                        ', Value = '
        write(unit=unit,fmt=*,iostat=iostatd,iomsg=iomsgd) str(no_sign=.true., n=this%Value)

        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper2D_I8P_Print


    subroutine DimensionsWrapper2D_L_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper2D
    !-----------------------------------------------------------------
        type(DimensionsWrapper2D_L_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper2D_L_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set logical Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_L_t), intent(INOUT) :: this
        class(*),                       intent(IN)    :: Value(:,:)
        integer                                       :: err
    !-----------------------------------------------------------------
        select type (Value)
            type is (logical)
                allocate(this%Value(size(Value,dim=1),  &
                                    size(Value,dim=2)), &
                                    stat=err)
                this%Value = Value
                if(err/=0) &
                    call msg%Error( txt='Setting Value: Allocation error ('//&
                                    str(no_sign=.true.,n=err)//')', &
                                    file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn( txt='Setting value: Expected data type (logical)', &
                               file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    subroutine DimensionsWrapper2D_L_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get logical Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_L_t), intent(IN)  :: this
        class(*),                       intent(OUT) :: Value(:,:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (logical)
                if(all(this%GetShape() == shape(Value))) then
                    Value = this%Value
                else
                    call msg%Warn(txt='Getting value: Expected shape ('//    &
                                  str(no_sign=.true.,n=this%GetShape())//')',&
                                  file=__FILE__, line=__LINE__ )
                endif
            class Default
                call msg%Warn(txt='Getting value: Expected data type (logical)',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    function DimensionsWrapper2D_L_GetShape(this) result(ValueShape) 
    !-----------------------------------------------------------------
    !< Get Wrapper Value Shape
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_L_t), intent(IN)  :: this
        integer(I4P), allocatable                   :: ValueShape(:)
    !-----------------------------------------------------------------
        allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper2D_L_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic pointer to Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_L_t), target, intent(IN) :: this
        class(*), pointer                                  :: Value(:,:)
    !-----------------------------------------------------------------
        Value => this%value
    end function


    subroutine DimensionsWrapper2D_L_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_L_t), intent(IN)  :: this
        class(*), allocatable,          intent(OUT) :: Value(:,:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1),  &
                       size(this%Value,dim=2)), &
                       source=this%Value)
    end subroutine


    subroutine DimensionsWrapper2D_L_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper2D
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_L_t), intent(INOUT) :: this
        integer                                       :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    function DimensionsWrapper2D_L_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the size of the data stored in bytes
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_L_t), intent(IN) :: this            !< Dimensions wrapper 0D
        integer(I4P)                               :: DataSizeInBytes !< Data size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = byte_size_logical(this%value(1,1))*size(this%value)
    end function DimensionsWrapper2D_L_DataSizeInBytes


    function DimensionsWrapper2D_L_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_L_t), intent(IN) :: this            !< Dimensions wrapper 2D
        class(*),                       intent(IN) :: Mold            !< Mold for data type comparison
        logical                                    :: isOfDataType    !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (logical)
                isOfDataType = .true.
        end select
    end function DimensionsWrapper2D_L_isOfDataType


    subroutine DimensionsWrapper2D_L_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_L_t),   intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A,$)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = L'//&
                        ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                        ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                        ', Value = '
        write(unit=unit,fmt=*,iostat=iostatd,iomsg=iomsgd) str(n=this%Value)
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper2D_L_Print


    subroutine DimensionsWrapper2D_R4P_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper2D
    !-----------------------------------------------------------------
        type(DimensionsWrapper2D_R4P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper2D_R4P_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set R4P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_R4P_t), intent(INOUT) :: this
        class(*),                         intent(IN)    :: Value(:,:)
        integer                                         :: err
    !-----------------------------------------------------------------
        select type (Value)
            type is (real(R4P))
                allocate(this%Value(size(Value,dim=1),  &
                                    size(Value,dim=2)), &
                                    stat=err)
                this%Value = Value
                if(err/=0) &
                    call msg%Error( txt='Setting Value: Allocation error ('//&
                                    str(no_sign=.true.,n=err)//')', &
                                    file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn( txt='Setting value: Expected data type (R8P)', &
                               file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    subroutine DimensionsWrapper2D_R4P_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get R4P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_R4P_t), intent(IN)  :: this
        class(*),                         intent(OUT) :: Value(:,:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (real(R4P))
                if(all(this%GetShape() == shape(Value))) then
                    Value = this%Value
                else
                    call msg%Warn(txt='Getting value: Expected shape ('//    &
                                  str(no_sign=.true.,n=this%GetShape())//')',&
                                  file=__FILE__, line=__LINE__ )
                endif
            class Default
                call msg%Warn(txt='Getting value: Expected data type (R8P)',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    function DimensionsWrapper2D_R4P_GetShape(this) result(ValueShape) 
    !-----------------------------------------------------------------
    !< Get Wrapper Value Shape
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_R4P_t), intent(IN)  :: this
        integer(I4P), allocatable                     :: ValueShape(:)
    !-----------------------------------------------------------------
        allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper2D_R4P_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic pointer to Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_R4P_t), target, intent(IN) :: this
        class(*), pointer                                    :: Value(:,:)
    !-----------------------------------------------------------------
        Value => this%value
    end function


    subroutine DimensionsWrapper2D_R4P_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_R4P_t), intent(IN)  :: this
        class(*), allocatable,            intent(OUT) :: Value(:,:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1),  &
                       size(this%Value,dim=2)), &
                       source=this%Value)
    end subroutine


    subroutine DimensionsWrapper2D_R4P_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper2D
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_R4P_t), intent(INOUT) :: this
        integer                                         :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    function DimensionsWrapper2D_R4P_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the size of the data stored in bytes
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_R4P_t), intent(IN) :: this            !< Dimensions wrapper 0D
        integer(I4P)                                 :: DataSizeInBytes !< Data size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = byte_size(this%value(1,1))*size(this%value)
    end function DimensionsWrapper2D_R4P_DataSizeInBytes


    function DimensionsWrapper2D_R4P_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_R4P_t), intent(IN) :: this          !< Dimensions wrapper 2D
        class(*),                         intent(IN) :: Mold          !< Mold for data type comparison
        logical                                      :: isOfDataType  !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (real(R8P))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper2D_R4P_isOfDataType


    subroutine DimensionsWrapper2D_R4P_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_R4P_t), intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A,$)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = R8P'//&
                        ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                        ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                        ', Value = '
        write(unit=unit,fmt=*,iostat=iostatd,iomsg=iomsgd) str(no_sign=.true., n=this%Value)

        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper2D_R4P_Print


    subroutine DimensionsWrapper2D_R8P_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper2D
    !-----------------------------------------------------------------
        type(DimensionsWrapper2D_R8P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper2D_R8P_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set R8P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_R8P_t), intent(INOUT) :: this
        class(*),                         intent(IN)    :: Value(:,:)
        integer                                         :: err
    !-----------------------------------------------------------------
        select type (Value)
            type is (real(R8P))
                allocate(this%Value(size(Value,dim=1),  &
                                    size(Value,dim=2)), &
                                    stat=err)
                this%Value = Value
                if(err/=0) &
                    call msg%Error( txt='Setting Value: Allocation error ('//&
                                    str(no_sign=.true.,n=err)//')', &
                                    file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn( txt='Setting value: Expected data type (R8P)', &
                               file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    subroutine DimensionsWrapper2D_R8P_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get R8P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_R8P_t), intent(IN)  :: this
        class(*),                         intent(OUT) :: Value(:,:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (real(R8P))
                if(all(this%GetShape() == shape(Value))) then
                    Value = this%Value
                else
                    call msg%Warn(txt='Getting value: Expected shape ('//    &
                                  str(no_sign=.true.,n=this%GetShape())//')',&
                                  file=__FILE__, line=__LINE__ )
                endif
            class Default
                call msg%Warn(txt='Getting value: Expected data type (R8P)',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    function DimensionsWrapper2D_R8P_GetShape(this) result(ValueShape) 
    !-----------------------------------------------------------------
    !< Get Wrapper Value Shape
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_R8P_t), intent(IN)  :: this
        integer(I4P), allocatable                     :: ValueShape(:)
    !-----------------------------------------------------------------
        allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper2D_R8P_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic pointer to Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_R8P_t), target, intent(IN) :: this
        class(*), pointer                                    :: Value(:,:)
    !-----------------------------------------------------------------
        Value => this%value
    end function


    subroutine DimensionsWrapper2D_R8P_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_R8P_t), intent(IN)  :: this
        class(*), allocatable,            intent(OUT) :: Value(:,:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1),  &
                       size(this%Value,dim=2)), &
                       source=this%Value)
    end subroutine


    subroutine DimensionsWrapper2D_R8P_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper2D
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_R8P_t), intent(INOUT) :: this
        integer                                         :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    function DimensionsWrapper2D_R8P_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the size of the data stored in bytes
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_R8P_t), intent(IN) :: this            !< Dimensions wrapper 0D
        integer(I4P)                                 :: DataSizeInBytes !< Data size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = byte_size(this%value(1,1))*size(this%value)
    end function DimensionsWrapper2D_R8P_DataSizeInBytes


    function DimensionsWrapper2D_R8P_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_R8P_t), intent(IN) :: this          !< Dimensions wrapper 2D
        class(*),                         intent(IN) :: Mold          !< Mold for data type comparison
        logical                                      :: isOfDataType  !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (real(R8P))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper2D_R8P_isOfDataType


    subroutine DimensionsWrapper2D_R8P_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper2D_R8P_t), intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A,$)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = R8P'//&
                        ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                        ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                        ', Value = '
        write(unit=unit,fmt=*,iostat=iostatd,iomsg=iomsgd) str(no_sign=.true., n=this%Value)

        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper2D_R8P_Print


    subroutine DimensionsWrapper3D_DLCA_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper3D
    !-----------------------------------------------------------------
        type(DimensionsWrapper3D_DLCA_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper3D_DLCA_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set DLCA Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_DLCA_t), intent(INOUT) :: this
        class(*),                          intent(IN)    :: Value(:,:,:)
        integer                                          :: err
    !-----------------------------------------------------------------
#ifdef __GFORTRAN__ 
        call msg%Warn(txt='Setting value: Array of deferred length allocatable arrays not supported in Gfortran)',&
                      file=__FILE__, line=__LINE__ )
#else   
        select type (Value)
            type is (character(len=*))
                allocate(character(len=len(Value))::               &
                                    this%Value(size(Value,dim=1),  &
                                    size(Value,dim=2),             &
                                    size(Value,dim=3)),            &
                                    stat=err)
                this%Value = Value
                if(err/=0) &
                    call msg%Error( txt='Setting Value: Allocation error ('//&
                                    str(no_sign=.true.,n=err)//')', &
                                    file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn( txt='Setting value: Expected data type (character(*))', &
                               file=__FILE__, line=__LINE__ )
        end select
#endif
    end subroutine


    subroutine DimensionsWrapper3D_DLCA_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get deferred length character array Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_DLCA_t), intent(IN)  :: this
        class(*),                          intent(OUT) :: Value(:,:,:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (character(len=*))
                if(all(this%GetShape() == shape(Value))) then
                    Value = this%Value
                else
                    call msg%Warn(txt='Getting value: Expected shape ('//    &
                                  str(no_sign=.true.,n=this%GetShape())//')',&
                                  file=__FILE__, line=__LINE__ )
                endif
            class Default
                call msg%Warn(txt='Getting value: Expected data type (character(*))',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    function DimensionsWrapper3D_DLCA_GetShape(this) result(ValueShape) 
    !-----------------------------------------------------------------
    !< Get Wrapper Value Shape
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_DLCA_t), intent(IN)  :: this
        integer(I4P), allocatable                      :: ValueShape(:)
    !-----------------------------------------------------------------
        allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper3D_DLCA_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic pointer to Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_DLCA_t), target, intent(IN)  :: this
        class(*), pointer                                      :: Value(:,:,:)
    !-----------------------------------------------------------------
        Value => this%Value
    end function


    subroutine DimensionsWrapper3D_DLCA_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_DLCA_t), intent(IN)  :: this
        class(*), allocatable,             intent(OUT) :: Value(:,:,:)
    !-----------------------------------------------------------------
!        allocate(Value(size(this%Value,dim=1),  &
!                       size(this%Value,dim=2),  &
!                       size(this%Value,dim=3)), &
!                       source=this%Value)
    end subroutine


    subroutine DimensionsWrapper3D_DLCA_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper3D
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_DLCA_t), intent(INOUT) :: this
        integer                                         :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    function DimensionsWrapper3D_DLCA_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the size of the data stored in bytes
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_DLCA_t), intent(IN) :: this            !< Dimensions wrapper 0D
        integer(I4P)                                  :: DataSizeInBytes !< Data size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = 0
        if(allocated(this%value)) DataSizeInBytes = byte_size(this%value(1,1,1))*size(this%value)
    end function DimensionsWrapper3D_DLCA_DataSizeInBytes


    function DimensionsWrapper3D_DLCA_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_DLCA_t), intent(IN) :: this         !< Dimensions wrapper 3D
        class(*),                          intent(IN) :: Mold         !< Mold for data type comparison
        logical                                  :: isOfDataType      !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (character(len=*))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper3D_DLCA_isOfDataType


    subroutine DimensionsWrapper3D_DLCA_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_DLCA_t),intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A,$)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = DLCA'//&
                        ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                        ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                        ', Value = '
        write(unit=unit,fmt=*,iostat=iostatd,iomsg=iomsgd) this%Value

        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper3D_DLCA_Print


    subroutine DimensionsWrapper3D_I1P_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper3D
    !-----------------------------------------------------------------
        type(DimensionsWrapper3D_I1P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper3D_I1P_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set I1P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_I1P_t), intent(INOUT) :: this
        class(*),                         intent(IN)    :: Value(:,:,:)
        integer                                         :: err
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I1P))
                allocate(this%Value(size(Value,dim=1),  &
                                    size(Value,dim=2),  &
                                    size(Value,dim=3)), &
                                    stat=err)
                this%Value = Value
                if(err/=0) &
                    call msg%Error( txt='Setting Value: Allocation error ('//&
                                    str(no_sign=.true.,n=err)//')', &
                                    file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn( txt='Setting value: Expected data type (I1P)', &
                               file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    subroutine DimensionsWrapper3D_I1P_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get I1P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_I1P_t), intent(IN)  :: this
        class(*),                         intent(OUT) :: Value(:,:,:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I1P))
                if(all(this%GetShape() == shape(Value))) then
                    Value = this%Value
                else
                    call msg%Warn(txt='Getting value: Expected shape ('//    &
                                  str(no_sign=.true.,n=this%GetShape())//')',&
                                  file=__FILE__, line=__LINE__ )
                endif
            class Default
                call msg%Warn(txt='Getting value: Expected data type (I1P)',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    function DimensionsWrapper3D_I1P_GetShape(this) result(ValueShape) 
    !-----------------------------------------------------------------
    !< Get Wrapper Value Shape
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_I1P_t), intent(IN)  :: this
        integer(I4P), allocatable                     :: ValueShape(:)
    !-----------------------------------------------------------------
        allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper3D_I1P_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic pointer to Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_I1P_t), target, intent(IN)  :: this
        class(*), pointer                                     :: Value(:,:,:)
    !-----------------------------------------------------------------
        Value => this%Value
    end function


    subroutine DimensionsWrapper3D_I1P_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_I1P_t), intent(IN)  :: this
        class(*), allocatable,            intent(OUT) :: Value(:,:,:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1),  &
                       size(this%Value,dim=2),  &
                       size(this%Value,dim=3)), &
                       source=this%Value)
    end subroutine



    subroutine DimensionsWrapper3D_I1P_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper3D
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_I1P_t), intent(INOUT) :: this
        integer                                         :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    function DimensionsWrapper3D_I1P_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the size of the data stored in bytes
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_I1P_t), intent(IN) :: this            !< Dimensions wrapper 0D
        integer(I4P)                                 :: DataSizeInBytes !< Data size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = byte_size(this%value(1,1,1))*size(this%value)
    end function DimensionsWrapper3D_I1P_DataSizeInBytes


    function DimensionsWrapper3D_I1P_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_I1P_t), intent(IN) :: this          !< Dimensions wrapper 3D
        class(*),                         intent(IN) :: Mold          !< Mold for data type comparison
        logical                                      :: isOfDataType  !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (integer(I1P))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper3D_I1P_isOfDataType


    subroutine DimensionsWrapper3D_I1P_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_I1P_t), intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A,$)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = I1P'//&
                        ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                        ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                        ', Value = '
        write(unit=unit,fmt=*,iostat=iostatd,iomsg=iomsgd) str(no_sign=.true., n=this%Value)

        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper3D_I1P_Print


    subroutine DimensionsWrapper3D_I2P_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper3D
    !-----------------------------------------------------------------
        type(DimensionsWrapper3D_I2P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper3D_I2P_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set I2P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_I2P_t), intent(INOUT) :: this
        class(*),                         intent(IN)    :: Value(:,:,:)
        integer                                         :: err
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I2P))
                allocate(this%Value(size(Value,dim=1),  &
                                    size(Value,dim=2),  &
                                    size(Value,dim=3)), &
                                    stat=err)
                this%Value = Value
                if(err/=0) &
                    call msg%Error( txt='Setting Value: Allocation error ('//&
                                    str(no_sign=.true.,n=err)//')', &
                                    file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn( txt='Setting value: Expected data type (I2P)', &
                               file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    subroutine DimensionsWrapper3D_I2P_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get I2P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_I2P_t), intent(IN)  :: this
        class(*),                         intent(OUT) :: Value(:,:,:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I2P))
                if(all(this%GetShape() == shape(Value))) then
                    Value = this%Value
                else
                    call msg%Warn(txt='Getting value: Expected shape ('//    &
                                  str(no_sign=.true.,n=this%GetShape())//')',&
                                  file=__FILE__, line=__LINE__ )
                endif
            class Default
                call msg%Warn(txt='Getting value: Expected data type (I2P)',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    function DimensionsWrapper3D_I2P_GetShape(this) result(ValueShape) 
    !-----------------------------------------------------------------
    !< Get Wrapper Value Shape
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_I2P_t), intent(IN)  :: this
        integer(I4P), allocatable                     :: ValueShape(:)
    !-----------------------------------------------------------------
        allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper3D_I2P_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic pointer to Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_I2P_t), target, intent(IN)  :: this
        class(*), pointer                                     :: Value(:,:,:)
    !-----------------------------------------------------------------
        Value => this%Value
    end function


    subroutine DimensionsWrapper3D_I2P_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_I2P_t), intent(IN)  :: this
        class(*), allocatable,            intent(OUT) :: Value(:,:,:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1),  &
                       size(this%Value,dim=2),  &
                       size(this%Value,dim=3)), &
                       source=this%Value)
    end subroutine


    subroutine DimensionsWrapper3D_I2P_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper3D
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_I2P_t), intent(INOUT) :: this
        integer                                         :: err = FPLSuccess
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    function DimensionsWrapper3D_I2P_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the size of the data stored in bytes
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_I2P_t), intent(IN) :: this            !< Dimensions wrapper 0D
        integer(I4P)                                 :: DataSizeInBytes !< Data size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = byte_size(this%value(1,1,1))*size(this%value)
    end function DimensionsWrapper3D_I2P_DataSizeInBytes


    function DimensionsWrapper3D_I2P_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_I2P_t), intent(IN) :: this          !< Dimensions wrapper 3D
        class(*),                         intent(IN) :: Mold          !< Mold for data type comparison
        logical                                      :: isOfDataType  !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (integer(I2P))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper3D_I2P_isOfDataType


    subroutine DimensionsWrapper3D_I2P_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_I2P_t), intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A,$)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = I2P'//&
                        ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                        ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                        ', Value = '
        write(unit=unit,fmt=*,iostat=iostatd,iomsg=iomsgd) str(no_sign=.true., n=this%Value)

        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper3D_I2P_Print


    subroutine DimensionsWrapper3D_I4P_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper3D
    !-----------------------------------------------------------------
        type(DimensionsWrapper3D_I4P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper3D_I4P_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set I4P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_I4P_t), intent(INOUT) :: this
        class(*),                         intent(IN)    :: Value(:,:,:)
        integer                                         :: err
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I4P))
                allocate(this%Value(size(Value,dim=1),  &
                                    size(Value,dim=2),  &
                                    size(Value,dim=3)), &
                                    stat=err)
                this%Value = Value
                if(err/=0) &
                    call msg%Error( txt='Setting Value: Allocation error ('//&
                                    str(no_sign=.true.,n=err)//')', &
                                    file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn( txt='Setting value: Expected data type (I4P)', &
                               file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    subroutine DimensionsWrapper3D_I4P_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get I4P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_I4P_t), intent(IN)  :: this
        class(*),                         intent(OUT) :: Value(:,:,:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I4P))
                if(all(this%GetShape() == shape(Value))) then
                    Value = this%Value
                else
                    call msg%Warn(txt='Getting value: Expected shape ('//    &
                                  str(no_sign=.true.,n=this%GetShape())//')',&
                                  file=__FILE__, line=__LINE__ )
                endif
            class Default
                call msg%Warn(txt='Getting value: Expected data type (I4P)',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    function DimensionsWrapper3D_I4P_GetShape(this) result(ValueShape) 
    !-----------------------------------------------------------------
    !< Get Wrapper Value Shape
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_I4P_t), intent(IN)  :: this
        integer(I4P), allocatable                     :: ValueShape(:)
    !-----------------------------------------------------------------
        allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper3D_I4P_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic pointer to Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_I4P_t), target, intent(IN)  :: this
        class(*), pointer                                     :: Value(:,:,:)
    !-----------------------------------------------------------------
        Value => this%Value
    end function


    subroutine DimensionsWrapper3D_I4P_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_I4P_t), intent(IN)  :: this
        class(*), allocatable,            intent(OUT) :: Value(:,:,:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1),  &
                       size(this%Value,dim=2),  &
                       size(this%Value,dim=3)), &
                       source=this%Value)
    end subroutine


    subroutine DimensionsWrapper3D_I4P_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper3D
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_I4P_t), intent(INOUT) :: this
        integer                                         :: err 
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    function DimensionsWrapper3D_I4P_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the size of the data stored in bytes
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_I4P_t), intent(IN) :: this            !< Dimensions wrapper 0D
        integer(I4P)                                 :: DataSizeInBytes !< Data size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = byte_size(this%value(1,1,1))*size(this%value)
    end function DimensionsWrapper3D_I4P_DataSizeInBytes


    function DimensionsWrapper3D_I4P_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_I4P_t), intent(IN) :: this           !< Dimensions wrapper 3D
        class(*),                         intent(IN) :: Mold           !< Mold for data type comparison
        logical                                      :: isOfDataType   !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (integer(I4P))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper3D_I4P_isOfDataType


    subroutine DimensionsWrapper3D_I4P_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_I4P_t), intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A,$)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = I4P'//&
                        ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                        ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                        ', Value = '
        write(unit=unit,fmt=*,iostat=iostatd,iomsg=iomsgd) str(no_sign=.true., n=this%Value)

        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper3D_I4P_Print


    subroutine DimensionsWrapper3D_I8P_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper3D
    !-----------------------------------------------------------------
        type(DimensionsWrapper3D_I8P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper3D_I8P_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set I8P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_I8P_t), intent(INOUT) :: this
        class(*),                         intent(IN)    :: Value(:,:,:)
        integer                                         :: err
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I8P))
                allocate(this%Value(size(Value,dim=1),  &
                                    size(Value,dim=2),  &
                                    size(Value,dim=3)), &
                                    stat=err)
                this%Value = Value
                if(err/=0) &
                    call msg%Error( txt='Setting Value: Allocation error ('//&
                                    str(no_sign=.true.,n=err)//')', &
                                    file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn( txt='Setting value: Expected data type (I8P)', &
                               file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    subroutine DimensionsWrapper3D_I8P_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get I8P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_I8P_t), intent(IN)  :: this
        class(*),                         intent(OUT) :: Value(:,:,:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I8P))
                if(all(this%GetShape() == shape(Value))) then
                    Value = this%Value
                else
                    call msg%Warn(txt='Getting value: Expected shape ('//    &
                                  str(no_sign=.true.,n=this%GetShape())//')',&
                                  file=__FILE__, line=__LINE__ )
                endif
            class Default
                call msg%Warn(txt='Getting value: Expected data type (I8P)',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    function DimensionsWrapper3D_I8P_GetShape(this) result(ValueShape) 
    !-----------------------------------------------------------------
    !< Get Wrapper Value Shape
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_I8P_t), intent(IN)  :: this
        integer(I4P), allocatable                     :: ValueShape(:)
    !-----------------------------------------------------------------
        allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper3D_I8P_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic pointer to Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_I8P_t), target, intent(IN)  :: this
        class(*), pointer                                     :: Value(:,:,:)
    !-----------------------------------------------------------------
        Value => this%Value
    end function


    subroutine DimensionsWrapper3D_I8P_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_I8P_t), intent(IN)  :: this
        class(*), allocatable,            intent(OUT) :: Value(:,:,:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1),  &
                       size(this%Value,dim=2),  &
                       size(this%Value,dim=3)), &
                       source=this%Value)
    end subroutine


    subroutine DimensionsWrapper3D_I8P_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper3D
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_I8P_t), intent(INOUT) :: this
        integer                                         :: err 
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    function DimensionsWrapper3D_I8P_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the size of the data stored in bytes
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_I8P_t), intent(IN) :: this            !< Dimensions wrapper 0D
        integer(I4P)                                 :: DataSizeInBytes !< Data size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = byte_size(this%value(1,1,1))*size(this%value)
    end function DimensionsWrapper3D_I8P_DataSizeInBytes


    function DimensionsWrapper3D_I8P_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_I8P_t), intent(IN) :: this          !< Dimensions wrapper 3D
        class(*),                         intent(IN) :: Mold          !< Mold for data type comparison
        logical                                      :: isOfDataType  !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (integer(I8P))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper3D_I8P_isOfDataType


    subroutine DimensionsWrapper3D_I8P_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_I8P_t), intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A,$)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = I8P'//&
                        ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                        ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                        ', Value = '
        write(unit=unit,fmt=*,iostat=iostatd,iomsg=iomsgd) str(no_sign=.true., n=this%Value)

        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper3D_I8P_Print


    subroutine DimensionsWrapper3D_L_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper3D
    !-----------------------------------------------------------------
        type(DimensionsWrapper3D_L_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper3D_L_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set logical Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_L_t), intent(INOUT) :: this
        class(*),                       intent(IN)    :: Value(:,:,:)
        integer                                       :: err
    !-----------------------------------------------------------------
        select type (Value)
            type is (logical)
                allocate(this%Value(size(Value,dim=1),  &
                                    size(Value,dim=2),  &
                                    size(Value,dim=3)), &
                                    stat=err)
                this%Value = Value
                if(err/=0) &
                    call msg%Error( txt='Setting Value: Allocation error ('//&
                                    str(no_sign=.true.,n=err)//')', &
                                    file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn( txt='Setting value: Expected data type (logical)', &
                               file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    subroutine DimensionsWrapper3D_L_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get logical Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_L_t), intent(IN)  :: this
        class(*),                       intent(OUT) :: Value(:,:,:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (logical)
                if(all(this%GetShape() == shape(Value))) then
                    Value = this%Value
                else
                    call msg%Warn(txt='Getting value: Expected shape ('//    &
                                  str(no_sign=.true.,n=this%GetShape())//')',&
                                  file=__FILE__, line=__LINE__ )
                endif
            class Default
                call msg%Warn(txt='Getting value: Expected data type (logical)',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    function DimensionsWrapper3D_L_GetShape(this) result(ValueShape) 
    !-----------------------------------------------------------------
    !< Get Wrapper Value Shape
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_L_t), intent(IN)  :: this
        integer(I4P), allocatable                   :: ValueShape(:)
    !-----------------------------------------------------------------
        allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper3D_L_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic pointer to Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_L_t), target, intent(IN)  :: this
        class(*), pointer                                   :: Value(:,:,:)
    !-----------------------------------------------------------------
        Value => this%Value
    end function


    subroutine DimensionsWrapper3D_L_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_L_t), intent(IN)  :: this
        class(*), allocatable,          intent(OUT) :: Value(:,:,:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1),  &
                       size(this%Value,dim=2),  &
                       size(this%Value,dim=3)), &
                       source=this%Value)
    end subroutine


    subroutine DimensionsWrapper3D_L_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper3D
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_L_t), intent(INOUT) :: this
        integer                                       :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    function DimensionsWrapper3D_L_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the size of the data stored in bytes
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_L_t), intent(IN) :: this            !< Dimensions wrapper 0D
        integer(I4P)                                 :: DataSizeInBytes !< Data size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = byte_size_logical(this%value(1,1,1))*size(this%value)
    end function DimensionsWrapper3D_L_DataSizeInBytes


    function DimensionsWrapper3D_L_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_L_t), intent(IN) :: this            !< Dimensions wrapper 3D
        class(*),                       intent(IN) :: Mold            !< Mold for data type comparison
        logical                                    :: isOfDataType    !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (logical)
                isOfDataType = .true.
        end select
    end function DimensionsWrapper3D_L_isOfDataType


    subroutine DimensionsWrapper3D_L_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_L_t), intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A,$)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = L'//&
                        ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                        ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                        ', Value = '
        write(unit=unit,fmt=*,iostat=iostatd,iomsg=iomsgd) str(n=this%Value)
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper3D_L_Print


    subroutine DimensionsWrapper3D_R4P_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper3D
    !-----------------------------------------------------------------
        type(DimensionsWrapper3D_R4P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper3D_R4P_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set R4P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_R4P_t), intent(INOUT) :: this
        class(*),                         intent(IN)    :: Value(:,:,:)
        integer                                         :: err
    !-----------------------------------------------------------------
        select type (Value)
            type is (real(R4P))
                allocate(this%Value(size(Value,dim=1),  &
                                    size(Value,dim=2),  &
                                    size(Value,dim=3)), &
                                    stat=err)
                this%Value = Value
                if(err/=0) &
                    call msg%Error( txt='Setting Value: Allocation error ('//&
                                    str(no_sign=.true.,n=err)//')', &
                                    file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn( txt='Setting value: Expected data type (R4P)', &
                               file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    subroutine DimensionsWrapper3D_R4P_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get R4P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_R4P_t), intent(IN)  :: this
        class(*),                         intent(OUT) :: Value(:,:,:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (real(R4P))
                if(all(this%GetShape() == shape(Value))) then
                    Value = this%Value
                else
                    call msg%Warn(txt='Getting value: Expected shape ('//    &
                                  str(no_sign=.true.,n=this%GetShape())//')',&
                                  file=__FILE__, line=__LINE__ )
                endif
            class Default
                call msg%Warn(txt='Getting value: Expected data type (R4P)',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    function DimensionsWrapper3D_r4P_GetShape(this) result(ValueShape) 
    !-----------------------------------------------------------------
    !< Get Wrapper Value Shape
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_R4P_t), intent(IN)  :: this
        integer(I4P), allocatable                     :: ValueShape(:)
    !-----------------------------------------------------------------
        allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper3D_R4P_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic pointer to Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_R4P_t), target, intent(IN)  :: this
        class(*), pointer                                     :: Value(:,:,:)
    !-----------------------------------------------------------------
        Value => this%Value
    end function


    subroutine DimensionsWrapper3D_R4P_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_R4P_t), intent(IN)  :: this
        class(*), allocatable,            intent(OUT) :: Value(:,:,:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1),  &
                       size(this%Value,dim=2),  &
                       size(this%Value,dim=3)), &
                       source=this%Value)
    end subroutine


    subroutine DimensionsWrapper3D_R4P_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper3D
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_R4P_t), intent(INOUT) :: this
        integer                                         :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    function DimensionsWrapper3D_R4P_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the size of the data stored in bytes
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_R4P_t), intent(IN) :: this            !< Dimensions wrapper 0D
        integer(I4P)                                 :: DataSizeInBytes !< Data size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = byte_size(this%value(1,1,1))*size(this%value)
    end function DimensionsWrapper3D_R4P_DataSizeInBytes


    function DimensionsWrapper3D_R4P_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_R4P_t), intent(IN) :: this          !< Dimensions wrapper 3D
        class(*),                         intent(IN) :: Mold          !< Mold for data type comparison
        logical                                      :: isOfDataType  !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (real(R4P))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper3D_R4P_isOfDataType


    subroutine DimensionsWrapper3D_R4P_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_R4P_t), intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A,$)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = R4P'//&
                        ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                        ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                        ', Value = '
        write(unit=unit,fmt=*,iostat=iostatd,iomsg=iomsgd) str(no_sign=.true., n=this%Value)
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper3D_R4P_Print


    subroutine DimensionsWrapper3D_R8P_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper3D
    !-----------------------------------------------------------------
        type(DimensionsWrapper3D_R8P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper3D_R8P_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set R8P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_R8P_t), intent(INOUT) :: this
        class(*),                         intent(IN)    :: Value(:,:,:)
        integer                                         :: err
    !-----------------------------------------------------------------
        select type (Value)
            type is (real(R8P))
                allocate(this%Value(size(Value,dim=1),  &
                                    size(Value,dim=2),  &
                                    size(Value,dim=3)), &
                                    stat=err)
                this%Value = Value
                if(err/=0) &
                    call msg%Error( txt='Setting Value: Allocation error ('//&
                                    str(no_sign=.true.,n=err)//')', &
                                    file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn( txt='Setting value: Expected data type (R8P)', &
                               file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    subroutine DimensionsWrapper3D_R8P_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get R8P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_R8P_t), intent(IN)  :: this
        class(*),                         intent(OUT) :: Value(:,:,:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (real(R8P))
                if(all(this%GetShape() == shape(Value))) then
                    Value = this%Value
                else
                    call msg%Warn(txt='Getting value: Expected shape ('//    &
                                  str(no_sign=.true.,n=this%GetShape())//')',&
                                  file=__FILE__, line=__LINE__ )
                endif
            class Default
                call msg%Warn(txt='Getting value: Expected data type (R4P)',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    function DimensionsWrapper3D_R8P_GetShape(this) result(ValueShape) 
    !-----------------------------------------------------------------
    !< Get Wrapper Value Shape
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_R8P_t), intent(IN)  :: this
        integer(I4P), allocatable                     :: ValueShape(:)
    !-----------------------------------------------------------------
        allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper3D_R8P_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic pointer to Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_R8P_t), target, intent(IN)  :: this
        class(*), pointer                                     :: Value(:,:,:)
    !-----------------------------------------------------------------
        Value => this%Value
    end function


    subroutine DimensionsWrapper3D_R8P_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_R8P_t), intent(IN)  :: this
        class(*), allocatable,            intent(OUT) :: Value(:,:,:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1),  &
                       size(this%Value,dim=2),  &
                       size(this%Value,dim=3)), &
                       source=this%Value)
    end subroutine


    subroutine DimensionsWrapper3D_R8P_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper3D
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_R8P_t), intent(INOUT) :: this
        integer                                         :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    function DimensionsWrapper3D_R8P_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the size of the data stored in bytes
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_R8P_t), intent(IN) :: this            !< Dimensions wrapper 0D
        integer(I4P)                                 :: DataSizeInBytes !< Data size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = byte_size(this%value(1,1,1))*size(this%value)
    end function DimensionsWrapper3D_R8P_DataSizeInBytes


    function DimensionsWrapper3D_R8P_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_R8P_t), intent(IN) :: this          !< Dimensions wrapper 3D
        class(*),                         intent(IN) :: Mold          !< Mold for data type comparison
        logical                                      :: isOfDataType  !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (real(R8P))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper3D_R8P_isOfDataType


    subroutine DimensionsWrapper3D_R8P_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper3D_R8P_t), intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A,$)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = R8P'//&
                        ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                        ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                        ', Value = '
        write(unit=unit,fmt=*,iostat=iostatd,iomsg=iomsgd) str(no_sign=.true., n=this%Value)

        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper3D_R8P_Print


    subroutine DimensionsWrapper4D_DLCA_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper4D
    !-----------------------------------------------------------------
        type(DimensionsWrapper4D_DLCA_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper4D_DLCA_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set DLCA Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_DLCA_t), intent(INOUT) :: this
        class(*),                          intent(IN)    :: Value(:,:,:,:)
        integer                                          :: err
    !-----------------------------------------------------------------
#ifdef __GFORTRAN__ 
        call msg%Warn(txt='Setting value: Array of deferred length allocatable arrays not supported in Gfortran)',&
                      file=__FILE__, line=__LINE__ )
#else   
        select type (Value)
            type is (character(len=*))
                allocate(character(len=len(Value))::               &
                                    this%Value(size(Value,dim=1),  &
                                    size(Value,dim=2),             &
                                    size(Value,dim=3),             &
                                    size(Value,dim=4)),            &
                                    stat=err)
                this%Value = Value
                if(err/=0) &
                    call msg%Error( txt='Setting Value: Allocation error ('//&
                                    str(no_sign=.true.,n=err)//')', &
                                    file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn( txt='Setting value: Expected data type (character(*))', &
                               file=__FILE__, line=__LINE__ )
        end select
#endif
    end subroutine


    subroutine DimensionsWrapper4D_DLCA_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get deferred length character array Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_DLCA_t), intent(IN)  :: this
        class(*),                          intent(OUT) :: Value(:,:,:,:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (character(len=*))
                if(all(this%GetShape() == shape(Value))) then
                    Value = this%Value
                else
                    call msg%Warn(txt='Getting value: Expected shape ('//    &
                                  str(no_sign=.true.,n=this%GetShape())//')',&
                                  file=__FILE__, line=__LINE__ )
                endif
            class Default
                call msg%Warn(txt='Getting value: Expected data type (character(*))',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    function DimensionsWrapper4D_DLCA_GetShape(this) result(ValueShape) 
    !-----------------------------------------------------------------
    !< Get Wrapper Value Shape
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_DLCA_t), intent(IN)  :: this
        integer(I4P), allocatable                      :: ValueShape(:)
    !-----------------------------------------------------------------
        allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper4D_DLCA_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic pointer to Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_DLCA_t), target, intent(IN)  :: this
        class(*), pointer                                      :: Value(:,:,:,:)
    !-----------------------------------------------------------------
        Value => this%Value
    end function


    subroutine DimensionsWrapper4D_DLCA_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_DLCA_t), intent(IN)  :: this
        class(*), allocatable,             intent(OUT) :: Value(:,:,:,:)
    !-----------------------------------------------------------------
!        allocate(Value(size(this%Value,dim=1),  &
!                       size(this%Value,dim=2),  &
!                       size(this%Value,dim=3),  &
!                       size(this%Value,dim=4)), &
!                       source=this%Value)
    end subroutine


    subroutine DimensionsWrapper4D_DLCA_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper4D
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_DLCA_t), intent(INOUT) :: this
        integer                                          :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    function DimensionsWrapper4D_DLCA_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the size of the data stored in bytes
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_DLCA_t), intent(IN) :: this            !< Dimensions wrapper 0D
        integer(I4P)                                  :: DataSizeInBytes !< Data size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = 0
        if(allocated(this%value)) DataSizeInBytes = byte_size(this%value(1,1,1,1))*size(this%value)
    end function DimensionsWrapper4D_DLCA_DataSizeInBytes


    function DimensionsWrapper4D_DLCA_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_DLCA_t), intent(IN) :: this         !< Dimensions wrapper 4D
        class(*),                          intent(IN) :: Mold         !< Mold for data type comparison
        logical                                  :: isOfDataType      !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (character(len=*))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper4D_DLCA_isOfDataType


    subroutine DimensionsWrapper4D_DLCA_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_DLCA_t),intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A,$)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = DLCA'//&
                        ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                        ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                        ', Value = '
        write(unit=unit,fmt=*,iostat=iostatd,iomsg=iomsgd) this%Value

        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper4D_DLCA_Print


    subroutine DimensionsWrapper4D_I1P_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper4D
    !-----------------------------------------------------------------
        type(DimensionsWrapper4D_I1P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper4D_I1P_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set I1P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_I1P_t), intent(INOUT) :: this
        class(*),                         intent(IN)    :: Value(:,:,:,:)
        integer                                         :: err
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I1P))
                allocate(this%Value(size(Value,dim=1),  &
                                    size(Value,dim=2),  &
                                    size(Value,dim=3),  &
                                    size(Value,dim=4)), &
                                    stat=err)
                this%Value = Value
                if(err/=0) &
                    call msg%Error( txt='Setting Value: Allocation error ('//&
                                    str(no_sign=.true.,n=err)//')', &
                                    file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn( txt='Setting value: Expected data type (I1P)', &
                               file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    subroutine DimensionsWrapper4D_I1P_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get I1P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_I1P_t), intent(IN)  :: this
        class(*),                         intent(OUT) :: Value(:,:,:,:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I1P))
                if(all(this%GetShape() == shape(Value))) then
                    Value = this%Value
                else
                    call msg%Warn(txt='Getting value: Expected shape ('//    &
                                  str(no_sign=.true.,n=this%GetShape())//')',&
                                  file=__FILE__, line=__LINE__ )
                endif
            class Default
                call msg%Warn(txt='Getting value: Expected data type (I1P)',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    function DimensionsWrapper4D_I1P_GetShape(this) result(ValueShape) 
    !-----------------------------------------------------------------
    !< Get Wrapper Value Shape
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_I1P_t), intent(IN)  :: this
        integer(I4P), allocatable                     :: ValueShape(:)
    !-----------------------------------------------------------------
        allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper4D_I1P_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic pointer to Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_I1P_t), target, intent(IN)  :: this
        class(*), pointer                                     :: Value(:,:,:,:)
    !-----------------------------------------------------------------
        Value => this%Value
    end function


    subroutine DimensionsWrapper4D_I1P_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_I1P_t), intent(IN)  :: this
        class(*), allocatable,            intent(OUT) :: Value(:,:,:,:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1),  &
                       size(this%Value,dim=2),  &
                       size(this%Value,dim=3),  &
                       size(this%Value,dim=4)), &
                       source=this%Value)
    end subroutine


    subroutine DimensionsWrapper4D_I1P_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper4D
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_I1P_t), intent(INOUT) :: this
        integer                                         :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    function DimensionsWrapper4D_I1P_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the size of the data stored in bytes
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_I1P_t), intent(IN) :: this            !< Dimensions wrapper 0D
        integer(I4P)                                 :: DataSizeInBytes !< Data size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = byte_size(this%value(1,1,1,1))*size(this%value)
    end function DimensionsWrapper4D_I1P_DataSizeInBytes


    function DimensionsWrapper4D_I1P_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_I1P_t), intent(IN) :: this          !< Dimensions wrapper 4D
        class(*),                         intent(IN) :: Mold          !< Mold for data type comparison
        logical                                      :: isOfDataType  !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (integer(I1P))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper4D_I1P_isOfDataType


    subroutine DimensionsWrapper4D_I1P_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_I1P_t), intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A,$)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = I1P'//&
                        ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                        ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                        ', Value = '
        write(unit=unit,fmt=*,iostat=iostatd,iomsg=iomsgd) str(no_sign=.true., n=this%Value)

        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper4D_I1P_Print


    subroutine DimensionsWrapper4D_I2P_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper4D
    !-----------------------------------------------------------------
        type(DimensionsWrapper4D_I2P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper4D_I2P_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set I2P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_I2P_t), intent(INOUT) :: this
        class(*),                         intent(IN)    :: Value(:,:,:,:)
        integer                                         :: err
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I2P))
                allocate(this%Value(size(Value,dim=1),  &
                                    size(Value,dim=2),  &
                                    size(Value,dim=3),  &
                                    size(Value,dim=4)), &
                                    stat=err)
                this%Value = Value
                if(err/=0) &
                    call msg%Error( txt='Setting Value: Allocation error ('//&
                                    str(no_sign=.true.,n=err)//')', &
                                    file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn( txt='Setting value: Expected data type (I2P)', &
                               file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    subroutine DimensionsWrapper4D_I2P_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get I2P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_I2P_t), intent(IN)  :: this
        class(*),                         intent(OUT) :: Value(:,:,:,:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I2P))
                if(all(this%GetShape() == shape(Value))) then
                    Value = this%Value
                else
                    call msg%Warn(txt='Getting value: Expected shape ('//    &
                                  str(no_sign=.true.,n=this%GetShape())//')',&
                                  file=__FILE__, line=__LINE__ )
                endif
            class Default
                call msg%Warn(txt='Getting value: Expected data type (I2P)',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    function DimensionsWrapper4D_I2P_GetShape(this) result(ValueShape) 
    !-----------------------------------------------------------------
    !< Get Wrapper Value Shape
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_I2P_t), intent(IN)  :: this
        integer(I4P), allocatable                     :: ValueShape(:)
    !-----------------------------------------------------------------
        allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper4D_I2P_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic pointer to Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_I2P_t), target, intent(IN)  :: this
        class(*), pointer                                     :: Value(:,:,:,:)
    !-----------------------------------------------------------------
        Value => this%Value
    end function


    subroutine DimensionsWrapper4D_I2P_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_I2P_t), intent(IN)  :: this
        class(*), allocatable,            intent(OUT) :: Value(:,:,:,:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1),  &
                       size(this%Value,dim=2),  &
                       size(this%Value,dim=3),  &
                       size(this%Value,dim=4)), &
                       source=this%Value)
    end subroutine


    subroutine DimensionsWrapper4D_I2P_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper4D
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_I2P_t), intent(INOUT) :: this
        integer                                         :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    function DimensionsWrapper4D_I2P_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the size of the data stored in bytes
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_I2P_t), intent(IN) :: this            !< Dimensions wrapper 0D
        integer(I4P)                                 :: DataSizeInBytes !< Data size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = byte_size(this%value(1,1,1,1))*size(this%value)
    end function DimensionsWrapper4D_I2P_DataSizeInBytes


    function DimensionsWrapper4D_I2P_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_I2P_t), intent(IN) :: this          !< Dimensions wrapper 4D
        class(*),                         intent(IN) :: Mold          !< Mold for data type comparison
        logical                                      :: isOfDataType  !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (integer(I2P))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper4D_I2P_isOfDataType


    subroutine DimensionsWrapper4D_I2P_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_I2P_t), intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A,$)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = I2P'//&
                        ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                        ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                        ', Value = '
        write(unit=unit,fmt=*,iostat=iostatd,iomsg=iomsgd) str(no_sign=.true., n=this%Value)

        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper4D_I2P_Print


    subroutine DimensionsWrapper4D_I4P_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper4D
    !-----------------------------------------------------------------
        type(DimensionsWrapper4D_I4P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper4D_I4P_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set I4P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_I4P_t), intent(INOUT) :: this
        class(*),                         intent(IN)    :: Value(:,:,:,:)
        integer                                         :: err
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I4P))
                allocate(this%Value(size(Value,dim=1),  &
                                    size(Value,dim=2),  &
                                    size(Value,dim=3),  &
                                    size(Value,dim=4)), &
                                    stat=err)
                this%Value = Value
                if(err/=0) &
                    call msg%Error( txt='Setting Value: Allocation error ('//&
                                    str(no_sign=.true.,n=err)//')', &
                                    file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn( txt='Setting value: Expected data type (I4P)', &
                               file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    subroutine DimensionsWrapper4D_I4P_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get I4P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_I4P_t), intent(IN)  :: this
        class(*),                         intent(OUT) :: Value(:,:,:,:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I4P))
                if(all(this%GetShape() == shape(Value))) then
                    Value = this%Value
                else
                    call msg%Warn(txt='Getting value: Expected shape ('//    &
                                  str(no_sign=.true.,n=this%GetShape())//')',&
                                  file=__FILE__, line=__LINE__ )
                endif
            class Default
                call msg%Warn(txt='Getting value: Expected data type (I4P)',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    function DimensionsWrapper4D_I4P_GetShape(this) result(ValueShape) 
    !-----------------------------------------------------------------
    !< Get Wrapper Value Shape
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_I4P_t), intent(IN)  :: this
        integer(I4P), allocatable                     :: ValueShape(:)
    !-----------------------------------------------------------------
        allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper4D_I4P_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic pointer to Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_I4P_t), target, intent(IN)  :: this
        class(*), pointer                                     :: Value(:,:,:,:)
    !-----------------------------------------------------------------
        Value => this%Value
    end function


    subroutine DimensionsWrapper4D_I4P_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_I4P_t), intent(IN)  :: this
        class(*), allocatable,            intent(OUT) :: Value(:,:,:,:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1),  &
                       size(this%Value,dim=2),  &
                       size(this%Value,dim=3),  &
                       size(this%Value,dim=4)), &
                       source=this%Value)
    end subroutine


    subroutine DimensionsWrapper4D_I4P_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper4D
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_I4P_t), intent(INOUT) :: this
        integer                                         :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    function DimensionsWrapper4D_I4P_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the size of the data stored in bytes
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_I4P_t), intent(IN) :: this            !< Dimensions wrapper 0D
        integer(I4P)                                 :: DataSizeInBytes !< Data size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = byte_size(this%value(1,1,1,1))*size(this%value)
    end function DimensionsWrapper4D_I4P_DataSizeInBytes


    function DimensionsWrapper4D_I4P_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_I4P_t), intent(IN) :: this           !< Dimensions wrapper 4D
        class(*),                         intent(IN) :: Mold           !< Mold for data type comparison
        logical                                      :: isOfDataType   !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (integer(I4P))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper4D_I4P_isOfDataType


    subroutine DimensionsWrapper4D_I4P_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_I4P_t), intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A,$)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = I4P'//&
                        ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                        ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                        ', Value = '
        write(unit=unit,fmt=*,iostat=iostatd,iomsg=iomsgd) str(no_sign=.true., n=this%Value)

        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper4D_I4P_Print


    subroutine DimensionsWrapper4D_I8P_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper4D
    !-----------------------------------------------------------------
        type(DimensionsWrapper4D_I8P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper4D_I8P_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set I8P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_I8P_t), intent(INOUT) :: this
        class(*),                         intent(IN)    :: Value(:,:,:,:)
        integer                                         :: err
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I8P))
                allocate(this%Value(size(Value,dim=1),  &
                                    size(Value,dim=2),  &
                                    size(Value,dim=3),  &
                                    size(Value,dim=4)), &
                                    stat=err)
                if(err/=0) &
                    call msg%Error( txt='Setting Value: Allocation error ('//&
                                    str(no_sign=.true.,n=err)//')', &
                                    file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn( txt='Setting value: Expected data type (I8P)', &
                               file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    subroutine DimensionsWrapper4D_I8P_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get I8P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_I8P_t), intent(IN)  :: this
        class(*),                         intent(OUT) :: Value(:,:,:,:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I8P))
                if(all(this%GetShape() == shape(Value))) then
                    Value = this%Value
                else
                    call msg%Warn(txt='Getting value: Expected shape ('//    &
                                  str(no_sign=.true.,n=this%GetShape())//')',&
                                  file=__FILE__, line=__LINE__ )
                endif
            class Default
                call msg%Warn(txt='Getting value: Expected data type (I8P)',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    function DimensionsWrapper4D_I8P_GetShape(this) result(ValueShape) 
    !-----------------------------------------------------------------
    !< Get Wrapper Value Shape
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_I8P_t), intent(IN)  :: this
        integer(I4P), allocatable                     :: ValueShape(:)
    !-----------------------------------------------------------------
        allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper4D_I8P_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic pointer to Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_I8P_t), target, intent(IN)  :: this
        class(*), pointer                                     :: Value(:,:,:,:)
    !-----------------------------------------------------------------
        Value => this%Value
    end function



    subroutine DimensionsWrapper4D_I8P_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_I8P_t), intent(IN)  :: this
        class(*), allocatable,            intent(OUT) :: Value(:,:,:,:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1),  &
                       size(this%Value,dim=2),  &
                       size(this%Value,dim=3),  &
                       size(this%Value,dim=4)), &
                       source=this%Value)
    end subroutine


    subroutine DimensionsWrapper4D_I8P_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper4D
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_I8P_t), intent(INOUT) :: this
        integer                                         :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    function DimensionsWrapper4D_I8P_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the size of the data stored in bytes
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_I8P_t), intent(IN) :: this            !< Dimensions wrapper 0D
        integer(I4P)                                 :: DataSizeInBytes !< Data size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = byte_size(this%value(1,1,1,1))*size(this%value)
    end function DimensionsWrapper4D_I8P_DataSizeInBytes


    function DimensionsWrapper4D_I8P_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_I8P_t), intent(IN) :: this          !< Dimensions wrapper 4D
        class(*),                         intent(IN) :: Mold          !< Mold for data type comparison
        logical                                      :: isOfDataType  !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (integer(I8P))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper4D_I8P_isOfDataType


    subroutine DimensionsWrapper4D_I8P_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_I8P_t), intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A,$)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = I8P'//&
                        ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                        ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                        ', Value = '
        write(unit=unit,fmt=*,iostat=iostatd,iomsg=iomsgd) str(no_sign=.true., n=this%Value)

        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper4D_I8P_Print


    subroutine DimensionsWrapper4D_L_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper4D
    !-----------------------------------------------------------------
        type(DimensionsWrapper4D_L_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper4D_L_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set logical Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_L_t), intent(INOUT) :: this
        class(*),                       intent(IN)    :: Value(:,:,:,:)
        integer                                       :: err
    !-----------------------------------------------------------------
        select type (Value)
            type is (logical)
                allocate(this%Value(size(Value,dim=1),  &
                                    size(Value,dim=2),  &
                                    size(Value,dim=3),  &
                                    size(Value,dim=4)), &
                                    stat=err)
                this%Value = Value
                if(err/=0) &
                    call msg%Error( txt='Setting Value: Allocation error ('//&
                                    str(no_sign=.true.,n=err)//')', &
                                    file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn( txt='Setting value: Expected data type (logical)', &
                               file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    subroutine DimensionsWrapper4D_L_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get logical Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_L_t), intent(IN)   :: this
        class(*),                       intent(OUT) :: Value(:,:,:,:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (logical)
                if(all(this%GetShape() == shape(Value))) then
                    Value = this%Value
                else
                    call msg%Warn(txt='Getting value: Expected shape ('//    &
                                  str(no_sign=.true.,n=this%GetShape())//')',&
                                  file=__FILE__, line=__LINE__ )
                endif
            class Default
                call msg%Warn(txt='Getting value: Expected data type (logical)',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    function DimensionsWrapper4D_L_GetShape(this) result(ValueShape) 
    !-----------------------------------------------------------------
    !< Get Wrapper Value Shape
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_L_t), intent(IN)  :: this
        integer(I4P), allocatable                   :: ValueShape(:)
    !-----------------------------------------------------------------
        allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper4D_L_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic pointer to Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_L_t), target, intent(IN)  :: this
        class(*), pointer                                   :: Value(:,:,:,:)
    !-----------------------------------------------------------------
        Value => this%Value
    end function


    subroutine DimensionsWrapper4D_L_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_L_t), intent(IN)  :: this
        class(*), allocatable,          intent(OUT) :: Value(:,:,:,:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1),  &
                       size(this%Value,dim=2),  &
                       size(this%Value,dim=3),  &
                       size(this%Value,dim=4)), &
                       source=this%Value)
    end subroutine


    subroutine DimensionsWrapper4D_L_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper4D
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_L_t), intent(INOUT) :: this
        integer                                       :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    function DimensionsWrapper4D_L_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the size of the data stored in bytes
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_L_t), intent(IN) :: this            !< Dimensions wrapper 0D
        integer(I4P)                               :: DataSizeInBytes !< Data size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = byte_size_logical(this%value(1,1,1,1))*size(this%value)
    end function DimensionsWrapper4D_L_DataSizeInBytes


    function DimensionsWrapper4D_L_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_L_t), intent(IN) :: this            !< Dimensions wrapper 4D
        class(*),                       intent(IN) :: Mold            !< Mold for data type comparison
        logical                                    :: isOfDataType    !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (logical)
                isOfDataType = .true.
        end select
    end function DimensionsWrapper4D_L_isOfDataType


    subroutine DimensionsWrapper4D_L_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_L_t),   intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A,$)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = L'//&
                        ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                        ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                        ', Value = '
        write(unit=unit,fmt=*,iostat=iostatd,iomsg=iomsgd) str(n=this%Value)

        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper4D_L_Print


    subroutine DimensionsWrapper4D_R4P_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper4D
    !-----------------------------------------------------------------
        type(DimensionsWrapper4D_R4P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper4D_R4P_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set R4P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_R4P_t), intent(INOUT) :: this
        class(*),                         intent(IN)    :: Value(:,:,:,:)
        integer                                         :: err
    !-----------------------------------------------------------------
        select type (Value)
            type is (real(R4P))
                allocate(this%Value(size(Value,dim=1),  &
                                    size(Value,dim=2),  &
                                    size(Value,dim=3),  &
                                    size(Value,dim=4)), &
                                    stat=err)
                this%Value = Value
                if(err/=0) &
                    call msg%Error( txt='Setting Value: Allocation error ('//&
                                    str(no_sign=.true.,n=err)//')', &
                                    file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn( txt='Setting value: Expected data type (R4P)', &
                               file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    subroutine DimensionsWrapper4D_R4P_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get R4P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_R4P_t), intent(IN)  :: this
        class(*),                         intent(OUT) :: Value(:,:,:,:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (real(R4P))
                if(all(this%GetShape() == shape(Value))) then
                    Value = this%Value
                else
                    call msg%Warn(txt='Getting value: Expected shape ('//    &
                                  str(no_sign=.true.,n=this%GetShape())//')',&
                                  file=__FILE__, line=__LINE__ )
                endif
            class Default
                call msg%Warn(txt='Getting value: Expected data type (R4P)',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    function DimensionsWrapper4D_R4P_GetShape(this) result(ValueShape) 
    !-----------------------------------------------------------------
    !< Get Wrapper Value Shape
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_R4P_t), intent(IN)  :: this
        integer(I4P), allocatable                     :: ValueShape(:)
    !-----------------------------------------------------------------
        allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper4D_R4P_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic pointer to Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_R4P_t), target, intent(IN)  :: this
        class(*), pointer                                     :: Value(:,:,:,:)
    !-----------------------------------------------------------------
        Value => this%Value
    end function


    subroutine DimensionsWrapper4D_R4P_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_R4P_t), intent(IN)  :: this
        class(*), allocatable,            intent(OUT) :: Value(:,:,:,:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1),  &
                       size(this%Value,dim=2),  &
                       size(this%Value,dim=3),  &
                       size(this%Value,dim=4)), &
                       source=this%Value)
    end subroutine


    subroutine DimensionsWrapper4D_R4P_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper4D
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_R4P_t), intent(INOUT) :: this
        integer                                         :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    function DimensionsWrapper4D_R4P_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the size of the data stored in bytes
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_R4P_t), intent(IN) :: this            !< Dimensions wrapper 0D
        integer(I4P)                                 :: DataSizeInBytes !< Data size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = byte_size(this%value(1,1,1,1))*size(this%value)
    end function DimensionsWrapper4D_R4P_DataSizeInBytes


    function DimensionsWrapper4D_R4P_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_R4P_t), intent(IN) :: this          !< Dimensions wrapper 4D
        class(*),                         intent(IN) :: Mold          !< Mold for data type comparison
        logical                                      :: isOfDataType  !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (real(R4P))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper4D_R4P_isOfDataType


    subroutine DimensionsWrapper4D_R4P_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_R4P_t), intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A,$)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = R4P'//&
                        ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                        ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                        ', Value = '
        write(unit=unit,fmt=*,iostat=iostatd,iomsg=iomsgd) str(no_sign=.true., n=this%Value)

        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper4D_R4P_Print


    subroutine DimensionsWrapper4D_R8P_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper4D
    !-----------------------------------------------------------------
        type(DimensionsWrapper4D_R8P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper4D_R8P_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set R8P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_R8P_t), intent(INOUT) :: this
        class(*),                         intent(IN)    :: Value(:,:,:,:)
        integer                                         :: err
    !-----------------------------------------------------------------
        select type (Value)
            type is (real(R8P))
                allocate(this%Value(size(Value,dim=1),  &
                                    size(Value,dim=2),  &
                                    size(Value,dim=3),  &
                                    size(Value,dim=4)), &
                                    stat=err)
                this%Value = Value
                if(err/=0) &
                    call msg%Error( txt='Setting Value: Allocation error ('//&
                                    str(no_sign=.true.,n=err)//')', &
                                    file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn( txt='Setting value: Expected data type (R8P)', &
                               file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    subroutine DimensionsWrapper4D_R8P_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get R8P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_R8P_t), intent(IN)  :: this
        class(*),                         intent(OUT) :: Value(:,:,:,:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (real(R8P))
                if(all(this%GetShape() == shape(Value))) then
                    Value = this%Value
                else
                    call msg%Warn(txt='Getting value: Expected shape ('//    &
                                  str(no_sign=.true.,n=this%GetShape())//')',&
                                  file=__FILE__, line=__LINE__ )
                endif
            class Default
                call msg%Warn(txt='Getting value: Expected data type (R8P)',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    function DimensionsWrapper4D_R8P_GetShape(this) result(ValueShape) 
    !-----------------------------------------------------------------
    !< Get Wrapper Value Shape
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_R8P_t), intent(IN)  :: this
        integer(I4P), allocatable                     :: ValueShape(:)
    !-----------------------------------------------------------------
        allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper4D_R8P_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic pointer to Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_R8P_t), target, intent(IN)  :: this
        class(*), pointer                                     :: Value(:,:,:,:)
    !-----------------------------------------------------------------
        Value => this%Value
    end function


    subroutine DimensionsWrapper4D_R8P_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_R8P_t), intent(IN)  :: this
        class(*), allocatable,            intent(OUT) :: Value(:,:,:,:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1),  &
                       size(this%Value,dim=2),  &
                       size(this%Value,dim=3),  &
                       size(this%Value,dim=4)), &
                       source=this%Value)
    end subroutine


    subroutine DimensionsWrapper4D_R8P_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper4D
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_R8P_t), intent(INOUT) :: this
        integer                                         :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    function DimensionsWrapper4D_R8P_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the size of the data stored in bytes
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_R8P_t), intent(IN) :: this            !< Dimensions wrapper 0D
        integer(I4P)                                 :: DataSizeInBytes !< Data size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = byte_size(this%value(1,1,1,1))*size(this%value)
    end function DimensionsWrapper4D_R8P_DataSizeInBytes


    function DimensionsWrapper4D_R8P_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_R8P_t), intent(IN) :: this          !< Dimensions wrapper 4D
        class(*),                         intent(IN) :: Mold          !< Mold for data type comparison
        logical                                      :: isOfDataType  !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (real(R8P))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper4D_R8P_isOfDataType


    subroutine DimensionsWrapper4D_R8P_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper4D_R8P_t), intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A,$)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = R8P'//&
                        ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                        ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                        ', Value = '
        write(unit=unit,fmt=*,iostat=iostatd,iomsg=iomsgd) str(no_sign=.true., n=this%Value)

        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper4D_R8P_Print


    subroutine DimensionsWrapper5D_DLCA_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper5D
    !-----------------------------------------------------------------
        type(DimensionsWrapper5D_DLCA_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper5D_DLCA_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set DLCA Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_DLCA_t), intent(INOUT) :: this
        class(*),                          intent(IN)    :: Value(:,:,:,:,:)
        integer                                          :: err
    !-----------------------------------------------------------------
#ifdef __GFORTRAN__ 
        call msg%Warn(txt='Setting value: Array of deferred length allocatable arrays not supported in Gfortran)',&
                      file=__FILE__, line=__LINE__ )
#else   
        select type (Value)
            type is (character(len=*))
                allocate(character(len=len(Value))::               &
                                    this%Value(size(Value,dim=1),  &
                                    size(Value,dim=2),             &
                                    size(Value,dim=3),             &
                                    size(Value,dim=4),             &
                                    size(Value,dim=5)),            &
                                    stat=err)
                this%Value = Value
                if(err/=0) &
                    call msg%Error( txt='Setting Value: Allocation error ('//&
                                    str(no_sign=.true.,n=err)//')', &
                                    file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn( txt='Setting value: Expected data type (character(*))', &
                               file=__FILE__, line=__LINE__ )

        end select
#endif
    end subroutine


    subroutine DimensionsWrapper5D_DLCA_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get deferred length character array Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_DLCA_t), intent(IN)  :: this
        class(*),                          intent(OUT) :: Value(:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (character(len=*))
                if(all(this%GetShape() == shape(Value))) then
                    Value = this%Value
                else
                    call msg%Warn(txt='Getting value: Expected shape ('//    &
                                  str(no_sign=.true.,n=this%GetShape())//')',&
                                  file=__FILE__, line=__LINE__ )
                endif
            class Default
                call msg%Warn(txt='Getting value: Expected data type (character(*))',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    function DimensionsWrapper5D_DLCA_GetShape(this) result(ValueShape) 
    !-----------------------------------------------------------------
    !< Get Wrapper Value Shape
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_DLCA_t), intent(IN)  :: this
        integer(I4P), allocatable                      :: ValueShape(:)
    !-----------------------------------------------------------------
        allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper5D_DLCA_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic pointer to Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_DLCA_t), target, intent(IN)  :: this
        class(*), pointer                                      :: Value(:,:,:,:,:)
    !-----------------------------------------------------------------
        Value => this%Value
    end function


    subroutine DimensionsWrapper5D_DLCA_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_DLCA_t), intent(IN)  :: this
        class(*), allocatable,             intent(OUT) :: Value(:,:,:,:,:)
    !-----------------------------------------------------------------
!        allocate(Value(size(this%Value,dim=1),  &
!                       size(this%Value,dim=2),  &
!                       size(this%Value,dim=3),  &
!                       size(this%Value,dim=4),  &
!                       size(this%Value,dim=5)), &
!                       source=this%Value)
    end subroutine


    subroutine DimensionsWrapper5D_DLCA_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper5D
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_DLCA_t), intent(INOUT) :: this
        integer                                         :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    function DimensionsWrapper5D_DLCA_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the size of the data stored in bytes
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_DLCA_t), intent(IN) :: this            !< Dimensions wrapper 0D
        integer(I4P)                                  :: DataSizeInBytes !< Data size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = 0
        if(allocated(this%value)) DataSizeInBytes = byte_size(this%value(1,1,1,1,1))*size(this%value)
    end function DimensionsWrapper5D_DLCA_DataSizeInBytes


    function DimensionsWrapper5D_DLCA_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_DLCA_t), intent(IN) :: this         !< Dimensions wrapper 5D
        class(*),                          intent(IN) :: Mold         !< Mold for data type comparison
        logical                                  :: isOfDataType      !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (character(len=*))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper5D_DLCA_isOfDataType


    subroutine DimensionsWrapper5D_DLCA_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_DLCA_t),intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A,$)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = DLCA'//&
                        ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                        ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                        ', Value = '
        write(unit=unit,fmt=*,iostat=iostatd,iomsg=iomsgd) this%Value
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper5D_DLCA_Print


    subroutine DimensionsWrapper5D_I1P_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper5D
    !-----------------------------------------------------------------
        type(DimensionsWrapper5D_I1P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper5D_I1P_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set I1P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_I1P_t), intent(INOUT) :: this
        class(*),                         intent(IN)    :: Value(:,:,:,:,:)
        integer                                         :: err
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I1P))
                allocate(this%Value(size(Value,dim=1),  &
                                    size(Value,dim=2),  &
                                    size(Value,dim=3),  &
                                    size(Value,dim=4),  &
                                    size(Value,dim=5)), &
                                    stat=err)
                this%Value = Value
                if(err/=0) &
                    call msg%Error( txt='Setting Value: Allocation error ('//&
                                    str(no_sign=.true.,n=err)//')', &
                                    file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn( txt='Setting value: Expected data type (I1P)', &
                               file=__FILE__, line=__LINE__ )

        end select
    end subroutine


    subroutine DimensionsWrapper5D_I1P_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get I1P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_I1P_t), intent(IN)  :: this
        class(*),                         intent(OUT) :: Value(:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I1P))
                if(all(this%GetShape() == shape(Value))) then
                    Value = this%Value
                else
                    call msg%Warn(txt='Getting value: Expected shape ('//    &
                                  str(no_sign=.true.,n=this%GetShape())//')',&
                                  file=__FILE__, line=__LINE__ )
                endif
            class Default
                call msg%Warn(txt='Getting value: Expected data type (I1P)',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    function DimensionsWrapper5D_I1P_GetShape(this) result(ValueShape) 
    !-----------------------------------------------------------------
    !< Get Wrapper Value Shape
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_I1P_t), intent(IN)  :: this
        integer(I4P), allocatable                     :: ValueShape(:)
    !-----------------------------------------------------------------
        allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper5D_I1P_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic pointer to Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_I1P_t), target, intent(IN) :: this
        class(*), pointer                                    :: Value(:,:,:,:,:)
    !-----------------------------------------------------------------
        Value => this%Value
    end function


    subroutine DimensionsWrapper5D_I1P_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_I1P_t), intent(IN)  :: this
        class(*), allocatable,            intent(OUT) :: Value(:,:,:,:,:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1),  &
                       size(this%Value,dim=2),  &
                       size(this%Value,dim=3),  &
                       size(this%Value,dim=4),  &
                       size(this%Value,dim=5)), &
                       source=this%Value)
    end subroutine



    subroutine DimensionsWrapper5D_I1P_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper5D
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_I1P_t), intent(INOUT) :: this
        integer                                         :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    function DimensionsWrapper5D_I1P_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the size of the data stored in bytes
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_I1P_t), intent(IN) :: this            !< Dimensions wrapper 0D
        integer(I4P)                                 :: DataSizeInBytes !< Data size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = byte_size(this%value(1,1,1,1,1))*size(this%value)
    end function DimensionsWrapper5D_I1P_DataSizeInBytes


    function DimensionsWrapper5D_I1P_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_I1P_t), intent(IN) :: this          !< Dimensions wrapper 5D
        class(*),                         intent(IN) :: Mold          !< Mold for data type comparison
        logical                                      :: isOfDataType  !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (integer(I1P))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper5D_I1P_isOfDataType


    subroutine DimensionsWrapper5D_I1P_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_I1P_t), intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A,$)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = I1P'//&
                        ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                        ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                        ', Value = '
        write(unit=unit,fmt=*,iostat=iostatd,iomsg=iomsgd) str(no_sign=.true., n=this%Value)
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper5D_I1P_Print


    subroutine DimensionsWrapper5D_I2P_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper5D
    !-----------------------------------------------------------------
        type(DimensionsWrapper5D_I2P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper5D_I2P_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set I2P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_I2P_t), intent(INOUT) :: this
        class(*),                         intent(IN)    :: Value(:,:,:,:,:)
        integer                                         :: err
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I2P))
                allocate(this%Value(size(Value,dim=1),  &
                                    size(Value,dim=2),  &
                                    size(Value,dim=3),  &
                                    size(Value,dim=4),  &
                                    size(Value,dim=5)), &
                                    stat=err)
                this%Value = Value
                if(err/=0) &
                    call msg%Error( txt='Setting Value: Allocation error ('//&
                                    str(no_sign=.true.,n=err)//')', &
                                    file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn( txt='Setting value: Expected data type (I2P)', &
                               file=__FILE__, line=__LINE__ )

        end select
    end subroutine


    subroutine DimensionsWrapper5D_I2P_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get I2P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_I2P_t), intent(IN)  :: this
        class(*),                         intent(OUT) :: Value(:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I2P))
                if(all(this%GetShape() == shape(Value))) then
                    Value = this%Value
                else
                    call msg%Warn(txt='Getting value: Expected shape ('//    &
                                  str(no_sign=.true.,n=this%GetShape())//')',&
                                  file=__FILE__, line=__LINE__ )
                endif
            class Default
                call msg%Warn(txt='Getting value: Expected data type (I2P)',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    function DimensionsWrapper5D_I2P_GetShape(this) result(ValueShape) 
    !-----------------------------------------------------------------
    !< Get Wrapper Value Shape
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_I2P_t), intent(IN)  :: this
        integer(I4P), allocatable                     :: ValueShape(:)
    !-----------------------------------------------------------------
        allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper5D_I2P_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic pointer to Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_I2P_t), target, intent(IN) :: this
        class(*), pointer                                    :: Value(:,:,:,:,:)
    !-----------------------------------------------------------------
        Value => this%Value
    end function


    subroutine DimensionsWrapper5D_I2P_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_I2P_t), intent(IN)  :: this
        class(*), allocatable,            intent(OUT) :: Value(:,:,:,:,:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1),  &
                       size(this%Value,dim=2),  &
                       size(this%Value,dim=3),  &
                       size(this%Value,dim=4),  &
                       size(this%Value,dim=5)), &
                       source=this%Value)
    end subroutine


    subroutine DimensionsWrapper5D_I2P_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper5D
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_I2P_t), intent(INOUT) :: this
        integer                                         :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    function DimensionsWrapper5D_I2P_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the size of the data stored in bytes
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_I2P_t), intent(IN) :: this            !< Dimensions wrapper 0D
        integer(I4P)                                 :: DataSizeInBytes !< Data size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = byte_size(this%value(1,1,1,1,1))*size(this%value)
    end function DimensionsWrapper5D_I2P_DataSizeInBytes


    function DimensionsWrapper5D_I2P_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_I2P_t), intent(IN) :: this          !< Dimensions wrapper 5D
        class(*),                         intent(IN) :: Mold          !< Mold for data type comparison
        logical                                      :: isOfDataType  !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (integer(I2P))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper5D_I2P_isOfDataType


    subroutine DimensionsWrapper5D_I2P_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_I2P_t), intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A,$)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = I2P'//&
                        ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                        ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                        ', Value = '
        write(unit=unit,fmt=*,iostat=iostatd,iomsg=iomsgd) str(no_sign=.true., n=this%Value)
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper5D_I2P_Print


    subroutine DimensionsWrapper5D_I4P_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper5D
    !-----------------------------------------------------------------
        type(DimensionsWrapper5D_I4P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper5D_I4P_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set I4P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_I4P_t), intent(INOUT) :: this
        class(*),                         intent(IN)    :: Value(:,:,:,:,:)
        integer                                         :: err
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I4P))
                allocate(this%Value(size(Value,dim=1),  &
                                    size(Value,dim=2),  &
                                    size(Value,dim=3),  &
                                    size(Value,dim=4),  &
                                    size(Value,dim=5)), &
                                    source=Value, stat=err)
                if(err/=0) &
                    call msg%Error( txt='Setting Value: Allocation error ('//&
                                    str(no_sign=.true.,n=err)//')', &
                                    file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn( txt='Setting value: Expected data type (I4P)', &
                               file=__FILE__, line=__LINE__ )

        end select
    end subroutine


    subroutine DimensionsWrapper5D_I4P_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get I4P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_I4P_t), intent(IN)  :: this
        class(*),                         intent(OUT) :: Value(:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I4P))
                if(all(this%GetShape() == shape(Value))) then
                    Value = this%Value
                else
                    call msg%Warn(txt='Getting value: Expected shape ('//    &
                                  str(no_sign=.true.,n=this%GetShape())//')',&
                                  file=__FILE__, line=__LINE__ )
                endif
            class Default
                call msg%Warn(txt='Getting value: Expected data type (I4P)',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    function DimensionsWrapper5D_I4P_GetShape(this) result(ValueShape) 
    !-----------------------------------------------------------------
    !< Get Wrapper Value Shape
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_I4P_t), intent(IN)  :: this
        integer(I4P), allocatable                     :: ValueShape(:)
    !-----------------------------------------------------------------
        allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper5D_I4P_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic pointer to Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_I4P_t), target, intent(IN) :: this
        class(*), pointer                                    :: Value(:,:,:,:,:)
    !-----------------------------------------------------------------
        Value => this%Value
    end function


    subroutine DimensionsWrapper5D_I4P_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_I4P_t), intent(IN)  :: this
        class(*), allocatable,            intent(OUT) :: Value(:,:,:,:,:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1),  &
                       size(this%Value,dim=2),  &
                       size(this%Value,dim=3),  &
                       size(this%Value,dim=4),  &
                       size(this%Value,dim=5)), &
                       source=this%Value)
    end subroutine


    subroutine DimensionsWrapper5D_I4P_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper5D
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_I4P_t), intent(INOUT) :: this
        integer                                         :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    function DimensionsWrapper5D_I4P_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the size of the data stored in bytes
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_I4P_t), intent(IN) :: this            !< Dimensions wrapper 0D
        integer(I4P)                                 :: DataSizeInBytes !< Data size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = byte_size(this%value(1,1,1,1,1))*size(this%value)
    end function DimensionsWrapper5D_I4P_DataSizeInBytes


    function DimensionsWrapper5D_I4P_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_I4P_t), intent(IN) :: this           !< Dimensions wrapper 5D
        class(*),                         intent(IN) :: Mold           !< Mold for data type comparison
        logical                                      :: isOfDataType   !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (integer(I4P))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper5D_I4P_isOfDataType


    subroutine DimensionsWrapper5D_I4P_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_I4P_t), intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A,$)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = I4P'//&
                        ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                        ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                        ', Value = '
        write(unit=unit,fmt=*,iostat=iostatd,iomsg=iomsgd) str(no_sign=.true., n=this%Value)
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper5D_I4P_Print


    subroutine DimensionsWrapper5D_I8P_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper5D
    !-----------------------------------------------------------------
        type(DimensionsWrapper5D_I8P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper5D_I8P_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set I8P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_I8P_t), intent(INOUT) :: this
        class(*),                         intent(IN)    :: Value(:,:,:,:,:)
        integer                                         :: err
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I8P))
                allocate(this%Value(size(Value,dim=1),  &
                                    size(Value,dim=2),  &
                                    size(Value,dim=3),  &
                                    size(Value,dim=4),  &
                                    size(Value,dim=5)), &
                                    stat=err)
                this%Value = Value
                if(err/=0) &
                    call msg%Error( txt='Setting Value: Allocation error ('//&
                                    str(no_sign=.true.,n=err)//')', &
                                    file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn( txt='Setting value: Expected data type (I8P)', &
                               file=__FILE__, line=__LINE__ )

        end select
    end subroutine


    subroutine DimensionsWrapper5D_I8P_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get I8P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_I8P_t), intent(IN)  :: this
        class(*),                         intent(OUT) :: Value(:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I8P))
                if(all(this%GetShape() == shape(Value))) then
                    Value = this%Value
                else
                    call msg%Warn(txt='Getting value: Expected shape ('//    &
                                  str(no_sign=.true.,n=this%GetShape())//')',&
                                  file=__FILE__, line=__LINE__ )
                endif
            class Default
                call msg%Warn(txt='Getting value: Expected data type (I8P)',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    function DimensionsWrapper5D_I8P_GetShape(this) result(ValueShape) 
    !-----------------------------------------------------------------
    !< Get Wrapper Value Shape
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_I8P_t), intent(IN)  :: this
        integer(I4P), allocatable                     :: ValueShape(:)
    !-----------------------------------------------------------------
        allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper5D_I8P_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic pointer to Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_I8P_t), target, intent(IN) :: this
        class(*), pointer                                    :: Value(:,:,:,:,:)
    !-----------------------------------------------------------------
        Value => this%Value
    end function

    subroutine DimensionsWrapper5D_I8P_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_I8P_t), intent(IN)  :: this
        class(*), allocatable,            intent(OUT) :: Value(:,:,:,:,:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1),  &
                       size(this%Value,dim=2),  &
                       size(this%Value,dim=3),  &
                       size(this%Value,dim=4),  &
                       size(this%Value,dim=5)), &
                       source=this%Value)
    end subroutine


    subroutine DimensionsWrapper5D_I8P_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper5D
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_I8P_t), intent(INOUT) :: this
        integer                                         :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    function DimensionsWrapper5D_I8P_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the size of the data stored in bytes
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_I8P_t), intent(IN) :: this            !< Dimensions wrapper 0D
        integer(I4P)                                 :: DataSizeInBytes !< Data size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = byte_size(this%value(1,1,1,1,1))*size(this%value)
    end function DimensionsWrapper5D_I8P_DataSizeInBytes


    function DimensionsWrapper5D_I8P_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_I8P_t), intent(IN) :: this          !< Dimensions wrapper 5D
        class(*),                         intent(IN) :: Mold          !< Mold for data type comparison
        logical                                      :: isOfDataType  !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (integer(I8P))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper5D_I8P_isOfDataType


    subroutine DimensionsWrapper5D_I8P_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_I8P_t), intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A,$)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = I8P'//&
                        ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                        ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                        ', Value = '
        write(unit=unit,fmt=*,iostat=iostatd,iomsg=iomsgd) str(no_sign=.true., n=this%Value)
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper5D_I8P_Print


    subroutine DimensionsWrapper5D_L_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper5D
    !-----------------------------------------------------------------
        type(DimensionsWrapper5D_L_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper5D_L_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set logical Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_L_t), intent(INOUT) :: this
        class(*),                       intent(IN)    :: Value(:,:,:,:,:)
        integer                                       :: err
    !-----------------------------------------------------------------
        select type (Value)
            type is (logical)
                allocate(this%Value(size(Value,dim=1),  &
                                    size(Value,dim=2),  &
                                    size(Value,dim=3),  &
                                    size(Value,dim=4),  &
                                    size(Value,dim=5)), &
                                    stat=err)
                this%Value = Value
                if(err/=0) &
                    call msg%Error( txt='Setting Value: Allocation error ('//&
                                    str(no_sign=.true.,n=err)//')', &
                                    file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn( txt='Setting value: Expected data type (logical)', &
                               file=__FILE__, line=__LINE__ )

        end select
    end subroutine


    subroutine DimensionsWrapper5D_L_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get logical Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_L_t), intent(IN)  :: this
        class(*),                       intent(OUT) :: Value(:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (logical)
                if(all(this%GetShape() == shape(Value))) then
                    Value = this%Value
                else
                    call msg%Warn(txt='Getting value: Expected shape ('//    &
                                  str(no_sign=.true.,n=this%GetShape())//')',&
                                  file=__FILE__, line=__LINE__ )
                endif
            class Default
                call msg%Warn(txt='Getting value: Expected data type (logical)',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    function DimensionsWrapper5D_L_GetShape(this) result(ValueShape) 
    !-----------------------------------------------------------------
    !< Get Wrapper Value Shape
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_L_t), intent(IN)  :: this
        integer(I4P), allocatable                   :: ValueShape(:)
    !-----------------------------------------------------------------
        allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper5D_L_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic pointer to Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_L_t), target, intent(IN) :: this
        class(*), pointer                                  :: Value(:,:,:,:,:)
    !-----------------------------------------------------------------
        Value => this%Value
    end function


    subroutine DimensionsWrapper5D_L_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_L_t), intent(IN)  :: this
        class(*), allocatable,          intent(OUT) :: Value(:,:,:,:,:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1),  &
                       size(this%Value,dim=2),  &
                       size(this%Value,dim=3),  &
                       size(this%Value,dim=4),  &
                       size(this%Value,dim=5)), &
                       source=this%Value)
    end subroutine


    subroutine DimensionsWrapper5D_L_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper5D
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_L_t), intent(INOUT) :: this
        integer                                         :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    function DimensionsWrapper5D_L_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the size of the data stored in bytes
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_L_t), intent(IN) :: this            !< Dimensions wrapper 0D
        integer(I4P)                               :: DataSizeInBytes !< Data size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = byte_size_logical(this%value(1,1,1,1,1))*size(this%value)
    end function DimensionsWrapper5D_L_DataSizeInBytes


    function DimensionsWrapper5D_L_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_L_t), intent(IN) :: this            !< Dimensions wrapper 5D
        class(*),                       intent(IN) :: Mold            !< Mold for data type comparison
        logical                                    :: isOfDataType    !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (logical)
                isOfDataType = .true.
        end select
    end function DimensionsWrapper5D_L_isOfDataType


    subroutine DimensionsWrapper5D_L_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_L_t),   intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A,$)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = L'//&
                        ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                        ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                        ', Value = '
        write(unit=unit,fmt=*,iostat=iostatd,iomsg=iomsgd) str(n=this%Value)
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper5D_L_Print


    subroutine DimensionsWrapper5D_R4P_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper5D
    !-----------------------------------------------------------------
        type(DimensionsWrapper5D_R4P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper5D_R4P_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set R4P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_R4P_t), intent(INOUT) :: this
        class(*),                         intent(IN)    :: Value(:,:,:,:,:)
        integer                                         :: err
    !-----------------------------------------------------------------
        select type (Value)
            type is (real(R4P))
                allocate(this%Value(size(Value,dim=1),  &
                                    size(Value,dim=2),  &
                                    size(Value,dim=3),  &
                                    size(Value,dim=4),  &
                                    size(Value,dim=5)), &
                                    stat=err)
                this%Value = Value
                if(err/=0) &
                    call msg%Error( txt='Setting Value: Allocation error ('//&
                                    str(no_sign=.true.,n=err)//')', &
                                    file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn( txt='Setting value: Expected data type (R4P)', &
                               file=__FILE__, line=__LINE__ )

        end select
    end subroutine


    subroutine DimensionsWrapper5D_R4P_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get R4P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_R4P_t), intent(IN)  :: this
        class(*),                         intent(OUT) :: Value(:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (real(R4P))
                if(all(this%GetShape() == shape(Value))) then
                    Value = this%Value
                else
                    call msg%Warn(txt='Getting value: Expected shape ('//    &
                                  str(no_sign=.true.,n=this%GetShape())//')',&
                                  file=__FILE__, line=__LINE__ )
                endif
            class Default
                call msg%Warn(txt='Getting value: Expected data type (R4P)',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    function DimensionsWrapper5D_R4P_GetShape(this) result(ValueShape) 
    !-----------------------------------------------------------------
    !< Get Wrapper Value Shape
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_R4P_t), intent(IN)  :: this
        integer(I4P), allocatable                     :: ValueShape(:)
    !-----------------------------------------------------------------
        allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper5D_R4P_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic pointer to Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_R4P_t), target, intent(IN) :: this
        class(*), pointer                                    :: Value(:,:,:,:,:)
    !-----------------------------------------------------------------
        Value => this%Value
    end function


    subroutine DimensionsWrapper5D_R4P_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_R4P_t), intent(IN)  :: this
        class(*), allocatable,            intent(OUT) :: Value(:,:,:,:,:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1),  &
                       size(this%Value,dim=2),  &
                       size(this%Value,dim=3),  &
                       size(this%Value,dim=4),  &
                       size(this%Value,dim=5)), &
                       source=this%Value)
    end subroutine


    subroutine DimensionsWrapper5D_R4P_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper5D
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_R4P_t), intent(INOUT) :: this
        integer                                         :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    function DimensionsWrapper5D_R4P_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the size of the data stored in bytes
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_R4P_t), intent(IN) :: this            !< Dimensions wrapper 0D
        integer(I4P)                                 :: DataSizeInBytes !< Data size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = byte_size(this%value(1,1,1,1,1))*size(this%value)
    end function DimensionsWrapper5D_R4P_DataSizeInBytes


    function DimensionsWrapper5D_R4P_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_R4P_t), intent(IN) :: this          !< Dimensions wrapper 5D
        class(*),                         intent(IN) :: Mold          !< Mold for data type comparison
        logical                                      :: isOfDataType  !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (real(R4P))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper5D_R4P_isOfDataType


    subroutine DimensionsWrapper5D_R4P_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_R4P_t), intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A,$)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = R4P'//&
                        ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                        ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                        ', Value = '
        write(unit=unit,fmt=*,iostat=iostatd,iomsg=iomsgd) str(no_sign=.true., n=this%Value)
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper5D_R4P_Print


    subroutine DimensionsWrapper5D_R8P_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper5D
    !-----------------------------------------------------------------
        type(DimensionsWrapper5D_R8P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper5D_R8P_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set R8P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_R8P_t), intent(INOUT) :: this
        class(*),                         intent(IN)    :: Value(:,:,:,:,:)
        integer                                         :: err
    !-----------------------------------------------------------------
        select type (Value)
            type is (real(R8P))
                allocate(this%Value(size(Value,dim=1),  &
                                    size(Value,dim=2),  &
                                    size(Value,dim=3),  &
                                    size(Value,dim=4),  &
                                    size(Value,dim=5)), &
                                    stat=err)
                this%Value = Value
                if(err/=0) &
                    call msg%Error( txt='Setting Value: Allocation error ('//&
                                    str(no_sign=.true.,n=err)//')', &
                                    file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn( txt='Setting value: Expected data type (R8P)', &
                               file=__FILE__, line=__LINE__ )

        end select
    end subroutine


    subroutine DimensionsWrapper5D_R8P_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get R8P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_R8P_t), intent(IN)  :: this
        class(*),                         intent(OUT) :: Value(:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (real(R8P))
                if(all(this%GetShape() == shape(Value))) then
                    Value = this%Value
                else
                    call msg%Warn(txt='Getting value: Expected shape ('//    &
                                  str(no_sign=.true.,n=this%GetShape())//')',&
                                  file=__FILE__, line=__LINE__ )
                endif
            class Default
                call msg%Warn(txt='Getting value: Expected data type (R8P)',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    function DimensionsWrapper5D_R8P_GetShape(this) result(ValueShape) 
    !-----------------------------------------------------------------
    !< Get Wrapper Value Shape
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_R8P_t), intent(IN)  :: this
        integer(I4P), allocatable                     :: ValueShape(:)
    !-----------------------------------------------------------------
        allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper5D_R8P_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic pointer to Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_R8P_t), target, intent(IN) :: this
        class(*), pointer                                    :: Value(:,:,:,:,:)
    !-----------------------------------------------------------------
        Value => this%Value
    end function


    subroutine DimensionsWrapper5D_R8P_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_R8P_t), intent(IN)  :: this
        class(*), allocatable,            intent(OUT) :: Value(:,:,:,:,:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1),  &
                       size(this%Value,dim=2),  &
                       size(this%Value,dim=3),  &
                       size(this%Value,dim=4),  &
                       size(this%Value,dim=5)), &
                       source=this%Value)
    end subroutine


    subroutine DimensionsWrapper5D_R8P_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper5D
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_R8P_t), intent(INOUT) :: this
        integer                                         :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    function DimensionsWrapper5D_r8P_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the size of the data stored in bytes
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_R8P_t), intent(IN) :: this            !< Dimensions wrapper 0D
        integer(I4P)                                 :: DataSizeInBytes !< Data size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = byte_size(this%value(1,1,1,1,1))*size(this%value)
    end function DimensionsWrapper5D_R8P_DataSizeInBytes


    function DimensionsWrapper5D_R8P_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_R8P_t), intent(IN) :: this          !< Dimensions wrapper 5D
        class(*),                         intent(IN) :: Mold          !< Mold for data type comparison
        logical                                      :: isOfDataType  !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (real(R8P))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper5D_R8P_isOfDataType


    subroutine DimensionsWrapper5D_R8P_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper5D_R8P_t), intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A,$)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = R8P'//&
                        ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                        ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                        ', Value = '
        write(unit=unit,fmt=*,iostat=iostatd,iomsg=iomsgd) str(no_sign=.true., n=this%Value)
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper5D_R8P_Print


    subroutine DimensionsWrapper6D_DLCA_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper6D
    !-----------------------------------------------------------------
        type(DimensionsWrapper6D_DLCA_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper6D_DLCA_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set DLCA Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_DLCA_t), intent(INOUT) :: this
        class(*),                          intent(IN)    :: Value(:,:,:,:,:,:)
        integer                                          :: err
    !-----------------------------------------------------------------
#ifdef __GFORTRAN__ 
        call msg%Warn(txt='Setting value: Array of deferred length allocatable arrays not supported in Gfortran)',&
                      file=__FILE__, line=__LINE__ )
#else   
        select type (Value)
            type is (character(len=*))
                allocate(character(len=len(Value))::               &
                                    this%Value(size(Value,dim=1),  &
                                    size(Value,dim=2),             &
                                    size(Value,dim=3),             &
                                    size(Value,dim=4),             &
                                    size(Value,dim=5),             &
                                    size(Value,dim=6)),            &
                                    stat=err)
                this%Value = Value
                if(err/=0) &
                    call msg%Error( txt='Setting Value: Allocation error ('//&
                                    str(no_sign=.true.,n=err)//')', &
                                    file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn( txt='Setting value: Expected data type (character(*))', &
                               file=__FILE__, line=__LINE__ )

        end select
#endif
    end subroutine


    subroutine DimensionsWrapper6D_DLCA_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get deferred length character array Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_DLCA_t), intent(IN)  :: this
        class(*),                          intent(OUT) :: Value(:,:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (character(len=*))
                if(all(this%GetShape() == shape(Value))) then
                    Value = this%Value
                else
                    call msg%Warn(txt='Getting value: Expected shape ('//    &
                                  str(no_sign=.true.,n=this%GetShape())//')',&
                                  file=__FILE__, line=__LINE__ )
                endif
            class Default
                call msg%Warn(txt='Getting value: Expected data type (character(*))',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    function DimensionsWrapper6D_DLCA_GetShape(this) result(ValueShape) 
    !-----------------------------------------------------------------
    !< Get Wrapper Value Shape
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_DLCA_t), intent(IN)  :: this
        integer(I4P), allocatable                      :: ValueShape(:)
    !-----------------------------------------------------------------
        allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper6D_DLCA_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic pointer to Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_DLCA_t), target, intent(IN)  :: this
        class(*), pointer                                      :: Value(:,:,:,:,:,:)
    !-----------------------------------------------------------------
        Value => this%Value
    end function


    subroutine DimensionsWrapper6D_DLCA_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_DLCA_t), intent(IN)  :: this
        class(*), allocatable,             intent(OUT) :: Value(:,:,:,:,:,:)
    !-----------------------------------------------------------------
!        allocate(Value(size(this%Value,dim=1),  &
!                       size(this%Value,dim=2),  &
!                       size(this%Value,dim=3),  &
!                       size(this%Value,dim=4),  &
!                       size(this%Value,dim=5),  &
!                       size(this%Value,dim=6)), &
!                       source=this%Value)
    end subroutine


    subroutine DimensionsWrapper6D_DLCA_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper6D
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_DLCA_t), intent(INOUT) :: this
        integer                                         :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    function DimensionsWrapper6D_DLCA_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the size of the data stored in bytes
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_DLCA_t), intent(IN) :: this            !< Dimensions wrapper 0D
        integer(I4P)                                 :: DataSizeInBytes !< Data size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = 0
        if(allocated(this%value)) DataSizeInBytes = byte_size(this%value(1,1,1,1,1,1))*size(this%value)
    end function DimensionsWrapper6D_DLCA_DataSizeInBytes


    function DimensionsWrapper6D_DLCA_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_DLCA_t), intent(IN) :: this         !< Dimensions wrapper 6D
        class(*),                          intent(IN) :: Mold         !< Mold for data type comparison
        logical                                  :: isOfDataType      !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (character(len=*))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper6D_DLCA_isOfDataType


    subroutine DimensionsWrapper6D_DLCA_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_DLCA_t),intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A,$)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = DLCA'//&
                        ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                        ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                        ', Value = '
        write(unit=unit,fmt=*,iostat=iostatd,iomsg=iomsgd) this%Value
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper6D_DLCA_Print


    subroutine DimensionsWrapper6D_I1P_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper6D
    !-----------------------------------------------------------------
        type(DimensionsWrapper6D_I1P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper6D_I1P_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set I1P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_I1P_t), intent(INOUT) :: this
        class(*),                         intent(IN)    :: Value(:,:,:,:,:,:)
        integer                                         :: err
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I1P))
                allocate(this%Value(size(Value,dim=1),  &
                                    size(Value,dim=2),  &
                                    size(Value,dim=3),  &
                                    size(Value,dim=4),  &
                                    size(Value,dim=5),  &
                                    size(Value,dim=6)), &
                                    stat=err)
                this%Value = Value
                if(err/=0) &
                    call msg%Error( txt='Setting Value: Allocation error ('//&
                                    str(no_sign=.true.,n=err)//')', &
                                    file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn( txt='Setting value: Expected data type (I1P)', &
                               file=__FILE__, line=__LINE__ )

        end select
    end subroutine


    subroutine DimensionsWrapper6D_I1P_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get I1P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_I1P_t), intent(IN)  :: this
        class(*),                         intent(OUT) :: Value(:,:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I1P))
                if(all(this%GetShape() == shape(Value))) then
                    Value = this%Value
                else
                    call msg%Warn(txt='Getting value: Expected shape ('//    &
                                  str(no_sign=.true.,n=this%GetShape())//')',&
                                  file=__FILE__, line=__LINE__ )
                endif
            class Default
                call msg%Warn(txt='Getting value: Expected data type (I1P)',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    function DimensionsWrapper6D_I1P_GetShape(this) result(ValueShape) 
    !-----------------------------------------------------------------
    !< Get Wrapper Value Shape
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_I1P_t), intent(IN)  :: this
        integer(I4P), allocatable                     :: ValueShape(:)
    !-----------------------------------------------------------------
        allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper6D_I1P_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic pointer to Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_I1P_t), target, intent(IN)  :: this
        class(*), pointer                                     :: Value(:,:,:,:,:,:)
    !-----------------------------------------------------------------
        Value => this%Value
    end function


    subroutine DimensionsWrapper6D_I1P_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_I1P_t), intent(IN)  :: this
        class(*), allocatable,            intent(OUT) :: Value(:,:,:,:,:,:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1),  &
                       size(this%Value,dim=2),  &
                       size(this%Value,dim=3),  &
                       size(this%Value,dim=4),  &
                       size(this%Value,dim=5),  &
                       size(this%Value,dim=6)), &
                       source=this%Value)
    end subroutine


    subroutine DimensionsWrapper6D_I1P_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper6D
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_I1P_t), intent(INOUT) :: this
        integer                                         :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    function DimensionsWrapper6D_I1P_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the size of the data stored in bytes
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_I1P_t), intent(IN) :: this            !< Dimensions wrapper 0D
        integer(I4P)                                 :: DataSizeInBytes !< Data size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = byte_size(this%value(1,1,1,1,1,1))*size(this%value)
    end function DimensionsWrapper6D_I1P_DataSizeInBytes


    function DimensionsWrapper6D_I1P_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_I1P_t), intent(IN) :: this          !< Dimensions wrapper 6D
        class(*),                         intent(IN) :: Mold          !< Mold for data type comparison
        logical                                      :: isOfDataType  !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (integer(I1P))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper6D_I1P_isOfDataType


    subroutine DimensionsWrapper6D_I1P_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_I1P_t), intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A,$)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = I1P'//&
                        ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                        ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                        ', Value = '
        write(unit=unit,fmt=*,iostat=iostatd,iomsg=iomsgd) str(no_sign=.true., n=this%Value)
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper6D_I1P_Print


    subroutine DimensionsWrapper6D_I2P_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper6D
    !-----------------------------------------------------------------
        type(DimensionsWrapper6D_I2P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper6D_I2P_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set I2P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_I2P_t), intent(INOUT) :: this
        class(*),                         intent(IN)    :: Value(:,:,:,:,:,:)
        integer                                         :: err
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I2P))
                allocate(this%Value(size(Value,dim=1),  &
                                    size(Value,dim=2),  &
                                    size(Value,dim=3),  &
                                    size(Value,dim=4),  &
                                    size(Value,dim=5),  &
                                    size(Value,dim=6)), &
                                    stat=err)
                this%Value = Value
                if(err/=0) &
                    call msg%Error( txt='Setting Value: Allocation error ('//&
                                    str(no_sign=.true.,n=err)//')', &
                                    file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn( txt='Setting value: Expected data type (I2P)', &
                               file=__FILE__, line=__LINE__ )

        end select
    end subroutine


    subroutine DimensionsWrapper6D_I2P_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get I2P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_I2P_t), intent(IN)  :: this
        class(*),                         intent(OUT) :: Value(:,:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I2P))
                if(all(this%GetShape() == shape(Value))) then
                    Value = this%Value
                else
                    call msg%Warn(txt='Getting value: Expected shape ('//    &
                                  str(no_sign=.true.,n=this%GetShape())//')',&
                                  file=__FILE__, line=__LINE__ )
                endif
            class Default
                call msg%Warn(txt='Getting value: Expected data type (I2P)',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    function DimensionsWrapper6D_I2P_GetShape(this) result(ValueShape) 
    !-----------------------------------------------------------------
    !< Get Wrapper Value Shape
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_I2P_t), intent(IN)  :: this
        integer(I4P), allocatable                     :: ValueShape(:)
    !-----------------------------------------------------------------
        allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper6D_I2P_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic pointer to Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_I2P_t), target, intent(IN)  :: this
        class(*), pointer                                     :: Value(:,:,:,:,:,:)
    !-----------------------------------------------------------------
        Value => this%Value
    end function


    subroutine DimensionsWrapper6D_I2P_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_I2P_t), intent(IN)  :: this
        class(*), allocatable,            intent(OUT) :: Value(:,:,:,:,:,:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1),  &
                       size(this%Value,dim=2),  &
                       size(this%Value,dim=3),  &
                       size(this%Value,dim=4),  &
                       size(this%Value,dim=5),  &
                       size(this%Value,dim=6)), &
                       source=this%Value)
    end subroutine


    subroutine DimensionsWrapper6D_I2P_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper6D
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_I2P_t), intent(INOUT) :: this
        integer                                         :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    function DimensionsWrapper6D_I2P_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the size of the data stored in bytes
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_I2P_t), intent(IN) :: this            !< Dimensions wrapper 0D
        integer(I4P)                                 :: DataSizeInBytes !< Data size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = byte_size(this%value(1,1,1,1,1,1))*size(this%value)
    end function DimensionsWrapper6D_I2P_DataSizeInBytes


    function DimensionsWrapper6D_I2P_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_I2P_t), intent(IN) :: this          !< Dimensions wrapper 6D
        class(*),                         intent(IN) :: Mold          !< Mold for data type comparison
        logical                                      :: isOfDataType  !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (integer(I2P))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper6D_I2P_isOfDataType


    subroutine DimensionsWrapper6D_I2P_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_I2P_t), intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A,$)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = I2P'//&
                        ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                        ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                        ', Value = '
        write(unit=unit,fmt=*,iostat=iostatd,iomsg=iomsgd) str(no_sign=.true., n=this%Value)
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper6D_I2P_Print


    subroutine DimensionsWrapper6D_I4P_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper6D
    !-----------------------------------------------------------------
        type(DimensionsWrapper6D_I4P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper6D_I4P_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set I4P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_I4P_t), intent(INOUT) :: this
        class(*),                         intent(IN)    :: Value(:,:,:,:,:,:)
        integer                                         :: err
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I4P))
                allocate(this%Value(size(Value,dim=1),  &
                                    size(Value,dim=2),  &
                                    size(Value,dim=3),  &
                                    size(Value,dim=4),  &
                                    size(Value,dim=5),  &
                                    size(Value,dim=6)), &
                                    stat=err)
                this%Value = Value
                if(err/=0) &
                    call msg%Error( txt='Setting Value: Allocation error ('//&
                                    str(no_sign=.true.,n=err)//')', &
                                    file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn( txt='Setting value: Expected data type (I4P)', &
                               file=__FILE__, line=__LINE__ )

        end select
    end subroutine


    subroutine DimensionsWrapper6D_I4P_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get I4P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_I4P_t), intent(IN)  :: this
        class(*),                         intent(OUT) :: Value(:,:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I4P))
                if(all(this%GetShape() == shape(Value))) then
                    Value = this%Value
                else
                    call msg%Warn(txt='Getting value: Expected shape ('//    &
                                  str(no_sign=.true.,n=this%GetShape())//')',&
                                  file=__FILE__, line=__LINE__ )
                endif
            class Default
                call msg%Warn(txt='Getting value: Expected data type (I4P)',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    function DimensionsWrapper6D_I4P_GetShape(this) result(ValueShape) 
    !-----------------------------------------------------------------
    !< Get Wrapper Value Shape
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_I4P_t), intent(IN)  :: this
        integer(I4P), allocatable                     :: ValueShape(:)
    !-----------------------------------------------------------------
        allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper6D_I4P_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic pointer to Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_I4P_t), target, intent(IN)  :: this
        class(*), pointer                                     :: Value(:,:,:,:,:,:)
    !-----------------------------------------------------------------
        Value => this%Value
    end function


    subroutine DimensionsWrapper6D_I4P_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_I4P_t), intent(IN)  :: this
        class(*), allocatable,            intent(OUT) :: Value(:,:,:,:,:,:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1),  &
                       size(this%Value,dim=2),  &
                       size(this%Value,dim=3),  &
                       size(this%Value,dim=4),  &
                       size(this%Value,dim=5),  &
                       size(this%Value,dim=6)), &
                       source=this%Value)
    end subroutine


    subroutine DimensionsWrapper6D_I4P_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper6D
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_I4P_t), intent(INOUT) :: this
        integer                                         :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    function DimensionsWrapper6D_I4P_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the size of the data stored in bytes
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_I4P_t), intent(IN) :: this            !< Dimensions wrapper 0D
        integer(I4P)                                 :: DataSizeInBytes !< Data size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = byte_size(this%value(1,1,1,1,1,1))*size(this%value)
    end function DimensionsWrapper6D_I4P_DataSizeInBytes


    function DimensionsWrapper6D_I4P_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_I4P_t), intent(IN) :: this           !< Dimensions wrapper 6D
        class(*),                         intent(IN) :: Mold           !< Mold for data type comparison
        logical                                      :: isOfDataType   !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (integer(I4P))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper6D_I4P_isOfDataType


    subroutine DimensionsWrapper6D_I4P_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_I4P_t), intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A,$)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = I4P'//&
                        ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                        ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                        ', Value = '
        write(unit=unit,fmt=*,iostat=iostatd,iomsg=iomsgd) str(no_sign=.true., n=this%Value)
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper6D_I4P_Print


    subroutine DimensionsWrapper6D_I8P_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper6D
    !-----------------------------------------------------------------
        type(DimensionsWrapper6D_I8P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper6D_I8P_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set I8P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_I8P_t), intent(INOUT) :: this
        class(*),                         intent(IN)    :: Value(:,:,:,:,:,:)
        integer                                         :: err
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I8P))
                allocate(this%Value(size(Value,dim=1),  &
                                    size(Value,dim=2),  &
                                    size(Value,dim=3),  &
                                    size(Value,dim=4),  &
                                    size(Value,dim=5),  &
                                    size(Value,dim=6)), &
                                    stat=err)
                this%Value = Value
                if(err/=0) &
                    call msg%Error( txt='Setting Value: Allocation error ('//&
                                    str(no_sign=.true.,n=err)//')', &
                                    file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn( txt='Setting value: Expected data type (I8P)', &
                               file=__FILE__, line=__LINE__ )

        end select
    end subroutine


    subroutine DimensionsWrapper6D_I8P_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get I8P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_I8P_t), intent(IN)  :: this
        class(*),                         intent(OUT) :: Value(:,:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I8P))
                if(all(this%GetShape() == shape(Value))) then
                    Value = this%Value
                else
                    call msg%Warn(txt='Getting value: Expected shape ('//    &
                                  str(no_sign=.true.,n=this%GetShape())//')',&
                                  file=__FILE__, line=__LINE__ )
                endif
            class Default
                call msg%Warn(txt='Getting value: Expected data type (I8P)',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    function DimensionsWrapper6D_I8P_GetShape(this) result(ValueShape) 
    !-----------------------------------------------------------------
    !< Get Wrapper Value Shape
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_I8P_t), intent(IN)  :: this
        integer(I4P), allocatable                     :: ValueShape(:)
    !-----------------------------------------------------------------
        allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper6D_I8P_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic pointer to Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_I8P_t), target, intent(IN)  :: this
        class(*), pointer                                     :: Value(:,:,:,:,:,:)
    !-----------------------------------------------------------------
        Value => this%Value
    end function


    subroutine DimensionsWrapper6D_I8P_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_I8P_t), intent(IN)  :: this
        class(*), allocatable,            intent(OUT) :: Value(:,:,:,:,:,:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1),  &
                       size(this%Value,dim=2),  &
                       size(this%Value,dim=3),  &
                       size(this%Value,dim=4),  &
                       size(this%Value,dim=5),  &
                       size(this%Value,dim=6)), &
                       source=this%Value)
    end subroutine


    subroutine DimensionsWrapper6D_I8P_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper6D
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_I8P_t), intent(INOUT) :: this
        integer                                         :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    function DimensionsWrapper6D_I8P_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the size of the data stored in bytes
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_I8P_t), intent(IN) :: this            !< Dimensions wrapper 0D
        integer(I4P)                                 :: DataSizeInBytes !< Data size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = byte_size(this%value(1,1,1,1,1,1))*size(this%value)
    end function DimensionsWrapper6D_I8P_DataSizeInBytes


    function DimensionsWrapper6D_I8P_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_I8P_t), intent(IN) :: this          !< Dimensions wrapper 6D
        class(*),                         intent(IN) :: Mold          !< Mold for data type comparison
        logical                                      :: isOfDataType  !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (integer(I8P))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper6D_I8P_isOfDataType


    subroutine DimensionsWrapper6D_I8P_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_I8P_t), intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A,$)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = I8P'//&
                        ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                        ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                        ', Value = '
        write(unit=unit,fmt=*,iostat=iostatd,iomsg=iomsgd) str(no_sign=.true., n=this%Value)
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper6D_I8P_Print


    subroutine DimensionsWrapper6D_L_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper6D
    !-----------------------------------------------------------------
        type(DimensionsWrapper6D_L_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper6D_L_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set logical Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_L_t), intent(INOUT) :: this
        class(*),                       intent(IN)    :: Value(:,:,:,:,:,:)
        integer                                       :: err
    !-----------------------------------------------------------------
        select type (Value)
            type is (logical)
                allocate(this%Value(size(Value,dim=1),  &
                                    size(Value,dim=2),  &
                                    size(Value,dim=3),  &
                                    size(Value,dim=4),  &
                                    size(Value,dim=5),  &
                                    size(Value,dim=6)), &
                                    stat=err)
                this%Value = Value
                if(err/=0) &
                    call msg%Error( txt='Setting Value: Allocation error ('//&
                                    str(no_sign=.true.,n=err)//')', &
                                    file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn( txt='Setting value: Expected data type (logical)', &
                               file=__FILE__, line=__LINE__ )

        end select
    end subroutine


    subroutine DimensionsWrapper6D_L_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get logical Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_L_t), intent(IN)  :: this
        class(*),                       intent(OUT) :: Value(:,:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (logical)
                if(all(this%GetShape() == shape(Value))) then
                    Value = this%Value
                else
                    call msg%Warn(txt='Getting value: Expected shape ('//    &
                                  str(no_sign=.true.,n=this%GetShape())//')',&
                                  file=__FILE__, line=__LINE__ )
                endif
            class Default
                call msg%Warn(txt='Getting value: Expected data type (logical)',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    function DimensionsWrapper6D_L_GetShape(this) result(ValueShape) 
    !-----------------------------------------------------------------
    !< Get Wrapper Value Shape
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_L_t), intent(IN)  :: this
        integer(I4P), allocatable                   :: ValueShape(:)
    !-----------------------------------------------------------------
        allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper6D_L_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic pointer to Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_L_t), target, intent(IN)  :: this
        class(*), pointer                                   :: Value(:,:,:,:,:,:)
    !-----------------------------------------------------------------
        Value => this%Value
    end function


    subroutine DimensionsWrapper6D_L_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_L_t), intent(IN)  :: this
        class(*), allocatable,          intent(OUT) :: Value(:,:,:,:,:,:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1),  &
                       size(this%Value,dim=2),  &
                       size(this%Value,dim=3),  &
                       size(this%Value,dim=4),  &
                       size(this%Value,dim=5),  &
                       size(this%Value,dim=6)), &
                       source=this%Value)
    end subroutine


    subroutine DimensionsWrapper6D_L_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper6D
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_L_t), intent(INOUT) :: this
        integer                                         :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    function DimensionsWrapper6D_L_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the size of the data stored in bytes
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_L_t), intent(IN) :: this            !< Dimensions wrapper 0D
        integer(I4P)                               :: DataSizeInBytes !< Data size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = byte_size_logical(this%value(1,1,1,1,1,1))*size(this%value)
    end function DimensionsWrapper6D_L_DataSizeInBytes


    function DimensionsWrapper6D_L_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_L_t), intent(IN) :: this            !< Dimensions wrapper 6D
        class(*),                       intent(IN) :: Mold            !< Mold for data type comparison
        logical                                    :: isOfDataType    !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (logical)
                isOfDataType = .true.
        end select
    end function DimensionsWrapper6D_L_isOfDataType


    subroutine DimensionsWrapper6D_L_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_L_t),   intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A,$)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = L'//&
                        ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                        ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                        ', Value = '
        write(unit=unit,fmt=*,iostat=iostatd,iomsg=iomsgd) str(n=this%Value)
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper6D_L_Print


    subroutine DimensionsWrapper6D_R4P_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper6D
    !-----------------------------------------------------------------
        type(DimensionsWrapper6D_R4P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper6D_R4P_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set R4P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_R4P_t), intent(INOUT) :: this
        class(*),                         intent(IN)    :: Value(:,:,:,:,:,:)
        integer                                         :: err
    !-----------------------------------------------------------------
        select type (Value)
            type is (real(R4P))
                allocate(this%Value(size(Value,dim=1),  &
                                    size(Value,dim=2),  &
                                    size(Value,dim=3),  &
                                    size(Value,dim=4),  &
                                    size(Value,dim=5),  &
                                    size(Value,dim=6)), &
                                    stat=err)
                this%Value = Value
                if(err/=0) &
                    call msg%Error( txt='Setting Value: Allocation error ('//&
                                    str(no_sign=.true.,n=err)//')', &
                                    file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn( txt='Setting value: Expected data type (R4P)', &
                               file=__FILE__, line=__LINE__ )

        end select
    end subroutine


    subroutine DimensionsWrapper6D_R4P_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get R4P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_R4P_t), intent(IN)  :: this
        class(*),                         intent(OUT) :: Value(:,:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (real(R4P))
                if(all(this%GetShape() == shape(Value))) then
                    Value = this%Value
                else
                    call msg%Warn(txt='Getting value: Expected shape ('//    &
                                  str(no_sign=.true.,n=this%GetShape())//')',&
                                  file=__FILE__, line=__LINE__ )
                endif
            class Default
                call msg%Warn(txt='Getting value: Expected data type (R4P)',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    function DimensionsWrapper6D_R4P_GetShape(this) result(ValueShape) 
    !-----------------------------------------------------------------
    !< Get Wrapper Value Shape
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_R4P_t), intent(IN)  :: this
        integer(I4P), allocatable                     :: ValueShape(:)
    !-----------------------------------------------------------------
        allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper6D_R4P_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic pointer to Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_R4P_t), target, intent(IN)  :: this
        class(*), pointer                                     :: Value(:,:,:,:,:,:)
    !-----------------------------------------------------------------
        Value => this%Value
    end function


    subroutine DimensionsWrapper6D_R4P_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_R4P_t), intent(IN)  :: this
        class(*), allocatable,            intent(OUT) :: Value(:,:,:,:,:,:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1),  &
                       size(this%Value,dim=2),  &
                       size(this%Value,dim=3),  &
                       size(this%Value,dim=4),  &
                       size(this%Value,dim=5),  &
                       size(this%Value,dim=6)), &
                       source=this%Value)
    end subroutine


    subroutine DimensionsWrapper6D_R4P_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper6D
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_R4P_t), intent(INOUT) :: this
        integer                                         :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    function DimensionsWrapper6D_R4P_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the size of the data stored in bytes
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_R4P_t), intent(IN) :: this            !< Dimensions wrapper 0D
        integer(I4P)                                 :: DataSizeInBytes !< Data size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = byte_size(this%value(1,1,1,1,1,1))*size(this%value)
    end function DimensionsWrapper6D_R4P_DataSizeInBytes


    function DimensionsWrapper6D_R4P_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_R4P_t), intent(IN) :: this          !< Dimensions wrapper 6D
        class(*),                         intent(IN) :: Mold          !< Mold for data type comparison
        logical                                      :: isOfDataType  !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (real(R4P))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper6D_R4P_isOfDataType


    subroutine DimensionsWrapper6D_R4P_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_R4P_t), intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A,$)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = R4P'//&
                        ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                        ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                        ', Value = '
        write(unit=unit,fmt=*,iostat=iostatd,iomsg=iomsgd) str(no_sign=.true., n=this%Value)
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper6D_R4P_Print


    subroutine DimensionsWrapper6D_R8P_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper6D
    !-----------------------------------------------------------------
        type(DimensionsWrapper6D_R8P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper6D_R8P_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set R8P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_R8P_t), intent(INOUT) :: this
        class(*),                         intent(IN)    :: Value(:,:,:,:,:,:)
        integer                                         :: err
    !-----------------------------------------------------------------
        select type (Value)
            type is (real(R8P))
                allocate(this%Value(size(Value,dim=1),  &
                                    size(Value,dim=2),  &
                                    size(Value,dim=3),  &
                                    size(Value,dim=4),  &
                                    size(Value,dim=5),  &
                                    size(Value,dim=6)), &
                                    stat=err)
                this%Value = Value
                if(err/=0) &
                    call msg%Error( txt='Setting Value: Allocation error ('//&
                                    str(no_sign=.true.,n=err)//')', &
                                    file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn( txt='Setting value: Expected data type (R8P)', &
                               file=__FILE__, line=__LINE__ )

        end select
    end subroutine


    subroutine DimensionsWrapper6D_R8P_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get R8P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_R8P_t), intent(IN)  :: this
        class(*),                         intent(OUT) :: Value(:,:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (real(R8P))
                if(all(this%GetShape() == shape(Value))) then
                    Value = this%Value
                else
                    call msg%Warn(txt='Getting value: Expected shape ('//    &
                                  str(no_sign=.true.,n=this%GetShape())//')',&
                                  file=__FILE__, line=__LINE__ )
                endif
            class Default
                call msg%Warn(txt='Getting value: Expected data type (R8P)',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    function DimensionsWrapper6D_R8P_GetShape(this) result(ValueShape) 
    !-----------------------------------------------------------------
    !< Get Wrapper Value Shape
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_R8P_t), intent(IN)  :: this
        integer(I4P), allocatable                     :: ValueShape(:)
    !-----------------------------------------------------------------
        allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper6D_R8P_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic pointer to Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_R8P_t), target, intent(IN)  :: this
        class(*), pointer                                     :: Value(:,:,:,:,:,:)
    !-----------------------------------------------------------------
        Value => this%Value
    end function


    subroutine DimensionsWrapper6D_R8P_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_R8P_t), intent(IN)  :: this
        class(*), allocatable,            intent(OUT) :: Value(:,:,:,:,:,:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1),  &
                       size(this%Value,dim=2),  &
                       size(this%Value,dim=3),  &
                       size(this%Value,dim=4),  &
                       size(this%Value,dim=5),  &
                       size(this%Value,dim=6)), &
                       source=this%Value)
    end subroutine


    subroutine DimensionsWrapper6D_R8P_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper6D
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_R8P_t), intent(INOUT) :: this
        integer                                         :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    function DimensionsWrapper6D_R8P_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the size of the data stored in bytes
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_R8P_t), intent(IN) :: this            !< Dimensions wrapper 0D
        integer(I4P)                                 :: DataSizeInBytes !< Data size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = byte_size(this%value(1,1,1,1,1,1))*size(this%value)
    end function DimensionsWrapper6D_R8P_DataSizeInBytes


    function DimensionsWrapper6D_R8P_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_R8P_t), intent(IN) :: this          !< Dimensions wrapper 6D
        class(*),                         intent(IN) :: Mold          !< Mold for data type comparison
        logical                                      :: isOfDataType  !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (real(R8P))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper6D_R8P_isOfDataType


    subroutine DimensionsWrapper6D_R8P_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper6D_R8P_t), intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A,$)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = R8P'//&
                        ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                        ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                        ', Value = '
        write(unit=unit,fmt=*,iostat=iostatd,iomsg=iomsgd) str(no_sign=.true., n=this%Value)
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper6D_R8P_Print


    subroutine DimensionsWrapper7D_DLCA_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper7D
    !-----------------------------------------------------------------
        type(DimensionsWrapper7D_DLCA_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper7D_DLCA_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set DLCA Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_DLCA_t), intent(INOUT) :: this
        class(*),                         intent(IN)    :: Value(:,:,:,:,:,:,:)
        integer                                         :: err
    !-----------------------------------------------------------------
#ifdef __GFORTRAN__ 
        call msg%Warn(txt='Setting value: Array of deferred length allocatable arrays not supported in Gfortran)',&
                      file=__FILE__, line=__LINE__ )
#else   
        select type (Value)
            type is (character(len=*))
                allocate(character(len=len(Value))::               &
                                    this%Value(size(Value,dim=1),  &
                                    size(Value,dim=2),             &
                                    size(Value,dim=3),             &
                                    size(Value,dim=4),             &
                                    size(Value,dim=5),             &
                                    size(Value,dim=6),             &
                                    size(Value,dim=7)),            &
                                    stat=err)
                this%Value = Value
                if(err/=0) &
                    call msg%Error( txt='Setting Value: Allocation error ('//&
                                    str(no_sign=.true.,n=err)//')', &
                                    file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn( txt='Setting value: Expected data type (character(*))', &
                               file=__FILE__, line=__LINE__ )
        end select
#endif
    end subroutine


    subroutine DimensionsWrapper7D_DLCA_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get deferred length character array Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_DLCA_t), intent(IN)  :: this
        class(*),                          intent(OUT) :: Value(:,:,:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (character(len=*))
                if(all(this%GetShape() == shape(Value))) then
                    Value = this%Value
                else
                    call msg%Warn(txt='Getting value: Expected shape ('//    &
                                  str(no_sign=.true.,n=this%GetShape())//')',&
                                  file=__FILE__, line=__LINE__ )
                endif
            class Default
                call msg%Warn(txt='Getting value: Expected data type (character(*))',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    function DimensionsWrapper7D_DLCA_GetShape(this) result(ValueShape) 
    !-----------------------------------------------------------------
    !< Get Wrapper Value Shape
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_DLCA_t), intent(IN)  :: this
        integer(I4P), allocatable                      :: ValueShape(:)
    !-----------------------------------------------------------------
        allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper7D_DLCA_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic pointer to Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_DLCA_t), target, intent(IN)  :: this
        class(*), pointer                                      :: Value(:,:,:,:,:,:,:)
    !-----------------------------------------------------------------
        Value => this%Value
    end function


    subroutine DimensionsWrapper7D_DLCA_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_DLCA_t), intent(IN)  :: this
        class(*), allocatable,             intent(OUT) :: Value(:,:,:,:,:,:,:)
    !-----------------------------------------------------------------
!        allocate(Value(size(this%Value,dim=1),  &
!                       size(this%Value,dim=2),  &
!                       size(this%Value,dim=3),  &
!                       size(this%Value,dim=4),  &
!                       size(this%Value,dim=5),  &
!                       size(this%Value,dim=6),  &
!                       size(this%Value,dim=7)), &
!                       source=this%Value)
    end subroutine


    subroutine DimensionsWrapper7D_DLCA_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper7D
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_DLCA_t), intent(INOUT) :: this
        integer                                         :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    function DimensionsWrapper7D_DLCA_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the size of the data stored in bytes
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_DLCA_t), intent(IN) :: this            !< Dimensions wrapper 0D
        integer(I4P)                                  :: DataSizeInBytes !< Data size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = 0
        if(allocated(this%value)) DataSizeInBytes = byte_size(this%value(1,1,1,1,1,1,1))*size(this%value)
    end function DimensionsWrapper7D_DLCA_DataSizeInBytes


    function DimensionsWrapper7D_DLCA_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_DLCA_t), intent(IN) :: this         !< Dimensions wrapper 7D
        class(*),                          intent(IN) :: Mold         !< Mold for data type comparison
        logical                                  :: isOfDataType      !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (character(len=*))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper7D_DLCA_isOfDataType


    subroutine DimensionsWrapper7D_DLCA_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_DLCA_t),intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A,$)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = DLCA'//&
                        ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                        ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                        ', Value = '
        write(unit=unit,fmt=*,iostat=iostatd,iomsg=iomsgd) this%Value
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper7D_DLCA_Print


    subroutine DimensionsWrapper7D_I1P_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper7D
    !-----------------------------------------------------------------
        type(DimensionsWrapper7D_I1P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper7D_I1P_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set I1P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_I1P_t), intent(INOUT) :: this
        class(*),                         intent(IN)    :: Value(:,:,:,:,:,:,:)
        integer                                         :: err
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I1P))
                allocate(this%Value(size(Value,dim=1),  &
                                    size(Value,dim=2),  &
                                    size(Value,dim=3),  &
                                    size(Value,dim=4),  &
                                    size(Value,dim=5),  &
                                    size(Value,dim=6),  &
                                    size(Value,dim=7)), &
                                    stat=err)
                this%Value = Value
                if(err/=0) &
                    call msg%Error( txt='Setting Value: Allocation error ('//&
                                    str(no_sign=.true.,n=err)//')', &
                                    file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn( txt='Setting value: Expected data type (I1P)', &
                               file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    subroutine DimensionsWrapper7D_I1P_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get I1P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_I1P_t), intent(IN)  :: this
        class(*),                         intent(OUT) :: Value(:,:,:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I1P))
                if(all(this%GetShape() == shape(Value))) then
                    Value = this%Value
                else
                    call msg%Warn(txt='Getting value: Expected shape ('//    &
                                  str(no_sign=.true.,n=this%GetShape())//')',&
                                  file=__FILE__, line=__LINE__ )
                endif
            class Default
                call msg%Warn(txt='Getting value: Expected data type (I1P)',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    function DimensionsWrapper7D_I1P_GetShape(this) result(ValueShape) 
    !-----------------------------------------------------------------
    !< Get Wrapper Value Shape
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_I1P_t), intent(IN)  :: this
        integer(I4P), allocatable                     :: ValueShape(:)
    !-----------------------------------------------------------------
        allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper7D_I1P_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic pointer to Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_I1P_t), target, intent(IN)  :: this
        class(*), pointer                                     :: Value(:,:,:,:,:,:,:)
    !-----------------------------------------------------------------
        Value => this%Value
    end function


    subroutine DimensionsWrapper7D_I1P_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_I1P_t), intent(IN)  :: this
        class(*), allocatable,            intent(OUT) :: Value(:,:,:,:,:,:,:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1),  &
                       size(this%Value,dim=2),  &
                       size(this%Value,dim=3),  &
                       size(this%Value,dim=4),  &
                       size(this%Value,dim=5),  &
                       size(this%Value,dim=6),  &
                       size(this%Value,dim=7)), &
                       source=this%Value)
    end subroutine


    subroutine DimensionsWrapper7D_I1P_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper7D
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_I1P_t), intent(INOUT) :: this
        integer                                         :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    function DimensionsWrapper7D_I1P_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the size of the data stored in bytes
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_I1P_t), intent(IN) :: this            !< Dimensions wrapper 0D
        integer(I4P)                                 :: DataSizeInBytes !< Data size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = byte_size(this%value(1,1,1,1,1,1,1))*size(this%value)
    end function DimensionsWrapper7D_I1P_DataSizeInBytes


    function DimensionsWrapper7D_I1P_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_I1P_t), intent(IN) :: this          !< Dimensions wrapper 7D
        class(*),                         intent(IN) :: Mold          !< Mold for data type comparison
        logical                                      :: isOfDataType  !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (integer(I1P))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper7D_I1P_isOfDataType


    subroutine DimensionsWrapper7D_I1P_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_I1P_t), intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A,$)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = I1P'//&
                        ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                        ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                        ', Value = '
        write(unit=unit,fmt=*,iostat=iostatd,iomsg=iomsgd) str(no_sign=.true., n=this%Value)
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper7D_I1P_Print


    subroutine DimensionsWrapper7D_I2P_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper7D
    !-----------------------------------------------------------------
        type(DimensionsWrapper7D_I2P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper7D_I2P_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set I2P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_I2P_t), intent(INOUT) :: this
        class(*),                         intent(IN)    :: Value(:,:,:,:,:,:,:)
        integer                                         :: err
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I2P))
                allocate(this%Value(size(Value,dim=1),  &
                                    size(Value,dim=2),  &
                                    size(Value,dim=3),  &
                                    size(Value,dim=4),  &
                                    size(Value,dim=5),  &
                                    size(Value,dim=6),  &
                                    size(Value,dim=7)), &
                                    stat=err)
                this%Value = Value
                if(err/=0) &
                    call msg%Error( txt='Setting Value: Allocation error ('//&
                                    str(no_sign=.true.,n=err)//')', &
                                    file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn( txt='Setting value: Expected data type (I2P)', &
                               file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    subroutine DimensionsWrapper7D_I2P_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get I2P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_I2P_t), intent(IN)  :: this
        class(*),                         intent(OUT) :: Value(:,:,:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I2P))
                if(all(this%GetShape() == shape(Value))) then
                    Value = this%Value
                else
                    call msg%Warn(txt='Getting value: Expected shape ('//    &
                                  str(no_sign=.true.,n=this%GetShape())//')',&
                                  file=__FILE__, line=__LINE__ )
                endif
            class Default
                call msg%Warn(txt='Getting value: Expected data type (I2P)',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    function DimensionsWrapper7D_I2P_GetShape(this) result(ValueShape) 
    !-----------------------------------------------------------------
    !< Get Wrapper Value Shape
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_I2P_t), intent(IN)  :: this
        integer(I4P), allocatable                     :: ValueShape(:)
    !-----------------------------------------------------------------
        allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper7D_I2P_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic pointer to Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_I2P_t), target, intent(IN)  :: this
        class(*), pointer                                     :: Value(:,:,:,:,:,:,:)
    !-----------------------------------------------------------------
        Value => this%Value
    end function


    subroutine DimensionsWrapper7D_I2P_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_I2P_t), intent(IN)  :: this
        class(*), allocatable,            intent(OUT) :: Value(:,:,:,:,:,:,:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1),  &
                       size(this%Value,dim=2),  &
                       size(this%Value,dim=3),  &
                       size(this%Value,dim=4),  &
                       size(this%Value,dim=5),  &
                       size(this%Value,dim=6),  &
                       size(this%Value,dim=7)), &
                       source=this%Value)
    end subroutine


    subroutine DimensionsWrapper7D_I2P_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper7D
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_I2P_t), intent(INOUT) :: this
        integer                                         :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    function DimensionsWrapper7D_I2P_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the size of the data stored in bytes
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_I2P_t), intent(IN) :: this            !< Dimensions wrapper 0D
        integer(I4P)                                  :: DataSizeInBytes !< Data size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = byte_size(this%value(1,1,1,1,1,1,1))*size(this%value)
    end function DimensionsWrapper7D_I2P_DataSizeInBytes


    function DimensionsWrapper7D_I2P_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_I2P_t), intent(IN) :: this          !< Dimensions wrapper 7D
        class(*),                         intent(IN) :: Mold          !< Mold for data type comparison
        logical                                      :: isOfDataType  !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (integer(I2P))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper7D_I2P_isOfDataType


    subroutine DimensionsWrapper7D_I2P_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_I2P_t), intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A,$)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = I2P'//&
                        ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                        ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                        ', Value = '
        write(unit=unit,fmt=*,iostat=iostatd,iomsg=iomsgd) str(no_sign=.true., n=this%Value)
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper7D_I2P_Print


    subroutine DimensionsWrapper7D_I4P_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper7D
    !-----------------------------------------------------------------
        type(DimensionsWrapper7D_I4P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper7D_I4P_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set I4P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_I4P_t), intent(INOUT) :: this
        class(*),                         intent(IN)    :: Value(:,:,:,:,:,:,:)
        integer                                         :: err
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I4P))
                allocate(this%Value(size(Value,dim=1),  &
                                    size(Value,dim=2),  &
                                    size(Value,dim=3),  &
                                    size(Value,dim=4),  &
                                    size(Value,dim=5),  &
                                    size(Value,dim=6),  &
                                    size(Value,dim=7)), &
                                    stat=err)
                this%Value = Value
                if(err/=0) &
                    call msg%Error( txt='Setting Value: Allocation error ('//&
                                    str(no_sign=.true.,n=err)//')', &
                                    file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn( txt='Setting value: Expected data type (I4P)', &
                               file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    subroutine DimensionsWrapper7D_I4P_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get I4P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_I4P_t), intent(IN)  :: this
        class(*),                         intent(OUT) :: Value(:,:,:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I4P))
                if(all(this%GetShape() == shape(Value))) then
                    Value = this%Value
                else
                    call msg%Warn(txt='Getting value: Expected shape ('//    &
                                  str(no_sign=.true.,n=this%GetShape())//')',&
                                  file=__FILE__, line=__LINE__ )
                endif
            class Default
                call msg%Warn(txt='Getting value: Expected data type (I4P)',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    function DimensionsWrapper7D_I4P_GetShape(this) result(ValueShape) 
    !-----------------------------------------------------------------
    !< Get Wrapper Value Shape
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_I4P_t), intent(IN)  :: this
        integer(I4P), allocatable                     :: ValueShape(:)
    !-----------------------------------------------------------------
        allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper7D_I4P_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic pointer to Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_I4P_t), target, intent(IN)  :: this
        class(*), pointer                                     :: Value(:,:,:,:,:,:,:)
    !-----------------------------------------------------------------
        Value => this%Value
    end function


    subroutine DimensionsWrapper7D_I4P_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_I4P_t), intent(IN)  :: this
        class(*), allocatable,            intent(OUT) :: Value(:,:,:,:,:,:,:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1),  &
                       size(this%Value,dim=2),  &
                       size(this%Value,dim=3),  &
                       size(this%Value,dim=4),  &
                       size(this%Value,dim=5),  &
                       size(this%Value,dim=6),  &
                       size(this%Value,dim=7)), &
                       source=this%Value)
    end subroutine


    subroutine DimensionsWrapper7D_I4P_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper7D
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_I4P_t), intent(INOUT) :: this
        integer                                         :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    function DimensionsWrapper7D_I4P_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the size of the data stored in bytes
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_I4P_t), intent(IN) :: this            !< Dimensions wrapper 0D
        integer(I4P)                                  :: DataSizeInBytes !< Data size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = byte_size(this%value(1,1,1,1,1,1,1))*size(this%value)
    end function DimensionsWrapper7D_I4P_DataSizeInBytes


    function DimensionsWrapper7D_I4P_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_I4P_t), intent(IN) :: this           !< Dimensions wrapper 7D
        class(*),                         intent(IN) :: Mold           !< Mold for data type comparison
        logical                                      :: isOfDataType   !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (integer(I4P))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper7D_I4P_isOfDataType


    subroutine DimensionsWrapper7D_I4P_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_I4P_t), intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A,$)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = I4P'//&
                        ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                        ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                        ', Value = '
        write(unit=unit,fmt=*,iostat=iostatd,iomsg=iomsgd) str(no_sign=.true., n=this%Value)
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper7D_I4P_Print


    subroutine DimensionsWrapper7D_I8P_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper7D
    !-----------------------------------------------------------------
        type(DimensionsWrapper7D_I8P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper7D_I8P_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set I8P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_I8P_t), intent(INOUT) :: this
        class(*),                         intent(IN)    :: Value(:,:,:,:,:,:,:)
        integer                                         :: err
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I8P))
                allocate(this%Value(size(Value,dim=1),  &
                                    size(Value,dim=2),  &
                                    size(Value,dim=3),  &
                                    size(Value,dim=4),  &
                                    size(Value,dim=5),  &
                                    size(Value,dim=6),  &
                                    size(Value,dim=7)), &
                                    stat=err)
                this%Value = Value
                if(err/=0) &
                    call msg%Error( txt='Setting Value: Allocation error ('//&
                                    str(no_sign=.true.,n=err)//')', &
                                    file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn( txt='Setting value: Expected data type (I8P)', &
                               file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    subroutine DimensionsWrapper7D_I8P_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get I8P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_I8P_t), intent(IN)  :: this
        class(*),                         intent(OUT) :: Value(:,:,:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (integer(I8P))
                if(all(this%GetShape() == shape(Value))) then
                    Value = this%Value
                else
                    call msg%Warn(txt='Getting value: Expected shape ('//    &
                                  str(no_sign=.true.,n=this%GetShape())//')',&
                                  file=__FILE__, line=__LINE__ )
                endif
            class Default
                call msg%Warn(txt='Getting value: Expected data type (I8P)',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    function DimensionsWrapper7D_I8P_GetShape(this) result(ValueShape) 
    !-----------------------------------------------------------------
    !< Get Wrapper Value Shape
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_I8P_t), intent(IN)  :: this
        integer(I4P), allocatable                     :: ValueShape(:)
    !-----------------------------------------------------------------
        allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper7D_I8P_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic pointer to Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_I8P_t), target, intent(IN)  :: this
        class(*), pointer                                     :: Value(:,:,:,:,:,:,:)
    !-----------------------------------------------------------------
        Value => this%Value
    end function


    subroutine DimensionsWrapper7D_I8P_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_I8P_t), intent(IN)  :: this
        class(*), allocatable,            intent(OUT) :: Value(:,:,:,:,:,:,:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1),  &
                       size(this%Value,dim=2),  &
                       size(this%Value,dim=3),  &
                       size(this%Value,dim=4),  &
                       size(this%Value,dim=5),  &
                       size(this%Value,dim=6),  &
                       size(this%Value,dim=7)), &
                       source=this%Value)
    end subroutine


    subroutine DimensionsWrapper7D_I8P_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper7D
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_I8P_t), intent(INOUT) :: this
        integer                                         :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    function DimensionsWrapper7D_I8P_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the size of the data stored in bytes
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_I8P_t), intent(IN) :: this            !< Dimensions wrapper 0D
        integer(I4P)                                  :: DataSizeInBytes !< Data size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = byte_size(this%value(1,1,1,1,1,1,1))*size(this%value)
    end function DimensionsWrapper7D_I8P_DataSizeInBytes


    function DimensionsWrapper7D_I8P_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_I8P_t), intent(IN) :: this          !< Dimensions wrapper 7D
        class(*),                         intent(IN) :: Mold          !< Mold for data type comparison
        logical                                      :: isOfDataType  !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (integer(I8P))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper7D_I8P_isOfDataType


    subroutine DimensionsWrapper7D_I8P_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_I8P_t), intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A,$)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = I8P'//&
                        ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                        ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                        ', Value = '
        write(unit=unit,fmt=*,iostat=iostatd,iomsg=iomsgd) str(no_sign=.true., n=this%Value)
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper7D_I8P_Print


    subroutine DimensionsWrapper7D_L_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper7D
    !-----------------------------------------------------------------
        type(DimensionsWrapper7D_L_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper7D_L_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set logical Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_L_t), intent(INOUT) :: this
        class(*),                       intent(IN)    :: Value(:,:,:,:,:,:,:)
        integer                                       :: err
    !-----------------------------------------------------------------
        select type (Value)
            type is (logical)
                allocate(this%Value(size(Value,dim=1),  &
                                    size(Value,dim=2),  &
                                    size(Value,dim=3),  &
                                    size(Value,dim=4),  &
                                    size(Value,dim=5),  &
                                    size(Value,dim=6),  &
                                    size(Value,dim=7)), &
                                    stat=err)
                this%Value = Value
                if(err/=0) &
                    call msg%Error( txt='Setting Value: Allocation error ('//&
                                    str(no_sign=.true.,n=err)//')', &
                                    file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn( txt='Setting value: Expected data type (logical)', &
                               file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    subroutine DimensionsWrapper7D_L_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get logical Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_L_t), intent(IN)  :: this
        class(*),                       intent(OUT) :: Value(:,:,:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (logical)
                if(all(this%GetShape() == shape(Value))) then
                    Value = this%Value
                else
                    call msg%Warn(txt='Getting value: Expected shape ('//    &
                                  str(no_sign=.true.,n=this%GetShape())//')',&
                                  file=__FILE__, line=__LINE__ )
                endif
            class Default
                call msg%Warn(txt='Getting value: Expected data type (logical)',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    function DimensionsWrapper7D_L_GetShape(this) result(ValueShape) 
    !-----------------------------------------------------------------
    !< Get Wrapper Value Shape
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_L_t), intent(IN)  :: this
        integer(I4P), allocatable                   :: ValueShape(:)
    !-----------------------------------------------------------------
        allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper7D_L_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic pointer to Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_L_t), target, intent(IN)  :: this
        class(*), pointer                                   :: Value(:,:,:,:,:,:,:)
    !-----------------------------------------------------------------
        Value => this%Value
    end function


    subroutine DimensionsWrapper7D_L_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_L_t), intent(IN)  :: this
        class(*), allocatable,          intent(OUT) :: Value(:,:,:,:,:,:,:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1),  &
                       size(this%Value,dim=2),  &
                       size(this%Value,dim=3),  &
                       size(this%Value,dim=4),  &
                       size(this%Value,dim=5),  &
                       size(this%Value,dim=6),  &
                       size(this%Value,dim=7)), &
                       source=this%Value)
    end subroutine


    subroutine DimensionsWrapper7D_L_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper7D
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_L_t), intent(INOUT) :: this
        integer                                         :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    function DimensionsWrapper7D_L_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the size of the data stored in bytes
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_L_t), intent(IN) :: this            !< Dimensions wrapper 0D
        integer(I4P)                               :: DataSizeInBytes !< Data size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = byte_size_logical(this%value(1,1,1,1,1,1,1))*size(this%value)
    end function DimensionsWrapper7D_L_DataSizeInBytes


    function DimensionsWrapper7D_L_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_L_t), intent(IN) :: this            !< Dimensions wrapper 7D
        class(*),                       intent(IN) :: Mold            !< Mold for data type comparison
        logical                                    :: isOfDataType    !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (logical)
                isOfDataType = .true.
        end select
    end function DimensionsWrapper7D_L_isOfDataType


    subroutine DimensionsWrapper7D_L_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_L_t),   intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A,$)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = L'//&
                        ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                        ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                        ', Value = '
        write(unit=unit,fmt=*,iostat=iostatd,iomsg=iomsgd) str(n=this%Value)
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper7D_L_Print


    subroutine DimensionsWrapper7D_R4P_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper7D
    !-----------------------------------------------------------------
        type(DimensionsWrapper7D_R4P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper7D_R4P_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set R4P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_R4P_t), intent(INOUT) :: this
        class(*),                         intent(IN)    :: Value(:,:,:,:,:,:,:)
        integer                                         :: err
    !-----------------------------------------------------------------
        select type (Value)
            type is (real(R4P))
                allocate(this%Value(size(Value,dim=1),  &
                                    size(Value,dim=2),  &
                                    size(Value,dim=3),  &
                                    size(Value,dim=4),  &
                                    size(Value,dim=5),  &
                                    size(Value,dim=6),  &
                                    size(Value,dim=7)), &
                                    stat=err)
                this%Value = Value
                if(err/=0) &
                    call msg%Error( txt='Setting Value: Allocation error ('//&
                                    str(no_sign=.true.,n=err)//')', &
                                    file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn( txt='Setting value: Expected data type (R4P)', &
                               file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    subroutine DimensionsWrapper7D_R4P_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get R4P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_R4P_t), intent(IN)  :: this
        class(*),                         intent(OUT) :: Value(:,:,:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (real(R4P))
                if(all(this%GetShape() == shape(Value))) then
                    Value = this%Value
                else
                    call msg%Warn(txt='Getting value: Expected shape ('//    &
                                  str(no_sign=.true.,n=this%GetShape())//')',&
                                  file=__FILE__, line=__LINE__ )
                endif
            class Default
                call msg%Warn(txt='Getting value: Expected data type (R4P)',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    function DimensionsWrapper7D_R4P_GetShape(this) result(ValueShape) 
    !-----------------------------------------------------------------
    !< Get Wrapper Value Shape
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_R4P_t), intent(IN)  :: this
        integer(I4P), allocatable                     :: ValueShape(:)
    !-----------------------------------------------------------------
        allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper7D_R4P_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic pointer to Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_R4P_t), target, intent(IN)  :: this
        class(*), pointer                                     :: Value(:,:,:,:,:,:,:)
    !-----------------------------------------------------------------
        Value => this%Value
    end function


    subroutine DimensionsWrapper7D_R4P_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_R4P_t), intent(IN)  :: this
        class(*), allocatable,            intent(OUT) :: Value(:,:,:,:,:,:,:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1),  &
                       size(this%Value,dim=2),  &
                       size(this%Value,dim=3),  &
                       size(this%Value,dim=4),  &
                       size(this%Value,dim=5),  &
                       size(this%Value,dim=6),  &
                       size(this%Value,dim=7)), &
                       source=this%Value)
    end subroutine


    subroutine DimensionsWrapper7D_R4P_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper7D
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_R4P_t), intent(INOUT) :: this
        integer                                         :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    function DimensionsWrapper7D_R4P_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the size of the data stored in bytes
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_R4P_t), intent(IN) :: this            !< Dimensions wrapper 0D
        integer(I4P)                                  :: DataSizeInBytes !< Data size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = byte_size(this%value(1,1,1,1,1,1,1))*size(this%value)
    end function DimensionsWrapper7D_R4P_DataSizeInBytes


    function DimensionsWrapper7D_R4P_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_R4P_t), intent(IN) :: this          !< Dimensions wrapper 7D
        class(*),                         intent(IN) :: Mold          !< Mold for data type comparison
        logical                                      :: isOfDataType  !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (real(R4P))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper7D_R4P_isOfDataType


    subroutine DimensionsWrapper7D_R4P_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_R4P_t), intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A,$)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = R4P'//&
                        ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                        ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                        ', Value = '
        write(unit=unit,fmt=*,iostat=iostatd,iomsg=iomsgd) str(no_sign=.true., n=this%Value)
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper7D_R4P_Print


    subroutine DimensionsWrapper7D_R8P_Final(this) 
    !-----------------------------------------------------------------
    !< Final procedure of DimensionsWrapper7D
    !-----------------------------------------------------------------
        type(DimensionsWrapper7D_R8P_t), intent(INOUT) :: this
    !-----------------------------------------------------------------
        call this%Free()
    end subroutine


    subroutine DimensionsWrapper7D_R8P_Set(this, Value) 
    !-----------------------------------------------------------------
    !< Set R8P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_R8P_t), intent(INOUT) :: this
        class(*),                         intent(IN)    :: Value(:,:,:,:,:,:,:)
        integer                                         :: err
    !-----------------------------------------------------------------
        select type (Value)
            type is (real(R8P))
                allocate(this%Value(size(Value,dim=1),  &
                                    size(Value,dim=2),  &
                                    size(Value,dim=3),  &
                                    size(Value,dim=4),  &
                                    size(Value,dim=5),  &
                                    size(Value,dim=6),  &
                                    size(Value,dim=7)), &
                                    source=Value, stat=err)
                if(err/=0) &
                    call msg%Error( txt='Setting Value: Allocation error ('//&
                                    str(no_sign=.true.,n=err)//')', &
                                    file=__FILE__, line=__LINE__ )
            class Default
                call msg%Warn( txt='Setting value: Expected data type (R8P)', &
                               file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    subroutine DimensionsWrapper7D_R8P_Get(this, Value) 
    !-----------------------------------------------------------------
    !< Get R8P Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_R8P_t), intent(IN)  :: this
        class(*),                         intent(OUT) :: Value(:,:,:,:,:,:,:)
    !-----------------------------------------------------------------
        select type (Value)
            type is (real(R8P))
                if(all(this%GetShape() == shape(Value))) then
                    Value = this%Value
                else
                    call msg%Warn(txt='Getting value: Expected shape ('//    &
                                  str(no_sign=.true.,n=this%GetShape())//')',&
                                  file=__FILE__, line=__LINE__ )
                endif
            class Default
                call msg%Warn(txt='Getting value: Expected data type (R8P)',&
                              file=__FILE__, line=__LINE__ )
        end select
    end subroutine


    function DimensionsWrapper7D_R8P_GetShape(this) result(ValueShape) 
    !-----------------------------------------------------------------
    !< Get Wrapper Value Shape
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_R8P_t), intent(IN)  :: this
        integer(I4P), allocatable                     :: ValueShape(:)
    !-----------------------------------------------------------------
        allocate(ValueShape(this%GetDimensions()))
        ValueShape = shape(this%Value)
    end function


    function DimensionsWrapper7D_R8P_GetPointer(this) result(Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic pointer to Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_R8P_t), target, intent(IN)  :: this
        class(*), pointer                                     :: Value(:,:,:,:,:,:,:)
    !-----------------------------------------------------------------
        Value => this%Value
    end function


    subroutine DimensionsWrapper7D_R8P_GetPolymorphic(this, Value) 
    !-----------------------------------------------------------------
    !< Get Unlimited Polymorphic Wrapper Value
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_R8P_t), intent(IN)  :: this
        class(*), allocatable,            intent(OUT) :: Value(:,:,:,:,:,:,:)
    !-----------------------------------------------------------------
        allocate(Value(size(this%Value,dim=1),  &
                       size(this%Value,dim=2),  &
                       size(this%Value,dim=3),  &
                       size(this%Value,dim=4),  &
                       size(this%Value,dim=5),  &
                       size(this%Value,dim=6),  &
                       size(this%Value,dim=7)), &
                       source=this%Value)
    end subroutine


    subroutine DimensionsWrapper7D_R8P_Free(this) 
    !-----------------------------------------------------------------
    !< Free a DimensionsWrapper7D
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_R8P_t), intent(INOUT) :: this
        integer                                         :: err
    !-----------------------------------------------------------------
        if(allocated(this%Value)) then
            deallocate(this%Value, stat=err)
            if(err/=0) call msg%Error(txt='Freeing Value: Deallocation error ('// &
                                      str(no_sign=.true.,n=err)//')',             &
                                      file=__FILE__, line=__LINE__ )
        endif
    end subroutine


    function DimensionsWrapper7D_R8P_DataSizeInBytes(this) result(DataSizeInBytes)
    !-----------------------------------------------------------------
    !< Return the size of the data stored in bytes
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_R8P_t), intent(IN) :: this            !< Dimensions wrapper 0D
        integer(I4P)                                  :: DataSizeInBytes !< Data size in bytes
    !-----------------------------------------------------------------
        DataSizeInBytes = byte_size(this%value(1,1,1,1,1,1,1))*size(this%value)
    end function DimensionsWrapper7D_R8P_DataSizeInBytes


    function DimensionsWrapper7D_R8P_isOfDataType(this, Mold) result(isOfDataType)
    !-----------------------------------------------------------------
    !< Check if Mold and Value are of the same datatype 
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_R8P_t), intent(IN) :: this          !< Dimensions wrapper 7D
        class(*),                         intent(IN) :: Mold          !< Mold for data type comparison
        logical                                      :: isOfDataType  !< Boolean flag to check if Value is of the same data type as Mold
    !-----------------------------------------------------------------
        isOfDataType = .false.
        select type (Mold)
            type is (real(R8P))
                isOfDataType = .true.
        end select
    end function DimensionsWrapper7D_R8P_isOfDataType


    subroutine DimensionsWrapper7D_R8P_Print(this, unit, prefix, iostat, iomsg)
    !-----------------------------------------------------------------
    !< Print Wrapper
    !-----------------------------------------------------------------
        class(DimensionsWrapper7D_R8P_t), intent(IN)  :: this         !< DimensionsWrapper
        integer(I4P),                     intent(IN)  :: unit         !< Logic unit.
        character(*), optional,           intent(IN)  :: prefix       !< Prefixing string.
        integer(I4P), optional,           intent(OUT) :: iostat       !< IO error.
        character(*), optional,           intent(OUT) :: iomsg        !< IO error message.
        character(len=:), allocatable                 :: prefd        !< Prefixing string.
        integer(I4P)                                  :: iostatd      !< IO error.
        character(500)                                :: iomsgd       !< Temporary variable for IO error message.
    !-----------------------------------------------------------------
        prefd = '' ; if (present(prefix)) prefd = prefix
        write(unit=unit,fmt='(A,$)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = R8P'//&
                        ', Dimensions = '//trim(str(no_sign=.true., n=this%GetDimensions()))//&
                        ', Bytes = '//trim(str(no_sign=.true., n=this%DataSizeInBytes()))//&
                        ', Value = '
        write(unit=unit,fmt=*,iostat=iostatd,iomsg=iomsgd) str(no_sign=.true., n=this%Value)
        if (present(iostat)) iostat = iostatd
        if (present(iomsg))  iomsg  = iomsgd
    end subroutine DimensionsWrapper7D_R8P_Print

end module DimensionsWrapper
