// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

use generic_array::{ArrayLength, GenericArray};
use std::fmt;
use std::marker::Sized;

/// The error type returned when a conversion from a generic byte slice to a
/// SizedBytes fails.
#[derive(Debug, Copy, Clone)]
pub struct TryFromSizedBytesError(());

impl fmt::Display for TryFromSizedBytesError {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self.__description(), f)
    }
}

impl TryFromSizedBytesError {
    pub fn new() -> TryFromSizedBytesError {
        TryFromSizedBytesError(())
    }

    #[inline]
    #[doc(hidden)]
    pub fn __description(&self) -> &str {
        "could not convert from generic byte slice"
    }
}

impl Default for TryFromSizedBytesError {
    fn default() -> TryFromSizedBytesError {
        TryFromSizedBytesError::new()
    }
}

/// A trait for sized key material that can be represented within a fixed byte
/// array size, used to represent our DH key types. This trait being
/// implemented with Error = SomeError allows you to derive
/// `TryFrom<&[u8], Error = SomeError>`.
pub trait SizedBytes: Sized {
    /// The typed representation of the byte length
    type Len: ArrayLength<u8> + 'static;

    /// Converts this sized key material to a `GenericArray` of the same
    /// size. One can convert this to a `&[u8]` with `GenericArray::as_slice()`
    /// but the size information is then lost from the type.
    fn to_arr(&self) -> GenericArray<u8, Self::Len>;

    /// How to parse such sized material from a correctly-sized byte slice.
    fn from_arr(arr: &GenericArray<u8, Self::Len>) -> Result<Self, TryFromSizedBytesError>;
}

/// The blanket implementation of SizedBytes for GenericArray
impl<N: ArrayLength<u8> + 'static> SizedBytes for GenericArray<u8, N> {
    type Len = N;

    fn to_arr(&self) -> GenericArray<u8, Self::Len> {
        self.clone()
    }
    fn from_arr(arr: &GenericArray<u8, Self::Len>) -> Result<Self, TryFromSizedBytesError> {
        Ok(arr.clone())
    }
}
