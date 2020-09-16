// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

use generic_array::{
    arr,
    typenum::{U0, U32},
    GenericArray,
};
use generic_bytes_derive::SizedBytes;

#[derive(SizedBytes)]
struct Foo<T> {
    f1: GenericArray<u8, U32>,
    f2: T,
}

struct Bar();

impl generic_bytes::SizedBytes for Bar {
    type Len = U0;

    fn to_arr(&self) -> GenericArray<u8, U0> {
        arr![u8;]
    }

    fn from_arr(
        _bytes: &GenericArray<u8, U0>,
    ) -> Result<Bar, generic_bytes::TryFromSizedBytesError> {
        Ok(Bar())
    }
}

#[test]
fn test_foo() {
    let d1 = [2u8; 32];

    let g2 = Bar();
    let g1 = GenericArray::from_slice(&d1);

    let _foo = Foo { f1: *g1, f2: g2 };
}
