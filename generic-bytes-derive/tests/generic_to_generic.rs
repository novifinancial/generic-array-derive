// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

use generic_array::{arr, typenum::U0, GenericArray};
use generic_bytes_derive::SizedBytes;

#[derive(SizedBytes)]
struct Foo<T, U> {
    f1: T,
    f2: U,
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
    let g1 = Bar();
    let g2 = Bar();

    let foo: Foo<Bar, Bar> = Foo { f1: g1, f2: g2 };

    let bytes: GenericArray<u8, U0> = generic_bytes::SizedBytes::to_arr(&foo);
    assert_eq!(bytes.len(), 0);
}
