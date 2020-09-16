// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

use generic_array::{typenum::U32, GenericArray};
use generic_bytes_derive::SizedBytes;

#[derive(SizedBytes)]
struct Foo {
    f1: GenericArray<u8, U32>,
    f2: GenericArray<u8, U32>,
}

#[test]
fn test_foo() {
    let d1 = [1u8; 32];
    let d2 = [2u8; 32];

    let g1 = GenericArray::from_slice(&d1);
    let g2 = GenericArray::from_slice(&d2);

    let _foo = Foo { f1: *g1, f2: *g2 };
}
