##  SizedBytes ![Build Status](https://github.com/novifinancial/generic-array-derive/workflows/Rust%20CI/badge.svg)

SizedBytes is a trait that embodies conversion to and from an array of bytes
with a type-level length. It leverages the
[generic-array](https://github.com/fizyk20/generic-array) and
[typenum](https://github.com/paholg/typenum) crates which offer this
[emulation](https://doi.org/10.1017/S0956796802004355) of dependent types for
type-level array lengths.

```rust
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
```

The crate `generic-bytes-derive` provides a derive macro that lets you derive a
`SizedBytes` impl for any :

- tuple of which all the elements satisfy a `SizedBytes` bound,
- struct of which all the fields satisfy a `SizeBytes` bound.

The fields or components are serialized in (resp. deserialized from) their byte
representation in the order in which they appear in the original struct or
tuple.

For example:

```rust
#[derive(SizedBytes)]
struct Foo <T: SizedBytes>{
    f1: T,
    f2: GenericArray<u8, U32>,
}
// you can now call `to_arr` and `from_arr` on any Foo
```

A blanket implementation of `SizedBytes` is also provided for `GenericArray<u8, N>`.

Purpose
----------

Many applications in Rust do a lot of conversions to and from byte slices,
manually or using methods such as [bincode](https://github.com/servo/bincode)
or [bytes](https://github.com/serde-rs/bytes) for performance. Nonetheless,
composing those representations has always been tedious and has led to length
checks which can be error-prone and cumbersome.

This lets you define conversion to a byte slice that bears its length in the
type, and compose such conversions, thereby offering a safer and more automated
way to deal with byte representations.

Documentation
-------------

The API can be found [here](https://docs.rs/generic-array-derive/).

Installation
------------

Add the following line to the dependencies of your `Cargo.toml`:

```toml
generic_bytes = "0.1.0"
generic_bytes_derive = "0.1.0"
```

Contributors
------------

The authors of this code are François Garillot ([@huitseeker](https://github.com/huitseeker)).
To learn more about contributing to this project, [see this document](./CONTRIBUTING.md).

License
-------

This project is [MIT licensed](./LICENSE).
