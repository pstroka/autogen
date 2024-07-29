autogen
======

[<img alt="github" src="https://img.shields.io/badge/github-pstroka/autogen-8da0cb?style=for-the-badge&labelColor=555555&logo=github" height="20">](https://github.com/pstroka/autogen)
[<img alt="crates.io" src="https://img.shields.io/crates/v/autogen.svg?style=for-the-badge&color=fc8d62&logo=rust" height="20">](https://crates.io/crates/autogen)
[<img alt="docs.rs" src="https://img.shields.io/badge/docs.rs-autogen-66c2a5?style=for-the-badge&labelColor=555555&logo=docs.rs" height="20">](https://docs.rs/autogen)

Tired of repeating all the generics in every `impl` block?

Autogen is a set of macros that allows you to automatically apply generics to `impl` blocks.
- the `#[register]` macro registers the generics of a `struct` or `enum`, including lifetimes and
the where clause.
- the `#[apply]` macro applies the generics to an `impl` block.
```rust
#[autogen::register]
struct Struct<'a, T, R: ?Sized>
where
    T: PartialEq,
{
    x: T,
    y: &'a R,
}

#[autogen::apply]
impl Struct {}
```
This will expand to:
```rust
impl<'a, T, R: ?Sized> Struct<'a, T, R> where T: PartialEq {}
```
Apart from the type itself, the generics can also be applied to:
- arrays of the given type
- slices of the given type
- references of the given type
- pointers of the given type
- tuples containing the given type
- other types containing the given type as a generic argument
- a combination of all of the above

The only restriction is that the `impl` block cannot contain different registered types.
```rust
#[autogen::register]
struct Struct<'a, T, R: ?Sized>
where
    T: PartialEq,
{
    x: T,
    y: &'a R,
}

#[autogen::apply]
impl Struct {
    fn x_equals(&self, other: &T) -> bool {
        &self.x == other
    }
    fn y(&self) -> &'a R {
        self.y
    }
}

trait Trait {
    fn type_of(&self) -> &'static str;
}

#[autogen::apply]
impl Trait for Struct {
    fn type_of(&self) -> &'static str {
        "regular"
    }
}

#[autogen::apply]
impl Trait for [Struct; 2] {
    fn type_of(&self) -> &'static str {
        "array of size 2"
    }
}

#[autogen::apply]
impl Trait for [Struct] {
    fn type_of(&self) -> &'static str {
        "slice"
    }
}

#[autogen::apply]
impl Trait for &Struct {
    fn type_of(&self) -> &'static str {
        "reference"
    }
}

#[autogen::apply]
impl Trait for *const Struct {
    fn type_of(&self) -> &'static str {
        "pointer"
    }
}

#[autogen::apply]
impl Trait for (Struct, &'static str, Struct) {
    fn type_of(&self) -> &'static str {
        "tuple"
    }
}

#[autogen::apply]
impl Trait for Result<Struct, String> {
    fn type_of(&self) -> &'static str {
        "generic argument"
    }
}

#[autogen::apply]
impl Trait for [([Option<&Struct>; 1], Struct, String)] {
    fn type_of(&self) -> &'static str {
        "crazy ****"
    }
}

let struct1 = Struct { x: 1, y: "abc" };
assert!(struct1.x_equals(&1));
assert_eq!(struct1.y(), "abc");

assert_eq!(struct1.type_of(), "regular");
assert_eq!((&&struct1).type_of(), "reference");

let pointer = &struct1 as *const Struct<'_, i32, str>;
assert_eq!(pointer.type_of(), "pointer");

let struct2 = Struct { x: 2, y: "xyz" };
assert!(struct2.x_equals(&2));
assert_eq!(struct2.y(), "xyz");

let tuple = (struct1, "string", struct2);
assert_eq!(tuple.type_of(), "tuple");

let array = [tuple.0, tuple.2];
assert_eq!(array.type_of(), "array of size 2");
assert_eq!(array[..].type_of(), "slice");

let struct3 = Struct { x: -1, y: "b" };
let result: Result<_, String> = Ok(struct3);
assert_eq!(result.type_of(), "generic argument");

let struct3 = result.unwrap();
let struct4 = Struct { x: -2, y: "s" };
let crazy = [([Some(&struct3)], struct4, "****".to_string())];
assert_eq!(crazy[..].type_of(), "crazy ****");
```

By default, the generics are registered with the struct/enum name, but you can provide a
custom identifier. This can be useful if a type with the same name is already registered in
another module.

This will not compile because `Name` is already registered.
```rust
#[autogen::register]
struct Name<T: PartialEq> {
    t: T,
}

mod sub {
    #[autogen::register]
    pub struct Name<S: FromStr> {
        s: S,
    }
}
```

To resolve this, you must provide a custom identifier.
```rust
#[autogen::register]
struct Name<T: PartialEq> {
    t: T,
}

#[autogen::apply]
impl Name {
    fn t(&self) -> &T {
        &self.t
    }
}

mod sub {
    use std::str::FromStr;

    #[autogen::register(CustomName)]
    pub struct Name<S: FromStr> {
        s: S,
    }

    #[autogen::apply(CustomName)]
    impl Name {
        pub fn parse(string: &str) -> Result<Self, S::Err> {
            Ok(Name { s: string.parse()? })
        }
        pub fn s(&self) -> &S {
            &self.s
        }
    }
}

let s1 = Name { t: 64 };
assert_eq!(s1.t(), &64);

let s2 = sub::Name::<u32>::parse("123").unwrap();
assert_eq!(s2.s(), &123);
```
