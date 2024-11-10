autogen
======

[<img alt="github" src="https://img.shields.io/badge/github-pstroka/autogen-8da0cb?style=for-the-badge&labelColor=555555&logo=github" height="20">](https://github.com/pstroka/autogen)
[<img alt="crates.io" src="https://img.shields.io/crates/v/autogen.svg?style=for-the-badge&color=fc8d62&logo=rust" height="20">](https://crates.io/crates/autogen)
[<img alt="docs.rs" src="https://img.shields.io/badge/docs.rs-autogen-66c2a5?style=for-the-badge&labelColor=555555&logo=docs.rs" height="20">](https://docs.rs/autogen)

Tired of repeating all the generics in every `impl` block or function?

Autogen is a set of macros that allows you to automatically apply generics to `impl` blocks and 
functions.
- the `#[register]` macro registers the generics of a `struct` or `enum`, including lifetimes and
the where clause.
- the `#[apply]` macro applies the generics to an `impl` block or function.

```rust
#[autogen::register]
struct Struct<'a, T, R: ?Sized>
where
    T: PartialEq,
{
    x: T,
    y: &'a R,
}

// This will expand to impl<'a, T, R: ?Sized> Struct<'a, T, R> where T: PartialEq {}
#[autogen::apply]
impl Struct {}

// This will expand to impl<'a, T, T> Struct<'a, T, T> where T: PartialEq {}
#[autogen::apply(R = T)]
impl Struct {}

// This will expand to impl<'a, String, str> Struct<'a, String, str>
#[autogen::apply(T = String, R = str)]
impl Struct {}
```

## Examples

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

let s = Struct { x: 1, y: "abc" };
assert!(s.x_equals(&1));
assert_eq!(s.y(), "abc");

#[autogen::apply]
impl TryFrom<Vec<Struct>> for Struct {
    type Error = String;

    fn try_from(vec: Vec<Struct>) -> Result<Struct, String> {
        let first: Option<Struct> = vec.into_iter().next();
        first.ok_or("empty".to_string())
    }
}

let vec = vec![s];
let s: Struct<'_, _, _> = vec.try_into().unwrap();
assert!(s.x_equals(&1));
assert_eq!(s.y(), "abc");

#[autogen::apply]
fn same_x(l: &Struct, r: &Struct) -> bool {
    l.x == r.x
}

let l = Struct { x: 2.1, y: &3 };
let r = Struct { x: 2.1, y: &7 };
assert!(same_x(&l, &r));
```

By default, the generics are registered with the struct/enum name, but you can provide a
custom identifier. This can be useful if a type with the same name is already registered in
another module.

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
    pub struct Name<S: FromStr, X> {
        pub s: S,
        pub x: X,
    }

    #[autogen::apply(CustomName)]
    impl Name {
        pub fn new(string: &str, x: X) -> Result<Self, S::Err> {
            Ok(Name { s: string.parse()?, x })
        }
    }

    #[autogen::apply(id = CustomName, X = S)]
    impl Name {
        pub fn parse(string: &str) -> Result<Self, S::Err> {
            Ok(Name { s: string.parse()?, x: string.parse()? })
        }
    }
}

let s1 = Name { t: 64 };
assert_eq!(s1.t(), &64);

let s2 = sub::Name::<u32, i32>::new("123", -5).unwrap();
assert_eq!(s2.s, 123);
assert_eq!(s2.x, -5);

let s3 = sub::Name::<f64, f64>::parse("5.6").unwrap();
assert_eq!(s3.s, 5.6);
assert_eq!(s3.x, 5.6);
```

For more examples and details, please see the [documentation](https://docs.rs/autogen)
