use std::fmt::Display;

#[autogen::register]
struct Struct<T: Display> {
    t: T,
}

#[test]
fn trait_generic_arg_as_input() {
    #[autogen::apply]
    impl From<Struct> for String {
        fn from(val: Struct) -> Self {
            format!("{} as string", val.t)
        }
    }

    let s = Struct { t: "abc" };
    let string: String = s.into();
    assert_eq!(string, "abc as string");
}

#[test]
fn trait_generic_arg_as_output() {
    #[autogen::apply]
    impl From<T> for Struct {
        fn from(val: T) -> Struct {
            let a: Struct = Struct { t: val };
            a
        }
    }

    let string: String = "abc".to_string();
    let s: Struct<_> = string.into();
    assert_eq!(s.t, "abc");
}
