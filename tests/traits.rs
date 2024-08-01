use std::fmt::Display;

#[autogen::register]
struct Struct<T: Display> {
    t: T,
}

#[test]
fn trait_generic_arg() {
    #[autogen::apply]
    impl From<Struct> for String {
        fn from(value: Struct<T>) -> Self {
            format!("{} as string", value.t)
        }
    }
    let s = Struct { t: "abc" };
    let string: String = s.into();
    assert_eq!(string, "abc as string");
}
