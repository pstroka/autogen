use std::fmt::Display;

#[autogen::register]
struct Struct<T: Display> {
    t: T,
}

trait Def<T> {
    const DEF: Option<T>;

    fn get_def(&self) -> Option<T> {
        Self::DEF
    }
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
            {
                let a: Struct = Struct { t: val };
                a
            }
        }
    }

    let string: String = "abc".to_string();
    let s: Struct<_> = string.into();
    assert_eq!(s.t, "abc");
}

#[test]
fn associated_type() {
    #[autogen::apply]
    impl Iterator for Struct {
        type Item = Struct;

        fn next(&mut self) -> Option<Struct> {
            None
        }
    }
}

#[test]
fn associated_const() {
    #[autogen::apply]
    impl Def<Struct> for Struct {
        const DEF: Option<Struct> = None;
    }

    let s = Struct { t: "t" };
    assert!(s.get_def().is_none());
}

#[test]
fn inner_fn() {
    impl Def<String> for String {
        const DEF: Option<String> = None;

        fn get_def(&self) -> Option<String> {
            return inner(self.into()).map(|s| s.t).cloned();

            #[autogen::apply]
            fn inner(s: Struct) -> Option<Struct> {
                Some(s)
            }
        }
    }

    let string = "abc".to_string();
    assert_eq!(string.get_def().unwrap(), "abc");
}
