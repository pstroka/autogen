#![allow(non_local_definitions)]
use std::fmt::{Debug, Display};

#[autogen::register]
struct Struct<X: Debug, Y: Debug> {
    x: X,
    y: Y,
}

#[autogen::apply]
impl Struct {}

trait Trait {
    fn type_of(&self) -> String;
}

#[test]
fn regular() {
    #[autogen::apply]
    impl Trait for Struct {
        fn type_of(&self) -> String {
            format!("regular {:?}, {:?}", self.x, self.y)
        }
    }

    let s = Struct { x: 3, y: 5.2 };
    assert_eq!(s.type_of(), "regular 3, 5.2");
}

#[test]
fn reference() {
    #[autogen::apply]
    impl Trait for &Struct {
        fn type_of(&self) -> String {
            format!("reference {:?}, {:?}", self.x, self.y)
        }
    }

    let s = Struct { x: 3, y: 5.2 };
    assert_eq!((&&s).type_of(), "reference 3, 5.2");
}

#[test]
fn array() {
    #[autogen::apply]
    impl Trait for [Struct; 1] {
        fn type_of(&self) -> String {
            format!("array {:?}, {:?}", self[0].x, self[0].y)
        }
    }

    let array = [Struct { x: 3, y: 5.2 }];
    assert_eq!(array.type_of(), "array 3, 5.2");
}

#[test]
fn slice() {
    #[autogen::apply]
    impl Trait for [Struct] {
        fn type_of(&self) -> String {
            format!("slice {:?}, {:?}", self[0].x, self[0].y)
        }
    }

    let array = [Struct { x: 3, y: 5.2 }];
    assert_eq!(array[..].type_of(), "slice 3, 5.2");
}

#[test]
fn pointer() {
    #[autogen::apply]
    impl Trait for *const Struct {
        fn type_of(&self) -> String {
            "pointer".to_string()
        }
    }

    let s = Struct { x: 3, y: 5.2 };
    let pointer = &s as *const Struct<_, _>;
    assert_eq!(pointer.type_of(), "pointer");
}

#[test]
fn tuple() {
    #[autogen::apply]
    impl Trait for (&'static str, Struct, Struct, Struct<f64, String>) {
        fn type_of(&self) -> String {
            format!(
                "tuple {}, {:?}, {:?}, {:?}, {:?}, {}, {}",
                self.0, self.1.x, self.1.y, self.2.x, self.2.y, self.3.x, self.3.y
            )
        }
    }

    let s1 = Struct { x: 3, y: 5.2 };
    let s2 = Struct { x: 4, y: 7.6 };
    let s3 = Struct {
        x: 4.2,
        y: "b".to_string(),
    };
    let tuple = ("a", s1, s2, s3);
    assert_eq!(tuple.type_of(), "tuple a, 3, 5.2, 4, 7.6, 4.2, b");
}

#[test]
fn generic_arg() {
    #[autogen::apply]
    impl Trait for Result<Struct, String> {
        fn type_of(&self) -> String {
            match self {
                Ok(s) => format!("ok {:?}, {:?}", s.x, s.y),
                Err(e) => format!("error {e}"),
            }
        }
    }

    let s = Struct { x: 3, y: 5.2 };
    let ok: Result<_, String> = Ok(s);
    let err: Result<Struct<i32, f64>, String> = Err("oops".to_string());
    assert_eq!(ok.type_of(), "ok 3, 5.2");
    assert_eq!(err.type_of(), "error oops");
}

#[test]
fn combo() {
    #[autogen::apply]
    impl Trait for [([Option<&Struct>; 1], Struct, String)] {
        fn type_of(&self) -> String {
            format!(
                "crazy {:?}, {:?}, {:?}, {:?} {}",
                self[0].0[0].unwrap().x,
                self[0].0[0].unwrap().y,
                self[0].1.x,
                self[0].1.y,
                self[0].2
            )
        }
    }
    let s1 = Struct { x: 3, y: 5.2 };
    let s2 = Struct { x: 4, y: 7.6 };
    let crazy = [([Some(&s1)], s2, "****".to_string())];
    assert_eq!(crazy[..].type_of(), "crazy 3, 5.2, 4, 7.6 ****");
}

#[test]
fn custom_id() {
    #[autogen::register(StructX)]
    struct Struct<X: Display> {
        x: X,
    }

    #[autogen::apply(StructX)]
    impl Trait for Struct {
        fn type_of(&self) -> String {
            format!("custom {}", self.x)
        }
    }
    let s = Struct { x: "x" };
    assert_eq!(s.type_of(), "custom x");
}

#[test]
fn custom_with_multiple_types() {
    #[autogen::register(StructY)]
    struct Struct<Y: Debug> {
        y: Y,
    }

    #[autogen::apply(id = StructY)]
    impl Trait for (Struct, String) {
        fn type_of(&self) -> String {
            format!("custom {:?} {}", self.0.y, self.1)
        }
    }
    let s = Struct { y: 0 };
    let tuple = (s, "y".to_string());
    assert_eq!(tuple.type_of(), "custom 0 y");
}

#[test]
fn custom_with_multiple_registered_types() {
    #[autogen::register]
    struct Struct2<Z: Display> {
        z: Z,
    }

    #[autogen::apply(Struct)]
    impl Trait for (Struct, Struct2<String>) {
        fn type_of(&self) -> String {
            format!("custom {:?} {:?} {}", self.0.x, self.0.y, self.1.z)
        }
    }
    let s1 = Struct { x: 1, y: 2 };
    let s2 = Struct2 { z: "z".to_string() };
    let tuple = (s1, s2);
    assert_eq!(tuple.type_of(), "custom 1 2 z");
}

#[test]
fn fn_arg() {
    #[autogen::apply]
    impl Struct {
        fn combine_as_string(&self, other: &Struct) -> String {
            format!("{:?} {:?} - {:?} {:?}", self.x, self.y, other.x, other.y)
        }
        fn combine<'a>(&'a self, other: &'a Struct) -> Vec<&'a Struct> {
            let v: Vec<&'a Struct> = [self, other].into_iter().collect();
            v
        }
    }

    let a = Struct { x: 1, y: 2 };
    let b = Struct { x: 3, y: 4 };
    let c = a.combine(&b);
    assert_eq!(a.combine_as_string(&b), "1 2 - 3 4");
    assert_eq!(c[0].x, a.x);
    assert_eq!(c[0].y, a.y);
    assert_eq!(c[1].x, b.x);
    assert_eq!(c[1].y, b.y);
}
