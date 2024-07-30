use std::fmt::Debug;

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
    assert_eq!(s.type_of(), "regular 3, 5.2".to_string());
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
    assert_eq!((&&s).type_of(), "reference 3, 5.2".to_string());
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
    assert_eq!(array.type_of(), "array 3, 5.2".to_string());
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
    assert_eq!(array[..].type_of(), "slice 3, 5.2".to_string());
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
    assert_eq!(pointer.type_of(), "pointer".to_string());
}

#[test]
fn tuple() {
    #[autogen::apply]
    impl Trait for (&'static str, Struct, Struct) {
        fn type_of(&self) -> String {
            format!(
                "tuple {}, {:?}, {:?}, {:?}, {:?}",
                self.0, self.1.x, self.1.y, self.2.x, self.2.y
            )
        }
    }

    let s1 = Struct { x: 3, y: 5.2 };
    let s2 = Struct { x: 4, y: 7.6 };
    let tuple = ("a", s1, s2);
    assert_eq!(tuple.type_of(), "tuple a, 3, 5.2, 4, 7.6".to_string());
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
    assert_eq!(ok.type_of(), "ok 3, 5.2".to_string());
    assert_eq!(err.type_of(), "error oops".to_string());
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
