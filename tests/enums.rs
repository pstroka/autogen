use std::fmt::Display;

#[autogen::register]
enum Enum<X, Y>
where
    X: Display,
    Y: Display,
{
    V1(X),
    V2(Y),
}

#[autogen::apply]
impl Enum {
    fn all(x: X, y: Y) -> (Self, Self) {
        (Enum::V1(x), Enum::V2(y))
    }
}

trait Trait {
    fn type_of(&self) -> String;
}

#[test]
fn variants() {
    #[autogen::apply]
    impl Trait for Enum {
        fn type_of(&self) -> String {
            match self {
                Enum::V1(x) => format!("v1: {x}"),
                Enum::V2(y) => format!("v2: {y}"),
            }
        }
    }

    let (v1, v2) = Enum::all("abc", 123);
    assert_eq!(v1.type_of(), "v1: abc".to_string());
    assert_eq!(v2.type_of(), "v2: 123".to_string());
}
