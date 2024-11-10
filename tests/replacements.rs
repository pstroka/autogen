#[autogen::register]
struct Regular<X, Y> {
    x: X,
    y: Y,
}

#[autogen::register]
struct Mixed<X, Y: Clone>
where
    X: PartialEq + Clone,
{
    x: X,
    y: Y,
}

#[autogen::register]
struct Where<X, Y>
where
    X: PartialEq,
    Y: PartialEq,
{
    x: X,
    y: Y,
}

#[test]
fn replace_regular() {
    #[autogen::apply(X = String)]
    impl Regular {
        fn x_string(x: X, y: Y) -> Regular {
            Regular { x, y }
        }
    }

    let r = Regular::x_string("X".to_string(), 5.2);
    assert_eq!(r.x, "X");
    assert_eq!(r.y, 5.2);
}

#[test]
fn replaceme_mixed() {
    #[autogen::apply(Y = X)]
    impl Mixed {
        fn equal(&self) -> bool {
            self.x == self.y
        }
    }

    let s = Mixed { x: 5, y: 5 };
    assert!(s.equal());
}

#[test]
fn replace_where() {
    #[autogen::apply(X = Y)]
    impl Where {
        fn equal(&self) -> bool {
            self.x == self.y
        }
    }

    let w = Where { x: 123, y: 123 };
    assert!(w.equal());

    #[autogen::apply(Y = String)]
    impl Where {
        fn y(&self) -> &str {
            &self.y
        }
    }

    let w = Where {
        x: 123,
        y: "Y".to_string(),
    };
    assert_eq!(w.y(), "Y");
}

// TODO: make this work
// #[test]
// fn replace_with_args() {
//     #[autogen::apply(X = Vec<String>)]
//     impl Regular {}
// }

// TODO: make this work
// #[test]
// fn replace_with_bounds() {
//     #[autogen::apply(X = X: PartialEq)]
//     impl Regular {}
// }

// TODO: replace lifetime generics
