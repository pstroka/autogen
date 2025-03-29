#![allow(non_local_definitions)]

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

// #[autogen::register]
// struct Lifetime<'a, 'b, X: ?Sized> {
//     x: &'a X,
//     y: &'b X,
// }

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
fn replace_mixed() {
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
// this will probably require to change the replacement type from Ident to Path or GenericArgument
// #[test]
// fn replace_with_args() {
//     #[autogen::apply(X = Vec<String>)]
//     impl Regular {}
// }

// TODO: make this work
// this has to work differently
// - X should not be removed from the list of generics but be replaced with X: PartialEq
// - X should not be replaced in the list of path arguments
// #[test]
// fn replace_with_bounds() {
//     #[autogen::apply(X = X: PartialEq)]
//     impl Regular {}
// }

// TODO: make this work
// #[test]
// fn replace_lifetime() {
//     // #[autogen::apply(X = str, _b = _a)]
//     impl<'a> Lifetime<'a, 'a, str> {
//         fn longer(&self) -> &'a str {
//             if self.x > self.y {
//                 self.x
//             } else {
//                 self.y
//             }
//         }
//     }
//
//     let x = "aaa";
//     let y = "aa";
//     let longer;
//     {
//         let l = Lifetime { x: &x, y: &y };
//         longer = l.longer();
//     }
//
//     assert_eq!(longer, x);
// }
