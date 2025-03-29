#![allow(non_local_definitions)]
use std::{fmt::Display, pin::Pin};

#[autogen::register]
struct Struct<T: Display + Clone + PartialEq> {
    t: T,
    y: T,
}

#[autogen::apply]
impl Struct {
    fn result<E>(&self, _e: E) -> Result<Struct, E> {
        todo!()
    }
}

trait Def<T>
where
    Self: Sized,
{
    type Item;
    const DEF: Option<Self::Item>;

    fn get_def(self) -> Option<Self::Item> {
        Self::DEF
    }
}

trait S {
    type Struct;

    fn get(&self) -> &Self::Struct;
}

#[test]
fn trait_generic_arg_as_input() {
    #[autogen::apply]
    impl From<Struct> for String {
        fn from(val: Struct) -> Self {
            format!("{} as string", val.t)
        }
    }

    let s = Struct { t: "abc", y: "y" };
    let string: String = s.into();
    assert_eq!(string, "abc as string");
}

#[test]
fn trait_generic_arg_as_output() {
    #[autogen::apply]
    impl From<T> for Struct {
        fn from(val: T) -> Struct {
            let a: Struct = Struct {
                t: val.clone(),
                y: val,
            };
            a
        }
    }

    let string: String = "abc".to_string();
    let s: Struct<_> = string.into();
    assert_eq!(s.t, "abc");
}

#[test]
fn associated_type_and_const() {
    #[autogen::apply]
    impl Def<Struct> for Struct {
        type Item = Struct;
        const DEF: Option<Struct> = None;
    }

    #[autogen::apply(Struct2 = Struct)]
    impl S for Struct2 {
        type Struct = Struct2;

        fn get(&self) -> &Self::Struct {
            self
        }
    }

    let s = Struct { t: "t", y: "y" };
    assert_eq!(s.get().t, s.t);
    assert!(s.get_def().is_none());
}

#[test]
fn inner_fn() {
    #[autogen::apply]
    impl Def<Struct> for T {
        type Item = Struct;
        const DEF: Option<Struct> = None;

        fn get_def(self) -> Option<Struct> {
            #[autogen::apply]
            fn inner(t: T) -> Option<Struct> {
                let s: Struct = Struct { y: t.clone(), t };
                Some(s)
            }

            inner(self)
        }
    }

    let string = "abc".to_string();
    assert_eq!(string.get_def().unwrap().t, "abc");
}

#[test]
fn expressions() {
    #[autogen::apply]
    impl Iterator for Struct {
        type Item = Struct;

        fn next(&mut self) -> Option<Struct> {
            None
        }
    }

    #[autogen::apply]
    impl From<Box<dyn Iterator<Item = Struct>>> for Struct {
        fn from(value: Box<dyn Iterator<Item = Struct>>) -> Self {
            let from_str = Struct::from("abc".to_string());
            let _aa: String = test3(from_str);
            // MethodCall
            let _ = any::<Struct>().result::<Struct>(any::<Struct>());
            let vec: Vec<Struct> = value.collect::<Vec<Struct>>().into_iter().collect();
            // Index, Reference
            let s: &Struct = &vec[calc_index::<&Struct>(&vec.iter())];
            // Binary
            let mut x = calc_index::<&Struct>(&vec.iter()) + calc_index::<&Struct>(&vec.iter());
            test(s);
            // Loop
            let _ = loop {
                // If
                if x > 1 && s.t == any::<Struct>().t {
                    // Break
                    break Some(test2::<Struct>(s));
                } else {
                    // Field
                    let _ = test2::<Struct>(s).t;
                    // Assign
                    x = calc_index::<&Struct>(&vec.iter()) + calc_index::<&Struct>(&vec.iter());
                }
            };

            // Cast
            let _ = s as *const Struct;

            // Closure
            let _: Vec<_> = vec
                .iter()
                .map(|s: &Struct| -> &Struct {
                    if s.t == any::<&Struct>().y {
                        s
                    } else {
                        any::<&Struct>()
                    }
                })
                .collect();

            // Unsafe
            unsafe {
                let a = &any::<Struct>();
                Pin::new_unchecked(a);
            }

            // Const
            let _: Struct = const { any::<Struct>() };

            // Let
            if let Some(a) = any::<Option<Struct>>() {
                let _: Struct = a;
            }

            // ForLoop
            for i in any::<Vec<Struct>>() {
                let _: Struct = i;
            }

            // Match
            let _ = match any::<Option<Struct>>() {
                Some(some) => some,
                // Return
                None => return any::<Struct>(),
            };

            // Paren
            (any::<Struct>());

            // Range
            for _ in to_any::<Struct, i32>()..to_any::<Struct, i32>() {
                let x = to_any::<Struct, i32>();
                println!("{x}");
            }

            // Repeat
            let _ = [const { any::<Struct>() }; 2];

            // Struct
            let _ = Struct {
                t: any::<Struct>().t,
                ..any::<Struct>()
            };

            // Tuple
            let _ = (any::<Struct>(), any::<Option<Struct>>());

            // Unary
            let _ = !to_any::<Struct, i32>();

            // While
            while to_any::<Struct, i32>() < 3 {
                // Call
                let _ = any::<Struct>();
            }

            try_test::<T>();

            let _a = a1(s);

            // Array
            let array = [test2::<Struct>(s)];
            array.into_iter().next().unwrap()
        }
    }

    #[autogen::apply]
    fn try_test() -> Option<Struct> {
        // Try
        let val = any::<Option<Struct>>()?;
        Some(val)
    }

    #[autogen::apply]
    fn test(_value: &impl Iterator<Item = Struct>) -> Struct {
        todo!()
    }

    fn test2<T>(_value: &impl Iterator<Item = T>) -> T {
        todo!()
    }

    fn calc_index<T>(_value: &impl Iterator<Item = T>) -> usize {
        todo!()
    }

    const fn any<A>() -> A {
        todo!()
    }

    fn to_any<A, B>() -> B {
        any::<A>();
        todo!()
    }

    #[autogen::apply]
    fn test3<S: From<Struct>>(value: Struct) -> S {
        value.into()
    }

    #[autogen::apply]
    async fn a1(value: &impl Iterator<Item = Struct>) {
        // Async
        let a = async { any::<Struct>() };
        a.await;
        // Await
        a2::<String, T>(test2::<Struct>(value)).await;
    }

    #[autogen::apply]
    async fn a2<S: From<Struct>>(value: Struct) -> S {
        value.into()
    }
}
