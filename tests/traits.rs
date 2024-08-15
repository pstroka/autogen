use std::fmt::Display;

#[autogen::register]
struct Struct<T: Display> {
    t: T,
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

    let s = Struct { t: "t" };
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
                let s: Struct = Struct { t };
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

    #[autogen::apply(debug = test)]
    impl From<Box<dyn Iterator<Item = Struct>>> for Struct {
        fn from(value: Box<dyn Iterator<Item = Struct>>) -> Self {
            let from_str = Struct::from("abc".to_string());
            let _aa: String = test3(from_str);
            let vec = value.collect::<Vec<Struct>>();
            let s: &Struct = &vec[calc_index::<&Struct>(&vec.iter())];
            test(s);
            test2::<Struct>(s)
        }
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

    #[autogen::apply]
    fn test3<S: From<Struct>>(value: Struct) -> S {
        value.into()
    }
}
