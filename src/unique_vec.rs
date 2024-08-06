use quote::ToTokens;
use syn::{Generics, Ident};

pub(crate) struct UniqueVec<T: UniqueEq>(Vec<T>);

impl<T: UniqueEq> UniqueVec<T> {
    pub(crate) const fn new() -> Self {
        UniqueVec(vec![])
    }

    pub(crate) fn push(&mut self, value: T) -> Option<&T> {
        match self.0.iter().position(|elem| elem.eq(&value)) {
            Some(index) => self.0.get(index),
            None => {
                self.0.push(value);
                None
            }
        }
    }

    pub(crate) fn iter(&self) -> std::slice::Iter<'_, T> {
        self.0.iter()
    }

    pub(crate) fn pop(&mut self) -> Option<T> {
        self.0.pop()
    }

    pub(crate) fn len(&self) -> usize {
        self.0.len()
    }
}

impl<T: UniqueEq, I: Iterator<Item = T>> From<I> for UniqueVec<T> {
    fn from(iter: I) -> Self {
        let mut vec = UniqueVec::new();
        iter.for_each(|elem| {
            vec.push(elem);
        });
        vec
    }
}

impl<T: UniqueEq> FromIterator<T> for UniqueVec<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        iter.into_iter().into()
    }
}

pub(crate) trait UniqueEq<Rhs: ?Sized = Self> {
    fn eq(&self, other: &Rhs) -> bool;
}

impl UniqueEq for Generics {
    fn eq(&self, other: &Self) -> bool {
        self.to_token_stream().to_string() == other.to_token_stream().to_string()
    }
}

impl UniqueEq for Ident {
    fn eq(&self, other: &Self) -> bool {
        self == other
    }
}

impl UniqueEq for (Ident, Ident) {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}
