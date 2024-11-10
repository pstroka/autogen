use syn::{GenericParam, Ident, Type, WherePredicate};
use try_match::match_ok;

use crate::unique_vec::UniqueEq;

pub(crate) struct Replacement {
    replaced: Ident,
    replacement: Ident,
}

impl Replacement {
    pub(crate) fn new(replaced: Ident, replacement: Ident) -> Self {
        Replacement {
            replaced,
            replacement,
        }
    }
}

impl UniqueEq for Replacement {
    fn eq(&self, other: &Self) -> bool {
        self.replaced == other.replaced
    }
}

pub(crate) trait Replaceable {
    fn is_replaced_with(&self, replacement: &Replacement) -> bool {
        match self.get_ident() {
            Some(ident) => ident == &replacement.replaced,
            None => false,
        }
    }
    fn is_replacement_for(&self, replacement: &Replacement) -> bool {
        match self.get_ident() {
            Some(ident) => ident == &replacement.replacement,
            None => false,
        }
    }
    fn get_replacement<'a>(&self, replacement: &'a Replacement) -> Option<&'a Ident> {
        if self.is_replaced_with(replacement) {
            Some(&replacement.replacement)
        } else {
            None
        }
    }
    fn get_ident(&self) -> Option<&Ident>;
    fn replace_with(&mut self, ident: Ident);
}

impl Replaceable for Ident {
    fn get_ident(&self) -> Option<&Ident> {
        Some(self)
    }

    fn replace_with(&mut self, ident: Ident) {
        let span = self.span();
        *self = ident;
        self.set_span(span)
    }
}

impl Replaceable for GenericParam {
    fn get_ident(&self) -> Option<&Ident> {
        if let GenericParam::Type(ty) = self {
            Some(&ty.ident)
        } else {
            None
        }
    }

    fn replace_with(&mut self, ident: Ident) {
        if let GenericParam::Type(ty) = self {
            ty.ident.replace_with(ident);
        }
    }
}

impl Replaceable for Type {
    fn get_ident(&self) -> Option<&Ident> {
        match_ok!(self, Type::Path(path))
            .and_then(|path| path.path.segments.last())
            .and_then(|segment| segment.ident.get_ident())
    }

    fn replace_with(&mut self, ident: Ident) {
        if let Some(segment) =
            match_ok!(self, Type::Path(path)).and_then(|path| path.path.segments.last_mut())
        {
            segment.ident.replace_with(ident)
        }
    }
}

impl Replaceable for WherePredicate {
    fn get_ident(&self) -> Option<&Ident> {
        match_ok!(self, WherePredicate::Type(ty)).and_then(|ty| ty.bounded_ty.get_ident())
    }

    fn replace_with(&mut self, ident: Ident) {
        if let Some(ty) = match_ok!(self, WherePredicate::Type(ty)) {
            ty.bounded_ty.replace_with(ident)
        }
    }
}
