use syn::{
    punctuated::Punctuated, spanned::Spanned, token::Comma, AngleBracketedGenericArguments, Error,
    Expr, GenericArgument, Ident, Meta, MetaNameValue, Path, Result,
};
use try_match::match_ok;

use crate::{
    replacement::{Replaceable, Replacement},
    unique_vec::UniqueVec,
};

pub(crate) struct Args {
    pub(crate) custom_id: Option<Ident>,
    replacements: UniqueVec<Replacement>,
}

impl Args {
    pub(crate) fn replace_arguments(&self, arguments: &mut AngleBracketedGenericArguments) {
        arguments
            .args
            .iter_mut()
            .filter_map(match_ok!(, GenericArgument::Type(ty)))
            .for_each(|ty| self.replace(ty))
    }

    pub(crate) fn replace(&self, replaceable: &mut impl Replaceable) {
        if let Some(replacement) = self.get_replacement(replaceable) {
            replaceable.replace_with(replacement.to_owned());
        }
    }

    pub(crate) fn is_replaced(&self, replaceable: &impl Replaceable) -> bool {
        self.replacements
            .iter()
            .any(|r| replaceable.is_replaced_with(r))
    }

    pub(crate) fn is_replacement(&self, replaceable: &impl Replaceable) -> bool {
        self.replacements
            .iter()
            .any(|r| replaceable.is_replacement_for(r))
    }

    fn get_replacement(&self, replaceable: &impl Replaceable) -> Option<&Ident> {
        self.replacements
            .iter()
            .find_map(|r| replaceable.get_replacement(r))
    }
}

impl TryFrom<Punctuated<Meta, Comma>> for Args {
    type Error = Error;

    fn try_from(args: Punctuated<Meta, Comma>) -> Result<Self> {
        fn try_parse_path_ident(path: Path, error_message: &'static str) -> Result<Ident> {
            match path.get_ident() {
                Some(ident) => Ok(ident.to_owned()),
                None => Err(Error::new_spanned(path, error_message)),
            }
        }

        fn try_parse_expr_ident(expr: Expr, error_message: &'static str) -> Result<Ident> {
            match expr {
                Expr::Path(p) => match p.path.get_ident() {
                    Some(ident) => Ok(ident.to_owned()),
                    None => Err(Error::new_spanned(p.path, error_message)),
                },
                _ => Err(Error::new(expr.span(), error_message)),
            }
        }

        fn try_parse_nv_idents(
            nv: MetaNameValue,
            error_message: &'static str,
        ) -> Result<Replacement> {
            match nv.path.get_ident() {
                Some(lhs) => {
                    let rhs = try_parse_expr_ident(nv.value, error_message)?;
                    Ok(Replacement::new(lhs.to_owned(), rhs))
                }
                None => Err(Error::new_spanned(nv.path, error_message)),
            }
        }

        let mut custom_ids = UniqueVec::new();
        let mut replacements = UniqueVec::new();
        for meta in args.into_iter() {
            match meta {
                Meta::Path(p) => {
                    custom_ids.push(try_parse_path_ident(
                        p,
                        "expected a custom identifier or a generic type replacemet",
                    )?);
                }
                Meta::List(l) => {
                    return Err(Error::new(
                        l.span(),
                        "expected a custom identifier or a generic type replacemet",
                    ))
                }
                Meta::NameValue(id_nv) if id_nv.path.is_ident("id") => {
                    let ident = try_parse_expr_ident(id_nv.value, "expected a custom identifier")?;
                    custom_ids.push(ident);
                }
                Meta::NameValue(rep_nv) => {
                    // TODO: error if duplicated?
                    replacements.push(try_parse_nv_idents(rep_nv, "expected a generic type")?);
                }
            }
        }
        if custom_ids.len() > 1 {
            return Err(custom_ids
                .iter()
                .map(|ident| {
                    Error::new_spanned(ident, "only one custom identifier can be specified")
                })
                .reduce(|mut l, r| {
                    l.combine(r);
                    l
                })
                .unwrap());
        }
        Ok(Args {
            custom_id: custom_ids.pop(),
            replacements,
        })
    }
}
