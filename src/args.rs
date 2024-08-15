use std::fmt::Debug;

use syn::{
    punctuated::Punctuated, spanned::Spanned, token::Comma, Error, Expr, Ident, Meta,
    MetaNameValue, Path, PathSegment, Result,
};

use crate::unique_vec::UniqueVec;

pub(crate) struct Args {
    pub(crate) custom_id: Option<Ident>,
    replacements: UniqueVec<(Ident, Ident)>,
}

impl Args {
    pub(crate) fn ids<'a>(&'a self, ident: &'a Ident) -> (&'a Ident, &'a Ident) {
        let ident = self.get_replacement(ident).unwrap_or(ident);
        let id = self.custom_id.as_ref().unwrap_or(ident);
        (ident, id)
    }

    pub(crate) fn get_replacement(&self, ident: &Ident) -> Option<&Ident> {
        self.replacements
            .iter()
            .find(|(l, _)| l == ident)
            .map(|(_, r)| r)
    }

    pub(crate) fn replace_ident(&self, segment: &mut PathSegment) {
        if let Some(ident) = self.get_replacement(&segment.ident) {
            let mut ident = ident.to_owned();
            ident.set_span(segment.ident.span());
            segment.ident = ident;
        }
    }

    pub(crate) fn is_replaced(&self, ident: &Ident) -> bool {
        self.replacements.iter().any(|(_, r)| r == ident)
    }

    pub(crate) fn debug_panic<T, D: Debug>(
        &self,
        f: impl Fn(&Ident) -> Option<T>,
        m: impl Fn(T) -> D,
    ) {
        if let Some(debug) = self.replacements.iter().find(|rep| rep.0 == "debug") {
            if let Some(t) = f(&debug.1) {
                let message = m(t);
                panic!("{message:#?}")
            }
        }
    }
    // x => {
    //     args.debug_panic(
    //         // |_| Some(&x),
    //         |_| match_ok!(&x, Pat::Type(e)),
    //         |e| e.to_token_stream(),
    //     );
    //     vec![]
    // }
}

impl TryFrom<Punctuated<Meta, Comma>> for Args {
    type Error = Error;

    fn try_from(args: Punctuated<Meta, Comma>) -> Result<Self> {
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

fn try_parse_nv_idents(nv: MetaNameValue, error_message: &'static str) -> Result<(Ident, Ident)> {
    match nv.path.get_ident() {
        Some(lhs) => {
            let rhs = try_parse_expr_ident(nv.value, error_message)?;
            Ok((lhs.to_owned(), rhs))
        }
        None => Err(Error::new_spanned(nv.path, error_message)),
    }
}
