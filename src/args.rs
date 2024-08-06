use syn::{
    punctuated::Punctuated, spanned::Spanned, token::Comma, Error, Expr, Ident, Meta,
    MetaNameValue, Path, Result,
};

use crate::unique_vec::UniqueVec;

pub(crate) struct Args {
    pub(crate) custom_id: Option<Ident>,
    replacements: UniqueVec<(Ident, Ident)>,
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
