use std::sync::Mutex;

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::ToTokens;
use syn::{Error, Generics, Ident, Item, Result, parse_str, spanned::Spanned};

use crate::unique_vec::{UniqueEq, UniqueVec};

static GENERICS: Mutex<UniqueVec<GenericsItem>> = Mutex::new(UniqueVec::new());

struct GenericsItem {
    id: String,
    ident: String,
    generics: String,
    where_clause: Option<String>,
}

pub(crate) fn register_generics(item: &Item, custom_id: Option<Ident>) -> Result<TokenStream> {
    let mut generics: GenericsItem = item.try_into()?;
    if let Some(id) = custom_id {
        generics.id = id.to_string();
    }
    let mut generics_vec = GENERICS.lock().map_err(|_| {
        Error::new(
            Span::call_site(),
            "could not register generics because of previous errors",
        )
    })?;
    // NOTE: had to disable it for now because the memory is no longer cleared after each
    // rust-analyzer check, which causes duplicate registrations on every code change
    // match generics_vec.push(generics) {
    //     Some(gen) => {
    //         let mut error = Error::new(
    //             get_ident(item)?.span(),
    //             format!("{} is already registered", gen.id),
    //         );
    //         error.combine(Error::new(Span::call_site(), "use a different identifier"));
    //         Err(error)
    //     }
    //     None => Ok(item.to_token_stream().into()),
    // }
    generics_vec.push(generics);
    Ok(item.to_token_stream().into())
}

pub(crate) fn find_generics(ident: &Ident, id: &Ident) -> Result<Option<Generics>> {
    let generics_vec = GENERICS.lock().map_err(|_| {
        Error::new(
            Span::call_site(),
            "could not apply generics because of previous errors",
        )
    })?;
    let id = id.to_string();
    let ident = ident.to_string();
    Ok(generics_vec
        .iter()
        .find(|g| g.id == id && g.ident == ident)
        .map(|g| g.into()))
}

pub(crate) fn find_ident_by_id(id: &Ident) -> Option<String> {
    let generics_vec = GENERICS.lock().ok()?;
    let id = id.to_string();
    generics_vec
        .iter()
        .find(|g| g.id == id)
        .map(|g| g.ident.clone())
}

fn get_ident(item: &Item) -> Result<&Ident> {
    match item {
        Item::Enum(item_enum) => Ok(&item_enum.ident),
        Item::Struct(item_struct) => Ok(&item_struct.ident),
        _ => Err(Error::new(item.span(), "expected `struct` or `enum`")),
    }
}

fn get_generics(item: &Item) -> Result<&Generics> {
    match item {
        Item::Enum(item_enum) => Ok(&item_enum.generics),
        Item::Struct(item_struct) => Ok(&item_struct.generics),
        _ => Err(Error::new(item.span(), "expected `struct` or `enum`")),
    }
}

impl UniqueEq for GenericsItem {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id && self.ident == other.ident
    }
}

impl TryFrom<&Item> for GenericsItem {
    type Error = Error;

    fn try_from(item: &Item) -> Result<Self> {
        let ident = get_ident(item)?.to_string();
        let id = ident.clone();
        let generics = get_generics(item)?;
        let where_clause = generics
            .where_clause
            .as_ref()
            .map(|clause| clause.to_token_stream().to_string());
        let generics = generics.to_token_stream().to_string();
        Ok(GenericsItem {
            id,
            ident,
            generics,
            where_clause,
        })
    }
}

impl From<&GenericsItem> for Generics {
    fn from(item: &GenericsItem) -> Self {
        let mut generics: Generics =
            parse_str(&item.generics).expect("the parsed string must be a `Generics` token stream");
        let where_clause = item.where_clause.as_ref().map(|clause| {
            parse_str(clause).expect("the parsed string must be a `WhereClause` token stream")
        });
        generics.where_clause = where_clause;
        generics
    }
}
