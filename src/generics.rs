use std::sync::Mutex;

use proc_macro::TokenStream;
use quote::ToTokens;
use syn::{parse_str, spanned::Spanned, Error, Generics, Ident, Item, Result};

use crate::unique_vec::{UniqueEq, UniqueVec};

static GENERICS: Mutex<UniqueVec<GenericsItem>> = Mutex::new(UniqueVec::new());

struct GenericsItem {
    ident: String,
    generics: String,
    where_clause: Option<String>,
}

pub(crate) fn register_generics(item: &Item, custom_id: Option<Ident>) -> Result<TokenStream> {
    let mut generics: GenericsItem = item.try_into()?;
    if let Some(id) = custom_id {
        generics.ident = id.to_string();
    }
    let mut generics_vec = GENERICS.lock().map_err(|_| {
        Error::new(
            item.span(),
            "could not register generics because of previous errors",
        )
    })?;
    match generics_vec.push(generics) {
        Some(gen) => Err(Error::new(
            get_ident(item)?.span(),
            format!(
                "{} is already registered; use a different identifier",
                gen.ident
            ),
        )),
        None => Ok(item.to_token_stream().into()),
    }
}

pub(crate) fn find_generics(ident: &Ident) -> Result<Option<Generics>> {
    let generics_vec = GENERICS.lock().map_err(|_| {
        Error::new(
            ident.span(),
            "could not apply generics because of previous errors",
        )
    })?;
    let name = ident.to_string();
    Ok(generics_vec
        .iter()
        .find(|gen| gen.ident == name)
        .map(|gen| gen.into()))
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
        self.ident == other.ident
    }
}

impl TryFrom<&Item> for GenericsItem {
    type Error = Error;

    fn try_from(item: &Item) -> Result<Self> {
        let ident = get_ident(item)?.to_string();
        let generics = get_generics(item)?;
        let where_clause = generics
            .where_clause
            .as_ref()
            .map(|clause| clause.to_token_stream().to_string());
        let generics = generics.to_token_stream().to_string();
        Ok(GenericsItem {
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
