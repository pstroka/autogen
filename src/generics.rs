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

impl GenericsItem {
    fn as_generics(&self) -> Generics {
        let mut generics: Generics =
            parse_str(&self.generics).expect("the parsed string must be a `Generics` token stream");
        let where_clause = self.where_clause.as_ref().map(|clause| {
            parse_str(clause).expect("the parsed string must be a `WhereClause` token stream")
        });
        generics.where_clause = where_clause;
        generics
    }
}

impl UniqueEq for GenericsItem {
    fn eq(&self, other: &Self) -> bool {
        self.ident == other.ident
    }
}

impl TryFrom<Item> for GenericsItem {
    type Error = Error;

    fn try_from(item: Item) -> Result<Self> {
        let (ident, generics) = match item {
            Item::Enum(item_enum) => (item_enum.ident, item_enum.generics),
            Item::Struct(item_struct) => (item_struct.ident, item_struct.generics),
            _ => return Err(Error::new(item.span(), "expected `struct` or `enum`")),
        };
        let ident = ident.to_string();
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

pub(crate) fn register_generics(item: Item, custom_id: Option<Ident>) -> Result<TokenStream> {
    let mut generics: GenericsItem = item.clone().try_into()?;
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
            item.span(),
            format!(
                "{} is already registered; use a different identifier",
                gen.ident
            ),
        )),
        None => Ok(item.to_token_stream().into()),
    }
}

pub(crate) fn find_generics(ident: &Ident) -> Result<Generics> {
    let generics_vec = GENERICS.lock().map_err(|_| {
        Error::new(
            ident.span(),
            "could not apply generics because of previous errors",
        )
    })?;
    let name = ident.to_string();
    generics_vec
        .iter()
        .find(|gen| gen.ident == name)
        .map(|gen| gen.as_generics())
        .ok_or_else(|| Error::new(ident.span(), format!("{name} is not registered")))
}
