//! [![github]](https://github.com/pstroka/autogen)&ensp;[![crates-io]](https://crates.io/crates/autogen)&ensp;[![docs-rs]](crate)
//!
//! [github]: https://img.shields.io/badge/github-pstroka/autogen-8da0cb?style=for-the-badge&labelColor=555555&logo=github
//! [crates-io]: https://img.shields.io/crates/v/autogen.svg?style=for-the-badge&color=fc8d62&logo=rust
//! [docs-rs]: https://img.shields.io/badge/docs.rs-autogen-66c2a5?style=for-the-badge&labelColor=555555&logo=docs.rs
//!
//! # autogen
//!
//! Tired of repeating all the generics in every `impl` block?
//!
//! Autogen is a set of macros that allows you to automatically apply generics to `impl` blocks.
//! - the [register](macro@register) macro registers the generics of a `struct` or `enum`,
//! including lifetimes and the where clause.
//! - the [apply](macro@apply) macro applies the generics to an `impl` block.
//! ```
//! #[autogen::register]
//! struct Struct<'a, T, R: ?Sized>
//! where
//!     T: PartialEq,
//! {
//!     x: T,
//!     y: &'a R,
//! }
//!
//! #[autogen::apply]
//! impl Struct {}
//! ```
//! This will expand to:
//! ```
//! # #[autogen::register]
//! # struct Struct<'a, T, R: ?Sized>
//! # where
//! #     T: PartialEq,
//! # {
//! #     x: T,
//! #     y: &'a R,
//! # }
//! impl<'a, T, R: ?Sized> Struct<'a, T, R> where T: PartialEq {}
//! ```
//! For more examples see the [register](macro@register) and [apply](macro@apply) docs.

mod args;
mod generics;
mod unique_vec;

use args::Args;
use generics::{find_generics, find_ident_by_id, register_generics};
use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{ToTokens, TokenStreamExt};
use std::borrow::BorrowMut;
use syn::{
    parse2, parse_macro_input, punctuated::Punctuated, spanned::Spanned, token::Comma, Block,
    Error, Expr, FnArg, GenericArgument, Generics, Ident, ImplItem, ImplItemFn, Item, ItemFn,
    ItemImpl, Local, Meta, Pat, Path, PathArguments, Result, ReturnType, Signature, Stmt, Type,
    TypeTuple,
};
use try_match::match_ok;
use unique_vec::UniqueVec;

/// This macro is used to register the generics of a `struct` or `enum` that can later be applied to
/// an `impl` block with the [apply](macro@apply) macro.
///
/// # Examples
///
/// ```
/// #[autogen::register]
/// struct Struct<'a, T, R: ?Sized>
/// where
///     T: PartialEq,
/// {
///     x: T,
///     y: &'a R,
/// }
///
/// #[autogen::apply]
/// impl Struct {
///     fn x_equals(&self, other: &T) -> bool {
///         &self.x == other
///     }
///     fn y(&self) -> &R {
///         self.y
///     }
/// }
///
/// let s = Struct { x: 1, y: "abc" };
/// assert!(s.x_equals(&1));
/// assert_eq!(s.y(), "abc");
///
/// #[autogen::register]
/// enum Enum<T, Y> {
///     V1(T),
///     V2(Y),
/// }
///
/// #[autogen::apply]
/// impl Enum {
///     fn same_variant_as(&self, other: Self) -> bool {
///         match self {
///             Self::V1(_) => matches!(other, Self::V1(_)),
///             Self::V2(_) => matches!(other, Self::V2(_)),
///         }
///     }
/// }
///
/// let e1 = Enum::<&str, u32>::V1("a");
/// let e2 = Enum::<&str, u32>::V1("b");
/// let e3 = Enum::<&str, u32>::V2(0);
///
/// assert!(e1.same_variant_as(e2));
/// assert!(!e1.same_variant_as(e3));
///
/// ```
///
/// ## Using a custom identifier
///
/// By default, the generics are registered with the struct/enum name, but you can provide a
/// custom identifier. This can be useful if a type with the same name is already registered in
/// another module.
///
/// This will not compile because `Name` is already registered.
/// ```compile_fail
/// #[autogen::register]
/// struct Name<T: PartialEq> {
///     t: T,
/// }
///
/// mod sub {
///     #[autogen::register]
///     pub struct Name<S: FromStr> {
///         s: S,
///     }
/// }
/// ```
///
/// To resolve this, you must provide a custom identifier.
/// ```
/// #[autogen::register]
/// struct Name<T: PartialEq> {
///     t: T,
/// }
///
/// #[autogen::apply]
/// impl Name {
///     fn t(&self) -> &T {
///         &self.t
///     }
/// }
///
/// mod sub {
///     use std::str::FromStr;
///
///     #[autogen::register(CustomName)]
///     pub struct Name<S: FromStr> {
///         s: S,
///     }
///
///     #[autogen::apply(CustomName)]
///     impl Name {
///         pub fn parse(string: &str) -> Result<Self, S::Err> {
///             Ok(Name { s: string.parse()? })
///         }
///         pub fn s(&self) -> &S {
///             &self.s
///         }
///     }
/// }
///
/// let s1 = Name { t: 64 };
/// assert_eq!(s1.t(), &64);
///
/// let s2 = sub::Name::<u32>::parse("123").unwrap();
/// assert_eq!(s2.s(), &123);
/// ```
#[proc_macro_attribute]
pub fn register(args: TokenStream, original: TokenStream) -> TokenStream {
    let custom_id = if args.is_empty() {
        None
    } else {
        Some(parse_macro_input!(args as Ident))
    };

    let item = parse_macro_input!(original as Item);
    register_generics(&item, custom_id).unwrap_or_else(|err| err.to_compile_error().into())
}

/// TODO: add new examples
/// This macro is used to apply the generics of a `struct` or `enum` that have been registered with
/// the [register](macro@register) macro to an `impl` block.
///
/// # Examples
///
/// ```
/// #[autogen::register]
/// struct Struct<'a, T, R: ?Sized>
/// where
///     T: PartialEq,
/// {
///     x: T,
///     y: &'a R,
/// }
///
/// #[autogen::apply]
/// impl Struct {
///     fn x_equals(&self, other: &T) -> bool {
///         &self.x == other
///     }
///     fn y(&self) -> &'a R {
///         self.y
///     }
/// }
///
/// trait Trait {
///     fn type_of(&self) -> &'static str;
/// }
///
/// #[autogen::apply]
/// impl Trait for Struct {
///     fn type_of(&self) -> &'static str {
///         "regular"
///     }
/// }
///
/// #[autogen::apply]
/// impl Trait for [Struct; 2] {
///     fn type_of(&self) -> &'static str {
///         "array of size 2"
///     }
/// }
///
/// #[autogen::apply]
/// impl Trait for [Struct] {
///     fn type_of(&self) -> &'static str {
///         "slice"
///     }
/// }
///
/// #[autogen::apply]
/// impl Trait for &Struct {
///     fn type_of(&self) -> &'static str {
///         "reference"
///     }
/// }
///
/// #[autogen::apply]
/// impl Trait for *const Struct {
///     fn type_of(&self) -> &'static str {
///         "pointer"
///     }
/// }
///
/// #[autogen::apply]
/// impl Trait for (Struct, &'static str, Struct) {
///     fn type_of(&self) -> &'static str {
///         "tuple"
///     }
/// }
///
/// #[autogen::apply]
/// impl Trait for Result<Struct, String> {
///     fn type_of(&self) -> &'static str {
///         "generic argument"
///     }
/// }
///
/// #[autogen::apply]
/// impl Trait for [([Option<&Struct>; 1], Struct, String)] {
///     fn type_of(&self) -> &'static str {
///         "crazy ****"
///     }
/// }
///
/// let struct1 = Struct { x: 1, y: "abc" };
/// assert!(struct1.x_equals(&1));
/// assert_eq!(struct1.y(), "abc");
///
/// assert_eq!(struct1.type_of(), "regular");
/// assert_eq!((&&struct1).type_of(), "reference");
///
/// let pointer = &struct1 as *const Struct<'_, i32, str>;
/// assert_eq!(pointer.type_of(), "pointer");
///
/// let struct2 = Struct { x: 2, y: "xyz" };
/// assert!(struct2.x_equals(&2));
/// assert_eq!(struct2.y(), "xyz");
///
/// let tuple = (struct1, "string", struct2);
/// assert_eq!(tuple.type_of(), "tuple");
///
/// let array = [tuple.0, tuple.2];
/// assert_eq!(array.type_of(), "array of size 2");
/// assert_eq!(array[..].type_of(), "slice");
///
/// let struct3 = Struct { x: -1, y: "b" };
/// let result: Result<_, String> = Ok(struct3);
/// assert_eq!(result.type_of(), "generic argument");
///
/// let struct3 = result.unwrap();
/// let struct4 = Struct { x: -2, y: "s" };
/// let crazy = [([Some(&struct3)], struct4, "****".to_string())];
/// assert_eq!(crazy[..].type_of(), "crazy ****");
/// ```
/// The only restriction is that you cannot use different registered types in one `impl` block.
/// This will not compile:
/// ```compile_fail
/// #[autogen::register]
/// struct Struct1<X> {
///     x: X,
/// }
///
/// #[autogen::register]
/// struct Struct2<Y> {
///     y: Y,
/// }
///
/// trait Trait {}
///
/// #[autogen::apply]
/// impl Trait for (Struct1, Struct2) {}
/// ```
/// To learn how to apply generics with a custom identifier, see the
/// [register](macro@register#using-a-custom-identifier) macro docs.
#[proc_macro_attribute]
// TODO: replace generics e.g. T0 = Self, T0 = T1, X = Y, D = String
pub fn apply(args: TokenStream, original: TokenStream) -> TokenStream {
    let args = parse_macro_input!(args with Punctuated::<Meta, Comma>::parse_terminated);
    let args: Args = match args.try_into() {
        Ok(args) => args,
        Err(err) => return err.to_compile_error().into(),
    };

    let mut item = parse_macro_input!(original as Item);
    expand_item(item.borrow_mut(), &args)
}

type Results = Vec<Result<Generics>>;

fn expand_item(item: &mut Item, args: &Args) -> TokenStream {
    match item {
        Item::Fn(fn_item) => expand_fn(fn_item, args),
        Item::Impl(impl_item) => expand_impl(impl_item, args),
        _ => Error::new(item.span(), "expected `impl` or `fn`")
            .to_compile_error()
            .into(),
    }
}

fn expand_fn(item: &mut ItemFn, args: &Args) -> TokenStream {
    let mut results = expand_fn_signature(item.sig.borrow_mut(), args);
    results.append(&mut expand_block(item.block.borrow_mut(), args));
    match combine_results(results, item, args) {
        Ok(generics) => {
            item.sig.generics = generics;
            item.to_token_stream().into()
        }
        Err(err) => err.to_compile_error().into(),
    }
}

fn expand_impl(item: &mut ItemImpl, args: &Args) -> TokenStream {
    let mut results = expand_type(item.self_ty.as_mut(), args);
    results.append(expand_impl_items(item, args).borrow_mut());
    if let Some((_, path, _)) = item.trait_.borrow_mut() {
        results.append(expand_generic_args(path, args).borrow_mut());
    }
    match combine_results(results, item, args) {
        Ok(generics) => {
            item.generics = generics;
            item.to_token_stream().into()
        }
        Err(err) => err.to_compile_error().into(),
    }
}

fn combine_results(results: Results, item: &impl ToTokens, args: &Args) -> Result<Generics> {
    let (ok, err): (Vec<_>, Vec<_>) = results.into_iter().partition(|result| result.is_ok());
    let error = err
        .into_iter()
        .filter_map(|result| result.err())
        .reduce(|mut l, r| {
            l.combine(r);
            l
        });
    if let Some(error) = error {
        return Err(error);
    }

    let mut unique_results: UniqueVec<_> =
        ok.into_iter().filter_map(|result| result.ok()).collect();

    if unique_results.len() > 1 {
        let mut error = Error::new_spanned(
            item,
            "applying generics to different registered types is not supported",
        );
        error.combine(Error::new(Span::call_site(), "specify which type to use"));
        Err(error)
    } else {
        unique_results.pop().ok_or_else(|| {
            args.custom_id.as_ref().map_or_else(
                || Error::new_spanned(item, "no registered type found"),
                |id| match find_ident_by_id(id) {
                    Some(ident) => Error::new_spanned(item, format!("{ident} not found")),
                    None => Error::new(Span::call_site(), format!("{id} is not registered")),
                },
            )
        })
    }
}

fn expand_impl_items(item: &mut ItemImpl, args: &Args) -> Results {
    item.items
        .iter_mut()
        .flat_map(|impl_item| expand_impl_item(impl_item, args))
        .collect()
}

fn expand_impl_item(impl_item: &mut ImplItem, args: &Args) -> Results {
    match impl_item {
        ImplItem::Const(const_item) => expand_type(const_item.ty.borrow_mut(), args),
        ImplItem::Fn(fn_item) => expand_impl_fn(fn_item, args),
        ImplItem::Type(type_item) => expand_type(type_item.ty.borrow_mut(), args),
        _ => vec![],
    }
}

fn expand_impl_fn(fn_item: &mut ImplItemFn, args: &Args) -> Results {
    let mut results: Vec<_> = expand_fn_signature(fn_item.sig.borrow_mut(), args);
    results.append(&mut expand_block(fn_item.block.borrow_mut(), args));
    results
}

fn expand_fn_signature(sig: &mut Signature, args: &Args) -> Results {
    let mut results: Vec<_> = sig
        .inputs
        .iter_mut()
        .filter_map(|input| match_ok!(input, FnArg::Typed(typed) => typed.ty.borrow_mut()))
        .flat_map(|ty| expand_type(ty, args))
        .collect();

    if let ReturnType::Type(_, ty) = sig.output.borrow_mut() {
        results.append(expand_type(ty.as_mut(), args).borrow_mut());
    }

    results
}

fn expand_block(block: &mut Block, args: &Args) -> Results {
    block
        .stmts
        .iter_mut()
        .flat_map(|s| match s {
            Stmt::Local(loc) => expand_local(loc, args),
            Stmt::Expr(expr, _) => expand_expr(expr, args),
            _ => vec![],
        })
        .collect()
}

fn expand_expr(expr: &mut Expr, args: &Args) -> Results {
    // TODO: check which ones must be implemented
    match expr {
        // Expr::Array(_) => todo!(),
        // Expr::Assign(_) => todo!(),
        // Expr::Async(_) => todo!(),
        // Expr::Await(_) => todo!(),
        // Expr::Binary(_) => todo!(),
        Expr::Block(block) => expand_block(block.block.borrow_mut(), args),
        // Expr::Break(_) => todo!(),
        // Expr::Call(_) => todo!(),
        // Expr::Cast(_) => todo!(),
        // Expr::Closure(_) => todo!(),
        // Expr::Const(_) => todo!(),
        // Expr::Continue(_) => todo!(),
        // Expr::Field(_) => todo!(),
        // Expr::ForLoop(_) => todo!(),
        // Expr::Group(_) => todo!(),
        // Expr::If(_) => todo!(),
        // Expr::Index(_) => todo!(),
        // Expr::Infer(_) => todo!(),
        // Expr::Let(_) => todo!(),
        // Expr::Lit(_) => todo!(),
        // Expr::Loop(_) => todo!(),
        // Expr::Macro(_) => todo!(),
        // Expr::Match(_) => todo!(),
        // Expr::MethodCall(_) => todo!(),
        // Expr::Paren(_) => todo!(),
        // Expr::Path(_) => todo!(),
        // Expr::Range(_) => todo!(),
        // Expr::Reference(_) => todo!(),
        // Expr::Repeat(_) => todo!(),
        // Expr::Return(_) => todo!(),
        // Expr::Struct(_) => todo!(),
        // Expr::Try(_) => todo!(),
        // Expr::TryBlock(_) => todo!(),
        // Expr::Tuple(_) => todo!(),
        // Expr::Unary(_) => todo!(),
        // Expr::Unsafe(_) => todo!(),
        // Expr::Verbatim(_) => todo!(),
        // Expr::While(_) => todo!(),
        // Expr::Yield(_) => todo!(),
        _ => vec![],
    }
}

fn expand_local(loc: &mut Local, args: &Args) -> Results {
    match loc.pat.borrow_mut() {
        Pat::Type(ty) => expand_type(ty.ty.borrow_mut(), args),
        _ => vec![],
    }
}

fn expand_type(ty: &mut Type, args: &Args) -> Results {
    match ty {
        Type::Array(ty) => expand_type(ty.elem.as_mut(), args),
        Type::Path(ty) => expand_path(ty.path.borrow_mut(), args),
        Type::Ptr(ty) => expand_type(ty.elem.as_mut(), args),
        Type::Reference(ty) => expand_type(ty.elem.as_mut(), args),
        Type::Slice(ty) => expand_type(ty.elem.as_mut(), args),
        Type::Tuple(ty) => expand_tuple(ty, args),
        _ => vec![],
    }
}

fn expand_path(path: &mut Path, args: &Args) -> Results {
    let mut vec = expand_generic_args(path.borrow_mut(), args);
    let segment = path
        .segments
        .last_mut()
        .expect("a path has to have at least one segment");
    let id = args.custom_id.as_ref().unwrap_or(&segment.ident);
    if let Some(result) = find_generics(&segment.ident, id).transpose() {
        if let Ok(generics) = result {
            let ty_generics = generics.split_for_impl().1;
            let mut arguments = segment.arguments.to_token_stream();
            arguments.append_all(ty_generics.to_token_stream());
            // if it cannot be parsed, it usually means that the type has manually set generics
            if let Ok(arguments) = parse2(arguments) {
                segment.arguments = PathArguments::AngleBracketed(arguments);
                vec.push(Ok(generics));
            }
        } else {
            vec.push(result);
        }
    }
    vec
}

fn expand_generic_args(path: &mut Path, args: &Args) -> Results {
    path.segments
        .iter_mut()
        .filter_map(|seg| match_ok!(&mut seg.arguments, PathArguments::AngleBracketed(args)))
        .flat_map(|args| &mut args.args)
        .filter_map(|arg| match_ok!(arg, GenericArgument::Type(ty)))
        .flat_map(|ty| expand_type(ty, args))
        .collect()
}

fn expand_tuple(ty: &mut TypeTuple, args: &Args) -> Results {
    ty.elems
        .iter_mut()
        .flat_map(|elem| expand_type(elem, args))
        .collect()
}

#[cfg(doc)]
mod readme_tests;
