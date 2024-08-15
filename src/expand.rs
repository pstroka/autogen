use std::{borrow::BorrowMut, ops::DerefMut};

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{ToTokens, TokenStreamExt};
use syn::{
    parse2,
    punctuated::Punctuated,
    spanned::Spanned,
    token::{Else, For, Not},
    AngleBracketedGenericArguments, BareFnArg, Block, Error, Expr, FnArg, GenericArgument,
    GenericParam, Generics, ImplItem, ImplItemFn, Item, ItemFn, ItemImpl, LocalInit, Pat, Path,
    PathArguments, PathSegment, Result, ReturnType, Signature, Stmt, Type, TypeParamBound,
    WhereClause, WherePredicate,
};
use try_match::match_ok;

use crate::{
    args::Args,
    generics::{find_generics, find_ident_by_id},
    unique_vec::UniqueVec,
};

macro_rules! expand {
    ($args:expr, $first:expr, $($x:expr),*) => {
        {
            let mut results = $first.expand($args);
            $(
                results.append($x.expand($args).borrow_mut());
            )*
            results
        }
    };
}

type Results = Vec<Result<Generics>>;

pub(crate) fn expand_item(item: Item, args: Args) -> TokenStream {
    match item {
        Item::Fn(mut item) => item.expand_item(&args),
        Item::Impl(mut item) => item.expand_item(&args),
        _ => Error::new(item.span(), "expected `impl` or `fn`")
            .to_compile_error()
            .into(),
    }
}

trait ExpandItem: Expand + ToTokens {
    fn expand_item(&mut self, args: &Args) -> TokenStream {
        let results = self.expand(args);
        match self.combine_results(results, args) {
            Ok(generics) => {
                self.merge(generics);
                self.to_token_stream().into()
            }
            Err(err) => err.to_compile_error().into(),
        }
    }

    fn combine_results(&self, results: Results, args: &Args) -> Result<Generics> {
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
                self,
                "applying generics to different registered types is not supported",
            );
            error.combine(Error::new(Span::call_site(), "specify which type to use"));
            Err(error)
        } else {
            unique_results.pop().ok_or_else(|| {
                args.custom_id.as_ref().map_or_else(
                    || Error::new_spanned(self, "no registered type found"),
                    |id| match find_ident_by_id(id) {
                        Some(ident) => Error::new_spanned(self, format!("{ident} not found")),
                        None => Error::new(Span::call_site(), format!("{id} is not registered")),
                    },
                )
            })
        }
    }

    fn merge(&mut self, generics: Generics) {
        let original = self.generics();
        generics
            .params
            .into_iter()
            .for_each(|param| original.params.push(param));
        match original.where_clause.as_mut() {
            Some(original_clause) => {
                if let Some(clause) = generics.where_clause {
                    clause
                        .predicates
                        .into_iter()
                        .for_each(|pred| original_clause.predicates.push(pred))
                }
            }
            None => original.where_clause = generics.where_clause,
        }
    }

    fn generics(&mut self) -> &mut Generics;
}

impl ExpandItem for ItemImpl {
    fn generics(&mut self) -> &mut Generics {
        self.generics.borrow_mut()
    }
}

impl ExpandItem for ItemFn {
    fn generics(&mut self) -> &mut Generics {
        self.sig.generics.borrow_mut()
    }
}

trait Expand {
    fn expand(&mut self, args: &Args) -> Results;
}

impl Expand for ItemImpl {
    fn expand(&mut self, args: &Args) -> Results {
        expand!(args, self.self_ty, self.items, self.trait_, self.generics)
    }
}

impl Expand for ItemFn {
    fn expand(&mut self, args: &Args) -> Results {
        expand!(args, self.sig, self.block)
    }
}

impl Expand for Generics {
    fn expand(&mut self, args: &Args) -> Results {
        expand!(args, self.params, self.where_clause)
    }
}

impl Expand for GenericParam {
    fn expand(&mut self, args: &Args) -> Results {
        match self {
            GenericParam::Lifetime(_) => vec![],
            GenericParam::Type(param) => expand!(args, param.bounds, param.default),
            GenericParam::Const(param) => expand!(args, param.ty, param.default),
        }
    }
}

impl Expand for WhereClause {
    fn expand(&mut self, args: &Args) -> Results {
        self.predicates.expand(args)
    }
}

impl Expand for WherePredicate {
    fn expand(&mut self, args: &Args) -> Results {
        match_ok!(self, WherePredicate::Type(pred) => pred.bounds.borrow_mut()).expand(args)
    }
}

type TraitDef = (Option<Not>, Path, For);
impl Expand for TraitDef {
    fn expand(&mut self, args: &Args) -> Results {
        self.1.expand(args)
    }
}

impl Expand for ImplItem {
    fn expand(&mut self, args: &Args) -> Results {
        match self {
            ImplItem::Const(item) => expand!(args, item.ty, item.expr, item.generics),
            ImplItem::Fn(item) => item.expand(args),
            ImplItem::Type(item) => expand!(args, item.ty, item.generics),
            _ => vec![],
        }
    }
}

impl Expand for ImplItemFn {
    fn expand(&mut self, args: &Args) -> Results {
        expand!(args, self.sig, self.block)
    }
}

impl Expand for Signature {
    fn expand(&mut self, args: &Args) -> Results {
        expand!(args, self.inputs, self.output, self.generics)
    }
}

impl Expand for FnArg {
    fn expand(&mut self, args: &Args) -> Results {
        match_ok!(self, FnArg::Typed(arg) => arg.ty.as_mut()).expand(args)
    }
}

impl Expand for BareFnArg {
    fn expand(&mut self, args: &Args) -> Results {
        self.ty.expand(args)
    }
}

impl Expand for ReturnType {
    fn expand(&mut self, args: &Args) -> Results {
        match self {
            ReturnType::Default => vec![],
            ReturnType::Type(_, ty) => ty.expand(args),
        }
    }
}

impl Expand for Block {
    fn expand(&mut self, args: &Args) -> Results {
        self.stmts.expand(args)
    }
}

impl Expand for Stmt {
    fn expand(&mut self, args: &Args) -> Results {
        match self {
            Stmt::Local(loc) => expand!(args, loc.pat, loc.init),
            Stmt::Expr(expr, _) => expr.expand(args),
            _ => vec![],
        }
    }
}

impl Expand for Pat {
    fn expand(&mut self, args: &Args) -> Results {
        // TODO: check if the other variants are needed
        match self {
            Pat::Const(pat) => pat.block.expand(args),
            Pat::Or(pat) => pat.cases.expand(args),
            Pat::Paren(pat) => pat.pat.expand(args),
            Pat::Type(pat) => expand!(args, pat.ty, pat.pat),
            _ => vec![],
        }
    }
}

impl Expand for LocalInit {
    fn expand(&mut self, args: &Args) -> Results {
        expand!(args, self.expr, self.diverge)
    }
}

type ElseDef = (Else, Box<Expr>);
impl Expand for ElseDef {
    fn expand(&mut self, args: &Args) -> Results {
        self.1.expand(args)
    }
}

impl Expand for Expr {
    fn expand(&mut self, args: &Args) -> Results {
        // TODO: check which ones must be implemented
        match self {
            // Expr::Array(expr) => expr, X
            // Expr::Assign(expr) => expr, ?
            Expr::Async(expr) => expr.block.expand(args),
            // Expr::Await(expr) => expr, ?
            // Expr::Binary(expr) => expr, ?
            Expr::Block(expr) => expr.block.expand(args),
            // Expr::Break(expr) => expr, ?
            Expr::Call(expr) => expr.func.expand(args),
            Expr::Cast(expr) => expr.ty.expand(args),
            Expr::Closure(expr) => expand!(args, expr.inputs, expr.output, expr.body),
            // Expr::Const(expr) => expr,
            // Expr::Continue(expr) => expr,
            // Expr::Field(expr) => expr,
            // Expr::ForLoop(expr) => expr,
            Expr::Group(expr) => expr.expr.expand(args),
            // Expr::If(expr) => expr,
            Expr::Index(expr) => expand!(args, expr.index, expr.expr),
            // Expr::Infer(expr) => expr,
            // Expr::Let(expr) => expr,
            // Expr::Lit(expr) => expr,
            // Expr::Loop(expr) => expr,
            // Expr::Macro(expr) => expr,
            // Expr::Match(expr) => ,
            Expr::MethodCall(expr) => expr.turbofish.expand(args),
            // Expr::Paren(expr) => expr,
            Expr::Path(expr) => expr.path.expand(args),
            // Expr::Range(expr) => expr,
            Expr::Reference(expr) => expr.expr.expand(args),
            // Expr::Repeat(expr) => expr,
            // Expr::Return(expr) => expr,
            // Expr::Struct(expr) => expr,
            // Expr::Try(expr) => expr,
            // Expr::TryBlock(expr) => expr,
            // Expr::Tuple(expr) => expr.elems.expand(args),
            // Expr::Unary(expr) => expr,
            Expr::Unsafe(expr) => expr.block.expand(args),
            // Expr::Verbatim(expr) => expr,
            // Expr::While(expr) => expr,
            // Expr::Yield(expr) => expr,
            _ => vec![],
        }
    }
}

impl Expand for Path {
    fn expand(&mut self, args: &Args) -> Results {
        self.segments.last_mut().expand(args)
    }
}

impl<T: Expand, P> Expand for Punctuated<T, P> {
    fn expand(&mut self, args: &Args) -> Results {
        self.iter_mut().flat_map(|t| t.expand(args)).collect()
    }
}

impl<T: Expand> Expand for Vec<T> {
    fn expand(&mut self, args: &Args) -> Results {
        self.iter_mut().flat_map(|t| t.expand(args)).collect()
    }
}

impl<T: Expand> Expand for &mut T {
    fn expand(&mut self, args: &Args) -> Results {
        self.deref_mut().expand(args)
    }
}

impl<T: Expand> Expand for Option<T> {
    fn expand(&mut self, args: &Args) -> Results {
        match self {
            Some(t) => t.expand(args),
            None => vec![],
        }
    }
}

impl Expand for PathSegment {
    fn expand(&mut self, args: &Args) -> Results {
        fn expand_ident(segment: &mut PathSegment, args: &Args) -> Option<Result<Generics>> {
            if args.is_replaced(&segment.ident) {
                return None;
            }
            let (ident, id) = args.ids(&segment.ident);
            let result = find_generics(ident, id).transpose()?;
            args.replace_ident(segment);
            if let Ok(generics) = result {
                let ty_generics = generics.split_for_impl().1;
                let mut arguments = segment.arguments.to_token_stream();
                arguments.append_all(ty_generics.to_token_stream());
                // this shouldn't fail anymore, but I'm leaving the old behavior just in case
                if let Ok(arguments) = parse2(arguments) {
                    segment.arguments = PathArguments::AngleBracketed(arguments);
                    Some(Ok(generics))
                } else {
                    None
                }
            } else {
                Some(result)
            }
        }

        match self.arguments.borrow_mut() {
            PathArguments::None => expand_ident(self, args).into_iter().collect(),
            PathArguments::AngleBracketed(a) => a.expand(args),
            PathArguments::Parenthesized(a) => expand!(args, a.inputs, a.output),
        }
    }
}

impl Expand for GenericArgument {
    fn expand(&mut self, args: &Args) -> Results {
        match self {
            GenericArgument::Type(arg) => arg.expand(args),
            GenericArgument::Const(arg) => arg.expand(args),
            GenericArgument::AssocType(arg) => expand!(args, arg.ty, arg.generics),
            _ => vec![],
        }
    }
}

impl Expand for AngleBracketedGenericArguments {
    fn expand(&mut self, args: &Args) -> Results {
        self.args.expand(args)
    }
}

impl Expand for Type {
    fn expand(&mut self, args: &Args) -> Results {
        match self {
            Type::Array(ty) => ty.elem.expand(args),
            Type::BareFn(ty) => expand!(args, ty.inputs, ty.output),
            Type::Group(ty) => ty.elem.expand(args),
            Type::ImplTrait(ty) => ty.bounds.expand(args),
            Type::Paren(ty) => ty.elem.expand(args),
            Type::Path(ty) => ty.path.expand(args),
            Type::Ptr(ty) => ty.elem.expand(args),
            Type::Reference(ty) => ty.elem.expand(args),
            Type::Slice(ty) => ty.elem.expand(args),
            Type::Tuple(ty) => ty.elems.expand(args),
            Type::TraitObject(ty) => ty.bounds.expand(args),
            _ => vec![],
        }
    }
}

impl Expand for TypeParamBound {
    fn expand(&mut self, args: &Args) -> Results {
        match_ok!(self, TypeParamBound::Trait(t) => t.path.borrow_mut()).expand(args)
    }
}
