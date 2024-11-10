use std::{borrow::BorrowMut, ops::DerefMut};

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{ToTokens, TokenStreamExt};
use syn::{
    parse2,
    punctuated::Punctuated,
    spanned::Spanned,
    token::{Else, For, If, Not},
    AngleBracketedGenericArguments, Arm, BareFnArg, Block, Error, Expr, FieldValue, FnArg,
    GenericArgument, GenericParam, Generics, ImplItem, ImplItemFn, Item, ItemFn, ItemImpl,
    LocalInit, Pat, Path, PathArguments, PathSegment, Result, ReturnType, Signature, Stmt, Type,
    TypeParamBound, WhereClause, WherePredicate,
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

    ($args:expr, $first:expr) => {
        {
            $first.expand($args)
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
                self.merge(args, generics);
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

    fn merge(&mut self, args: &Args, generics: Generics) {
        let original = self.generics();
        generics
            .params
            .into_iter()
            .filter(|param| !args.is_replaced(param))
            .for_each(|param| original.params.push(param));
        if let Some(clause) = generics.where_clause {
            let original_clause = original.make_where_clause();
            clause
                .predicates
                .into_iter()
                .filter(|pred| !args.is_replaced(pred))
                .for_each(|pred| original_clause.predicates.push(pred))
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
        match self {
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

type IfDef = (If, Box<Expr>);
impl Expand for IfDef {
    fn expand(&mut self, args: &Args) -> Results {
        self.1.expand(args)
    }
}

type ElseDef = (Else, Box<Expr>);
impl Expand for ElseDef {
    fn expand(&mut self, args: &Args) -> Results {
        self.1.expand(args)
    }
}

impl Expand for Arm {
    fn expand(&mut self, args: &Args) -> Results {
        expand!(args, self.pat, self.guard, self.body)
    }
}

impl Expand for FieldValue {
    fn expand(&mut self, args: &Args) -> Results {
        self.expr.expand(args)
    }
}

impl Expand for Expr {
    fn expand(&mut self, args: &Args) -> Results {
        match self {
            Expr::Array(expr) => expr.elems.expand(args),
            Expr::Assign(expr) => expand!(args, expr.left, expr.right),
            Expr::Async(expr) => expr.block.expand(args),
            Expr::Await(expr) => expr.base.expand(args),
            Expr::Binary(expr) => expand!(args, expr.left, expr.right),
            Expr::Block(expr) => expr.block.expand(args),
            Expr::Break(expr) => expr.expr.expand(args),
            Expr::Call(expr) => expand!(args, expr.func, expr.args),
            Expr::Cast(expr) => expand!(args, expr.ty, expr.expr),
            Expr::Closure(expr) => expand!(args, expr.inputs, expr.output, expr.body),
            Expr::Const(expr) => expr.block.expand(args),
            Expr::Field(expr) => expr.base.expand(args),
            Expr::ForLoop(expr) => expand!(args, expr.expr, expr.body, expr.pat),
            Expr::Group(expr) => expr.expr.expand(args),
            Expr::If(expr) => expand!(args, expr.cond, expr.then_branch, expr.else_branch),
            Expr::Index(expr) => expand!(args, expr.index, expr.expr),
            Expr::Let(expr) => expand!(args, expr.pat, expr.expr),
            Expr::Loop(expr) => expr.body.expand(args),
            Expr::Match(expr) => expand!(args, expr.expr, expr.arms),
            Expr::MethodCall(expr) => expand!(args, expr.receiver, expr.args, expr.turbofish),
            Expr::Paren(expr) => expr.expr.expand(args),
            Expr::Path(expr) => expr.path.expand(args),
            Expr::Range(expr) => expand!(args, expr.start, expr.end),
            Expr::Reference(expr) => expr.expr.expand(args),
            Expr::Repeat(expr) => expand!(args, expr.expr, expr.len),
            Expr::Return(expr) => expr.expr.expand(args),
            Expr::Struct(expr) => expand!(args, expr.fields, expr.rest),
            Expr::Try(expr) => expr.expr.expand(args),
            // NOTE: this feature is experimental
            Expr::TryBlock(expr) => expr.block.expand(args),
            Expr::Tuple(expr) => expr.elems.expand(args),
            Expr::Unary(expr) => expr.expr.expand(args),
            Expr::Unsafe(expr) => expr.block.expand(args),
            Expr::While(expr) => expand!(args, expr.cond, expr.body),
            // NOTE: this feature is experimental
            Expr::Yield(expr) => expr.expr.expand(args),
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

impl<T: Expand> Expand for Box<T> {
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
            if args.is_replacement(&segment.ident) {
                return None;
            }
            args.replace(&mut segment.ident);
            let id = args.custom_id.as_ref().unwrap_or(&segment.ident);
            let result = find_generics(&segment.ident, id).transpose()?;
            if let Ok(generics) = result {
                let ty_generics = generics.split_for_impl().1;
                let mut arguments = segment.arguments.to_token_stream();
                arguments.append_all(ty_generics.to_token_stream());
                // this shouldn't fail anymore, but I'm leaving the old behavior just in case
                if let Ok(mut arguments) = parse2(arguments) {
                    args.replace_arguments(&mut arguments);
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
