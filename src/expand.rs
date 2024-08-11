use std::borrow::BorrowMut;

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{ToTokens, TokenStreamExt};
use syn::{
    parse2, punctuated::Punctuated, spanned::Spanned, token::PathSep, Block, Error, Expr, FnArg,
    GenericArgument, Generics, ImplItem, ImplItemFn, Item, ItemFn, ItemImpl, Local, Pat, Path,
    PathArguments, PathSegment, Result, ReturnType, Signature, Stmt, Type, TypeTuple,
};
use try_match::match_ok;

use crate::{
    args::Args,
    generics::{find_generics, find_ident_by_id},
    unique_vec::UniqueVec,
};

type Results = Vec<Result<Generics>>;

pub(crate) fn expand_item(item: Item, args: Args) -> TokenStream {
    match item {
        Item::Fn(mut fn_item) => fn_item.expand_item(&args),
        Item::Impl(mut impl_item) => impl_item.expand_item(&args),
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
                self.apply(generics);
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

    fn apply(&mut self, generics: Generics);
}

impl ExpandItem for ItemImpl {
    fn apply(&mut self, generics: Generics) {
        self.generics = generics;
    }
}

impl ExpandItem for ItemFn {
    fn apply(&mut self, generics: Generics) {
        self.sig.generics = generics;
    }
}

trait Expand {
    fn expand(&mut self, args: &Args) -> Results;
}

impl Expand for ItemImpl {
    fn expand(&mut self, args: &Args) -> Results {
        let mut results = self.self_ty.expand(args);
        self.items
            .iter_mut()
            .flat_map(|item| item.expand(args))
            .for_each(|result| results.push(result));
        if let Some((_, path, _)) = self.trait_.borrow_mut() {
            results.append(path.segments.expand(args).borrow_mut());
        }
        results
    }
}

impl Expand for ImplItem {
    fn expand(&mut self, args: &Args) -> Results {
        match self {
            ImplItem::Const(const_item) => const_item.ty.expand(args),
            ImplItem::Fn(fn_item) => fn_item.expand(args),
            ImplItem::Type(type_item) => type_item.ty.expand(args),
            _ => vec![],
        }
    }
}

impl Expand for ImplItemFn {
    fn expand(&mut self, args: &Args) -> Results {
        let mut results: Vec<_> = self.sig.expand(args);
        results.append(self.block.expand(args).borrow_mut());
        results
    }
}

impl Expand for Signature {
    fn expand(&mut self, args: &Args) -> Results {
        let mut results: Vec<_> = self
            .inputs
            .iter_mut()
            .filter_map(match_ok!(, FnArg::Typed(typed)))
            .flat_map(|ty| ty.ty.expand(args))
            .collect();
        if let ReturnType::Type(_, ty) = self.output.borrow_mut() {
            results.append(ty.expand(args).borrow_mut());
        }
        results
    }
}

impl Expand for Block {
    fn expand(&mut self, args: &Args) -> Results {
        self.stmts
            .iter_mut()
            .flat_map(|s| match s {
                Stmt::Local(loc) => loc.expand(args),
                Stmt::Expr(expr, _) => expr.expand(args),
                _ => vec![],
            })
            .collect()
    }
}

impl Expand for Local {
    fn expand(&mut self, args: &Args) -> Results {
        match self.pat.borrow_mut() {
            Pat::Type(ty) => ty.ty.expand(args),
            _ => vec![],
        }
    }
}

impl Expand for Expr {
    fn expand(&mut self, args: &Args) -> Results {
        // TODO: check which ones must be implemented
        match self {
            // Expr::Array(_) => todo!(),
            // Expr::Assign(_) => todo!(),
            // Expr::Async(_) => todo!(),
            // Expr::Await(_) => todo!(),
            // Expr::Binary(_) => todo!(),
            Expr::Block(block) => block.block.expand(args),
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
}

impl Expand for ItemFn {
    fn expand(&mut self, args: &Args) -> Results {
        let mut results = self.sig.expand(args);
        results.append(self.block.expand(args).borrow_mut());
        results
    }
}

impl Expand for Path {
    fn expand(&mut self, args: &Args) -> Results {
        let mut results = self.segments.expand(args);
        let segment = self
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
                    results.push(Ok(generics));
                }
            } else {
                results.push(result);
            }
        }
        results
    }
}

impl Expand for Punctuated<PathSegment, PathSep> {
    fn expand(&mut self, args: &Args) -> Results {
        self.iter_mut()
            .filter_map(|seg| match_ok!(&mut seg.arguments, PathArguments::AngleBracketed(args)))
            .flat_map(|args| &mut args.args)
            .filter_map(|arg| match_ok!(arg, GenericArgument::Type(ty)))
            .flat_map(|ty| ty.expand(args))
            .collect()
    }
}

impl Expand for Type {
    fn expand(&mut self, args: &Args) -> Results {
        match self {
            Type::Array(ty) => ty.elem.expand(args),
            Type::Path(ty) => ty.path.expand(args),
            Type::Ptr(ty) => ty.elem.expand(args),
            Type::Reference(ty) => ty.elem.expand(args),
            Type::Slice(ty) => ty.elem.expand(args),
            Type::Tuple(ty) => ty.expand(args),
            _ => vec![],
        }
    }
}

impl Expand for TypeTuple {
    fn expand(&mut self, args: &Args) -> Results {
        self.elems
            .iter_mut()
            .flat_map(|elem| elem.expand(args))
            .collect()
    }
}
