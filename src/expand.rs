use std::borrow::BorrowMut;

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{ToTokens, TokenStreamExt};
use syn::{
    parse2, punctuated::Punctuated, spanned::Spanned, Block, Error, Expr, FnArg, GenericArgument,
    Generics, ImplItem, ImplItemFn, Item, ItemFn, ItemImpl, Pat, Path, PathArguments, PathSegment,
    Result, ReturnType, Signature, Stmt, Type,
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
        results.append(self.items.expand(args).borrow_mut());
        if let Some((_, path, _)) = self.trait_.borrow_mut() {
            results.append(path.expand(args).borrow_mut());
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
        let mut results: Vec<_> = self.inputs.expand(args);
        results.append(self.output.expand(args).borrow_mut());
        results
    }
}

impl Expand for FnArg {
    fn expand(&mut self, args: &Args) -> Results {
        match_ok!(self, FnArg::Typed(typed))
            .iter_mut()
            .flat_map(|ty| ty.ty.expand(args))
            .collect()
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
            Stmt::Local(loc) => loc.pat.expand(args),
            Stmt::Expr(expr, _) => expr.expand(args),
            _ => vec![],
        }
    }
}

impl Expand for Pat {
    fn expand(&mut self, args: &Args) -> Results {
        match self {
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
            // Expr::Match(m) => ,
            // Expr::MethodCall(_) => todo!(),
            // Expr::Paren(_) => todo!(),
            // Expr::Path(p) => p.path.expand(args),
            // Expr::Range(_) => todo!(),
            // Expr::Reference(r) => r.expr.expand(args),
            // Expr::Repeat(_) => todo!(),
            // Expr::Return(_) => todo!(),
            // Expr::Struct(_) => todo!(),
            // Expr::Try(_) => todo!(),
            // Expr::TryBlock(_) => todo!(),
            // Expr::Tuple(expr) => expr.elems.expand(args),
            // Expr::Unary(_) => todo!(),
            Expr::Unsafe(expr) => expr.block.expand(args),
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
        self.segments.expand(args)
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

impl Expand for PathSegment {
    fn expand(&mut self, args: &Args) -> Results {
        fn expand_ident(segment: &mut PathSegment, args: &Args) -> Option<Result<Generics>> {
            let id = args.custom_id.as_ref().unwrap_or(&segment.ident);
            let result = find_generics(&segment.ident, id).transpose()?;
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
            PathArguments::AngleBracketed(a) => a.args.expand(args),
            PathArguments::Parenthesized(a) => {
                let mut results = a.inputs.expand(args);
                results.append(a.output.expand(args).borrow_mut());
                results
            }
        }
    }
}

impl Expand for GenericArgument {
    fn expand(&mut self, args: &Args) -> Results {
        // TODO: check if the other variants are needed
        match_ok!(self, GenericArgument::Type(ty))
            .iter_mut()
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
            Type::Tuple(ty) => ty.elems.expand(args),
            _ => vec![],
        }
    }
}
