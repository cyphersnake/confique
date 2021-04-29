use proc_macro2::{Span, TokenStream};
use syn::{
    Error, Ident,
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    spanned::Spanned,
};

use crate::ast::{Expr, Input, Node};



impl Parse for Input {
    fn parse(input: ParseStream) -> Result<Self, syn::Error> {
        let mut outer_attrs = input.call(syn::Attribute::parse_inner)?;
        let doc = extract_doc(&mut outer_attrs)?;

        // `#![visibility = "..."]`
        let visibility = extract_single_name_value_attr("visibility", &mut outer_attrs)?
            .map(|v| Ok::<_, syn::Error>(assert_string_lit(v)?.parse::<TokenStream>()?))
            .transpose()?;

        assert_no_extra_attrs(&outer_attrs)?;
        let children = input.call(<Punctuated<_, syn::Token![,]>>::parse_terminated)?;

        let root = Node::Internal {
            doc,
            name: Ident::new("config", Span::call_site()),
            children: children.into_iter().collect(),
        };

        Ok(Self { root, visibility })
    }
}


impl Parse for Node {
    fn parse(input: ParseStream) -> Result<Self, syn::Error> {
        let mut attrs = input.call(syn::Attribute::parse_outer)?;
        let doc = extract_doc(&mut attrs)?;

        // All nodes start with an identifier and a colon.
        let name = input.parse()?;
        let _: syn::Token![:] = input.parse()?;

        let out = if input.lookahead1().peek(syn::token::Brace) {
            // --- A nested Internal ---

            let inner;
            syn::braced!(inner in input);
            let fields = inner.call(<Punctuated<_, syn::Token![,]>>::parse_terminated)?;

            Self::Internal {
                doc,
                name,
                children: fields.into_iter().collect(),
            }
        } else {
            // --- A single value ---

            // Type is mandatory.
            let ty = input.parse()?;

            // Optional default value.
            let default = if input.lookahead1().peek(syn::Token![=]) {
                let _: syn::Token![=] = input.parse()?;
                Some(input.parse()?)
            } else {
                None
            };

            // Optional example value.
            let example = attrs.iter()
                .position(|attr| attr.path.is_ident("example"))
                .map(|i| {
                    let attr = attrs.remove(i);
                    parse_attr_value::<Expr>(attr.tokens)
                })
                .transpose()?;

            if example.is_none() && default.is_none() {
                let msg = "either a default value or an example value has to be specified";
                return Err(Error::new(name.span(), msg));
            }

            Self::Leaf { doc, name, ty, default, example }
        };

        assert_no_extra_attrs(&attrs)?;

        Ok(out)
    }
}

impl Parse for Expr {
    fn parse(input: ParseStream) -> Result<Self, syn::Error> {
        let lit = input.parse::<syn::Lit>()?;
        let out = match lit {
            syn::Lit::Str(l) => Self::Str(l),
            syn::Lit::Int(l) => Self::Int(l),
            syn::Lit::Float(l) => Self::Float(l),
            syn::Lit::Bool(l) => Self::Bool(l),

            _ => {
                let msg = "only string, integer, float and bool literals are allowed here";
                return Err(Error::new(lit.span(), msg));
            }
        };

        Ok(out)
    }
}

/// Makes sure that the given list is empty or returns an error otherwise.
fn assert_no_extra_attrs(attrs: &[syn::Attribute]) -> Result<(), Error> {
    if let Some(attr) = attrs.get(0) {
        let msg = "unknown/unexpected/duplicate attribute in this position";
        return Err(Error::new(attr.span(), msg));
    }

    Ok(())
}

/// Parses the tokenstream as a `T` preceeded by a `=`. This is useful for
/// attributes of the form `#[foo = <T>]`.
fn parse_attr_value<T: Parse>(tokens: TokenStream) -> Result<T, Error> {
    use syn::parse::Parser;

    fn parser<T: Parse>(input: ParseStream) -> Result<T, Error> {
        let _: syn::Token![=] = input.parse()?;
        input.parse()
    }

    parser.parse2(tokens)
}

/// Extract all doc attributes from the list and return them as simple strings.
fn extract_doc(attrs: &mut Vec<syn::Attribute>) -> Result<Vec<String>, Error> {
    let out = attrs.iter()
        .filter(|attr| attr.path.is_ident("doc"))
        .map(|attr| parse_attr_value::<syn::LitStr>(attr.tokens.clone()).map(|lit| lit.value()))
        .collect::<Result<_, _>>()?;

    // I know this is algorithmically not optimal, but `drain_filter` is still
    // unstable and I can't be bothered to write the proper algorithm right now.
    attrs.retain(|attr| !attr.path.is_ident("doc"));

    Ok(out)
}

fn extract_single_name_value_attr(
    name: &str,
    attrs: &mut Vec<syn::Attribute>,
) -> Result<Option<syn::Lit>, Error> {
    let mut filtered = attrs.iter().filter(|attr| attr.path.is_ident(name));
    let meta = match filtered.next() {
        None => return Ok(None),
        Some(attr) => attr.parse_meta()?,
    };

    let nv = match meta {
        syn::Meta::NameValue(nv) => nv,
        other => {
            let msg = format!(r#"expected `name = "value"` attribute syntax for `{}`"#, name);
            return Err(Error::new(other.span(), msg));
        }
    };

    if let Some(dupe) = filtered.next() {
        let msg = format!("duplicate `{}` attribute", name);
        return Err(Error::new(dupe.span(), msg));
    }

    // Remove the attribute from the vector
    attrs.retain(|attr| !attr.path.is_ident(name));

    Ok(Some(nv.lit))
}

fn assert_string_lit(lit: syn::Lit) -> Result<String, Error> {
    match lit {
        syn::Lit::Str(s) => Ok(s.value()),
        _ => Err(Error::new(lit.span(), "expected string literal")),
    }
}
