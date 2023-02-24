use heck::{
    ToKebabCase, ToLowerCamelCase, ToPascalCase, ToShoutyKebabCase, ToShoutySnakeCase, ToSnakeCase,
    ToTitleCase, ToTrainCase,
};
use proc_macro2::{Delimiter, Spacing, Span, TokenTree};
use quote::{format_ident, quote, ToTokens};
use syn::{
    parse::Parser, parse_macro_input, parse_macro_input::ParseMacroInput, parse_str, Attribute,
    AttributeArgs, DeriveInput, Expr, Field, Ident, Meta, NestedMeta, Type,
};

#[proc_macro_derive(EnumIt, attributes(enumit))]
pub fn enum_it(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    match input.data {
        syn::Data::Struct(s) => match parse_struct_level_opts(&input.attrs) {
            Ok(opts) => {
                let struct_name = opts.name.unwrap_or(format_ident!("{}Enum", input.ident));
                let vis = &input.vis;
                let (generics, ty_generics, where_clause) = &input.generics.split_for_impl();
                let (repr, tag_ty, mut start) = match &opts.tagged {
                    Some((ty, start)) => (quote! {#[repr(#ty)]}, ty.clone(), *start),
                    None => (quote!(), format_ident!("__Ident"), 0),
                };
                let mut fields = Vec::new();
                let mut fields_token_stream = Vec::new();
                let mut displays = Vec::new();
                for f in s.fields {
                    match parse_field_level_options(&f) {
                        Ok(field_opts) => {
                            if field_opts.skip {
                                continue;
                            }
                            match &f.ident {
                                Some(name) => {
                                    let name =
                                        field_opts.name.as_ref().cloned().unwrap_or_else(|| {
                                            format_ident!("{}", name.to_string().to_pascal_case())
                                        });
                                    let ty = field_opts
                                        .ty
                                        .as_ref()
                                        .unwrap_or_else(|| opts.ty.as_ref().unwrap_or(&f.ty));
                                    let attrs = &field_opts.attrs;
                                    let default = if field_opts.is_default {
                                        quote! {#[default]}
                                    } else {
                                        quote! {}
                                    };
                                    let display = field_opts.display.clone().unwrap_or_else(|| {
                                        match opts.display {
                                            Case::Lowercase => name.to_string().to_lowercase(),
                                            Case::Uppercase => name.to_string().to_uppercase(),
                                            Case::PascalCase => name.to_string().to_pascal_case(),
                                            Case::CamelCase => {
                                                name.to_string().to_lower_camel_case()
                                            }
                                            Case::SnakeCase => name.to_string().to_snake_case(),
                                            Case::ScreamingSnakeCase => {
                                                name.to_string().to_shouty_snake_case()
                                            }
                                            Case::KebabCase => name.to_string().to_kebab_case(),
                                            Case::ScreamingKebabCase => {
                                                name.to_string().to_shouty_kebab_case()
                                            }
                                            Case::TitleCase => name.to_string().to_title_case(),
                                            Case::TrainCase => name.to_string().to_train_case(),
                                        }
                                    });

                                    let tag = match &field_opts.tag {
                                        Some(tag) => quote! {
                                            = #tag
                                        },
                                        None => {
                                            if opts.tagged.is_some() {
                                                quote! {
                                                    = #start as #tag_ty
                                                }
                                            } else {
                                                quote! {}
                                            }
                                        }
                                    };

                                    if opts.is_typed {
                                        fields_token_stream.push(quote! {
                                            #(#attrs)*
                                            #default
                                            #name(#ty) #tag
                                        });
                                        displays.push(quote! {
                                                #struct_name::#name(val) => ::core::fmt::Display::fmt(val, f)
                                            });
                                    } else {
                                        #[allow(clippy::collapsible_else_if)]
                                        if let Some(ty) = &field_opts.ty {
                                            fields_token_stream.push(quote! {
                                                #(#attrs)*
                                                #default
                                                #name(#ty) #tag
                                            });

                                            displays.push(quote! {
                                                    #struct_name::#name(val) => ::core::fmt::Display::fmt(val, f)
                                                });
                                        } else {
                                            fields_token_stream.push(quote! {
                                                #(#attrs)*
                                                #default
                                                #name #tag
                                            });
                                            displays.push(quote! {
                                                    #struct_name::#name => ::core::fmt::Display::fmt(#display, f)
                                                });
                                        }
                                    }

                                    fields.push((field_opts, f.clone()));
                                }
                                None => {
                                    return syn::Error::new_spanned(
                                        f.ident,
                                        "Unnamed fields are not supported",
                                    )
                                    .to_compile_error()
                                    .into()
                                }
                            }
                            start += 1;
                        }
                        Err(e) => return e.to_compile_error().into(),
                    }
                }
                let struct_level_attrs = &opts.attrs;

                if opts.is_typed {
                    let display = if opts.skip_display {
                        quote! {}
                    } else {
                        quote! {
                            impl #generics ::core::fmt::Display for #struct_name #ty_generics #where_clause {
                                fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
                                    use core::fmt::Pointer;
                                    match self {
                                        #(#displays),*
                                    }
                                }
                            }
                        }
                    };
                    quote! {
                        #(#struct_level_attrs)*
                        #repr
                        #vis enum #struct_name #generics {
                            #(#fields_token_stream),*
                        }

                        #display
                    }
                    .into()
                } else {
                    let display = if opts.skip_display {
                        quote! {}
                    } else {
                        quote! {
                            impl ::core::fmt::Display for #struct_name {
                                fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
                                    use core::fmt::Pointer;
                                    match self {
                                        #(#displays),*
                                    }
                                }
                            }
                        }
                    };

                    quote! {
                        #(#struct_level_attrs)*
                        #[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Ord, PartialOrd)]
                        #repr
                        #vis enum #struct_name {
                            #(#fields_token_stream),*
                        }

                        #display
                    }
                    .into()
                }
            }
            Err(e) => e.to_compile_error().into(),
        },
        _ => syn::Error::new(input.ident.span(), "EnumIt only works on structs")
            .to_compile_error()
            .into(),
    }
}

#[derive(Default)]
struct StructLevelOptions {
    name: Option<Ident>,
    is_typed: bool,
    ty: Option<syn::Type>,
    display: Case,
    skip_display: bool,
    attrs: Vec<Attribute>,
    tagged: Option<(Ident, i64)>,
}

#[derive(Default)]
enum Case {
    Lowercase,
    Uppercase,
    #[default]
    PascalCase,
    CamelCase,
    SnakeCase,
    ScreamingSnakeCase,
    KebabCase,
    ScreamingKebabCase,
    TitleCase,
    TrainCase,
}

struct FieldLevelOptions {
    name: Option<Ident>,
    display: Option<String>,
    is_default: bool,
    skip: bool,
    attrs: Vec<Attribute>,
    tag: Option<Expr>,
    ty: Option<Type>,
}

fn parse_field_level_options(field: &Field) -> syn::Result<FieldLevelOptions> {
    let mut attrs = Vec::new();
    let mut field_name = None;
    let mut display = None;
    let mut ty = None;
    let mut skip = false;
    let mut is_default = false;
    let mut tag = None;

    let mut has_default = false;
    let mut has_skip = false;

    for attr in &field.attrs {
        match () {
            () if attr.path.is_ident("enumit") => {
                let mut iter = match attr.tokens.to_token_stream().into_iter().next() {
                    Some(TokenTree::Group(g)) if g.delimiter() == Delimiter::Parenthesis => {
                        Ok(g.stream())
                    }
                    _ => Err(syn::Error::new_spanned(
                        attr,
                        "The enumit attribute must be parenthesized",
                    )),
                }?
                .into_iter()
                .peekable();

                while iter.peek().is_some() {
                    match iter.next().unwrap() {
                        TokenTree::Ident(name) => match name.to_string().as_str() {
                            "name" => {
                                if field_name.is_some() {
                                    return Err(syn::Error::new_spanned(
                                        name,
                                        "Only one name attribute is allowed",
                                    ));
                                }

                                field_name =
                                    Some(syn::parse_str(&parse_str_val(&name.span(), &mut iter)?)?);
                            }
                            "display" => {
                                if display.is_some() {
                                    return Err(syn::Error::new_spanned(
                                        name,
                                        "Only one display attribute is allowed",
                                    ));
                                }

                                display = Some(parse_str_val(&name.span(), &mut iter)?);
                            }
                            "tag" => {
                                if tag.is_some() {
                                    return Err(syn::Error::new_spanned(
                                        name,
                                        "Only one tag attribute is allowed",
                                    ));
                                }

                                tag = Some(parse_expr_val(&name.span(), &mut iter)?);
                            }
                            "skip" => {
                                if has_skip {
                                    return Err(syn::Error::new_spanned(
                                        name,
                                        "Only one skip attribute is allowed",
                                    ));
                                }
                                let stream = (&mut iter).take_while(
                                    |i| !matches!(i, TokenTree::Punct(p) if p.as_char() == ',' && p.spacing() == Spacing::Alone),
                                )
                                .collect::<proc_macro2::TokenStream>();
                                if !stream.is_empty() {
                                    return Err(syn::Error::new_spanned(
                                        stream,
                                        "skip doesn't have any value",
                                    ));
                                }
                                has_skip = true;
                                skip = true;
                            }
                            "default" => {
                                if has_default {
                                    return Err(syn::Error::new_spanned(
                                        name,
                                        "Only one default attribute is allowed",
                                    ));
                                }
                                let stream = (&mut iter).take_while(
                                    |i| !matches!(i, TokenTree::Punct(p) if p.as_char() == ',' && p.spacing() == Spacing::Alone),
                                )
                                .collect::<proc_macro2::TokenStream>();
                                if !stream.is_empty() {
                                    return Err(syn::Error::new_spanned(
                                        stream,
                                        "default doesn't have any value",
                                    ));
                                }
                                has_default = true;
                                is_default = true;
                            }
                            "type" => {
                                if ty.is_some() {
                                    return Err(syn::Error::new_spanned(
                                        name,
                                        "Only one type attribute is allowed",
                                    ));
                                }

                                ty = Some(parse_str::<syn::Type>(&parse_str_val(
                                    &name.span(),
                                    &mut iter,
                                )?)?);
                            }
                            "attrs" => {
                                let stream = (&mut iter).take_while(
                                    |i| !matches!(i, TokenTree::Punct(p) if p.as_char() == ',' && p.spacing() == Spacing::Alone),
                                )
                                .collect::<proc_macro2::TokenStream>();
                                if stream.is_empty() {
                                    return Err(syn::Error::new_spanned(
                                        name,
                                        "attrs must have a value",
                                    ));
                                }

                                let g = match stream.into_iter().next() {
                                    Some(TokenTree::Group(g))
                                        if g.delimiter() == Delimiter::Parenthesis =>
                                    {
                                        Ok(g)
                                    }
                                    _ => Err(syn::Error::new_spanned(
                                        attr,
                                        "The attrs attribute must be parenthesized",
                                    )),
                                }?;
                                let attributes = AttributeArgs::parse.parse2(quote! {attrs #g})?;

                                for nested_meta in &attributes {
                                    match nested_meta {
                                        NestedMeta::Meta(Meta::List(l)) => {
                                            for n in &l.nested {
                                                attrs.extend(
                                                    Attribute::parse_outer
                                                        .parse2(quote! {#[#n]})?,
                                                );
                                            }
                                        }
                                        _ => {
                                            return Err(syn::Error::new_spanned(
                                                nested_meta,
                                                "format for attributes",
                                            ))
                                        }
                                    }
                                }
                            }
                            x => {
                                return Err(syn::Error::new_spanned(
                                    name,
                                    format!("Unexpected identifier {}", x),
                                ));
                            }
                        },
                        TokenTree::Punct(p) => {
                            let char = p.as_char();
                            if char == ',' {
                                continue;
                            }
                            return Err(syn::Error::new_spanned(
                                p,
                                format!("Unexpected character {}", char),
                            ));
                        }
                        x => {
                            return Err(syn::Error::new_spanned(
                                x.clone(),
                                format!("Unexpected token {}", x.to_token_stream()),
                            ));
                        }
                    }
                }
            }
            () => {}
        }
    }
    Ok(FieldLevelOptions {
        name: field_name,
        display,
        ty,
        attrs,
        is_default,
        skip,
        tag,
    })
}

fn parse_str_val(span: &Span, iter: &mut dyn Iterator<Item = TokenTree>) -> syn::Result<String> {
    let equals = iter
        .next()
        .ok_or_else(|| syn::Error::new(*span, "Expected named value pair"))?;
    match equals {
        TokenTree::Punct(p) if p.as_char() == '=' => {}
        x => {
            return Err(syn::Error::new(
                x.span(),
                format!("Expected '=' but got {}", x),
            ))
        }
    }
    let lit = iter.take_while(
        |i| !matches!(i, TokenTree::Punct(p) if p.as_char() == ',' && p.spacing() == Spacing::Alone),
    )
    .collect::<proc_macro2::TokenStream>().to_string();
    Ok(
        lit[usize::from(lit.starts_with('"'))..lit.len() - usize::from(lit.ends_with('"'))]
            .to_string(),
    )
}

fn parse_expr_val(span: &Span, iter: &mut dyn Iterator<Item = TokenTree>) -> syn::Result<Expr> {
    let equals = iter
        .next()
        .ok_or_else(|| syn::Error::new(*span, "Expected named value pair"))?;
    match equals {
        TokenTree::Punct(p) if p.as_char() == '=' => {}
        x => {
            return Err(syn::Error::new(
                x.span(),
                format!("Expected '=' but got {}", x),
            ))
        }
    }
    let lit = iter.take_while(
        |i| !matches!(i, TokenTree::Punct(p) if p.as_char() == ',' && p.spacing() == Spacing::Alone),
    )
    .collect::<proc_macro2::TokenStream>();
    syn::parse2(lit)
}

fn parse_struct_level_opts(attrs: &[Attribute]) -> syn::Result<StructLevelOptions> {
    let mut opts = StructLevelOptions::default();
    let mut has_enumit = false;
    for attr in attrs {
        if attr.path.is_ident("enumit") {
            if has_enumit {
                return Err(syn::Error::new_spanned(
                    attr,
                    "Only one enumit attribute is allowed",
                ));
            }
            has_enumit = true;
            let meta = attr.parse_meta()?;
            if let syn::Meta::List(list) = meta {
                let mut has_name = false;
                let mut has_display_all = false;
                let mut has_skip_display = false;
                let mut has_is_typed = None;
                for nested in list.nested {
                    if let syn::NestedMeta::Meta(meta) = nested {
                        match () {
                            () if meta.path().is_ident("name") => {
                                if has_name {
                                    return Err(syn::Error::new_spanned(
                                        meta,
                                        "Only one name attribute is allowed",
                                    ));
                                }

                                if let syn::Meta::NameValue(name_value) = meta {
                                    if let syn::Lit::Str(s) = name_value.lit {
                                        opts.name = Some(parse_str(&s.value())?);
                                        has_name = true;
                                    } else {
                                        return Err(syn::Error::new_spanned(
                                            name_value.lit,
                                            "The name attribute value must be a string literal",
                                        ));
                                    }
                                } else {
                                    return Err(syn::Error::new_spanned(
                                        meta,
                                        "The name attribute must be a name value pair",
                                    ));
                                }
                            }
                            () if meta.path().is_ident("skip_display") => {
                                if has_skip_display {
                                    return Err(syn::Error::new_spanned(
                                        meta,
                                        "Only one skip_display attribute is allowed",
                                    ));
                                }

                                if has_display_all {
                                    return Err(syn::Error::new_spanned(
                                        meta,
                                        "skip_display and display_all are mutually exclusive",
                                    ));
                                }
                                opts.skip_display = true;
                                has_skip_display = true;
                            }
                            () if meta.path().is_ident("display_all") => {
                                if has_display_all {
                                    return Err(syn::Error::new_spanned(
                                        meta,
                                        "Only one display_all attribute is allowed",
                                    ));
                                }

                                if has_skip_display {
                                    return Err(syn::Error::new_spanned(
                                        meta,
                                        "skip_display and display_all are mutually exclusive",
                                    ));
                                }
                                if let syn::Meta::NameValue(name_value) = meta {
                                    if let syn::Lit::Str(s) = name_value.lit {
                                        has_display_all = true;
                                        match s.value().as_str() {
                                            "lowercase" => opts.display = Case::Lowercase,
                                            "UPPERCASE" => opts.display = Case::Uppercase,
                                            "PascalCase" => opts.display = Case::PascalCase,
                                            "camelCase" => opts.display = Case::CamelCase,
                                            "snake_case" => opts.display = Case::SnakeCase,
                                            "SCREAMING_SNAKE_CASE" => {
                                                opts.display = Case::ScreamingSnakeCase
                                            }
                                            "kebab-case" => opts.display = Case::KebabCase,
                                            "SCREAMING-KEBAB-CASE" => {
                                                opts.display = Case::ScreamingKebabCase
                                            }
                                            "Title Case" => opts.display = Case::TitleCase,
                                            "Train-Case" => opts.display = Case::TrainCase,
                                            _ => {
                                                return Err(syn::Error::new(
                                                    s.span(),
                                                    r#"Invalid display_all value, The possible values are "lowercase", "UPPERCASE", "PascalCase", "camelCase", "snake_case", "SCREAMING_SNAKE_CASE", "kebab-case", "SCREAMING-KEBAB-CASE", "Title Case", "Train-Case"."#,
                                                ))
                                            }
                                        }
                                    } else {
                                        return Err(syn::Error::new_spanned(name_value.lit, "The display_all attribute value must be a string literal"));
                                    }
                                } else {
                                    return Err(syn::Error::new_spanned(
                                        meta,
                                        "The display_all attribute must be a name value pair",
                                    ));
                                }
                            }
                            () if meta.path().is_ident("unit") => match has_is_typed {
                                Some(v) => {
                                    return Err(syn::Error::new_spanned(
                                        meta,
                                        format!("Enum has been set to be {}", v),
                                    ))
                                }
                                None => {
                                    has_is_typed = Some("unit");
                                    opts.is_typed = false;
                                }
                            },
                            () if meta.path().is_ident("typed") => {
                                match has_is_typed {
                                    Some(v) => {
                                        return Err(syn::Error::new_spanned(
                                            meta,
                                            format!("Enum has been set to be {}", v),
                                        ))
                                    }
                                    None => {
                                        match meta {
                                            Meta::Path(_) => {}
                                            Meta::List(l) => {
                                                opts.ty = Some(parse_str::<syn::Type>(
                                                    &syn::parse2::<syn::LitStr>(
                                                        l.nested.to_token_stream(),
                                                    )?
                                                    .value(),
                                                )?);
                                            }
                                            Meta::NameValue(_) => {
                                                return Err(syn::Error::new_spanned(meta, "The typed attribute cannot be a name value pair"));
                                            }
                                        }
                                        has_is_typed = Some("typed");
                                        opts.is_typed = true;
                                    }
                                }
                            }
                            () if meta.path().is_ident("tagged") => {
                                if opts.tagged.is_some() {
                                    return Err(syn::Error::new_spanned(
                                        meta,
                                        "Only one tagged attribute is allowed",
                                    ));
                                }

                                let mut start = None;
                                let mut ty = None;
                                if let syn::Meta::List(l) = meta {
                                    for nested in &l.nested {
                                        if let syn::NestedMeta::Meta(meta) = nested {
                                            match () {
                                                () if meta.path().is_ident("start") => {
                                                    if let syn::Meta::NameValue(name_value) = meta {
                                                        if let syn::Lit::Int(s) = &name_value.lit {
                                                            start = Some(s.base10_parse::<i64>()?);
                                                        } else {
                                                            return Err(syn::Error::new_spanned(&name_value.lit, "The name attribute value must be a int literal"));
                                                        }
                                                    } else {
                                                        return Err(syn::Error::new_spanned(meta, "The name attribute must be a name value pair"));
                                                    }
                                                }
                                                () if meta.path().is_ident("type") => {
                                                    if let syn::Meta::NameValue(name_value) = meta {
                                                        if let syn::Lit::Str(s) = &name_value.lit {
                                                            ty = Some(parse_str::<syn::Ident>(
                                                                &s.value(),
                                                            )?);
                                                        } else {
                                                            return Err(syn::Error::new_spanned(&name_value.lit, "The name attribute value must be a string literal"));
                                                        }
                                                    } else {
                                                        return Err(syn::Error::new_spanned(meta, "The name attribute must be a name value pair"));
                                                    }
                                                }
                                                () => {
                                                    return Err(syn::Error::new_spanned(
                                                        meta,
                                                        "The tagged attribute must be a list",
                                                    ))
                                                }
                                            }
                                        } else {
                                            return Err(syn::Error::new_spanned(
                                                nested,
                                                "The tagged attribute must be a list",
                                            ));
                                        }
                                    }

                                    match (ty, start) {
                                        (None, None) => continue,
                                        (None, Some(_)) => {
                                            return Err(syn::Error::new_spanned(
                                                l,
                                                "The tagged attribute must have a type",
                                            ))
                                        }
                                        (Some(ty), None) => opts.tagged = Some((ty, 0)),
                                        (Some(ty), Some(start)) => opts.tagged = Some((ty, start)),
                                    }
                                } else {
                                    return Err(syn::Error::new_spanned(
                                        meta,
                                        "The tagged attribute must be a list",
                                    ));
                                }
                            }
                            () if meta.path().is_ident("attrs") => {
                                if let syn::Meta::List(list) = meta {
                                    for n in &list.nested {
                                        opts.attrs.extend(
                                            Attribute::parse_outer.parse2(quote::quote! {#[#n]})?,
                                        );
                                    }
                                } else {
                                    return Err(syn::Error::new_spanned(
                                        meta,
                                        "The attrs attribute must be a list",
                                    ));
                                }
                            }
                            () => {}
                        }
                    }
                }
            }
        }
    }
    Ok(opts)
}
