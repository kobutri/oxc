use std::{fs, io::Read};

use indexmap::map::Entry;
use syn::{
    braced,
    parse::{Parse, ParseBuffer},
    parse_file,
    punctuated::Punctuated,
    Attribute, Generics, Ident, Item, ItemEnum, ItemMacro, ItemStruct, Token, Variant, Visibility,
    WhereClause,
};

use super::{
    schema::FileId,
    skeleton::{EnumSkeleton, Skeleton, StructSkeleton},
    FxIndexMap,
};

/// Load file and extract structs and enums with `#[ast]` attr.
///
/// Only parses enough to get:
/// * Name of type.
/// * Inherits of enums wrapped in `inherit_variants!` macro.
///
/// Inserts `TypeSkeleton`s into `skeletons` and adds mappings from type name to type ID.
///
/// This is the bare minimum to be able to "link up" types to each other in next pass.
pub fn load_file(file_id: FileId, file_path: &str, skeletons: &mut FxIndexMap<String, Skeleton>) {
    let mut file = fs::File::open(file_path).unwrap();
    let mut content = String::new();
    file.read_to_string(&mut content).unwrap();

    let file = parse_file(content.as_str()).unwrap();

    for item in file.items {
        let (name, skeleton) = match item {
            Item::Struct(item) => {
                let Some(skeleton) = parse_struct(item, file_id) else { continue };
                (skeleton.name.clone(), Skeleton::Struct(skeleton))
            }
            Item::Enum(item) => {
                let Some(skeleton) = parse_enum(item, file_id) else { continue };
                (skeleton.name.clone(), Skeleton::Enum(skeleton))
            }
            Item::Macro(item) => {
                let Some(skeleton) = parse_macro(&item, file_id) else { continue };
                (skeleton.name.clone(), Skeleton::Enum(skeleton))
            }
            _ => continue,
        };

        match skeletons.entry(name) {
            Entry::Occupied(entry) => panic!("2 types with same name: {}", entry.key()),
            Entry::Vacant(entry) => {
                entry.insert(skeleton);
            }
        }
    }
}

fn parse_struct(item: ItemStruct, file_id: FileId) -> Option<StructSkeleton> {
    if !has_ast_attr(&item.attrs) {
        return None;
    }

    let name = item.ident.to_string();
    Some(StructSkeleton { name, item, file_id })
}

fn parse_enum(item: ItemEnum, file_id: FileId) -> Option<EnumSkeleton> {
    if !has_ast_attr(&item.attrs) {
        return None;
    }

    let name = item.ident.to_string();
    Some(EnumSkeleton { name, item, inherits: vec![], file_id })
}

fn parse_macro(item: &ItemMacro, file_id: FileId) -> Option<EnumSkeleton> {
    if !item.mac.path.is_ident("inherit_variants") {
        return None;
    }

    let skeleton = item
        .mac
        .parse_body_with(|input: &ParseBuffer| {
            // Because of `@inherit`s we can't use the actual `ItemEnum` parse,
            // This closure is similar to how `ItemEnum` parser works, With the exception
            // of how we approach our variants, First we try to parse a variant out of our
            // tokens if we fail we try parsing the inheritance, And we would raise an
            // error only if both of these fail.
            let attrs = input.call(Attribute::parse_outer)?;

            let vis = input.parse::<Visibility>()?;
            let enum_token = input.parse::<Token![enum]>()?;
            let ident = input.parse::<Ident>()?;
            let generics = input.parse::<Generics>()?;

            let name = ident.to_string();

            let where_clause = input.parse::<Option<WhereClause>>()?;
            assert!(where_clause.is_none(), "Types with `where` clauses are not supported");

            assert!(
                has_ast_attr(&attrs),
                "Enum in `inherit_variants!` macro must have `#[ast]` attr: {name}",
            );

            let content;
            let brace_token = braced!(content in input);
            let mut variants = Punctuated::new();
            let mut inherits = vec![];
            while !content.is_empty() {
                if let Ok(variant) = Variant::parse(&content) {
                    variants.push_value(variant);
                    let punct = content.parse()?;
                    variants.push_punct(punct);
                } else if content.parse::<Token![@]>().is_ok()
                    && content.parse::<Ident>().is_ok_and(|id| id == "inherit")
                {
                    let inherit_ident = content.parse::<Ident>()?;
                    inherits.push(inherit_ident.to_string());
                } else {
                    panic!("Invalid inherit_variants usage!");
                }
            }

            let item = ItemEnum { attrs, vis, enum_token, ident, generics, brace_token, variants };
            Ok(EnumSkeleton { name, item, inherits, file_id })
        })
        .expect("Failed to parse contents of `inherit_variants!` macro");

    Some(skeleton)
}

fn has_ast_attr(attrs: &[Attribute]) -> bool {
    attrs.iter().any(|attr| attr.path().is_ident("ast"))
}