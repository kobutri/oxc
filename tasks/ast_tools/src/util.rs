use itertools::Itertools;
use proc_macro2::{Group, TokenStream, TokenTree};
use quote::{format_ident, ToTokens};
use serde::Serialize;
use syn::{spanned::Spanned, GenericArgument, Ident, ItemMacro, PathArguments, Type, TypePath};

use crate::{codegen::EarlyCtx, TypeId};

pub trait NormalizeError<T> {
    fn normalize(self) -> crate::Result<T>;
    fn normalize_with<E>(self, err: E) -> crate::Result<T>
    where
        E: ToString;
}

impl<T, E> NormalizeError<T> for Result<T, E>
where
    E: ToString,
{
    fn normalize(self) -> crate::Result<T> {
        self.map_err(|e| e.to_string())
    }

    fn normalize_with<U>(self, err: U) -> crate::Result<T>
    where
        U: ToString,
    {
        self.map_err(|_| err.to_string())
    }
}

impl<T> NormalizeError<T> for Option<T> {
    fn normalize(self) -> crate::Result<T> {
        self.normalize_with(String::default())
    }

    fn normalize_with<E>(self, err: E) -> crate::Result<T>
    where
        E: ToString,
    {
        self.map_or_else(|| Err(err.to_string()), |r| Ok(r))
    }
}

pub trait TokenStreamExt {
    fn replace_ident(self, needle: &str, replace: &Ident) -> TokenStream;
}

pub trait TypeExt {
    fn get_ident(&self) -> TypeIdentResult;
    fn analyze(&self, ctx: &EarlyCtx) -> TypeAnalysis;
}

pub trait StrExt: AsRef<str> {
    /// Dead simple, just adds either `s` or `es` based on the last character.
    /// doesn't handle things like `sh`, `x`, `z`, etc. It also creates wrong results when the word
    /// ends with `y` but there is a preceding vowl similar to `toys`,
    /// It WILL output the WRONG result `toies`!
    /// As an edge case would output `children` for the input `child`.
    fn to_plural(self) -> String;
}

pub trait ToIdent {
    fn to_ident(&self) -> Ident;
}

#[derive(Debug)]
pub enum TypeIdentResult<'a> {
    Ident(&'a Ident),
    Vec(Box<TypeIdentResult<'a>>),
    Box(Box<TypeIdentResult<'a>>),
    Option(Box<TypeIdentResult<'a>>),
    Reference(Box<TypeIdentResult<'a>>),
    /// We bailed on detecting wrapper
    Complex(Box<TypeIdentResult<'a>>),
}

impl<'a> TypeIdentResult<'a> {
    fn boxed(inner: Self) -> Self {
        Self::Box(Box::new(inner))
    }

    fn vec(inner: Self) -> Self {
        Self::Vec(Box::new(inner))
    }

    fn option(inner: Self) -> Self {
        Self::Option(Box::new(inner))
    }

    fn complex(inner: Self) -> Self {
        Self::Complex(Box::new(inner))
    }

    fn reference(inner: Self) -> Self {
        Self::Reference(Box::new(inner))
    }

    pub fn inner_ident(&self) -> &'a Ident {
        match self {
            Self::Ident(it) => it,
            Self::Complex(it)
            | Self::Vec(it)
            | Self::Box(it)
            | Self::Option(it)
            | Self::Reference(it) => it.inner_ident(),
        }
    }

    pub fn as_ident(&self) -> Option<&'a Ident> {
        if let Self::Ident(it) = self {
            Some(it)
        } else {
            None
        }
    }
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub enum TypeWrapper {
    None,
    Box,
    Vec,
    Opt,
    #[expect(dead_code)]
    VecBox,
    VecOpt,
    OptBox,
    OptVec,
    Ref,
    /// We bailed on detecting the type wrapper
    Complex,
}

#[derive(Debug, Clone, Serialize)]
pub struct TypeAnalysis {
    pub type_id: Option<TypeId>,
    pub wrapper: TypeWrapper,
    // pub name: String,
    #[serde(skip)]
    pub typ: Type,
}

impl TypeExt for Type {
    fn get_ident(&self) -> TypeIdentResult {
        match self {
            Type::Path(TypePath { path, .. }) => {
                let seg1 = path.segments.first().unwrap();
                match &seg1.arguments {
                    PathArguments::None => TypeIdentResult::Ident(&seg1.ident),
                    PathArguments::AngleBracketed(it) => {
                        let args = &it.args.iter().collect_vec();
                        assert!(args.len() < 3, "Max path arguments here is 2, eg `Box<'a, Adt>`");
                        if let Some(second) = args.get(1) {
                            let GenericArgument::Type(second) = second else { panic!() };
                            let inner = second.get_ident();
                            if seg1.ident == "Box" {
                                TypeIdentResult::boxed(inner)
                            } else if seg1.ident == "Vec" {
                                TypeIdentResult::vec(inner)
                            } else {
                                panic!();
                            }
                        } else {
                            match args.first() {
                                Some(GenericArgument::Type(it)) => {
                                    let inner = it.get_ident();
                                    if seg1.ident == "Option" {
                                        TypeIdentResult::option(inner)
                                    } else {
                                        TypeIdentResult::complex(inner)
                                    }
                                }
                                Some(GenericArgument::Lifetime(_)) => {
                                    TypeIdentResult::Ident(&seg1.ident)
                                }
                                _ => panic!("unsupported type!"),
                            }
                        }
                    }
                    PathArguments::Parenthesized(_) => {
                        panic!("Parenthesized path arguments aren't supported!")
                    }
                }
            }
            Type::Reference(typ) => TypeIdentResult::reference(typ.elem.get_ident()),
            _ => panic!("Unsupported type."),
        }
    }

    fn analyze(&self, ctx: &EarlyCtx) -> TypeAnalysis {
        fn analyze<'a>(res: &'a TypeIdentResult) -> Option<(&'a Ident, TypeWrapper)> {
            let mut wrapper = TypeWrapper::None;
            let ident = match res {
                TypeIdentResult::Ident(inner) => inner,
                TypeIdentResult::Complex(inner) => {
                    wrapper = TypeWrapper::Complex;
                    let (inner, _) = analyze(inner)?;
                    inner
                }
                TypeIdentResult::Box(inner) => {
                    wrapper = TypeWrapper::Box;
                    let (inner, inner_kind) = analyze(inner)?;
                    assert!(inner_kind == TypeWrapper::None,);
                    inner
                }
                TypeIdentResult::Vec(inner) => {
                    wrapper = TypeWrapper::Vec;
                    let (inner, inner_kind) = analyze(inner)?;
                    if inner_kind == TypeWrapper::Opt {
                        wrapper = TypeWrapper::VecOpt;
                    } else if inner_kind != TypeWrapper::None {
                        panic!();
                    }
                    inner
                }
                TypeIdentResult::Option(inner) => {
                    wrapper = TypeWrapper::Opt;
                    let (inner, inner_kind) = analyze(inner)?;
                    if inner_kind == TypeWrapper::Vec {
                        wrapper = TypeWrapper::OptVec;
                    } else if inner_kind == TypeWrapper::Box {
                        wrapper = TypeWrapper::OptBox;
                    } else if inner_kind != TypeWrapper::None {
                        panic!();
                    }
                    inner
                }
                TypeIdentResult::Reference(_) => return None,
            };
            Some((ident, wrapper))
        }
        let type_ident = self.get_ident();
        let Some((type_ident, wrapper)) = analyze(&type_ident) else {
            return TypeAnalysis { type_id: None, wrapper: TypeWrapper::Ref, typ: self.clone() };
        };

        let type_id = ctx.type_id(&type_ident.to_string());
        TypeAnalysis { type_id, wrapper, typ: self.clone() }
    }
}

impl<T: AsRef<str>> StrExt for T {
    fn to_plural(self) -> String {
        let txt = self.as_ref();
        if txt.is_empty() {
            return String::default();
        }

        let mut txt = txt.to_string();
        if txt.ends_with("child") {
            txt.push_str("ren");
        } else {
            match txt.chars().last() {
                Some('s') => {
                    txt.push_str("es");
                }
                Some('y') => {
                    txt.pop();
                    txt.push_str("ies");
                }
                _ => txt.push('s'),
            }
        }
        txt
    }
}

impl TokenStreamExt for TokenStream {
    fn replace_ident(self, needle: &str, replace: &Ident) -> TokenStream {
        self.into_iter()
            .map(|it| match it {
                TokenTree::Ident(ident) if ident == needle => replace.to_token_stream(),
                TokenTree::Group(group) => {
                    Group::new(group.delimiter(), group.stream().replace_ident(needle, replace))
                        .to_token_stream()
                }
                _ => it.to_token_stream(),
            })
            .collect()
    }
}

// From https://doc.rust-lang.org/reference/keywords.html
#[rustfmt::skip]
static RESERVED_NAMES: &[&str] = &[
    // Strict keywords
    "as", "break", "const", "continue", "crate", "else", "enum", "extern", "false", "fn", "for", "if",
    "impl", "in", "let", "loop", "match", "mod", "move", "mut", "pub", "ref", "return", "self", "Self",
    "static", "struct", "super", "trait", "true", "type", "unsafe", "use", "where", "while", "async",
    "await", "dyn",
    // Reserved keywords
    "abstract", "become", "box", "do", "final", "macro", "override", "priv", "typeof", "unsized",
    "virtual", "yield", "try",
    // Weak keywords
    "macro_rules", "union", // "dyn" also listed as a weak keyword, but is already on strict list
];

impl<S> ToIdent for S
where
    S: AsRef<str>,
{
    fn to_ident(&self) -> Ident {
        let name = self.as_ref();
        if RESERVED_NAMES.contains(&name) {
            format_ident!("r#{name}")
        } else {
            format_ident!("{name}")
        }
    }
}

pub fn unexpanded_macro_err(mac: &ItemMacro) -> String {
    format!("Unexpanded macro: {:?}:{:?}", mac.ident, mac.span())
}

/// Macro to implement conversion methods to/from integers for a fieldless enum.
///
/// Implements:
/// * Constant `VARIANTS` - array of all variants.
/// * Constant `MAX_VALUE` - the maximum value.
/// * Conversion methods `to_id`, `to_usize`, `from_id`, `from_usize`, `try_from_id`, `try_from_usize`.
/// * Method `name`
///
/// Derives `Clone` and `Copy` on the enum.
///
/// Enum must have a `#[repr]` attr e.g. `#[repr(u8)]`.
///
/// ```
/// id_enum! {
///     /// Foo
///     #[repr(u8)]
///     #[derive(Debug)]
///     pub enum FooId {
///         Bar,
///         Qux,
///         Gim,
///     }
/// }
/// ```
///
/// Expands to:
///
/// ```
/// /// Foo
/// #[repr(u8)]
/// #[derive(Clone, Copy, Debug)]
/// pub enum FooId {
///     Bar,
///     Qux,
///     Gim,
/// }
///
/// const _: () = {
///     assert!(
///         std::mem::size_of::<u8>() <= std::mem::size_of::<usize>(),
///         "repr type must be smaller than or same size as `usize`"
///     );
/// };
///
/// impl FooId {
///     pub const VARIANTS: &[Self] = &[Self::Bar, Self::Qux, Self::Gim];
///     pub const MAX_VALUE: u8 = 2;
///
///     #[inline]
///     #[allow(non_upper_case_globals)]
///     pub const fn try_from_value(value: u8) -> Option<Self> {
///         const VALUE_Bar: u8 = FooId::Bar as u8;
///         const VALUE_Qux: u8 = FooId::Qux as u8;
///         const VALUE_Gim: u8 = FooId::Gim as u8;
///
///         match value {
///             VALUE_Bar => Some(Self::Bar),
///             VALUE_Qux => Some(Self::Qux),
///             VALUE_Gim => Some(Self::Gim),
///             _ => None,
///         }
///     }
///
///     #[inline]
///     pub const fn from_value(value: u8) -> Self {
///         if let Some(out) = Self::try_from_value(value) {
///             out
///         } else {
///             panic!("Invalid value");
///         }
///     }
///
///     #[inline]
///     pub const fn try_from_usize(value: usize) -> Option<Self> {
///         if value > u8::MAX as usize {
///             None
///         } else {
///             Self::try_from_value(value as u8)
///         }
///     }
///
///     #[inline]
///     pub const fn from_usize(value: usize) -> Self {
///         if let Some(out) = Self::try_from_usize(value) {
///             out
///         } else {
///             panic!("Invalid value");
///         }
///     }
///
///     #[inline]
///     pub const fn to_value(self) -> u8 {
///         self as u8
///     }
///
///     #[inline]
///     pub const fn to_usize(self) -> usize {
///         self as usize
///     }
///
///     #[inline]
///     pub const fn name(self) -> &'static str {
///         match self {
///             Self::Bar => "Bar",
///             Self::Qux => "Qux",
///             Self::Gim => "Gim",
///         }
///     }
/// }
/// ```
macro_rules! id_enum {
    (
        $(#[doc = $doc:literal])*
        #[repr($repr:ident)]
        $(#[$($attr:tt)+])*
        $vis:vis enum $name:ident {
            $($variant:ident $(= $id:literal)?,)+
        }
    ) => {
        $(#[doc = $doc])*
        #[repr($repr)]
        $(#[$($attr)+])*
        #[derive(Clone, Copy)]
        $vis enum $name {
            $($variant $(= $id)?),+
        }

        const _: () = {
            assert!(
                std::mem::size_of::<$repr>() <= std::mem::size_of::<usize>(),
                "repr type must be smaller than or same size as `usize`"
            );
        };

        impl $name {
            pub const VARIANTS: &[Self] = &[$(Self::$variant),*];
            pub const MAX_VALUE: $repr = {
                let mut max = 0;
                let mut index = 0;
                loop {
                    if index == Self::VARIANTS.len() {
                        break;
                    }
                    let value = Self::VARIANTS[index] as $repr;
                    if value > max {
                        max = value;
                    }
                    index += 1;
                }
                max
            };

            #[inline]
            #[allow(non_upper_case_globals)]
            pub const fn try_from_value(value: u8) -> Option<Self> {
                $(::paste::paste! {
                    const [<VALUE _ $variant>]: $repr = $name::$variant as $repr;
                })+

                match value {
                    $(::paste::paste!([<VALUE _ $variant>]) => Some(Self::$variant),)+
                    _ => None,
                }
            }

            #[inline]
            pub const fn from_value(value: u8) -> Self {
                if let Some(out) = Self::try_from_value(value) {
                    out
                } else {
                    panic!("Invalid value");
                }
            }

            #[inline]
            #[expect(clippy::cast_possible_truncation)]
            pub const fn try_from_usize(value: usize) -> Option<Self> {
                if value > $repr::MAX as usize {
                    None
                } else {
                    Self::try_from_value(value as $repr)
                }
            }

            #[inline]
            pub const fn from_usize(value: usize) -> Self {
                if let Some(out) = Self::try_from_usize(value) {
                    out
                } else {
                    panic!("Invalid value");
                }
            }

            #[inline]
            pub const fn to_value(self) -> $repr {
                self as $repr
            }

            #[inline]
            pub const fn to_usize(self) -> usize {
                self as usize
            }

            #[inline]
            pub const fn name(self) -> &'static str {
                match self {
                    $(Self::$variant => stringify!($variant),)+
                }
            }
        }
    };
}
pub(crate) use id_enum;
