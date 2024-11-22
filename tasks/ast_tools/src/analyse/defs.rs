#![expect(dead_code)]

use syn::{ItemEnum, ItemStruct};

use super::{FileId, TypeId};

type Discriminant = u8;

#[derive(Debug)]
pub enum TypeDef {
    Struct(StructDef),
    Enum(EnumDef),
    Primitive(PrimitiveDef),
    Option(OptionDef),
    Box(BoxDef),
    Vec(VecDef),
    Cell(CellDef),
}

impl TypeDef {
    /// Get type name.
    pub fn name(&self) -> &str {
        match self {
            TypeDef::Struct(def) => &def.name,
            TypeDef::Enum(def) => &def.name,
            TypeDef::Primitive(def) => def.name,
            TypeDef::Option(def) => &def.name,
            TypeDef::Box(def) => &def.name,
            TypeDef::Vec(def) => &def.name,
            TypeDef::Cell(def) => &def.name,
        }
    }

    /// Get whether type is visitable.
    ///
    /// Returns `true` if type is tagged `#[ast(visit)]`.
    pub fn is_visitable(&self) -> bool {
        match self {
            TypeDef::Struct(def) => def.is_visitable,
            TypeDef::Enum(def) => def.is_visitable,
            _ => false,
        }
    }
}

#[derive(Debug)]
pub struct StructDef {
    pub name: String,
    pub has_lifetime: bool,
    pub fields: Vec<FieldDef>,
    pub is_visitable: bool,
    pub file_id: FileId,
    pub item: ItemStruct,
}

#[derive(Debug)]
pub struct EnumDef {
    pub name: String,
    pub has_lifetime: bool,
    pub variants: Vec<VariantDef>,
    /// For `@inherits` inherited enum variants
    pub inherits: Vec<TypeId>,
    pub is_visitable: bool,
    pub file_id: FileId,
    pub item: ItemEnum,
}

#[derive(Debug)]
pub struct VariantDef {
    pub name: String,
    pub fields: Vec<FieldDef>,
    pub discriminant: Discriminant,
}

#[derive(Debug)]
pub struct FieldDef {
    /// `None` if unnamed field
    pub name: Option<String>,
    pub type_id: TypeId,
}

#[derive(Debug)]
pub struct PrimitiveDef {
    pub name: &'static str,
}

#[derive(Debug)]
pub struct OptionDef {
    pub name: String,
    pub inner_type_id: TypeId,
}

#[derive(Debug)]
pub struct BoxDef {
    pub name: String,
    pub inner_type_id: TypeId,
}

#[derive(Debug)]
pub struct VecDef {
    pub name: String,
    pub inner_type_id: TypeId,
}

#[derive(Debug)]
pub struct CellDef {
    pub name: String,
    pub inner_type_id: TypeId,
}
