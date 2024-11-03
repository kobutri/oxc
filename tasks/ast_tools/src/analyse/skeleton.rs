use syn::{ItemEnum, ItemStruct};

use super::FileId;

#[derive(Debug)]
pub enum Skeleton {
    Struct(StructSkeleton),
    Enum(EnumSkeleton),
}

#[derive(Debug)]
pub struct StructSkeleton {
    pub name: String,
    pub item: ItemStruct,
    pub file_id: FileId,
}

#[derive(Debug)]
pub struct EnumSkeleton {
    pub name: String,
    pub item: ItemEnum,
    pub inherits: Vec<String>,
    pub file_id: FileId,
}

impl Skeleton {
    pub fn name(&self) -> &str {
        match self {
            Self::Struct(skeleton) => skeleton.name.as_str(),
            Self::Enum(skeleton) => skeleton.name.as_str(),
        }
    }
}
