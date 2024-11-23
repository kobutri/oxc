use rustc_hash::FxHashMap;

use crate::{derives::Derive, DERIVES};

pub type DeriveId = usize;

pub struct Codegen {
    /// Mapping from derive name to `DeriveId`
    derive_name_to_id: FxHashMap<&'static str, DeriveId>,
    /// Mapping from type attr to `DeriveId` of derive which uses the attr
    #[expect(dead_code)]
    type_attrs: FxHashMap<&'static str, DeriveId>,
    /// Mapping from field attr to `DeriveId` of derive which uses the attr
    #[expect(dead_code)]
    field_attrs: FxHashMap<&'static str, DeriveId>,
}

impl Codegen {
    pub fn new() -> Self {
        let mut derive_name_to_id = FxHashMap::default();
        let mut type_attrs = FxHashMap::default();
        let mut field_attrs = FxHashMap::default();

        for (id, &derive) in DERIVES.iter().enumerate() {
            derive_name_to_id.insert(derive.trait_name(), id);

            for &type_attr in derive.type_attrs() {
                let old_id = type_attrs.insert(type_attr, id);
                if let Some(old_id) = old_id {
                    panic!(
                        "Two derives expect same type attr {type_attr:?}: {old_id:?} and {id:?}"
                    );
                }
            }

            for &field_attr in derive.field_attrs() {
                let old_id = field_attrs.insert(field_attr, id);
                if let Some(old_id) = old_id {
                    panic!(
                        "Two derives expect same field attr {field_attr:?}: {old_id:?} and {id:?}"
                    );
                }
            }
        }

        Self { derive_name_to_id, type_attrs, field_attrs }
    }

    pub fn get_derive(&self, id: DeriveId) -> &'static dyn Derive {
        DERIVES[id]
    }

    pub fn get_derive_id_by_name(&self, name: &str) -> DeriveId {
        self.derive_name_to_id.get(name).copied().unwrap_or_else(|| {
            panic!("Unknown derive trait {name:?}");
        })
    }

    #[expect(dead_code)]
    pub fn get_derive_by_name(&self, name: &str) -> &dyn Derive {
        self.get_derive(self.get_derive_id_by_name(name))
    }
}
