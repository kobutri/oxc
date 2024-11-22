use rustc_hash::FxHashMap;

use crate::derives::{Derive, DeriveId};

#[derive(Default)]
pub struct CodegenBuilder {
    derives: Vec<Box<dyn Derive>>,
}

impl CodegenBuilder {
    pub fn add_derive<D: Derive + 'static>(mut self, derive: D) -> Self {
        self.derives.push(Box::new(derive));
        self
    }

    pub fn into_codegen(self) -> Codegen {
        Codegen::new(self.derives)
    }
}

pub struct Codegen {
    derives: Vec<Option<Box<dyn Derive>>>,
    derives_name_to_id: FxHashMap<&'static str, DeriveId>,
    #[expect(dead_code)]
    type_attrs: FxHashMap<&'static str, DeriveId>,
    #[expect(dead_code)]
    field_attrs: FxHashMap<&'static str, DeriveId>,
}

impl Codegen {
    pub fn new(unordered_derives: Vec<Box<dyn Derive>>) -> Self {
        // Put derives in `Vec` indexed by `DeriveId`,
        // Populate hashmap mapping derive name to `DeriveId`.
        // Populate hashmaps mapping type and field attrs to `DeriveId` which handles them.
        let mut derives = vec![];
        let mut derives_name_to_id = FxHashMap::default();
        let mut type_attrs = FxHashMap::default();
        let mut field_attrs = FxHashMap::default();
        for derive in unordered_derives {
            let id = derive.id();
            let index = id.to_usize();
            while derives.len() < index + 1 {
                derives.push(None);
            }

            let old_derive = derives[index].replace(derive);
            if let Some(old_derive) = old_derive {
                panic!("Same derive added twice: {}", old_derive.trait_name());
            }
            let derive = derives[index].as_ref().unwrap().as_ref();

            derives_name_to_id.insert(id.name(), id);

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

        Self { derives, derives_name_to_id, type_attrs, field_attrs }
    }

    pub fn get_derive(&self, id: DeriveId) -> &dyn Derive {
        self.derives[id.to_usize()].as_ref().unwrap().as_ref()
    }

    pub fn get_derive_id_by_name(&self, name: &str) -> DeriveId {
        self.derives_name_to_id.get(name).copied().unwrap_or_else(|| {
            panic!("Unknown derive trait {name:?}");
        })
    }

    #[expect(dead_code)]
    pub fn get_derive_by_name(&self, name: &str) -> &dyn Derive {
        self.get_derive(self.get_derive_id_by_name(name))
    }

    #[expect(dead_code)]
    pub fn derives(&self) -> impl Iterator<Item = &dyn Derive> {
        self.derives.iter().filter_map(Option::as_ref).map(AsRef::as_ref)
    }
}
