use std::fmt::{self, Debug};

use crate::derives::DeriveId;

/// Number of bytes required for bit set which can represent all `DeriveId`s.
const NUM_BYTES: usize = (DeriveId::MAX_VALUE as usize / 8) + 1;

/// Bit set with a bit for each `DeriveId`.
#[derive(Clone, Copy)]
pub struct Derives([u8; NUM_BYTES]);

impl Derives {
    pub const fn none() -> Self {
        Self([0; NUM_BYTES])
    }

    pub const fn all() -> Self {
        let mut out = Self::none();
        let mut index = 0;
        while index <= DeriveId::VARIANTS.len() {
            out = out.with(DeriveId::VARIANTS[index]);
            index += 1;
        }
        out
    }

    pub const fn has(self, id: DeriveId) -> bool {
        let (byte_index, mask) = Self::byte_index_and_mask(id);
        (self.0[byte_index] & mask) != 0
    }

    pub const fn with(mut self, id: DeriveId) -> Self {
        let (byte_index, mask) = Self::byte_index_and_mask(id);
        self.0[byte_index] |= mask;
        self
    }

    pub const fn without(mut self, id: DeriveId) -> Self {
        let (byte_index, mask) = Self::byte_index_and_mask(id);
        self.0[byte_index] &= !mask;
        self
    }

    pub fn add(&mut self, id: DeriveId) {
        *self = self.with(id);
    }

    pub fn remove(&mut self, id: DeriveId) {
        *self = self.without(id);
    }

    const fn byte_index_and_mask(id: DeriveId) -> (usize, u8) {
        let value = id.to_usize();
        (value / 8, 1u8 << (value & 7))
    }
}

impl IntoIterator for Derives {
    type Item = DeriveId;
    type IntoIter = DerivesIter;

    fn into_iter(self) -> DerivesIter {
        DerivesIter::new(self)
    }
}

impl IntoIterator for &Derives {
    type Item = DeriveId;
    type IntoIter = DerivesIter;

    fn into_iter(self) -> DerivesIter {
        DerivesIter::new(*self)
    }
}

pub struct DerivesIter {
    derives: Derives,
    index: usize,
}

impl DerivesIter {
    fn new(derives: Derives) -> Self {
        Self { derives, index: 0 }
    }
}

impl Iterator for DerivesIter {
    type Item = DeriveId;

    fn next(&mut self) -> Option<DeriveId> {
        while self.index < DeriveId::VARIANTS.len() {
            let id = DeriveId::VARIANTS[self.index];
            self.index += 1;

            if self.derives.has(id) {
                return Some(id);
            }
        }

        None
    }
}

impl Debug for Derives {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list().entries(self).finish()
    }
}
