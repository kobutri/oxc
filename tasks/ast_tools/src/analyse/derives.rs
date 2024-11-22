use std::fmt::{self, Debug};

use crate::derives::DeriveId;

/// Number of bytes required for bit set which can represent all `DeriveId`s.
const NUM_BYTES: usize = (DeriveId::MAX_USIZE / 8) + 1;

/// Bit set with a bit for each `DeriveId`.
#[derive(Clone, Copy)]
pub struct Derives([u8; NUM_BYTES]);

// Public methods
impl Derives {
    pub const fn none() -> Self {
        Self([0; NUM_BYTES])
    }

    pub const fn all() -> Self {
        let mut out = Self::none();

        let mut index = 0;
        while index <= DeriveId::MAX_USIZE {
            if let Some(id) = DeriveId::try_from_usize(index) {
                out = out.with(id);
            }
            index += 1;
        }

        out
    }

    pub const fn has(self, id: DeriveId) -> bool {
        self.has_index(id.to_usize())
    }

    pub const fn with(mut self, id: DeriveId) -> Self {
        let (byte_index, mask) = Self::index_and_mask_of_id(id);
        self.0[byte_index] |= mask;
        self
    }

    pub const fn without(mut self, id: DeriveId) -> Self {
        let (byte_index, mask) = Self::index_and_mask_of_id(id);
        self.0[byte_index] &= !mask;
        self
    }
}

// Internal methods
impl Derives {
    const fn has_index(self, index: usize) -> bool {
        let (byte_index, mask) = Self::index_and_mask_of_index(index);
        (self.0[byte_index] & mask) != 0
    }

    const fn index_and_mask_of_id(id: DeriveId) -> (usize, u8) {
        Self::index_and_mask_of_index(id.to_usize())
    }

    const fn index_and_mask_of_index(index: usize) -> (usize, u8) {
        (index / 8, 1u8 << (index & 7))
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
        while self.index <= DeriveId::MAX_USIZE {
            let index = self.index;
            self.index += 1;

            if self.derives.has_index(index) {
                return Some(DeriveId::from_usize(index));
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
