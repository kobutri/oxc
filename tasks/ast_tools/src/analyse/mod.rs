use std::{hash::BuildHasherDefault, path::PathBuf};

use indexmap::{IndexMap, IndexSet};
use itertools::Itertools;
use rustc_hash::FxHasher;

use crate::{log, log_success};

mod defs;
mod load;
mod parse;
mod schema;
mod skeleton;
use load::load_file;
use parse::parse;
use schema::{File, FileId, Schema};
use skeleton::Skeleton;

type FxIndexMap<K, V> = IndexMap<K, V, BuildHasherDefault<FxHasher>>;
type FxIndexSet<K> = IndexSet<K, BuildHasherDefault<FxHasher>>;

/// Analyse the files with provided paths, and generate a `Schema`.
pub fn analyse(file_paths: &[&str]) -> Schema {
    // Load files and populate `Vec` of skeletons + mapping from type name to `TypeId`.
    // `TypeId` is index into `skeletons`.
    let mut skeletons = FxIndexMap::default();

    let files = file_paths
        .iter()
        .enumerate()
        .map(|(file_id, &file_path)| analyse_file(file_id, file_path, &mut skeletons))
        .collect::<Vec<_>>();

    // Convert skeletons into schema
    parse(skeletons, files)
}

/// Analyse file with provided path and add types to `skeletons`.
///
/// Returns a `File`.
fn analyse_file(
    file_id: FileId,
    file_path: &str,
    skeletons: &mut FxIndexMap<String, Skeleton>,
) -> File {
    log!("Load {file_path}... ");
    let import_path = get_import_path(file_path);
    load_file(file_id, file_path, skeletons);
    log_success!();

    File { file_path: file_path.to_string(), import_path }
}

/// Convert file path to import path.
/// `crates/oxc_ast/src/ast/js.rs` -> `oxc_ast::ast::js`.
fn get_import_path(file_path: &str) -> String {
    // Remove extension
    let path = PathBuf::from(file_path).with_extension("");
    let path = path.to_string_lossy();

    let mut parts = path.split('/');
    assert_eq!(parts.next(), Some("crates"));
    let krate = parts.next().unwrap();
    assert_eq!(parts.next(), Some("src"));
    let parts = parts.filter(|&part| part != "mod");
    let mut parts = [krate].into_iter().chain(parts);
    parts.join("::")
}
