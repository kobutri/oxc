use std::path::PathBuf;

use itertools::Itertools;
use rustc_hash::FxHashMap;

use crate::{log, log_success};

mod defs;
mod load;
mod parse;
mod skeleton;
use load::load_file;
use parse::parse;

type FileId = usize;
type TypeId = usize;

#[derive(Debug)]
pub struct File {
    #[expect(dead_code)]
    pub file_path: String,
    pub import_path: String,
}

pub fn analyse(file_paths: &[&str]) {
    // Load files and populate `Vec` of skeletons + mapping from type name to `TypeId`.
    // `TypeId` is index into `skeletons`.
    let mut skeletons = vec![];
    let mut names_to_ids = FxHashMap::default();

    let files = file_paths
        .iter()
        .enumerate()
        .map(|(file_id, &file_path)| {
            log!("Load {file_path}... ");
            let import_path = get_import_path(file_path);
            load_file(file_id, file_path, &mut skeletons, &mut names_to_ids);
            let file_path = file_path.to_string();
            log_success!();
            File { file_path, import_path }
        })
        .collect::<Vec<_>>();

    // Convert skeletons into schema
    let _defs = parse(skeletons, &mut names_to_ids, &files);
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
