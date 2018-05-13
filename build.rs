extern crate peg;
extern crate regex;

use regex::Regex;
use std::io;
use std::io::prelude::*;
use std::path::{Path, PathBuf};
use std::convert::AsRef;
use std::fs::File;
use std::process::exit;
use std::env;

fn main() {
    let mut stderr = io::stderr();
    let input_path: &Path = "grammar.rustpeg".as_ref();

    let mut peg_source = String::new();
    if let Err(e) = File::open(input_path).and_then(|mut x| x.read_to_string(&mut peg_source)) {
        writeln!(stderr, "Could not read PEG input file `{}`: {}", input_path.display(), e).unwrap();
        exit(1);
    }

    println!("cargo:rerun-if-changed={}", input_path.display());

    let result = peg::compile(&peg_source);

    let re = Regex::new(r"fn ([_a-zA-Z0-9]*?) < 'input >").unwrap();

    let original_rust_source = match result {
        Ok(s) => s,
        Err(msg) => {
            writeln!(stderr, "Error compiling PEG grammar").unwrap();
            writeln!(stderr, "{}", msg).unwrap();
            exit(1);
        }
    };

    let rust_source = re.replace_all(&original_rust_source, "fn $1 < 'input, 'a >");

    let out_dir: PathBuf = env::var_os("OUT_DIR").unwrap().into();
    let rust_path = out_dir.join(input_path.file_name().unwrap()).with_extension("rs");

    let mut output_file = File::create(&rust_path).unwrap();
    output_file.write_all(rust_source.as_bytes()).unwrap();
}
