use std::{io, env, fs};
use std::path::Path;

fn main() {
    match main_result() {
        Ok(()) => (),
        Err(e) => panic!("{:?}", e),
    }
}

fn main_result() -> io::Result<()> {
    println!("rerun-if-changed=build.rs");

    let out_dir = Path::new(&env::var("OUT_DIR").unwrap()).join("xcbproto-rs");
    let schema_dir = concat!(env!("CARGO_MANIFEST_DIR"), "/xcbproto/");
    fs::create_dir_all(&out_dir)?;

    xcodegen::process_dir(Path::new(schema_dir), &out_dir)?;

    Ok(())
}
