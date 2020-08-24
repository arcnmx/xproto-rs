use std::io::{self, Read, Write};
use std::fs;
use std::path::{Path, PathBuf};
use super::{generate, generate_meta};

pub fn process_dir(input_dir_path: &Path, output_dir_path: &Path) -> io::Result<()> {
    let xml_files = list_xmls(input_dir_path)?;
    let module = xcbgen::defs::Module::new();
    let mut parser = xcbgen::Parser::new(module.clone());
    for file_path in xml_files.iter() {
        eprintln!("Loading {:?}", file_path);
        println!("rerun-if-changed={}", file_path.display());
        load_namespace(file_path, &mut parser)?;
    }

    eprintln!("{} XMLs loaded", module.namespaces.borrow().len());

    xcbgen::resolve(&module)
        .map_err(|e| io::Error::new(io::ErrorKind::Other, format!("{:?}", e)))?;

    let mod_rs = output_dir_path.join("mod.rs");
    let mut mod_rs = fs::File::create(mod_rs)?;
    for ns in module.sorted_namespaces() {
        let header = match &ns.header[..] {
            "xproto" => "xcore",
            header => header,
        };
        let outpath = output_dir_path.join(format!("{}.rs", header));
        eprintln!("{}", outpath.display());
        let outfile = fs::File::create(outpath)?;
        generate(outfile, &ns)?;

        writeln!(&mut mod_rs)?;
        if ns.ext_info.is_some() {
            writeln!(&mut mod_rs, "#[cfg(feature = \"{}\")]", header)?;
        }
        writeln!(&mut mod_rs, "pub mod {};", header)?;
    }
    writeln!(&mut mod_rs)?;
    generate_meta(&mut mod_rs, module.sorted_namespaces())?;

    /*println!("[features]");
    for ns in module.sorted_namespaces() {
        let deps: String = ns.imports.borrow().iter()
            .filter(|(name, _)| &name[..] != "xproto")
            .map(|(name, _)| format!("\"{}\",", name.to_owned()))
            .collect();
        if ns.ext_info.is_some() {
            println!("{} = [{}]", ns.header, deps);
        }
    }*/

    Ok(())
}

fn list_xmls(dir_path: &Path) -> io::Result<Vec<PathBuf>> {
    let mut files = Vec::new();
    let dir_reader = fs::read_dir(dir_path)?;
    for entry in dir_reader {
        let entry = entry?;
        let file_path = entry.path();
        if file_path.extension() == Some(std::ffi::OsStr::new("xml")) {
            files.push(file_path);
        }
    }
    files.sort();
    Ok(files)
}

fn load_namespace(path: &Path, parser: &mut xcbgen::Parser) -> io::Result<()> {
    let file_bytes = read_file(path)?;
    let file_string = String::from_utf8(file_bytes)
        .map_err(|e| io::Error::new(io::ErrorKind::Other, e))?;
    let xml_doc = roxmltree::Document::parse(&file_string)
        .map_err(|e| io::Error::new(io::ErrorKind::Other, e))?;
    parser.parse_namespace(xml_doc.root().first_element_child().unwrap())
        .map_err(|e| io::Error::new(io::ErrorKind::Other, format!("{:?}", e)))?;
    Ok(())
}


fn read_file(path: &Path) -> io::Result<Vec<u8>> {
    let mut file = fs::OpenOptions::new()
        .read(true)
        .write(false)
        .open(path)?;

    let mut buf = Vec::new();
    file.read_to_end(&mut buf)?;
    Ok(buf)
}
