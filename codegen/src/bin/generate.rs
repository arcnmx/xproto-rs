use std::io;
use std::path::Path;

fn main() -> io::Result<()> {
    xcodegen::process_dir(Path::new("../xcbproto/src"), Path::new("out"))
}
