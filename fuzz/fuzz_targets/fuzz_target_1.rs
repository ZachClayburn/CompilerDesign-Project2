#![no_main]
use compiler_design_project_2::{parser::parse, scanner::Scanner};
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    if let Ok(s) = std::str::from_utf8(data) {
        let scan = Scanner::from_text(s);
        let _ = parse(scan);
    }
});
