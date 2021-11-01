#![no_main]
use libfuzzer_sys::fuzz_target;
use compiler_design_project_2::{scanner::Scanner,parser::parse};

fuzz_target!(|data: &[u8]| {
    if let Ok(s) = std::str::from_utf8(data) {
        let scan = Scanner::from_text(s);
        let _ = parse(scan);
    }

});
