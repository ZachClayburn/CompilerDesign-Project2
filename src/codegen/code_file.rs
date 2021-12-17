use indoc::indoc;
use std::fmt::Display;

use itertools::Itertools;

pub(super) struct CodeFile {
    pub rodata: Vec<String>,
    pub data: Vec<String>,
    pub bss: Vec<String>,
    pub procedures: Vec<String>,
    pub main: Vec<String>,
}

impl Default for CodeFile {
    fn default() -> Self {
        Self {
            rodata: vec![
                r#"numPrinter db "%d",0x0a,0"#.to_owned(),
                r#"ishPrinter db "%f",0x0a,0"#.to_owned(),
                r#"stringPrinter db "%s",0x0a,0"#.to_owned(),
                r#"numReader db "%d",0"#.to_owned(),
                r#"ishReader db "%f",0"#.to_owned(),
            ],
            data: Default::default(),
            bss: Default::default(),
            procedures: vec![],
            main: Default::default(),
        }
    }
}

impl CodeFile {
    pub(super) fn new() -> Self {
        Self::default()
    }
}

impl Display for CodeFile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let rodata = if !self.rodata.is_empty() {
            "\n".to_owned() + &self.rodata.join("\n")
        } else {
            "".to_owned()
        };
        let data = if !self.data.is_empty() {
            "\n".to_owned() + &self.data.join("\n")
        } else {
            "".to_owned()
        };
        let bss = if !self.bss.is_empty() {
            "\n".to_owned() + &self.bss.join("\n")
        } else {
            "".to_owned()
        };
        let procedures = if !self.procedures.is_empty() {
            "\n".to_owned() + &self.procedures.join("\n")
        } else {
            "".to_owned()
        };
        let main = if !self.main.is_empty() {
            "\n".to_owned() + &self.main.join("\n")
        } else {
            "".to_owned()
        };
        write!(
            f,
            indoc! {"
            global main
            extern printf
            extern scanf
            section .rodata{}
            section .data{}
            section .bss{}
            section .text{}
            main:{}
            "},
            rodata, data, bss, procedures, main
        )
    }
}
