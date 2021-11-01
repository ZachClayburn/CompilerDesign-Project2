#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Location {
    pub line: usize,
    pub column: usize,
}

impl Default for Location {
    fn default() -> Self {
        Self { line: 1, column: 0 }
    }
}

impl Location {
    pub fn next_col(&mut self) {
        self.column += 1;
    }

    pub fn advance_col(&mut self, count: usize) {
        self.column += count;
    }

    pub fn next_line(&mut self) {
        self.line += 1;
        self.column = 0;
    }

    pub fn advance_line(&mut self, count: usize) {
        if count == 0 {
            return;
        }
        self.line += count;
        self.column = 0;
    }
}
