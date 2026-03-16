use serde::{Serialize, Deserialize};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct EmitOptions {
    pub skip_sections: Vec<String>,
    pub include_frontmatter: bool,
    pub include_pagefind: bool,
}

impl Default for EmitOptions {
    fn default() -> Self {
        Self {
            skip_sections: vec![],
            include_frontmatter: true,
            include_pagefind: true
        }
    }
}

impl EmitOptions {
    pub fn with_skip_sections<S, I>(mut self, sections: I) -> Self 
    where 
        S: Into<String>,
        I: IntoIterator<Item = S>,
    {
        self.skip_sections = sections.into_iter().map(Into::into).collect();
        self
    }
}
