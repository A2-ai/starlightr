use std::collections::HashMap;
use std::path::Path;

use anyhow::{Context, Result as AnyhowResult};
use fs_err as fs;
use serde::{Deserialize, Serialize};

#[derive(Debug, Default, Clone, PartialEq, Serialize, Deserialize)]
pub struct ExampleOutput {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub txt: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub png: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub html: Option<String>,
}

#[derive(Debug, Default, Clone, PartialEq, Serialize, Deserialize)]
pub struct EmitOptions {
    pub skip_sections: Vec<String>,
    pub section_order: Vec<String>,
    pub include_pagefind: bool,
    #[serde(default)]
    pub external_links: HashMap<String, String>,
    #[serde(default)]
    pub example_outputs: HashMap<String, ExampleOutput>,
}

impl From<Config> for EmitOptions {
    fn from(config: Config) -> Self {
        let mut emit_opts = EmitOptions::default();
        if let Some(r) = config.reference {
            if let Some(s) = r.skip_sections {
                emit_opts.skip_sections = s;
            };

            if let Some(o) = r.section_order {
                emit_opts.section_order = o;
            };

            if let Some(p) = r.include_pagefind {
                emit_opts.include_pagefind = p;
            }
        };

        emit_opts
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

    pub fn with_section_order<S, I>(mut self, order: I) -> Self
    where
        S: Into<String>,
        I: IntoIterator<Item = S>,
    {
        self.section_order = order.into_iter().map(Into::into).collect();
        self
    }

    pub fn with_pagefind(mut self) -> Self {
        self.include_pagefind = true;
        self
    }

    pub fn from_file(path: impl AsRef<Path>) -> AnyhowResult<Self> {
        let config = Config::read_config(path)?;
        Ok(Self::from(config))
    }

    pub fn with_external_links_file(mut self, path: impl AsRef<Path>) -> AnyhowResult<Self> {
        let contents =
            fs::read_to_string(path.as_ref()).context("Failed to read external links file")?;
        self.external_links =
            serde_json::from_str(&contents).context("Failed to parse external links JSON")?;
        Ok(self)
    }

    pub fn with_example_outputs_file(mut self, path: impl AsRef<Path>) -> AnyhowResult<Self> {
        let contents =
            fs::read_to_string(path.as_ref()).context("Failed to read example outputs file")?;
        self.example_outputs =
            serde_json::from_str(&contents).context("Failed to parse example outputs JSON")?;
        Ok(self)
    }
}

#[derive(Debug, Default, Clone, PartialEq, Serialize, Deserialize)]
pub struct ReferenceConfig {
    skip_sections: Option<Vec<String>>,
    section_order: Option<Vec<String>>,
    include_pagefind: Option<bool>,
}

#[derive(Debug, Default, Clone, PartialEq, Serialize, Deserialize)]
struct Config {
    reference: Option<ReferenceConfig>,
}

impl Config {
    pub fn read_config(path: impl AsRef<Path>) -> AnyhowResult<Config> {
        let contents = fs::read_to_string(path.as_ref()).context("Failed to read config file")?;

        let config: Config = toml::from_str(&contents).context("Failed to parse config file")?;

        Ok(config)
    }
}
