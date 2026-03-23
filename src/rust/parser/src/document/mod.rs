mod code;
mod link;
mod list;
mod node;
mod sections;

pub use code::CodeKind;
pub use link::{LinkMode, LinkTarget};
pub use list::{ListItem, ListKind};
pub use node::Node;
pub use sections::Argument;

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Document {
    pub children: Vec<Node>,
}

impl Document {
    pub fn get_title_node(&self) -> Option<&Node> {
        self.children.iter().find(|n| n.is_title())
    }

    pub fn get_name_node(&self) -> Option<&Node> {
        self.children.iter().find(|n| n.is_name())
    }

    pub fn get_description_node(&self) -> Option<&Node> {
        self.children.iter().find(|n| n.is_description())
    }

    pub fn filter_sections<S: AsRef<str>>(&mut self, skip_sections: &[S]) {
        self.children
            .retain(|n| !n.in_skipped_sections(skip_sections));
    }

    pub fn order_sections<S: AsRef<str>>(&mut self, order: &[S]) {
        if order.is_empty() {
            return;
        }
        let mut ordered = Vec::new();
        let mut remaining = std::mem::take(&mut self.children);

        for section_name in order {
            let (matched, rest): (Vec<_>, Vec<_>) = remaining
                .into_iter()
                .partition(|n| {
                    n.get_section_key()
                        .is_some_and(|k| k.to_lowercase() == section_name.as_ref().to_lowercase())
                });
            ordered.extend(matched);
            remaining = rest;
        }
        ordered.extend(remaining);
        self.children = ordered;
    }

    pub fn lower(self) -> Self {
        let children = self.children.into_iter().map(Node::lower).collect();
        Self { children }
    }
}
