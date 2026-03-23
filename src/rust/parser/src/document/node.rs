use serde::{Deserialize, Serialize};

use crate::document::{
    code::{CodeKind, lower_code_command},
    link::{LinkMode, LinkTarget, lower_link_command},
    list::{ListItem, ListKind, lower_list_command},
    sections::{Argument, lower_section_command},
};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Node {
    Text(String),
    NewLine,
    EscapedChar(char),
    Command {
        name: String,
        options: Option<Vec<Node>>,
        args: Vec<Vec<Node>>,
    },
    List {
        kind: ListKind,
        items: Vec<ListItem>,
    },
    Link {
        label: Vec<Node>,
        target: LinkTarget,
        mode: LinkMode,
    },
    Url {
        href: Vec<Node>,
    },
    Href {
        href: Vec<Node>,
        label: Vec<Node>,
    },
    Section {
        title: Vec<Node>,
        children: Vec<Node>,
    },
    ArgumentTable(Vec<Argument>),
    Code {
        title: Option<String>,
        kind: CodeKind,
        children: Vec<Node>,
    },
}

impl Node {
    fn is_command_named(&self, expected: &str) -> bool {
        matches!(self, Node::Command { name, .. } if name == expected)
    }

    fn is_section_named(&self, expected: &str) -> bool {
        matches!(self, Node::Section { title, .. } if matches!(title.as_slice(), [Node::Text(s)] if s == expected))
    }

    pub fn get_section_key(&self) -> Option<&str> {
        match self {
            Node::Command { name, .. } => Some(name),
            Node::Section { title, .. } => title.first().and_then(|n| match n {
                Node::Text(s) => Some(s.as_str()),
                _ => None,
            }),
            Node::ArgumentTable(_) => Some("Arguments"),
            _ => None,
        }
    }

    pub fn in_skipped_sections<S: AsRef<str>>(&self, sections_to_skip: &[S]) -> bool {
        self.get_section_key().is_some_and(|k| {
            sections_to_skip
                .iter()
                .any(|s| s.as_ref().to_lowercase() == k.to_lowercase())
        })
    }

    pub fn is_title(&self) -> bool {
        self.is_section_named("Title")
    }

    pub fn is_description(&self) -> bool {
        self.is_section_named("Description")
    }

    pub fn is_name(&self) -> bool {
        self.is_section_named("Name")
    }

    pub fn is_item(&self) -> bool {
        self.is_command_named("item")
    }

    pub fn is_itemize(&self) -> bool {
        self.is_command_named("itemize")
    }

    pub fn is_enumerate(&self) -> bool {
        self.is_command_named("enumerate")
    }

    pub fn is_describe(&self) -> bool {
        self.is_command_named("describe")
    }

    pub fn is_text(&self) -> bool {
        matches!(&self, Node::Text(_),)
    }

    pub fn lower(self) -> Self {
        match self {
            Node::Command {
                name,
                options,
                args,
            } => {
                let args = args
                    .into_iter()
                    .map(|group| group.into_iter().map(Node::lower).collect())
                    .collect();

                let options = options.map(|nodes| nodes.into_iter().map(Node::lower).collect());

                let cmd = Node::Command {
                    name: name.clone(),
                    options,
                    args,
                };
                match name.as_str() {
                    "itemize" | "enumerate" | "describe" => lower_list_command(name, cmd),
                    "link" | "url" | "href" | "code" => lower_link_command(name, cmd),
                    "section" | "subsection" | "description" | "details" | "value" | "note"
                    | "seealso" | "author" | "references" | "arguments"
                    | "name" | "title" | "format"
                    | "alias" | "keyword" | "concept" | "docType" => {
                        lower_section_command(name, cmd)
                    }
                    "examples" | "example" | "usage" | "dontrun" | "dontshow" | "donttest" => {
                        lower_code_command(name, cmd)
                    }
                    // Pure metadata — no visual output
                    "encoding" | "Rdversion" => Node::Text(String::new()),
                    _ => cmd,
                }
            }
            other => other,
        }
    }
}
