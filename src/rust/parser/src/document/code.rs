use serde::{Deserialize, Serialize};

use crate::document::Node;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum CodeKind {
    Plain,
    DontRun,
    DontTest,
    DontShow,
}

fn lower_code(node: Node) -> Node {
    let Node::Command { name, args, .. } = node else {
        return node;
    };
    
    let title = match name.as_str() {
        "examples" | "example" => Some("Examples".to_string()),
        "usage" => Some("Usage".to_string()),
        _ => None,
    };

    let kind = match name.as_str() {
        "dontrun" => CodeKind::DontRun,
        "dontshow" => CodeKind::DontShow,
        "donttest" => CodeKind::DontTest,
        _ => CodeKind::Plain,
    };

    let children = args.into_iter().next().unwrap_or_default();

    Node::Code { title, kind, children }
}

pub(crate) fn lower_code_command(name: impl AsRef<str>, cmd: Node) -> Node {
    match name.as_ref() {
        "examples" | "example" | "usage" | "dontrun" | "donttest" | "dontshow" => lower_code(cmd),
        _ => cmd,
    }
}
