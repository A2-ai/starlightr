use serde::{Deserialize, Serialize};

use crate::document::Node;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Argument {
    pub name: Vec<Node>,
    pub description: Vec<Node>,
}

fn section_title(name: &str) -> Vec<Node> {
    let title = match name {
        "description" => "Description",
        "details" => "Details",
        "value" => "Value",
        "note" => "Note",
        "seealso" => "See Also",
        "author" => "Author",
        "references" => "References",
        "arguments" => "Arguments",
        other => other,
    };

    vec![Node::Text(title.to_string())]
}

fn lower_argument_items(nodes: Vec<Node>) -> Vec<Argument> {
    nodes
        .into_iter()
        .filter_map(|node| {
            let Node::Command { name, args, .. } = node else {
                return None;
            };

            if name != "item" {
                return None;
            }

            let mut it = args.into_iter();
            let name = it.next().unwrap_or_default();
            let description = it.next().unwrap_or_default();

            Some(Argument { name, description })
        })
        .collect()
}

fn lower_arguments(node: Node) -> Node {
    let Node::Command { args, .. } = node else {
        return node;
    };

    args
        .into_iter()
        .next()
        .map(lower_argument_items)
        .map(Node::ArgumentTable)
        .unwrap_or_else(|| Node::ArgumentTable(Vec::new()))
}

fn lower_titled_section(title: Vec<Node>, node: Node) -> Node {
    let Node::Command { args, .. } = node else {
        return node;
    };

    let children = args.into_iter().next().unwrap_or_default();

    Node::Section { title, children }
}

fn lower_section(node: Node) -> Node {
    let Node::Command { args, .. } = node else {
        return node;
    };

    let mut it = args.into_iter();
    let title = it.next().unwrap_or_default();
    let children = it.next().unwrap_or_default();

    Node::Section{ title, children }
}

pub(crate) fn lower_section_command(name: &str, cmd: Node) -> Node {
    match name {
        "arguments" => lower_arguments(cmd),
        "section" | "subsection" => lower_section(cmd),
        "description" | "details" | "value" | "note" | "seealso" | "author" | "references" => {
            lower_titled_section(section_title(name), cmd)
        }
        _ => cmd,
    }
}
