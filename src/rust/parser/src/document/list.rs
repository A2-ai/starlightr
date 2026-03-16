use serde::{Deserialize, Serialize};

use crate::document::Node;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ListKind {
    Itemize,
    Enumerate,
    Describe,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ListItem {
    pub term: Option<Vec<Node>>,
    pub children: Vec<Node>,
}

impl ListItem {
    pub fn new(children: Vec<Node>) -> Self {
        Self {
            term: None,
            children,
        }
    }

    pub fn has_term(&self) -> bool {
        match &self.term {
            Some(t) => !t.is_empty(),
            None => false,
        }
    }

    pub fn term_nodes(&self) -> Option<&[Node]> {
        self.term.as_deref()
    }

    pub fn content_nodes(&self) -> &[Node] {
        self.children.as_ref()
    }
}

fn lower_simple_list(node: Node, kind: ListKind) -> Node {
    let Node::Command { args, .. } = node else {
        return node;
    };

    let mut items = Vec::new();
    let mut current = Vec::new();
    let body = args.into_iter().next().unwrap_or_default();
    let mut seen_item = false;

    for node in body {
        if node.is_item() {
            if !current.is_empty() {
                items.push(ListItem::new(current));
                current = Vec::new();
            }
            seen_item = true;
        } else if seen_item {
            current.push(node);
        } else {
            continue;
        }
    }

    if seen_item && !current.is_empty() {
        items.push(ListItem::new(current));
    }

    Node::List { kind, items }
}

fn lower_itemize(node: Node) -> Node {
    // \itemize{
    // \item 1: Underweight: BMI < 18.5 kg/m²
    // \item 2: Normal weight: BMI 18.5 to < 25 kg/m²
    // }
    //
    // Command { 'itemize', None, [command(item), text(words)] }
    lower_simple_list(node, ListKind::Itemize)
}

fn lower_enumerate(node: Node) -> Node {
    lower_simple_list(node, ListKind::Enumerate)
}

fn lower_describe(node: Node) -> Node {
    // handles \describe

    // outer Vec<Vec<Node>> args
    let Node::Command { args, .. } = node else {
        return node;
    };

    let mut items = Vec::new();
    let body = args.into_iter().next().unwrap_or_default();

    for node in body {
        if !node.is_item() {
            continue;
        }
        // Inner Vec<Vec<Node>>
        let Node::Command { args, .. } = node else {
            continue;
        };

        let mut it = args.into_iter();
        let term = it.next();
        let children = it.next().unwrap_or_default();

        items.push(ListItem { term, children });
    }

    Node::List {
        kind: ListKind::Describe,
        items,
    }
}

pub(crate) fn lower_list_command(name: &str, cmd: Node) -> Node {
    match name {
        "itemize" => lower_itemize(cmd),
        "enumerate" => lower_enumerate(cmd),
        "describe" => lower_describe(cmd),
        _ => cmd,
    }
}
