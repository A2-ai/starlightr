use serde::{Deserialize, Serialize};

use crate::document::{ListItem, ListKind, Node};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Argument {
    pub name: Vec<Node>,
    pub description: Vec<Node>,
}

fn section_title(name: impl AsRef<str>) -> Vec<Node> {
    let title = match name.as_ref() {
        "description" => "Description",
        "details" => "Details",
        "value" => "Value",
        "note" => "Note",
        "seealso" => "See Also",
        "author" => "Author",
        "references" => "References",
        "arguments" => "Arguments",
        "name" => "Name",
        "title" => "Title",
        "format" => "Format",
        "alias" => "Alias",
        "keyword" => "Keyword",
        "concept" => "Concept",
        "docType" => "Doc Type",
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

    args.into_iter()
        .next()
        .map(lower_argument_items)
        .map(Node::ArgumentTable)
        .unwrap_or_else(|| Node::ArgumentTable(Vec::new()))
}

/// Collapse maximal runs of bare `\item{name}{desc}` commands into
/// description lists.
///
/// Real Rd files sometimes use `\item` pairs directly inside a section like
/// `\value` without wrapping them in `\describe` (e.g. base R's
/// `.Platform`). Lowering only special-cases `\item` inside `\itemize`,
/// `\enumerate`, `\describe`, and `\arguments` — an `\item` anywhere else
/// survives as a plain `item` command, which the emitter doesn't know how to
/// render and drops. Since a bare `\item{name}{desc}` pair reads exactly
/// like a `\describe` entry, render it the same way.
fn group_bare_item_pairs(children: Vec<Node>) -> Vec<Node> {
    let mut result = Vec::new();
    let mut pending: Vec<ListItem> = Vec::new();

    for node in children {
        match node {
            Node::Command { name, args, .. } if name == "item" => {
                let mut it = args.into_iter();
                let term = it.next();
                let children = it.next().unwrap_or_default();
                pending.push(ListItem { term, children });
            }
            // Rd source layout (newlines/indentation) between `\item`s is
            // not content — skip it while a run is open rather than let it
            // split one logical run of items into many single-item lists.
            ref node if !pending.is_empty() && is_insignificant_whitespace(node) => {}
            other => {
                flush_pending_items(&mut pending, &mut result);
                result.push(other);
            }
        }
    }
    flush_pending_items(&mut pending, &mut result);

    result
}

fn is_insignificant_whitespace(node: &Node) -> bool {
    matches!(node, Node::NewLine) || matches!(node, Node::Text(s) if s.trim().is_empty())
}

fn flush_pending_items(pending: &mut Vec<ListItem>, result: &mut Vec<Node>) {
    if pending.is_empty() {
        return;
    }
    result.push(Node::List {
        kind: ListKind::Describe,
        items: std::mem::take(pending),
    });
}

fn lower_titled_section(title: Vec<Node>, node: Node) -> Node {
    let Node::Command { args, .. } = node else {
        return node;
    };

    let children = args.into_iter().next().unwrap_or_default();
    let children = group_bare_item_pairs(children);

    Node::Section { title, children }
}

fn lower_section(node: Node) -> Node {
    let Node::Command { args, .. } = node else {
        return node;
    };

    let mut it = args.into_iter();
    let title = it.next().unwrap_or_default();
    let children = it.next().unwrap_or_default();

    Node::Section { title, children }
}

pub(crate) fn lower_section_command(name: impl AsRef<str>, cmd: Node) -> Node {
    match name.as_ref() {
        "arguments" => lower_arguments(cmd),
        "section" | "subsection" => lower_section(cmd),
        "description" | "details" | "value" | "note" | "seealso" | "author" | "references"
        | "name" | "title" | "format" | "alias" | "keyword" | "concept" | "docType" => {
            lower_titled_section(section_title(name), cmd)
        }
        _ => cmd,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn item(name: &str, desc: &str) -> Node {
        Node::Command {
            name: "item".to_string(),
            options: None,
            args: vec![
                vec![Node::Text(name.to_string())],
                vec![Node::Text(desc.to_string())],
            ],
        }
    }

    fn value_command(children: Vec<Node>) -> Node {
        Node::Command {
            name: "value".to_string(),
            options: None,
            args: vec![children],
        }
    }

    #[test]
    fn bare_item_pairs_in_value_become_one_describe_list() {
        let cmd = value_command(vec![
            item("OS.type", "the OS family"),
            Node::NewLine,
            item("file.sep", "the file separator"),
        ]);

        let lowered = lower_section_command("value", cmd);

        let Node::Section { children, .. } = lowered else {
            panic!("expected a Section node");
        };
        assert_eq!(children.len(), 1, "one list, not one per item: {children:?}");
        let Node::List { kind, items } = &children[0] else {
            panic!("expected a List node, got {:?}", children[0]);
        };
        assert_eq!(*kind, ListKind::Describe);
        assert_eq!(items.len(), 2);
        assert_eq!(items[0].term_nodes(), Some(&[Node::Text("OS.type".into())][..]));
    }

    #[test]
    fn prose_before_bare_items_is_preserved() {
        let cmd = value_command(vec![
            Node::Text("A list with at least the following components:".to_string()),
            Node::NewLine,
            item("OS.type", "the OS family"),
        ]);

        let lowered = lower_section_command("value", cmd);

        let Node::Section { children, .. } = lowered else {
            panic!("expected a Section node");
        };
        // The intro prose survives, and the item(s) after it become exactly
        // one list — not silently dropped by the item-grouping pass. (The
        // NewLine between the prose and the first `\item` is preserved too,
        // since whitespace is only insignificant *between* items in an
        // already-open run, not before the run starts.)
        assert!(matches!(&children[0], Node::Text(s) if s.contains("following components")));
        let lists: Vec<_> = children
            .iter()
            .filter(|n| matches!(n, Node::List { kind: ListKind::Describe, .. }))
            .collect();
        assert_eq!(lists.len(), 1, "{children:?}");
    }

    #[test]
    fn describe_block_is_still_handled_by_its_own_lowering() {
        // A real `\describe{...}` should be untouched by this pass — it's
        // already a single `describe` Command node, not a bare `\item` run.
        let describe = Node::Command {
            name: "describe".to_string(),
            options: None,
            args: vec![vec![item("a", "desc a")]],
        };
        let cmd = value_command(vec![describe.clone()]);

        let lowered = lower_section_command("value", cmd);

        let Node::Section { children, .. } = lowered else {
            panic!("expected a Section node");
        };
        assert_eq!(children, vec![describe]);
    }
}
