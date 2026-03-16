use serde::{Deserialize, Serialize};

use crate::document::Node;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum LinkTarget {
    Local(Vec<Node>),
    Package {
        package: Vec<Node>,
        topic: Vec<Node>,
    },
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum LinkMode {
    Text,
    Code,
}

fn normalize_link(mut nodes: Vec<Node>) -> Vec<Node> {
    let Some(first) = nodes.first_mut() else {
        return nodes;
    };

    if let Node::Text(s) = first {
        let normal = s
            .trim()
            .trim_matches(['(', ')'])
            .to_lowercase()
            .replace('.', "-")
            .replace("/", "");
        *s = normal;
    }

    nodes
}

fn strip_leading_equals(mut nodes: Vec<Node>) -> Vec<Node> {
    let Some(first) = nodes.first_mut() else {
        return nodes;
    };

    match first {
        Node::Text(s) => {
            if let Some(stripped) = s.strip_prefix('=') {
                *s = stripped.to_string();
            }
        }
        Node::EscapedChar('=') => {
            nodes.remove(0);
        }
        _ => {}
    }

    nodes
}

fn split_package_topic(mut nodes: Vec<Node>) -> (Vec<Node>, Option<Vec<Node>>) {
    let Some(first) = nodes.first_mut() else {
        return (Vec::new(), None);
    };

    if let Node::Text(s) = first
        && let Some((package, topic)) = s.split_once(':')
    {
        let package_nodes = vec![Node::Text(package.to_string())];
        let mut topic_nodes = vec![Node::Text(topic.to_string())];
        topic_nodes.extend(nodes.into_iter().skip(1));
        return (package_nodes, Some(topic_nodes));
    }

    (nodes, None)
}

fn create_target(label: Vec<Node>, options: Option<Vec<Vec<Node>>>) -> LinkTarget {
    match options.and_then(|opts| opts.into_iter().next()) {
        None => {
            let target = normalize_link(label.clone());
            LinkTarget::Local(target)
        }
        Some(option_nodes) => {
            let is_local = matches!(
                option_nodes.first(),
                Some(Node::Text(s)) if s.starts_with('=')
            ) || matches!(option_nodes.first(), Some(Node::EscapedChar('=')));

            if is_local {
                let target = strip_leading_equals(option_nodes);
                let target = normalize_link(target);
                LinkTarget::Local(target)
            } else {
                let (package, topic) = split_package_topic(option_nodes);
                let topic = topic.unwrap_or_else(|| label.clone());
                LinkTarget::Package { package, topic }
            }
        }
    }
}

fn absorb_code_suffix(mut label: Vec<Node>, rest: &[Node]) -> Option<Vec<Node>> {
    for node in rest {
        match node {
            Node::Text(_) | Node::EscapedChar(_) => label.push(node.clone()),
            Node::NewLine => {}
            _ => return None,
        }
    }

    Some(label)
}

fn lower_code_link(node: Node) -> Node {
    let Node::Command { name, args, .. } = &node else {
        return node;
    };

    if name != "code" {
        return node;
    };
    let [group] = args.as_slice() else {
        return node;
    };
    let Some((first, rest)) = group.split_first() else {
        return node;
    };

    let base_link = match first {
        Node::Link { label, target, .. } => Some((label.clone(), target.clone())),
        Node::Command { name, .. } if name == "link" => match lower_link(first.clone()) {
            Node::Link { label, target, .. } => Some((label, target)),
            _ => None,
        },
        _ => None,
    };

    let Some((label, target)) = base_link else {
        return node;
    };

    let Some(label) = absorb_code_suffix(label, rest) else {
        return node;
    };

    Node::Link {
        label,
        target,
        mode: LinkMode::Code,
    }
}

fn lower_link(node: Node) -> Node {
    let Node::Command { args, options, .. } = node else {
        return node;
    };

    let mut arg_it = args.into_iter();
    let label = arg_it.next().unwrap_or_default();
    let target = create_target(label.clone(), options);

    Node::Link {
        label,
        target,
        mode: LinkMode::Text,
    }
}

fn lower_url(node: Node) -> Node {
    let Node::Command { args, .. } = node else {
        return node;
    };

    let href = args.into_iter().next().unwrap_or_default();
    Node::Url { href }
}

fn lower_href(node: Node) -> Node {
    let Node::Command { args, .. } = node else {
        return node;
    };

    let mut it = args.into_iter();
    let href = it.next().unwrap_or_default();
    let label = it.next().unwrap_or_default();

    Node::Href { href, label }
}

pub(crate) fn lower_link_command(name: &str, cmd: Node) -> Node {
    match name {
        "link" => lower_link(cmd),
        "code" => lower_code_link(cmd),
        "url" => lower_url(cmd),
        "href" => lower_href(cmd),
        _ => cmd,
    }
}
