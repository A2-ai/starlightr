use std::{collections::BTreeSet, slice};

use anyhow::{Result as AnyhowResult, bail};

use crate::document::Document;
use crate::document::{Argument, CodeKind, LinkMode, LinkTarget, ListItem, ListKind, Node};
use crate::emit::EmitOptions;

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Emitter {
    output: String,
    imports: BTreeSet<String>,
    math_mode_depth: usize,
    code_mode_depth: usize,
}

impl Emitter {
    pub fn new() -> Self {
        Self {
            output: "".to_string(),
            imports: BTreeSet::new(),
            math_mode_depth: 0,
            code_mode_depth: 0,
        }
    }

    fn in_math_mode(&self) -> bool {
        self.math_mode_depth > 0
    }

    fn in_code_mode(&self) -> bool {
        self.code_mode_depth > 0
    }

    fn emit_math_nodes(&mut self, args: &[Vec<Node>]) {
        self.math_mode_depth += 1;
        self.emit_node_group(args);
        self.math_mode_depth -= 1;
    }

    fn render_nodes_to_string(&mut self, nodes: &[Node]) -> String {
        let mut emitter = Emitter::new();
        for node in nodes {
            emitter.emit_node(node);
        }
        self.imports.extend(emitter.imports.clone());
        emitter.output
    }

    fn render_table_cell(&mut self, nodes: &[Node]) -> String {
        self.render_compact_nodes(nodes)
    }

    fn escape_html(text: &str) -> String {
        text.replace('&', "&amp;")
            .replace('<', "&lt;")
            .replace('>', "&gt;")
    }

    fn escape_mdx_text(text: &str) -> String {
        text.replace('<', "\\<")
    }

    fn render_compact_nodes(&mut self, nodes: &[Node]) -> String {
        self.render_nodes_to_string(nodes)
            .split_whitespace()
            .collect::<Vec<_>>()
            .join(" ")
    }

    fn emit_node(&mut self, node: &Node) {
        match node {
            Node::Text(s) => {
                if self.in_code_mode() {
                    self.emit_text(s);
                } else {
                    self.emit_text(&Self::escape_mdx_text(s));
                }
            }
            Node::NewLine => self.emit_text("\n"),
            Node::EscapedChar(c) => {
                let text = c.to_string();
                if self.in_code_mode() {
                    self.emit_text(&text);
                } else {
                    self.emit_text(&Self::escape_mdx_text(&text));
                }
            }
            Node::List { kind, items } => self.emit_list(kind, items),
            Node::Command {
                name,
                options,
                args,
            } => self.emit_command(name, options.as_deref(), args),
            Node::Link {
                label,
                target,
                mode,
            } => self.emit_link(label, target, mode),
            Node::Url { href } => self.emit_url(href),
            Node::Href { href, label } => self.emit_href(href, label),
            Node::Section { title, children } => self.emit_section(title, children),
            Node::ArgumentTable(args) => self.emit_argument_table(args),
            Node::Code { title, kind, children } => self.emit_code_contents(title.as_deref(), kind, children),
        }
    }

    fn emit_nodes(&mut self, nodes: &[Node]) {
        for node in nodes {
            self.emit_node(node);
        }
    }

    fn emit_node_group(&mut self, nodes: &[Vec<Node>]) {
        for node_group in nodes {
            self.emit_nodes(node_group);
        }
    }

    fn emit_text(&mut self, s: &str) {
        self.output += s;
    }

    fn emit_command(&mut self, name: &str, option: Option<&[Node]>, args: &[Vec<Node>]) {
        match name {
            "code" => self.emit_code(option, args),
            "verb" => self.emit_code(option, args),
            //"usage" => self.emit_titled_code_block("Usage", option, args),
            //"examples" => self.emit_titled_code_block("Examples", option, args),
            "emph" => self.emit_emph(option, args),
            "strong" => self.emit_strong(option, args),
            "eqn" => self.emit_eqn(option, args),
            "deqn" => self.emit_deqn(option, args),
            _ if self.in_math_mode() => self.emit_math_command(name, option, args),
            _ => eprintln!("warning: unhandled Rd command '\\{name}' — output may be incomplete"),
        }
    }

    fn emit_math_command(&mut self, name: &str, option: Option<&[Node]>, args: &[Vec<Node>]) {
        self.emit_text("\\");
        self.emit_text(name);

        if let Some(option) = option {
            self.emit_text("[");
            self.emit_nodes(option);
            self.emit_text("]");
        }

        for arg in args {
            self.emit_text("{");
            self.emit_nodes(arg);
            self.emit_text("}");
        }
    }

    fn emit_code(&mut self, _option: Option<&[Node]>, args: &[Vec<Node>]) {
        self.emit_text("`");
        self.code_mode_depth += 1;
        self.emit_node_group(args);
        self.code_mode_depth -= 1;
        self.emit_text("`");
    }

    fn emit_emph(&mut self, _option: Option<&[Node]>, args: &[Vec<Node>]) {
        self.emit_text("*");
        self.emit_node_group(args);
        self.emit_text("*");
    }

    fn emit_strong(&mut self, _option: Option<&[Node]>, args: &[Vec<Node>]) {
        self.emit_text("**");
        self.emit_node_group(args);
        self.emit_text("**");
    }

    fn emit_link(&mut self, label: &[Node], target: &LinkTarget, mode: &LinkMode) {
        match target {
            LinkTarget::Local(nodes) => self.emit_local_link(label, nodes, mode),
            LinkTarget::Package { package, topic } => {
                self.emit_package_link(label, package, topic, mode)
            }
        }
    }

    fn emit_local_link(&mut self, label: &[Node], target: &[Node], mode: &LinkMode) {
        match mode {
            LinkMode::Text => self.emit_text("["),
            LinkMode::Code => self.emit_text("[`"),
        };
        self.emit_nodes(label);
        match mode {
            LinkMode::Text => self.emit_text("](../"),
            LinkMode::Code => self.emit_text("`](../"),
        };
        self.emit_nodes(target);
        self.emit_text("/)");
    }

    fn emit_package_link(
        &mut self,
        label: &[Node],
        package: &[Node],
        topic: &[Node],
        mode: &LinkMode,
    ) {
        match mode {
            LinkMode::Text => self.emit_text("["),
            LinkMode::Code => self.emit_text("[`"),
        };
        self.emit_nodes(label);
        match mode {
            LinkMode::Text => self.emit_text("]"),
            LinkMode::Code => self.emit_text("`]"),
        };
        self.emit_text("(https://rdrr.io/search?package=");
        self.emit_nodes(package);
        self.emit_text("&repo=cran&q=");
        self.emit_nodes(topic);
        self.emit_text(")");
        // Add external link icon
        self.emit_text("<span style = {{ display: 'inline-block', verticalAlign: 'middle' }}><Icon name=\"external\" /></span>");
        self.imports
            .insert("import { Icon } from '@astrojs/starlight/components';".to_string());
    }

    fn emit_url(&mut self, href: &[Node]) {
        let url = self.render_nodes_to_string(href);
        self.emit_text(&format!("[{url}]({url})"));
    }

    fn emit_href(&mut self, href: &[Node], label: &[Node]) {
        let url = self.render_nodes_to_string(href);
        let label = self.render_nodes_to_string(label);
        self.emit_text(&format!("[{label}]({url})"));
    }

    fn emit_eqn(&mut self, _option: Option<&[Node]>, args: &[Vec<Node>]) {
        self.emit_text("$");
        if let Some(math) = args.first() {
            self.emit_math_nodes(slice::from_ref(math));
        }
        self.emit_text("$");
    }

    fn emit_deqn(&mut self, _option: Option<&[Node]>, args: &[Vec<Node>]) {
        self.emit_text("$$\n");
        if let Some(math) = args.first() {
            self.emit_math_nodes(slice::from_ref(math));
        }
        self.emit_text("\n$$\n");
    }

    fn emit_list(&mut self, kind: &ListKind, items: &[ListItem]) {
        match kind {
            ListKind::Itemize => self.emit_itemize_list(items),
            ListKind::Enumerate => self.emit_enumerate_list(items),
            ListKind::Describe => self.emit_describe_list(items),
        }
    }

    fn emit_itemize_list(&mut self, items: &[ListItem]) {
        for list in items {
            let content = self.render_compact_nodes(list.content_nodes());
            self.emit_text("- ");
            self.emit_text(&content);
            self.emit_text("\n");
        }
    }

    fn emit_enumerate_list(&mut self, items: &[ListItem]) {
        for list in items {
            let content = self.render_compact_nodes(list.content_nodes());
            self.emit_text("1. ");
            self.emit_text(&content);
            self.emit_text("\n");
        }
    }

    fn emit_describe_list(&mut self, items: &[ListItem]) {
        for list in items {
            let content = list.content_nodes();

            self.emit_text("- ");
            if list.has_term() {
                let t = list.term_nodes().unwrap();
                let term = self.render_compact_nodes(t);
                self.emit_text("**");
                self.emit_text(&term);
                self.emit_text("**: ");
            };

            let rendered = self.render_compact_nodes(content);
            self.emit_text(&rendered);

            self.emit_text("\n");
        }
    }

    fn emit_section(&mut self, title: &[Node], children: &[Node]) {
        self.emit_text("## ");
        self.emit_nodes(title);
        self.emit_text("\n\n");
        let rendered = self.render_nodes_to_string(children);
        self.emit_text(rendered.trim());
        self.emit_text("\n\n");
    }

    fn emit_argument_table(&mut self, args: &[Argument]) {
        self.emit_argument_table_header();
        for arg in args {
            let name = Self::escape_html(&self.render_table_cell(&arg.name));
            let description = self.render_table_cell(&arg.description);
            self.emit_text("<tr>\n");
            self.emit_text(&format!("<td><code>{name}</code></td>\n"));
            self.emit_text(&format!("<td>{description}</td>\n"));
            self.emit_text("</tr>\n");
        }
        self.emit_argument_table_footer();
    }

    fn emit_argument_table_header(&mut self) {
        self.emit_text("## Arguments\n\n");
        self.emit_text("<div class=\"arg-table\">\n");
        self.emit_text("<table>\n");
        self.emit_text("<thead>\n");
        self.emit_text("<tr><th>Argument</th><th>Description</th></tr>\n");
        self.emit_text("</thead>\n");
        self.emit_text("<tbody>\n");
    }

    fn emit_argument_table_footer(&mut self) {
        self.emit_text("</tbody>\n");
        self.emit_text("</table>\n");
        self.emit_text("</div>\n\n");
    }

    fn emit_code_body_nodes(&mut self, kind: &CodeKind, nodes: &[Node]) {
        let start = nodes
            .iter()
            .position(|node| !matches!(node, Node::NewLine))
            .unwrap_or(nodes.len());
        let end = nodes
            .iter()
            .rposition(|node| !matches!(node, Node::NewLine))
            .map(|index| index + 1)
            .unwrap_or(start);
        let nodes = &nodes[start..end];

        match kind {
            CodeKind::Plain => {}
            CodeKind::DontRun => self.emit_text("# Not run:\n"),
            CodeKind::DontTest => self.emit_text("# Not tested:\n"),
            CodeKind::DontShow => return,
        }

        for node in nodes {
            match node {
                Node::Code {
                    kind: CodeKind::DontShow,
                    ..
                } => {}
                Node::Code { kind, children, .. } => self.emit_code_body_nodes(kind, children),
                _ => self.emit_node(node),
            }
        }
    }

    fn emit_code_contents(&mut self, title: Option<&str>, kind: &CodeKind, children: &[Node]) {
        if matches!(kind, CodeKind::DontShow) {
            return;
        }

        if let Some(t) = title {
            self.emit_text("## ");
            self.emit_text(t);
            self.emit_text("\n\n");
        }

        self.emit_text("```r\n");
        self.code_mode_depth += 1;
        self.emit_code_body_nodes(kind, children);
        self.code_mode_depth -= 1;
        if !self.output.ends_with('\n') {
            self.emit_text("\n");
        }
        self.emit_text("```\n");
    }
}

fn clean_description_text(text: String) -> String {
    let normalized = text.split_whitespace().collect::<Vec<_>>().join(" ");

    let sentences = normalized
        .split('.')
        .map(str::trim)
        .map(|s| s.trim_end_matches([',', ';', ':']))
        .filter(|s| !s.is_empty())
        .filter(|s| s.chars().any(|c| c.is_alphanumeric()))
        .collect::<Vec<_>>();

    let rebuilt = if sentences.is_empty() {
        String::new()
    } else {
        format!("{}.", sentences.join(". "))
    };

    rebuilt.chars().take(160).collect()
}

fn escape_yaml(text: &str) -> String {
    text.replace('\\', "\\\\").replace('"', "\\\"")
}

fn create_frontmatter(document: &Document, pagefind: bool) -> AnyhowResult<String> {
    let mut emitter = Emitter::new();
    emitter.emit_text("---\n");

    let title = document
        .get_title_node()
        .or_else(|| document.get_name_node())
        .and_then(|node| match node {
            Node::Command { args, .. } => args.first(),
            _ => None,
        })
        .map(|nodes| emitter.render_nodes_to_string(nodes).trim().to_string())
        .filter(|s| !s.is_empty());

    let Some(title) = title else {
        bail!("Please add a title or name command to the Rd document");
    };

    let desc = document
        .get_description_node()
        .and_then(|node| match node {
            Node::Section { children, .. } => Some(children),
            _ => None,
        })
        .map(|nodes| {
            let filtered: Vec<Node> = nodes
                .iter()
                .filter_map(|n| match n {
                    Node::NewLine => Some(Node::Text(" ".to_string())),
                    Node::Url { .. } | Node::Href { .. } | Node::Link { .. } => None,
                    _ => Some(n.clone()),
                })
                .collect();

            emitter.render_nodes_to_string(&filtered)
        })
        .map(clean_description_text)
        .filter(|s| !s.is_empty());

    emitter.emit_text(&format!("title: \"{}\"\n", escape_yaml(&title)));

    if let Some(desc) = desc {
        emitter.emit_text(&format!("description: \"{}\"\n", escape_yaml(&desc)));
    };
    emitter.emit_text(&format!("pagefind: {pagefind}\n"));

    emitter.emit_text("---\n\n");
    Ok(emitter.output)
}

pub fn emit_document(mut document: Document, options: &EmitOptions) -> AnyhowResult<String> {
    let mut emitter = Emitter::new();

    let frontmatter = create_frontmatter(&document, options.include_pagefind)?;

    document.filter_sections(&options.skip_sections);
    emitter.emit_nodes(&document.children);

    let content = emitter.output.trim_start_matches('\n').to_string();

    let mdx = if emitter.imports.is_empty() {
        content
    } else {
        let imports = emitter.imports.into_iter().collect::<Vec<_>>().join("\n");
        format!("{imports}\n{content}")
    };

    Ok(format!("{frontmatter}\n{mdx}"))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parsing::parse_file;
    use insta::{assert_snapshot, glob};
    use std::path::PathBuf;

    #[test]
    fn can_emit_rd_files() {
        let test_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("test_data");
        glob!(test_dir, "*.Rd", |path| {
            let document = parse_file(path).unwrap();
            assert_snapshot!(emit_document(document, &EmitOptions::default()).unwrap());
        });
    }

    #[test]
    fn can_skip_sections() {
        let test_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("test_data");
        let opts = EmitOptions::default().with_skip_sections(vec![
            "arguments",
            "name",
            "title",
            "description",
            "usage",
            "value",
        ]);

        glob!(test_dir, "*.Rd", |path| {
            let document = parse_file(path).unwrap();
            assert_snapshot!(emit_document(document, &opts).unwrap());
        });
    }
}
