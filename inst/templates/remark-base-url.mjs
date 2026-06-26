import { visit } from "unist-util-visit";

/**
 * Remark plugin to prepend ASTRO_BASE to asset paths.
 * This ensures images and videos work correctly in versioned documentation.
 *
 * @param {string[]} assetDirs - Root-absolute asset directories to prefix.
 *   Defaults to ["figures"]; pass e.g. ["figures", "videos"] to add more.
 */
export function remarkBaseUrl(assetDirs = ["figures"]) {
  const base = (process.env.ASTRO_BASE || "/").replace(/\/$/, "");

  return (tree) => {
    visit(tree, "image", (node) => {
      if (node.url && assetDirs.some((dir) => node.url.startsWith(`/${dir}/`))) {
        node.url = base + node.url;
      }
    });

    // Raw HTML nodes (.md files): rewrite src/poster attributes in the string.
    visit(tree, "html", (node) => {
      if (!node.value) return;
      for (const dir of assetDirs) {
        node.value = node.value.replace(
          new RegExp(`(src|poster)="/${dir}/`, "g"),
          `$1="${base}/${dir}/`
        );
      }
    });

    // MDX JSX elements (.mdx files): rewrite src/poster attributes on the node.
    // Raw <video>/<source>/<img> become mdxJsx*Element nodes, not html nodes.
    visit(tree, ["mdxJsxFlowElement", "mdxJsxTextElement"], (node) => {
      for (const attr of node.attributes || []) {
        if (
          attr.type === "mdxJsxAttribute" &&
          (attr.name === "src" || attr.name === "poster") &&
          typeof attr.value === "string" &&
          assetDirs.some((dir) => attr.value.startsWith(`/${dir}/`))
        ) {
          attr.value = base + attr.value;
        }
      }
    });
  };
}
