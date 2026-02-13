import { visit } from "unist-util-visit";

/**
 * Remark plugin to prepend ASTRO_BASE to image paths.
 * This ensures images work correctly in versioned documentation.
 */
export function remarkBaseUrl() {
  const base = (process.env.ASTRO_BASE || "/").replace(/\/$/, "");

  return (tree) => {
    visit(tree, "image", (node) => {
      if (node.url && node.url.startsWith("/figures/")) {
        node.url = base + node.url;
      }
    });
  };
}
