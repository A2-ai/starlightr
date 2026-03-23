// @ts-check
import { defineConfig } from "astro/config";
import starlight from "@astrojs/starlight";
{{#use_remark}}
import { remarkBaseUrl } from "./remark-base-url.mjs";
{{/use_remark}}
{{#use_katex}}
import { starlightKatex } from "starlight-katex";
{{/use_katex}}

// https://astro.build/config
export default defineConfig({
  site: process.env.ASTRO_SITE || "http://localhost",
  base: process.env.ASTRO_BASE || "/",
  trailingSlash: "always",
{{#use_remark}}
  markdown: {
    remarkPlugins: [remarkBaseUrl],
  },
{{/use_remark}}
  integrations: [
    starlight({
      title: "{{{title}}}",
      customCss: ["./src/styles/starlightr.css", "./src/styles/custom.css"],
{{#use_katex}}
      plugins: [starlightKatex()],
{{/use_katex}}
{{#has_versions}}
      components: { SiteTitle: "./src/components/VersionSelect.astro" },
{{/has_versions}}
{{#has_logo}}
      logo: { src: "./src/assets/logo.png", alt: "Logo" },
{{/has_logo}}
{{#has_favicon}}
      favicon: "/images/favicon.png",
{{/has_favicon}}
{{#has_github}}
      social: [{ icon: 'github', label: 'GitHub', href: '{{{github_url}}}' }],
{{/has_github}}
      sidebar: {{{sidebar_config}}}
    })
  ]
});
