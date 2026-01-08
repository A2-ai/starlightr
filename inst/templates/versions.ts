export interface Version {
  tag: string;
  label: string;
  default?: boolean;
}

export const VERSIONS: Version[] = [
{{#versions}}
  {
    tag: "{{tag}}",
    label: "{{label}}",
{{#default}}
    default: true,
{{/default}}
  },
{{/versions}}
];

export const CURRENT_VERSION = "{{currentVersion}}";
