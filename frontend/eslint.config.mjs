import { defineConfig } from "eslint/config";
import js from "@eslint/js";
import globals from "globals";
import regexp from "eslint-plugin-regexp";
import tseslint from "typescript-eslint";
import unicorn from "eslint-plugin-unicorn";

// The hand-written JavaScript beside the PureScript: browser FFI in src/,
// Node in test/ and the tool configs. As strict as ESLint gets: type-aware
// rules from typescript-eslint, plus unicorn and regexp.
export default defineConfig(
  { ignores: ["output/**", "dist/**", "assets/app.js", ".spago/**", ".parcel-cache/**", "node_modules/**"] },
  js.configs.all,
  tseslint.configs.strictTypeChecked,
  tseslint.configs.stylisticTypeChecked,
  unicorn.configs.recommended,
  regexp.configs["flat/all"],
  {
    languageOptions: {
      ecmaVersion: "latest",
      sourceType: "module",
      parserOptions: { projectService: true, tsconfigRootDir: import.meta.dirname },
    },
    linterOptions: { reportUnusedDisableDirectives: "error", reportUnusedInlineConfigs: "error" },
    rules: {
      // Where JavaScript is a better fit than any style rule
      "no-magic-numbers": ["error", { ignore: [-1, 0, 1, 2], ignoreDefaultValues: true, enforceConst: true }],
      "one-var": ["error", "never"],
      "sort-imports": "off",
      "sort-keys": "off",
      "id-length": ["error", { min: 1 }],
      "max-lines-per-function": ["error", { max: 60, skipComments: true }],
      "max-statements": ["error", 20],
      "max-params": ["error", 4],
      "max-depth": ["error", 3],
      complexity: ["error", 10],
      "capitalized-comments": "off",
      "no-inline-comments": "off",
      "line-comment-position": "off",
      "no-ternary": "off",
      "no-undefined": "off",
      "no-plusplus": "off",
      "func-style": ["error", "expression"],
      "prefer-destructuring": ["error", { object: true, array: false }],
      "init-declarations": "off",
      "no-warning-comments": "off",
      "no-continue": "off",
      // How to say a promise is left to run, for no-floating-promises
      "no-void": ["error", { allowAsStatement: true }],
      "max-lines": ["error", 300],
      "@typescript-eslint/no-unnecessary-condition": "error",
      "@typescript-eslint/strict-boolean-expressions": "error",
      "@typescript-eslint/switch-exhaustiveness-check": "error",
      "@typescript-eslint/consistent-type-imports": "error",
      "@typescript-eslint/prefer-readonly": "error",
      "@typescript-eslint/promise-function-async": "error",
      "@typescript-eslint/require-array-sort-compare": "error",
      "@typescript-eslint/no-floating-promises": "error",
      "@typescript-eslint/no-misused-promises": "error",
      "@typescript-eslint/no-explicit-any": "error",
      "@typescript-eslint/no-non-null-assertion": "error",
      // PureScript's FFI calling convention is curried functions and
      // exports named after the PureScript side
      "unicorn/prevent-abbreviations": "off",
      "unicorn/no-null": "off",
      "unicorn/filename-case": "off",
      "unicorn/no-array-callback-reference": "off",
      "unicorn/prefer-global-this": "off",
      "unicorn/consistent-boolean-name": "off",
      "unicorn/consistent-function-scoping": "off",
      // JSDoc is the type syntax in plain JavaScript, checked by tsc
      "unicorn/no-asterisk-prefix-in-documentation-comments": "off",
      "unicorn/single-line-block-comment-style": "off",
      "no-underscore-dangle": "off",
      strict: "off",
    },
  },
  {
    files: ["src/**/*.js"],
    languageOptions: { globals: globals.browser },
  },
  {
    files: ["test/**/*.js", "tailwind.config.js", "eslint.config.mjs", "playwright.config.js"],
    languageOptions: { globals: globals.node },
  },
  {
    files: ["tailwind.config.js"],
    languageOptions: { sourceType: "commonjs" },
    rules: { "unicorn/prefer-module": "off", "@typescript-eslint/no-require-imports": "off" },
  },
  {
    // Settings, not logic
    files: ["tailwind.config.js", "eslint.config.mjs"],
    rules: { "no-magic-numbers": "off" },
  },
);
