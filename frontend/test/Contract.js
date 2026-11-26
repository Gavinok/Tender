import { existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";

// Tests run from frontend/

/**
 * @param {string} name
 * @returns {() => string}
 */
export const readBackendGolden = (name) => () => readFileSync(`../backend/.golden/${name}/golden`, "utf8");

/**
 * Like hspec-golden on the backend: write the golden when it's missing,
 * otherwise compare, writing `actual` beside it when they differ
 * @template R
 * @param {R} same
 * @returns {(differs: (expected: string) => R) => (name: string) => (actual: string) => () => R}
 */
export const pinGolden = (same) => (differs) => (name) => (actual) => () => {
  const directory = `.golden/${name}`;
  const golden = `${directory}/golden`;
  if (!existsSync(golden)) {
    mkdirSync(directory, { recursive: true });
    writeFileSync(golden, actual);
    return same;
  }
  const expected = readFileSync(golden, "utf8");
  if (expected === actual) {
    return same;
  }
  writeFileSync(`${directory}/actual`, actual);
  return differs(expected);
};
