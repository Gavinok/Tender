import { existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";

// Tests run from frontend/

export const readBackendGolden = (name) => () => readFileSync(`../backend/.golden/${name}/golden`, "utf8");

// Like hspec-golden on the backend: write the golden when it's missing,
// otherwise compare, writing `actual` beside it when they differ
export const pinGolden = (same) => (differs) => (name) => (actual) => () => {
  const dir = `.golden/${name}`;
  const golden = `${dir}/golden`;
  if (!existsSync(golden)) {
    mkdirSync(dir, { recursive: true });
    writeFileSync(golden, actual);
    return same;
  }
  const expected = readFileSync(golden, "utf8");
  if (expected === actual) return same;
  writeFileSync(`${dir}/actual`, actual);
  return differs(expected);
};
