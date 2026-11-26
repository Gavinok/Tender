import { readFileSync } from "node:fs";

// The backend's golden JSON, which pins what it sends. Tests run from frontend/.
export const readGolden = (name) => () => readFileSync(`../backend/.golden/${name}/golden`, "utf8");
