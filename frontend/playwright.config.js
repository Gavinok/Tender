// The fit tests (test/fit), against the built frontend in dist/ with the API
// stood in. `make fit` builds dist/ first.
import { defineConfig } from "@playwright/test";

const PORT = 4173;

export default defineConfig({
  testDir: "test/fit",
  reporter: [["list"]],
  use: { baseURL: `http://localhost:${String(PORT)}`, permissions: [] },
  webServer: { command: `node test/fit/server.js ${String(PORT)}`, port: PORT, reuseExistingServer: true },
});
