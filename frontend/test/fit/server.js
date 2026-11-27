// Serves the built frontend (dist/) for the fit tests, on the port given.
// Paths that aren't a file get index.html, as the backend does for session
// links.
import { createServer } from "node:http";
import { readFile } from "node:fs/promises";
import path from "node:path";

const ROOT = path.resolve(import.meta.dirname, "../../dist");
const OK = 200;

/** @type {ReadonlyMap<string, string>} */
const TYPES = new Map([
  [".html", "text/html"],
  [".js", "text/javascript"],
  [".css", "text/css"],
  [".svg", "image/svg+xml"],
  [".woff", "font/woff"],
  [".woff2", "font/woff2"],
]);

/**
 * The file for a path, or the app's page when there's no such file
 * @param {string} urlPath
 * @returns {Promise<{ body: Buffer, type: string }>}
 */
const load = async (urlPath) => {
  const file = path.join(ROOT, path.normalize(urlPath));
  if (file.startsWith(ROOT) && path.extname(file) !== "") {
    try {
      return { body: await readFile(file), type: TYPES.get(path.extname(file)) ?? "application/octet-stream" };
    } catch {
      // Not a file, so the app's page
    }
  }
  return { body: await readFile(path.join(ROOT, "index.html")), type: "text/html" };
};

/**
 * Answer one request
 * @param {import("node:http").IncomingMessage} request
 * @param {import("node:http").ServerResponse} response
 * @returns {Promise<void>}
 */
const serve = async (request, response) => {
  try {
    const { body, type } = await load(new URL(request.url ?? "/", "http://localhost").pathname);
    response.writeHead(OK, { "content-type": type });
    response.end(body);
  } catch (error) {
    response.destroy(error instanceof Error ? error : undefined);
  }
};

createServer((request, response) => {
  void serve(request, response);
}).listen(Number(process.argv[2]));
