import { renderSVG } from "uqr";

export const qrDataUrl = (text) =>
  "data:image/svg+xml;charset=utf-8," + encodeURIComponent(renderSVG(text));
