import { renderSVG } from "uqr";

/**
 * @param {string} text
 * @returns {string}
 */
export const qrDataUrl = (text) => `data:image/svg+xml;charset=utf-8,${encodeURIComponent(renderSVG(text))}`;
