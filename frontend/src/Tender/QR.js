import { renderSVG } from "uqr";

const VIEW_BOX = 'viewBox="0 0 ';

/**
 * The SVG with a width and height taken from its viewBox. Without them the
 * image has no natural size, which iOS Safari treats as 0 wide.
 * @param {string} svg
 * @returns {string}
 */
const sized = (svg) => {
  const at = svg.indexOf(VIEW_BOX);
  const from = at + VIEW_BOX.length;
  const [width, height] = svg.slice(from, svg.indexOf('"', from)).split(" ", 2);
  return at === -1 || width === undefined || height === undefined
    ? svg
    : `${svg.slice(0, at)}width="${width}" height="${height}" ${svg.slice(at)}`;
};

/**
 * @param {string} text
 * @returns {string}
 */
export const qrDataUrl = (text) => `data:image/svg+xml;charset=utf-8,${encodeURIComponent(sized(renderSVG(text)))}`;
