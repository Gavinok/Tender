// Each screen's main actions fit on a phone, with the most content it can
// have, and are big enough to tap. Run with `make fit`.
import { expect, test } from "@playwright/test";
import { standIn } from "./api.js";

/** @typedef {import("@playwright/test").Locator} Locator */
/** @typedef {import("@playwright/test").Page} Page */

const PHONES = [
  { name: "older Android", width: 360, height: 640 },
  { name: "Android", width: 360, height: 740 },
  { name: "iPhone SE", width: 375, height: 667 },
  { name: "recent iPhone", width: 393, height: 852 },
];

// The smallest comfortable tap target, in CSS pixels
const SMALLEST_TARGET = 44;

/**
 * Fully on screen without scrolling, and big enough to tap
 * @param {Locator} control
 * @returns {Promise<void>}
 */
const fitsAndTaps = async (control) => {
  await expect(control).toBeInViewport({ ratio: 1 });
  const box = await control.boundingBox();
  expect(box?.width).toBeGreaterThanOrEqual(SMALLEST_TARGET);
  expect(box?.height).toBeGreaterThanOrEqual(SMALLEST_TARGET);
};

/**
 * How far the page runs past the screen, for the report
 * @param {Page} page
 * @returns {Promise<number>}
 */
const overflow = async (page) =>
  page.evaluate(() => globalThis.document.documentElement.scrollHeight - globalThis.innerHeight);

for (const phone of PHONES) {
  test.describe(`${phone.name} (${String(phone.width)}x${String(phone.height)})`, () => {
    test.use({ viewport: { width: phone.width, height: phone.height } });

    test.beforeEach(async ({ page }) => {
      await standIn(page);
      await page.goto("/");
    });

    test("the lobby's invite and start fit", async ({ page }) => {
      const start = page.getByRole("button", { name: "Start swiping" });
      await start.waitFor();
      test.info().annotations.push({ type: "overflow", description: `${String(await overflow(page))}px` });
      await fitsAndTaps(page.getByRole("button", { name: "Share invite" }));
      await fitsAndTaps(start);
    });

    test("swiping's Like and Nope fit, with a notice showing", async ({ page }) => {
      await page.getByRole("button", { name: "Start swiping" }).click();
      const nope = page.getByRole("button", { name: "Nope" });
      await nope.click();
      // Everyone's avatar is in the header, and the failed swipe brings up
      // the offline notice, the longest one
      await page.getByRole("button", { name: "Session: 3 here" }).waitFor();
      await page.getByText("Can't reach the server").waitFor();
      // Phones don't hover, and a hovered button is scaled up
      await page.mouse.move(0, 0);
      test.info().annotations.push({ type: "overflow", description: `${String(await overflow(page))}px` });
      await fitsAndTaps(nope);
      await fitsAndTaps(page.getByRole("button", { name: "Like" }));
    });
  });
}
