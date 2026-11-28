// Each screen's main actions fit on a phone, with the most content it can
// have, and are big enough to tap. Run with `make fit`.
import { expect, test } from "@playwright/test";
import { standIn } from "./api.js";

/** @typedef {import("@playwright/test").Locator} Locator */
/** @typedef {import("@playwright/test").Page} Page */

const PHONES = [
  { name: "older Android", width: 360, height: 640 },
  { name: "Android", width: 360, height: 740 },
  { name: "Galaxy S", width: 360, height: 800 },
  { name: "iPhone SE", width: 375, height: 667 },
  { name: "recent iPhone", width: 393, height: 852 },
  { name: "Pixel", width: 412, height: 915 },
  { name: "iPhone Pro Max", width: 430, height: 932 },
  // What an iPhone 17e shows of a page with Safari's bars
  { name: "iPhone 17e in Safari", width: 390, height: 664 },
];

// The smallest comfortable tap target, in CSS pixels
const SMALLEST_TARGET = 44;
const OK = 200;
// Long enough for the match screen's pop and card to finish, in ms
const ANIMATIONS = 600;
// The smallest QR code that scans easily from across a table
const SMALLEST_QR = 180;

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
 * Nothing runs off the side, such as a crowded header
 * @param {Page} page
 * @returns {Promise<void>}
 */
const noSideways = async (page) => {
  expect(await page.evaluate(() => globalThis.document.documentElement.scrollWidth - globalThis.innerWidth)).toBe(0);
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
    // A touch screen: with a mouse the lobby's vibe chips wrap instead of
    // scrolling, which isn't what a phone shows
    test.use({ viewport: { width: phone.width, height: phone.height }, hasTouch: true, isMobile: true });

    test.beforeEach(async ({ page }) => {
      await standIn(page);
      await page.goto("/");
    });

    test("the lobby's invite, start and QR code fit", async ({ page }) => {
      const start = page.getByRole("button", { name: "Start swiping" });
      await start.waitFor();
      test.info().annotations.push({ type: "overflow", description: `${String(await overflow(page))}px` });
      await fitsAndTaps(page.getByRole("button", { name: "Share invite" }));
      await fitsAndTaps(start);
      await noSideways(page);
      const qr = await page.getByRole("img", { name: "QR code to join this session" }).boundingBox();
      expect(qr?.height).toBeGreaterThanOrEqual(SMALLEST_QR);
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
      await noSideways(page);
    });

    test("the match screen fits, heading and all", async ({ page }) => {
      // The first like is a match
      await page.route("**/api/decision", async (route) => {
        /** @type {unknown} */
        const swipe = route.request().postDataJSON();
        const restaurant = typeof swipe === "object" && swipe !== null && "restaurant" in swipe ? swipe.restaurant : undefined;
        await route.fulfill({ status: OK, contentType: "application/json", body: JSON.stringify({ matches: [restaurant], othersLiked: [], people: [] }) });
      });
      await page.getByRole("button", { name: "Start swiping" }).click();
      await page.getByRole("button", { name: "Like" }).click();
      const heading = page.getByText("It's a match!", { exact: true });
      await heading.waitFor();
      // Past the entrance animations
      await page.waitForTimeout(ANIMATIONS);
      await expect(heading).toBeInViewport({ ratio: 1 });
      await fitsAndTaps(page.getByRole("button", { name: "Keep swiping" }));
      await fitsAndTaps(page.getByRole("link", { name: "See the menu" }));
      await fitsAndTaps(page.getByRole("link", { name: "Let's go" }));
      await noSideways(page);
    });
  });
}
