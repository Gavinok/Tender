// The keyboard shortcuts work in a real browser, where the unit tests can't
// reach: the arrow keys swipe, and Escape closes what's on top.
import { expect, test } from "@playwright/test";
import { standIn } from "./api.js";

test("the arrow keys swipe and Escape closes a sheet", async ({ page }) => {
  await standIn(page);
  await page.goto("/");
  await page.getByRole("button", { name: "Start swiping" }).click();
  await expect(page.getByRole("heading", { name: "No. 0" })).toBeVisible();
  await page.keyboard.press("ArrowLeft");
  await expect(page.getByRole("heading", { name: "No. 1" })).toBeVisible();
  await page.getByRole("button", { name: "Details about" }).click();
  await expect(page.getByRole("dialog")).toBeVisible();
  await page.keyboard.press("Escape");
  await expect(page.getByRole("dialog")).toHaveCount(0);
});
