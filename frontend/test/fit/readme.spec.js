// The README's screenshots (docs/screenshots.jpg): the lobby, a card and a
// match on a phone, side by side. Run with `make screenshots`; other test
// runs skip it. The restaurants are made up and their photos are Twemoji
// food (CC-BY 4.0), fetched from a pinned release, since Google's terms
// don't allow storing its photos and details in the repository.
import path from "node:path";
import { expect, test } from "@playwright/test";

/** @typedef {import("@playwright/test").Page} Page */
/** @typedef {import("@playwright/test").Route} Route */

// Playwright runs from frontend/, where make screenshots is
const OUTPUT = path.resolve("../docs/screenshots.jpg");
const TWEMOJI = "https://cdn.jsdelivr.net/gh/jdecked/twemoji@v16.0.1/assets/svg";
const OK = 200;
const PHONE = { width: 390, height: 844 };
const SHARP = 2;
const GAP = 16;
const PANES = 3;
const QUALITY = 75;
// Long enough for entrance animations and photos, in ms
const SETTLE = 800;
const AN_HOUR = 3600;

const vibe = [
  { id: "cheap", label: "💸 Cheap eats" },
  { id: "fancy", label: "✨ Treat yourself" },
  { id: "patio", label: "☀️ Patio" },
  { id: "date-night", label: "🕯️ Date night" },
  { id: "big-group", label: "👯 Big group" },
  { id: "veggie", label: "🥦 Veggie" },
  { id: "drinks", label: "🍸 Drinks" },
  { id: "brunch", label: "🥞 Brunch" },
  { id: "late-night", label: "🌙 Late night" },
  { id: "open-now", label: "🕐 Open now" },
];
const chosen = vibe.filter((chip) => ["patio", "drinks"].includes(chip.id));

/**
 * A made-up restaurant, its Twemoji and the colours behind it
 * @param {{ id: string, name: string, category: string, emoji: string, from: string, to: string, summary: string, distance: number, price: string, rating: number, credit: string }} place
 */
const restaurant = (place) => ({
  source: "google",
  id: place.id,
  name: place.name,
  url: "https://maps.google.com/",
  rating: place.rating,
  price: place.price,
  category: place.category,
  moreCategories: [],
  photos: [`${place.emoji}/${place.from}/${place.to}`, "x", "y"].map((name) => ({ name, url: null, author: { name: place.credit, uri: null } })),
  summary: { text: place.summary, disclosure: null },
  reviews: [],
  confirmed: ["patio", "drinks"],
  address: null,
  openNow: true,
  distance: place.distance,
});

const restaurants = [
  restaurant({ id: "pho", name: "Pho Real", category: "Vietnamese Restaurant", emoji: "1f35c", from: "#ffd59e", to: "#f08a5d", summary: "Cozy noodle bar with a sunny patio, known for its slow-simmered pho and late-night bowls.", distance: 650, price: "$$", rating: 4.6, credit: "Sam Chen" }),
  restaurant({ id: "taco", name: "Taco Time Machine", category: "Mexican Restaurant", emoji: "1f32e", from: "#fff3a8", to: "#9be38f", summary: "Street tacos and agua fresca on a string-lit patio.", distance: 1200, price: "$", rating: 4.4, credit: "Lee" }),
];

/** @type {readonly (readonly [string, unknown])[]} */
const answers = [
  ["/api/newsession", "00000000-0000-0000-0000-000000000001"],
  ["/api/addtosession", "00000000-0000-0000-0000-000000000002"],
  ["/api/vibe", { chips: chosen.map((chip) => chip.id), choices: vibe, locked: false, place: null, craving: "pho" }],
  ["/api/presence", {
    matches: [],
    next: AN_HOUR,
    people: [
      { you: true, status: "swiping", swiped: 4, left: 16 },
      { you: false, status: "swiping", swiped: 11, left: 9 },
      { you: false, status: "done", swiped: 20, left: 0 },
    ],
  }],
  ["/api/restaurants", { restaurants, othersLiked: ["pho"], vibe: chosen, craving: "pho", radius: 5000, maxRadius: 50_000 }],
];

/**
 * A Twemoji on a gradient, as a restaurant's photo
 * @param {string} name e.g. "1f35c/#ffd59e/#f08a5d"
 * @returns {Promise<string>}
 */
const photo = async (name) => {
  const [emoji = "1f37d", from = "#ffd59e", to = "#f08a5d"] = name.split("/", PANES);
  const response = await fetch(`${TWEMOJI}/${emoji}.svg`);
  const art = await response.text();
  const inner = art.slice(art.indexOf(">") + 1, art.lastIndexOf("</svg>"));
  return `<svg xmlns="http://www.w3.org/2000/svg" width="800" height="800" viewBox="0 0 36 36"><defs><linearGradient id="g" x1="0" y1="0" x2="0" y2="1"><stop offset="0" stop-color="${from}"/><stop offset="1" stop-color="${to}"/></linearGradient></defs><rect width="36" height="36" fill="url(#g)"/><g transform="translate(9.9 8) scale(0.45)">${inner}</g></svg>`;
};

/**
 * The restaurant a swipe liked, which is a match
 * @param {Route} route
 * @returns {unknown[]}
 */
const matched = (route) => {
  /** @type {unknown} */
  const swipe = route.request().postDataJSON();
  return typeof swipe === "object" && swipe !== null && "liked" in swipe && swipe.liked === true && "restaurant" in swipe ? [swipe.restaurant] : [];
};

/**
 * Answer the API as the made-up session would. The first like is a match.
 * @param {Route} route
 * @returns {Promise<void>}
 */
const answer = async (route) => {
  const url = new URL(route.request().url());
  if (url.pathname.startsWith("/api/photo")) {
    await route.fulfill({ status: OK, contentType: "image/svg+xml", body: await photo(url.searchParams.get("name") ?? "") });
    return;
  }
  if (url.pathname.startsWith("/api/decision")) {
    await route.fulfill({ status: OK, contentType: "application/json", body: JSON.stringify({ matches: matched(route), othersLiked: [], people: [] }) });
    return;
  }
  const known = answers.find(([prefix]) => url.pathname.startsWith(prefix));
  await route.fulfill({ status: OK, contentType: "application/json", body: JSON.stringify(known?.[1] ?? {}) });
};

/**
 * A screenshot, settled, as a data url
 * @param {Page} page
 * @returns {Promise<string>}
 */
const shot = async (page) => {
  await page.waitForTimeout(SETTLE);
  const png = await page.screenshot();
  return `data:image/png;base64,${png.toString("base64")}`;
};

// Somewhere in Victoria, so there's no location warning
const HERE = { latitude: 48.4284, longitude: -123.3656 };

test.use({ viewport: PHONE, deviceScaleFactor: SHARP, hasTouch: true, isMobile: true, geolocation: HERE, permissions: ["geolocation"] });

test("the README's screenshots", async ({ browser, page }) => {
  test.skip(process.env["README_SHOTS"] === undefined, "Only with make screenshots");
  await page.route("**/api/**", answer);
  await page.goto("/");
  await page.getByRole("button", { name: "Start swiping" }).waitFor();
  const lobby = await shot(page);
  await page.getByRole("button", { name: "Start swiping" }).click();
  await page.getByRole("button", { name: "Session: 3 here" }).waitFor();
  const card = await shot(page);
  await page.getByRole("button", { name: "Nope" }).click();
  await page.getByRole("button", { name: "Like" }).click();
  await expect(page.getByText("It's a match!", { exact: true })).toBeVisible();
  const match = await shot(page);

  // The three side by side, at the phone's size
  const sheet = await browser.newPage({ viewport: { width: (PHONE.width + GAP) * PANES + GAP, height: PHONE.height + GAP * 2 }, deviceScaleFactor: 1 });
  await sheet.setContent(`<body style="margin:0;padding:${String(GAP)}px;display:flex;gap:${String(GAP)}px;background:#ffe4ef">${
    [lobby, card, match].map((screen) => `<img src="${screen}" width="${String(PHONE.width)}" style="border-radius:24px;box-shadow:0 0 0 2px #14122b">`).join("")}</body>`);
  await sheet.screenshot({ path: OUTPUT, type: "jpeg", quality: QUALITY });
});
