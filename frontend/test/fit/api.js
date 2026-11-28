// A stand-in for the backend, with the most content each screen has to fit:
// four people, a long name, every kind of chip, and a notice (a swipe that
// can't be sent).

/** @typedef {import("@playwright/test").Page} Page */
/** @typedef {import("@playwright/test").Route} Route */

const OK = 200;
const UNAVAILABLE = 503;
const RESTAURANTS = 5;
const AN_HOUR = 3600;
const PHOTOS = 3;

// Every chip the backend offers
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

const chosen = vibe.filter((chip) => ["patio", "veggie", "drinks"].includes(chip.id));

/**
 * A restaurant with every detail filled in, and a long name
 * @param {number} index
 */
const restaurant = (index) => ({
  source: "google",
  id: `place-${String(index)}`,
  name: `The Extraordinarily Long-Named Pho and Noodle House No. ${String(index)}`,
  url: "https://maps.google.com/?cid=1",
  rating: 4.6,
  price: "$$",
  category: "Vietnamese Restaurant",
  moreCategories: ["Bar", "Noodle shop"],
  photos: Array.from({ length: PHOTOS }, (_, photo) => ({ name: `places/p/photos/${String(photo)}`, url: null, author: { name: "Sam", uri: null } })),
  summary: { text: "Bright noodle spot known for its slow-simmered broth and generous portions, with a busy patio in summer.", disclosure: "Summarized with Gemini" },
  reviews: [{ author: { name: "Kai", uri: null }, rating: 5, text: "Rich broth, fast service.", when: "a month ago" }],
  confirmed: ["patio", "veggie"],
  address: "1234 Government St",
  openNow: true,
  distance: 850,
});

const PHOTO = '<svg xmlns="http://www.w3.org/2000/svg" width="800" height="600"><rect width="800" height="600" fill="#c79a6b"/></svg>';

/** @type {readonly (readonly [string, unknown])[]} */
const answers = [
  ["/api/newsession", "00000000-0000-0000-0000-000000000001"],
  ["/api/addtosession", "00000000-0000-0000-0000-000000000002"],
  ["/api/vibe", { chips: chosen.map((chip) => chip.id), choices: vibe, locked: false, place: { name: "Downtown Victoria", latitude: 48.43, longitude: -123.37 }, craving: "spicy ramen" }],
  ["/api/presence", {
    matches: [],
    next: AN_HOUR,
    people: [
      { you: true, status: "swiping", swiped: 3, left: 17 },
      { you: false, status: "swiping", swiped: 8, left: 12 },
      { you: false, status: "done", swiped: 20, left: 0 },
      { you: false, status: "away", swiped: 2, left: 18 },
    ],
  }],
  ["/api/restaurants", {
    restaurants: Array.from({ length: RESTAURANTS }, (_, index) => restaurant(index)),
    othersLiked: [],
    vibe: chosen,
    craving: "spicy ramen",
    radius: 5000,
    maxRadius: 50_000,
  }],
];

/**
 * Answer a request to the API
 * @param {Route} route
 * @returns {Promise<void>}
 */
const answer = async (route) => {
  const { pathname } = new URL(route.request().url());
  if (pathname.startsWith("/api/photo")) {
    await route.fulfill({ status: OK, contentType: "image/svg+xml", body: PHOTO });
    return;
  }
  const known = answers.find(([prefix]) => pathname.startsWith(prefix));
  // Anything else, such as a swipe, can't be sent, so the offline notice shows
  await route.fulfill(known === undefined
    ? { status: UNAVAILABLE, contentType: "application/json", body: JSON.stringify({ error: "Try again" }) }
    : { status: OK, contentType: "application/json", body: JSON.stringify(known[1]) });
};

/**
 * Send the page's API requests to the stand-in
 * @param {Page} page
 * @returns {Promise<void>}
 */
export const standIn = async (page) => {
  await page.route("**/api/**", answer);
};
