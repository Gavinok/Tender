// Drag the element marked data-swipe sideways. Released past the threshold
// it sends "swipe-like" or "swipe-nope" on the document; otherwise it springs
// back. The card is moved directly rather than through the model so dragging
// doesn't re-render on every pointer move.

/** @typedef {"like" | "nope"} Direction */

// Of the card's width
const THRESHOLD = 0.3;
// A tap moves less than this many pixels
const TAP_SLOP = 6;
// The card's left third flips back; the rest flips forward
const BACK_ZONE = 3;
// Vertical drag follows the finger this much, and the card tilts by 1/18 degree per pixel
const VERTICAL_DRAG = 0.2;
const PIXELS_PER_DEGREE = 18;
// How far off screen, in percent of its width, and how far tilted, a card flies
const FLY_DISTANCE = 130;
const FLY_TILT = 25;

/**
 * The entrance animation holds transform: none, which beats inline styles
 * @param {HTMLElement} card
 * @returns {void}
 */
const takeOver = (card) => {
  card.style.animation = "none";
};

/**
 * @param {HTMLElement} card
 * @param {number} dx
 * @returns {void}
 */
const setStamps = (card, dx) => {
  const amount = Math.min(1, Math.abs(dx) / (card.offsetWidth * THRESHOLD));
  card.style.setProperty("--like", String(dx > 0 ? amount : 0));
  card.style.setProperty("--nope", String(dx < 0 ? amount : 0));
};

/**
 * @param {Direction} direction
 * @returns {() => void}
 */
export const _flyOut = (direction) => () => {
  const card = document.querySelector("[data-swipe]");
  if (!(card instanceof HTMLElement)) {
    return;
  }
  const sign = direction === "like" ? 1 : -1;
  delete card.dataset["swipe"];
  takeOver(card);
  setStamps(card, sign * card.offsetWidth);
  card.style.transition = "transform 0.3s ease-in, opacity 0.3s ease-in";
  card.style.transform = `translateX(${String(sign * FLY_DISTANCE)}%) rotate(${String(sign * FLY_TILT)}deg)`;
  card.style.opacity = "0";
};

/**
 * @param {EventTarget | null} target
 * @returns {HTMLElement | null}
 */
const swipeCardUnder = (target) => {
  // Links, buttons and the summary box on the card still work as normal
  if (!(target instanceof Element) || target.closest("a, button, [data-no-swipe]") !== null) {
    return null;
  }
  const card = target.closest("[data-swipe]");
  return card instanceof HTMLElement ? card : null;
};

/**
 * A tap flips between the card's photos: the left third goes back
 * @param {HTMLElement} card
 * @param {number} clientX
 * @returns {void}
 */
const tap = (card, clientX) => {
  card.style.transform = "";
  setStamps(card, 0);
  const box = card.getBoundingClientRect();
  const side = clientX < box.left + box.width / BACK_ZONE ? "left" : "right";
  document.dispatchEvent(new CustomEvent(`card-tap-${side}`));
};

/**
 * @param {HTMLElement} card
 * @param {number} dx
 * @returns {void}
 */
const finishDrag = (card, dx) => {
  if (Math.abs(dx) > card.offsetWidth * THRESHOLD) {
    document.dispatchEvent(new CustomEvent(dx > 0 ? "swipe-like" : "swipe-nope"));
  } else {
    card.style.transition = "transform 0.3s cubic-bezier(0.2, 0.9, 0.3, 1.3)";
    card.style.transform = "";
    setStamps(card, 0);
  }
};

/** @returns {void} */
export const listen = () => {
  /** @type {HTMLElement | null} */
  let card = null;
  let startX = 0;
  let startY = 0;
  let dx = 0;

  document.addEventListener("pointerdown", (event) => {
    card = swipeCardUnder(event.target);
    if (card === null) {
      return;
    }
    startX = event.clientX;
    startY = event.clientY;
    dx = 0;
    card.setPointerCapture(event.pointerId);
    takeOver(card);
    card.style.transition = "none";
  });

  document.addEventListener("pointermove", (event) => {
    if (card === null) {
      return;
    }
    dx = event.clientX - startX;
    const dy = (event.clientY - startY) * VERTICAL_DRAG;
    card.style.transform = `translate(${String(dx)}px, ${String(dy)}px) rotate(${String(dx / PIXELS_PER_DEGREE)}deg)`;
    setStamps(card, dx);
  });

  /** @param {PointerEvent} event */
  const release = (event) => {
    if (card === null) {
      return;
    }
    const released = card;
    card = null;
    if (Math.abs(dx) < TAP_SLOP && Math.abs(event.clientY - startY) < TAP_SLOP) {
      tap(released, event.clientX);
    } else {
      finishDrag(released, dx);
    }
  };
  document.addEventListener("pointerup", release);
  document.addEventListener("pointercancel", release);
};
