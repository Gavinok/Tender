// Drag the element marked data-swipe sideways. Released past the threshold
// it sends "swipe-like" or "swipe-nope" on the document; otherwise it springs
// back. The card is moved directly rather than through the model so dragging
// doesn't re-render on every pointer move.

const threshold = 0.3; // of the card's width

// The entrance animation holds transform: none, which beats inline styles
const takeOver = (card) => {
  card.style.animation = "none";
};

const setStamps = (card, dx) => {
  const amount = Math.min(1, Math.abs(dx) / (card.offsetWidth * threshold));
  card.style.setProperty("--like", dx > 0 ? amount : 0);
  card.style.setProperty("--nope", dx < 0 ? amount : 0);
};

export const _flyOut = (direction) => () => {
  const card = document.querySelector("[data-swipe]");
  if (!card) return;
  const sign = direction === "like" ? 1 : -1;
  card.removeAttribute("data-swipe");
  takeOver(card);
  setStamps(card, sign * card.offsetWidth);
  card.style.transition = "transform 0.3s ease-in, opacity 0.3s ease-in";
  card.style.transform = `translateX(${sign * 130}%) rotate(${sign * 25}deg)`;
  card.style.opacity = "0";
};

export const listen = () => {
  let card = null;
  let startX = 0;
  let startY = 0;
  let dx = 0;

  document.addEventListener("pointerdown", (e) => {
    // Links and buttons on the card still work as normal
    if (e.target.closest("a, button")) return;
    card = e.target.closest("[data-swipe]");
    if (!card) return;
    startX = e.clientX;
    startY = e.clientY;
    dx = 0;
    card.setPointerCapture(e.pointerId);
    takeOver(card);
    card.style.transition = "none";
  });

  document.addEventListener("pointermove", (e) => {
    if (!card) return;
    dx = e.clientX - startX;
    const dy = (e.clientY - startY) * 0.2;
    card.style.transform = `translate(${dx}px, ${dy}px) rotate(${dx / 18}deg)`;
    setStamps(card, dx);
  });

  const release = () => {
    if (!card) return;
    const released = card;
    card = null;
    if (Math.abs(dx) > released.offsetWidth * threshold) {
      document.dispatchEvent(new CustomEvent(dx > 0 ? "swipe-like" : "swipe-nope"));
    } else {
      released.style.transition = "transform 0.3s cubic-bezier(0.2, 0.9, 0.3, 1.3)";
      released.style.transform = "";
      setStamps(released, 0);
    }
  };
  document.addEventListener("pointerup", release);
  document.addEventListener("pointercancel", release);
};
