// Keeps keyboard and screen reader focus in a modal dialog (an element with
// aria-modal="true") while it is open: focus moves in when it appears, Tab
// wraps around inside it, and focus goes back to where it was when it closes.

const MODAL = '[aria-modal="true"]';
const FOCUSABLE = 'a[href], button:not([disabled]), input:not([disabled]), [tabindex]:not([tabindex="-1"])';

/** @returns {Element | null} */
const openDialog = () => document.querySelector(MODAL);

/**
 * The dialog marks the control to start on (or a wrapper around it), e.g.
 * the one that closes it
 * @param {Element} dialog
 * @returns {void}
 */
const moveInto = (dialog) => {
  const marked = dialog.querySelector("[data-autofocus]");
  const start = marked?.matches(FOCUSABLE) === true ? marked : (marked ?? dialog).querySelector(FOCUSABLE);
  if (start instanceof HTMLElement) {
    start.focus();
  }
};

/**
 * Tab from the last control goes to the first, and the reverse
 * @param {KeyboardEvent} event
 * @param {Element} dialog
 * @returns {void}
 */
const keepInside = (event, dialog) => {
  const controls = [...dialog.querySelectorAll(FOCUSABLE)].filter((control) => control instanceof HTMLElement);
  const first = controls.at(0);
  const last = controls.at(-1);
  if (first === undefined || last === undefined) {
    return;
  }
  const edge = event.shiftKey ? first : last;
  if (document.activeElement !== edge && dialog.contains(document.activeElement)) {
    return;
  }
  event.preventDefault();
  (event.shiftKey ? last : first).focus();
};

/** @returns {void} */
export const watch = () => {
  /** @type {Element | null} */
  let shown = null;
  /** @type {Element | null} */
  let opener = null;

  new MutationObserver(() => {
    const dialog = openDialog();
    if (dialog !== null && shown === null) {
      opener = document.activeElement;
      moveInto(dialog);
    } else if (dialog === null && shown !== null && opener instanceof HTMLElement && opener.isConnected) {
      opener.focus();
    }
    shown = dialog;
  }).observe(document.body, { childList: true, subtree: true });

  document.addEventListener("keydown", (event) => {
    const dialog = openDialog();
    if (dialog !== null && event.key === "Tab") {
      keepInside(event, dialog);
    }
  });
};
