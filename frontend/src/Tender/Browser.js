/**
 * @typedef {() => void} Done
 * @typedef {(cancelError: Error, onCancelerError: (error: Error) => void, onCancelerSuccess: Done) => void} Canceler
 * @typedef {{ latitude: number, longitude: number }} Loc
 */

const FIFTEEN_SECONDS = 15_000;
const FIVE_MINUTES = 300_000;
// Buzz, pause, buzz longer (milliseconds)
// eslint-disable-next-line no-magic-numbers
const VIBRATION = [60, 40, 120];

/**
 * The site's location permission: "granted", "denied", "prompt", or
 * "unknown" where the Permissions API can't say
 * @returns {Promise<string>}
 */
const permissionState = async () => {
  try {
    const status = await navigator.permissions.query({ name: "geolocation" });
    return status.state;
  } catch {
    return "unknown";
  }
};

/**
 * Why a refused or failed position request happened. A denial carries the
 * same code whether the site or the device refused, so the permission
 * tells them apart: granted for the site means the device said no.
 * @param {GeolocationPositionError} error
 * @returns {Promise<string>}
 */
const problemWith = async (error) => {
  if (error.code === error.TIMEOUT) {
    return "timeout";
  }
  if (error.code === error.POSITION_UNAVAILABLE) {
    return "unavailable";
  }
  const state = await permissionState();
  if (state === "granted") {
    return "device";
  }
  return state === "denied" ? "blocked" : "not-allowed";
};

/**
 * @param {unknown} error
 * @returns {Error}
 */
const asError = (error) => (error instanceof Error ? error : new Error(String(error)));

/**
 * Hands a promise's outcome to Aff's callbacks
 * @template T
 * @param {Promise<T>} promise
 * @param {(error: Error) => void} onError
 * @param {(value: T) => void} onSuccess
 * @returns {void}
 */
const settle = (promise, onError, onSuccess) => {
  /** @param {unknown} error */
  const reportUnexpected = (error) => {
    // A callback threw: surface it as an uncaught error
    throw error;
  };
  const run = async () => {
    let value;
    try {
      value = await promise;
    } catch (error) {
      onError(asError(error));
      return;
    }
    onSuccess(value);
  };
  // Nothing here can await: this is where the promise world ends
  // eslint-disable-next-line unicorn/prefer-await
  run().catch(reportUnexpected);
};

/**
 * Resolves with found(loc) or failed(problem).
 * @template R
 * @param {(loc: Loc) => R} found
 * @returns {(failed: (problem: string) => R) => (onError: (error: Error) => void, onSuccess: (result: R) => void) => Canceler}
 */
export const _locate = (found) => (failed) => (onError, onSuccess) => {
  if ("geolocation" in navigator) {
    navigator.geolocation.getCurrentPosition(
      (position) => {
        onSuccess(found({ latitude: position.coords.latitude, longitude: position.coords.longitude }));
      },
      (error) => {
        settle(problemWith(error), onError, (problem) => {
          onSuccess(failed(problem));
        });
      },
      // A fix from the last few minutes is fine for finding restaurants,
      // and saves waiting for a new one
      { timeout: FIFTEEN_SECONDS, maximumAge: FIVE_MINUTES },
    );
  } else {
    // Only available in secure contexts (https or localhost)
    onSuccess(failed("unsupported"));
  }
  return (_cancelError, _onCancelerError, onCancelerSuccess) => {
    onCancelerSuccess();
  };
};

/** @returns {void} */
export const reload = () => {
  location.reload();
};

/**
 * @param {string} path
 * @returns {() => void}
 */
export const _replacePath = (path) => () => {
  history.replaceState(null, "", path);
};

/**
 * Ignored by browsers without vibration (desktop, iOS Safari)
 * @returns {void}
 */
export const vibrate = () => {
  if ((/** @type {Partial<Navigator>} */ (navigator).vibrate) !== undefined) {
    navigator.vibrate(VIBRATION);
  }
};

/**
 * Shares the url through the native sheet where there is one, otherwise
 * copies it. Resolves with what happened.
 * @param {string} url
 * @returns {Promise<string>}
 */
const shareOrCopy = async (url) => {
  const available = /** @type {Partial<Navigator>} */ (navigator);
  if (available.share !== undefined) {
    try {
      await navigator.share({ title: "Tender", text: "Help pick where we eat", url });
      return "shared";
    } catch (error) {
      if (error instanceof Error && error.name === "AbortError") {
        return "cancelled";
      }
      throw error;
    }
  }
  if (available.clipboard !== undefined) {
    await navigator.clipboard.writeText(url);
    return "copied";
  }
  throw new Error("Sharing isn't supported in this browser");
};

/**
 * @param {string} url
 * @returns {(onError: (error: Error) => void, onSuccess: (how: string) => void) => Canceler}
 */
export const _shareOrCopy = (url) => (onError, onSuccess) => {
  settle(shareOrCopy(url), onError, onSuccess);
  return (_cancelError, _onCancelerError, onCancelerSuccess) => {
    onCancelerSuccess();
  };
};

/**
 * Storage can be unavailable (e.g. some private browsing modes); then every
 * visit joins as a new person, as before.
 * @param {string} session
 * @returns {string}
 */
const userKey = (session) => `tender:user:${session}`;

/**
 * @template M
 * @param {(user: string) => M} just
 * @returns {(nothing: M) => (session: string) => () => M}
 */
export const _rememberedUser = (just) => (nothing) => (session) => () => {
  try {
    const user = localStorage.getItem(userKey(session));
    return user === null || user === "" ? nothing : just(user);
  } catch {
    return nothing;
  }
};

/**
 * @param {string} session
 * @returns {(user: string) => () => void}
 */
export const _rememberUser = (session) => (user) => () => {
  try {
    localStorage.setItem(userKey(session), user);
  } catch {
    // Nothing to do: the next visit joins as a new person
  }
};

// The browser's own dialog is accessible and enough for a rare, one-line question
/** @returns {boolean} */
// eslint-disable-next-line no-alert
export const confirmLeave = () => confirm("Leave this session and start a new one? Your group stays in it, and Back brings you here.");

/** @returns {boolean} */
export const tabHidden = () => document.visibilityState === "hidden";

/**
 * @param {string} text
 * @returns {string}
 */
export const encodeQuery = (text) => encodeURIComponent(text);

/**
 * The key pressed, or "" for any other event
 * @param {Event} event
 * @returns {string}
 */
export const keyOf = (event) => (event instanceof KeyboardEvent ? event.key : "");
