// The site's location permission: "granted", "denied", "prompt", or
// "unknown" where the Permissions API can't say
const permissionState = () =>
  navigator.permissions
    ? navigator.permissions.query({ name: "geolocation" }).then((p) => p.state, () => "unknown")
    : Promise.resolve("unknown");

// Resolves with found(loc) or failed(problem). A denial carries the same
// code whether the site or the device refused, so the permission tells
// them apart: granted for the site means the device said no.
export const _locate = (found) => (failed) => (onError, onSuccess) => {
  if (!navigator.geolocation) {
    // Only available in secure contexts (https or localhost)
    onSuccess(failed("unsupported"));
  } else {
    navigator.geolocation.getCurrentPosition(
      (position) =>
        onSuccess(found({ latitude: position.coords.latitude, longitude: position.coords.longitude })),
      (error) => {
        if (error.code === error.TIMEOUT) onSuccess(failed("timeout"));
        else if (error.code === error.POSITION_UNAVAILABLE) onSuccess(failed("unavailable"));
        else
          permissionState().then((state) =>
            onSuccess(failed(state === "granted" ? "device" : state === "denied" ? "blocked" : "not-allowed"))
          );
      },
      // A fix from the last few minutes is fine for finding restaurants,
      // and saves waiting for a new one
      { timeout: 15000, maximumAge: 5 * 60 * 1000 }
    );
  }
  return (cancelError, onCancelerError, onCancelerSuccess) => {
    onCancelerSuccess();
  };
};

export const reload = () => window.location.reload();

export const _replacePath = (path) => () => window.history.replaceState(null, "", path);

// Ignored by browsers without vibration (desktop, iOS Safari)
export const vibrate = () => navigator.vibrate?.([60, 40, 120]);

// Native share sheet where there is one (mostly phones), otherwise copy
export const _shareOrCopy = (url) => (onError, onSuccess) => {
  if (navigator.share) {
    navigator
      .share({ title: "Tender", text: "Help pick where we eat", url })
      .then(() => onSuccess("shared"))
      .catch((e) => (e.name === "AbortError" ? onSuccess("cancelled") : onError(e)));
  } else if (navigator.clipboard) {
    navigator.clipboard.writeText(url).then(() => onSuccess("copied"), onError);
  } else {
    onError(new Error("Sharing isn't supported in this browser"));
  }
  return (cancelError, onCancelerError, onCancelerSuccess) => onCancelerSuccess();
};

// Storage can be unavailable (e.g. some private browsing modes); then every
// visit joins as a new person, as before.
const userKey = (session) => `tender:user:${session}`;

export const _rememberedUser = (just) => (nothing) => (session) => () => {
  try {
    const user = window.localStorage.getItem(userKey(session));
    return user ? just(user) : nothing;
  } catch (e) {
    return nothing;
  }
};

export const _rememberUser = (session) => (user) => () => {
  try {
    window.localStorage.setItem(userKey(session), user);
  } catch (e) {}
};

export const tabHidden = () => document.visibilityState === "hidden";
