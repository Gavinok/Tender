export const _geolocation = (onError, onSuccess) => {
  if (!navigator.geolocation) {
    // Only available in secure contexts (https or localhost)
    onError(new Error("Location is not available in this browser"));
  } else {
    navigator.geolocation.getCurrentPosition(
      (position) =>
        onSuccess({
          latitude: position.coords.latitude,
          longitude: position.coords.longitude,
        }),
      (error) => onError(new Error(error.message))
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
