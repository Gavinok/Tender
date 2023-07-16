export const _geolocation = (right) => (left) => (cb) => () => {
  navigator.geolocation.getCurrentPosition((position) => {
    cb(
      right({
        latitude: position.coords.latitude,
        longitude: position.coords.longitude,
      })
    )();
  });
};
