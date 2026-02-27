if ('serviceWorker' in navigator && location.hostname !== 'localhost') {
  window.addEventListener('load', () => {
    navigator.serviceWorker.register(new URL('./sw.js', import.meta.url).href).catch(() => {});
  });
}

window.refreshApp = function () {
  if (!navigator.serviceWorker) {
    location.reload();
    return;
  }
  navigator.serviceWorker.getRegistrations().then(function (regs) {
    return Promise.all(regs.map(function (r) { return r.unregister(); }));
  }).then(function () {
    location.reload();
  });
};
