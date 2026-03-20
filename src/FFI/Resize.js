// Make the sidebar resizable by dragging a handle.
// The handle sits between #cy and #sidebar.
export const initResize = () => {
  var handle = document.getElementById("resize-handle");
  var sidebar = document.getElementById("sidebar");
  if (!handle || !sidebar) return;

  var startX = 0;
  var startW = 0;

  handle.addEventListener("mousedown", (e) => {
    e.preventDefault();
    startX = e.clientX;
    startW = sidebar.offsetWidth;

    var onMove = (ev) => {
      var delta = startX - ev.clientX;
      var newW = Math.max(200, Math.min(800, startW + delta));
      sidebar.style.width = newW + "px";
    };

    var onUp = () => {
      document.removeEventListener("mousemove", onMove);
      document.removeEventListener("mouseup", onUp);
    };

    document.addEventListener("mousemove", onMove);
    document.addEventListener("mouseup", onUp);
  });
};
