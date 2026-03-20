// Make panels resizable by dragging handles.

function makeResizable(handleId, panelId, direction) {
  var handle = document.getElementById(handleId);
  var panel = document.getElementById(panelId);
  if (!handle || !panel) return;

  handle.addEventListener("mousedown", (e) => {
    e.preventDefault();
    var startX = e.clientX;
    var startW = panel.offsetWidth;

    var onMove = (ev) => {
      var delta = direction === "left"
        ? ev.clientX - startX
        : startX - ev.clientX;
      var newW = Math.max(150, Math.min(900, startW + delta));
      panel.style.width = newW + "px";
    };

    var onUp = () => {
      document.removeEventListener("mousemove", onMove);
      document.removeEventListener("mouseup", onUp);
    };

    document.addEventListener("mousemove", onMove);
    document.addEventListener("mouseup", onUp);
  });
}

export const initResize = () => {
  makeResizable("left-resize-handle", "repo-panel", "left");
  makeResizable("resize-handle", "sidebar", "right");
};
