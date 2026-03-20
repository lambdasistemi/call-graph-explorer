export const highlightCode = () => {
  requestAnimationFrame(() => {
    var el = document.querySelector(
      "#source-code code"
    );
    if (el && window.hljs) {
      el.removeAttribute("data-highlighted");
      window.hljs.highlightElement(el);
    }
  });
};
