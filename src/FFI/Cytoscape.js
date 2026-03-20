// exposed on window for debugging
var _cy = null;

export const initCytoscape = (containerId) => (elements) => () => {
  var container = document.getElementById(containerId);
  if (!container) return;

  if (_cy) {
    _cy.destroy();
    _cy = null;
  }

  _cy = cytoscape({
    container: container,
    elements: elements,
    style: [
      {
        selector: "node",
        style: {
          label: "data(label)",
          "text-valign": "center",
          "text-halign": "center",
          "font-size": "12px",
          "font-family": "monospace",
          "background-color": "#4a90d9",
          color: "#fff",
          "text-outline-color": "#4a90d9",
          "text-outline-width": 1.5,
          width: "label",
          height: "label",
          padding: "6px",
          shape: "round-rectangle",
        },
      },
      {
        selector: "node.module",
        style: {
          "background-color": "#1a1a2e",
          "background-opacity": 0.15,
          "border-width": 2,
          "border-color": "#4a90d9",
          "border-opacity": 0.6,
          "text-valign": "top",
          "text-halign": "center",
          "font-size": "16px",
          "font-weight": "bold",
          color: "#4a90d9",
          "text-outline-width": 0,
          padding: "20px",
          shape: "round-rectangle",
        },
      },
      {
        selector: "node.type",
        style: {
          "background-color": "#8e44ad",
          "text-outline-color": "#8e44ad",
          "font-size": "13px",
          "font-weight": "bold",
          shape: "diamond",
          padding: "8px",
        },
      },
      {
        selector: "node.function",
        style: {
          "background-color": "#27ae60",
          "text-outline-color": "#27ae60",
          shape: "ellipse",
        },
      },
      {
        selector: "node.constructor",
        style: {
          "background-color": "#e67e22",
          "text-outline-color": "#e67e22",
          shape: "rectangle",
          "font-size": "10px",
        },
      },
      {
        selector: "node.field",
        style: {
          "background-color": "#3498db",
          "text-outline-color": "#3498db",
          shape: "round-rectangle",
          "font-size": "10px",
        },
      },
      {
        selector: "edge",
        style: {
          width: 0.8,
          "line-color": "#555",
          "target-arrow-color": "#555",
          "target-arrow-shape": "triangle",
          "curve-style": "bezier",
          "arrow-scale": 0.5,
          opacity: 0.5,
        },
      },
      {
        selector: "edge.type-edge",
        style: {
          "line-style": "dotted",
          "line-color": "#7f4a9e",
          "target-arrow-color": "#7f4a9e",
          opacity: 0.2,
        },
      },
      {
        selector: "node:selected",
        style: {
          "border-width": 2,
          "border-color": "#e74c3c",
        },
      },
      {
        selector: ".highlighted",
        style: {
          "background-color": "#e74c3c",
          "text-outline-color": "#e74c3c",
          "line-color": "#e74c3c",
          "target-arrow-color": "#e74c3c",
          width: 2,
          opacity: 1,
        },
      },
      {
        selector: ".dimmed",
        style: {
          opacity: 0.08,
        },
      },
    ],
    layout: { name: "preset" },
    wheelSensitivity: 0.3,
    minZoom: 0.1,
    maxZoom: 5,
  });

  // Try ELK (async), fall back to cose if unavailable
  var layoutName = _cy.layoutUtilities ? "elk" : "elk";
  try {
    _cy.layout({
      name: "elk",
      elk: {
        algorithm: "layered",
        "elk.direction": "DOWN",
        "elk.layered.spacing.nodeNodeBetweenLayers": "60",
        "elk.spacing.nodeNode": "25",
        "elk.hierarchyHandling": "INCLUDE_CHILDREN",
      },
      fit: true,
      padding: 30,
    }).run();
  } catch (e) {
    console.warn("ELK layout failed, using cose:", e);
    _cy.layout({
      name: "cose",
      nodeRepulsion: 8000,
      idealEdgeLength: 80,
      fit: true,
      padding: 30,
      animate: false,
    }).run();
  }
};

export const onNodeTap = (callback) => () => {
  if (!_cy) return;
  _cy.on("tap", "node", function (evt) {
    var node = evt.target;
    callback(node.id())(node.data())();
  });
};

export const onNodeDoubleTap = (callback) => () => {
  if (!_cy) return;
  _cy.on("dbltap", "node", function (evt) {
    var node = evt.target;
    callback(node.id())();
  });
};

export const highlightNeighborhood = (nodeId) => () => {
  if (!_cy) return;
  _cy.elements().removeClass("highlighted dimmed");
  var node = _cy.getElementById(nodeId);
  if (node.empty()) return;
  var neighborhood = node.neighborhood().add(node);
  _cy.elements().not(neighborhood).addClass("dimmed");
  neighborhood.addClass("highlighted");
};

export const clearHighlight = () => {
  if (!_cy) return;
  _cy.elements().removeClass("highlighted dimmed");
};

export const fitToNode = (nodeId) => () => {
  if (!_cy) return;
  var node = _cy.getElementById(nodeId);
  if (node.empty()) return;
  _cy.animate({
    fit: {
      eles: node.neighborhood().add(node),
      padding: 50,
    },
    duration: 300,
  });
};

export const fitAll = () => {
  if (!_cy) return;
  _cy.animate({ fit: { padding: 30 }, duration: 300 });
};

export const collapseNode = (_nodeId) => () => {};
export const expandNode = (_nodeId) => () => {};
export const collapseAll = () => {};
export const expandAll = () => {};
